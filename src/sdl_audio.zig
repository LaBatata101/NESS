const std = @import("std");

const c = @import("root.zig").c;
const Sample = @import("apu/buffer.zig").Sample;

const OUT_SAMPLE_RATE: i32 = 44100;
// 4x the base 1x buffer so at 4x speed (which pushes 4*735 samples/frame) we
// always have enough ring-buffer headroom between the produce and consume sides.
const BUFFER_SIZE: usize = @divExact(@as(usize, @intCast(OUT_SAMPLE_RATE)), 15) * 4;

const BufferOut = struct {
    samples: [BUFFER_SIZE]Sample,
    input_counter: usize,
    playback_counter: usize,
    input_samples: usize,
    too_slow: bool,
};

pub const SDLAudioOut = struct {
    buffer: BufferOut,
    mutex: std.Thread.Mutex,
    cond: std.Thread.Condition,
    stream: *c.SDL_AudioStream,
    // To disable the audio output when running the test ROMs from CLI.
    disable: bool = false,
    paused: std.atomic.Value(bool) = .init(false),
    /// Normal emulation uses audio backpressure to pace its worker thread.
    /// Authoritative netplay clients run frames while servicing UI events, so
    /// they must drop excess presentation samples instead of blocking there.
    producer_blocking: std.atomic.Value(bool) = .init(true),
    current_speed: f32 = 1.0,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) !*Self {
        const self = try allocator.create(Self);
        errdefer allocator.destroy(self);

        self.* = .{
            .buffer = .{
                .samples = [_]Sample{0} ** BUFFER_SIZE,
                .input_counter = 0,
                .playback_counter = 0,
                .input_samples = 0,
                .too_slow = false,
            },
            .mutex = .{},
            .cond = .{},
            .stream = undefined,
        };

        var desired: c.SDL_AudioSpec = .{};
        desired.freq = OUT_SAMPLE_RATE;
        desired.format = c.SDL_AUDIO_S16LE;
        desired.channels = 1;

        self.stream = c.SDL_OpenAudioDeviceStream(c.SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &desired, audio_stream_callback, self) orelse return error.SDLInitFailed;
        errdefer _ = c.SDL_DestroyAudioStream(self.stream);

        _ = c.SDL_ResumeAudioStreamDevice(self.stream);

        return self;
    }

    pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        _ = c.SDL_DestroyAudioStream(self.stream);
        alloc.destroy(self);
    }

    /// Change playback speed. The SDL stream's source sample rate is set to
    /// OUT_SAMPLE_RATE * speed so that SDL resamples back to OUT_SAMPLE_RATE.
    /// At Nx speed the APU produces Nx samples/frame; SDL therefore drains Nx
    /// samples/frame, keeping the ring buffer balanced.
    pub fn setSpeed(self: *Self, speed: f32) void {
        if (self.current_speed == speed) return;
        self.current_speed = speed;

        self.mutex.lock();
        defer self.mutex.unlock();

        // Flush any stale samples so the callback doesn't play them at the
        // new rate and produce a pitch glitch.
        self.buffer.input_samples = 0;
        self.buffer.input_counter = self.buffer.playback_counter;

        const new_freq: c_int = @intFromFloat(@as(f32, OUT_SAMPLE_RATE) * speed);
        const src_spec: c.SDL_AudioSpec = .{
            .freq = new_freq,
            .format = c.SDL_AUDIO_S16LE,
            .channels = 1,
        };
        _ = c.SDL_SetAudioStreamFormat(self.stream, &src_spec, null);
    }

    pub fn setPaused(self: *Self, paused: bool) void {
        if (self.paused.swap(paused, .acq_rel) == paused) return;

        self.clearQueuedSamples();
        if (paused) {
            _ = c.SDL_PauseAudioStreamDevice(self.stream);
        } else {
            _ = c.SDL_ResumeAudioStreamDevice(self.stream);
        }
    }

    pub fn setProducerBlocking(self: *Self, blocking: bool) void {
        self.producer_blocking.store(blocking, .release);

        // Wake a producer that may already be waiting for space so it can
        // observe the new policy immediately.
        self.mutex.lock();
        defer self.mutex.unlock();
        self.cond.broadcast();
    }

    pub fn play(self: *Self, buffer: []const Sample) void {
        if (self.disable) return;

        self.mutex.lock();
        defer self.mutex.unlock();

        while (!self.paused.load(.acquire) and
            self.producer_blocking.load(.acquire) and
            self.buffer.input_samples + buffer.len > BUFFER_SIZE)
        {
            self.cond.wait(&self.mutex);
        }

        if (self.paused.load(.acquire) or
            self.buffer.input_samples + buffer.len > BUFFER_SIZE)
        {
            return;
        }

        if (self.buffer.too_slow) {
            std.log.warn("SDL: Audio transfer can't keep up", .{});
            self.buffer.too_slow = false;
        }

        var in_index: usize = 0;
        var out_index = self.buffer.input_counter;
        const out_len = BUFFER_SIZE;
        const in_len = buffer.len;

        while (in_index < in_len) {
            self.buffer.samples[out_index] = buffer[in_index];
            in_index += 1;
            out_index += 1;
            if (out_index == out_len) {
                out_index = 0;
            }
        }
        self.buffer.input_counter = (self.buffer.input_counter + in_len) % out_len;
        self.buffer.input_samples += in_len;
    }

    pub fn sampleRate(self: *const Self) f64 {
        _ = self;
        return @floatFromInt(OUT_SAMPLE_RATE);
    }

    fn clearQueuedSamples(self: *Self) void {
        {
            self.mutex.lock();
            defer self.mutex.unlock();

            self.buffer.input_samples = 0;
            self.buffer.input_counter = self.buffer.playback_counter;
            self.buffer.too_slow = false;
            self.cond.signal();
        }
        _ = c.SDL_ClearAudioStream(self.stream);
    }
};

fn audio_stream_callback(
    userdata: ?*anyopaque,
    stream_arg: ?*c.SDL_AudioStream,
    additional_amount: c_int,
    total_amount: c_int,
) callconv(.c) void {
    _ = additional_amount;
    const stream: *c.SDL_AudioStream = @ptrCast(stream_arg.?);
    const this: *SDLAudioOut = @ptrCast(@alignCast(userdata.?));

    this.mutex.lock();
    defer this.mutex.unlock();

    const sample_size: usize = @sizeOf(Sample);
    const total_bytes = total_amount;
    const max_bytes = this.buffer.input_samples * sample_size;
    const transferred_bytes_usize: usize = @intCast(@min(max_bytes, total_bytes));

    const transferred_bytes: c_int = @intCast(transferred_bytes_usize);
    const transferred_samples: usize = @divExact(transferred_bytes_usize, sample_size);

    if (transferred_bytes < total_bytes) {
        this.buffer.too_slow = true;
    }

    if (transferred_samples > 0) {
        const first_samples = BUFFER_SIZE - this.buffer.playback_counter;
        if (transferred_samples <= first_samples) {
            const src_ptr: [*]const u8 = @ptrCast(&this.buffer.samples[this.buffer.playback_counter]);
            _ = c.SDL_PutAudioStreamData(stream, src_ptr, transferred_bytes);
        } else {
            // First part
            const first_bytes: c_int = @intCast(first_samples * sample_size);
            const src1_ptr: [*]const u8 = @ptrCast(&this.buffer.samples[this.buffer.playback_counter]);
            _ = c.SDL_PutAudioStreamData(stream, src1_ptr, first_bytes);

            // Second part
            const second_samples = transferred_samples - first_samples;
            const second_bytes: c_int = @intCast(second_samples * sample_size);
            const src2_ptr: [*]const u8 = @ptrCast(&this.buffer.samples[0]);
            _ = c.SDL_PutAudioStreamData(stream, src2_ptr, second_bytes);
        }

        this.buffer.input_samples -= transferred_samples;
        this.buffer.playback_counter = (this.buffer.playback_counter + transferred_samples) % BUFFER_SIZE;
    }

    if (this.buffer.too_slow) {
        this.buffer.input_counter = this.buffer.playback_counter;
    }

    this.cond.signal();
}
