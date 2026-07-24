const std = @import("std");

const Rom = @import("../rom.zig").Rom;
const buffer = @import("buffer.zig");
const SDLAudioOut = @import("../sdl_audio.zig").SDLAudioOut;
const Pulse = @import("channels/pulse.zig").Pulse;
const Triangle = @import("channels/triangle.zig").Triangle;
const Noise = @import("channels/noise.zig").Noise;
const DMC = @import("channels/dmc.zig").DMC;
const SampleBuffer = buffer.SampleBuffer;
const Waveform = buffer.Waveform;

const NTSC_TICK_LENGTH_TABLE: [2][6]u64 = [_][6]u64{
    .{ 7459, 7456, 7458, 7458, 7458, 0 },
    .{ 1, 7458, 7456, 7458, 7458, 7452 },
};

const VOLUME_MULT: i32 = ((@as(i16, 32767) / 16) / 3);

const Frame = packed struct(u8) {
    _: u6 = 0,
    suppress_irq: bool = false,
    /// `false` = 4-step, `true` = 5-step
    mode: bool = false,
};

const Jitter = union(enum) {
    Delay: struct { u64, u8 },
    None,
};

pub const APU = struct {
    alloc: std.mem.Allocator,

    pulse1: Pulse,
    pulse2: Pulse,
    triangle: Triangle,
    noise: Noise,
    frame: Frame,
    dmc: DMC,

    pulse_buffer: *SampleBuffer,
    tnd_buffer: *SampleBuffer,

    device: *SDLAudioOut,

    global_cycle: u64,
    tick_cycle: u8,
    next_tick_cycle: u64,
    next_transfer_cyc: u64,
    last_frame_cyc: u64,

    irq_interrupt: bool,

    jitter: Jitter,

    const Self = @This();

    pub const Snapshot = struct {
        pulse1: Pulse.Snapshot,
        pulse2: Pulse.Snapshot,
        triangle: Triangle.Snapshot,
        noise: Noise.Snapshot,
        frame: Frame,
        dmc: DMC.Snapshot,
        global_cycle: u64,
        tick_cycle: u8,
        next_tick_cycle: u64,
        next_transfer_cyc: u64,
        last_frame_cyc: u64,
        irq_interrupt: bool,
        jitter: Jitter,
    };

    /// `rom` is used by the DMC channel to read the PRG-ROM.
    pub fn init(allocator: std.mem.Allocator, device: *SDLAudioOut, rom: *Rom) !Self {
        const sample_rate = device.sampleRate();

        const pulse_buffer = try SampleBuffer.init(allocator, sample_rate);
        const tnd_buffer = try SampleBuffer.init(allocator, sample_rate);
        const clocks_needed = pulse_buffer.clocks_needed();

        return .{
            .alloc = allocator,

            .pulse1 = Pulse.init(false, Waveform.init(pulse_buffer, VOLUME_MULT)),
            .pulse2 = Pulse.init(true, Waveform.init(pulse_buffer, VOLUME_MULT)),
            .triangle = Triangle.init(Waveform.init(tnd_buffer, VOLUME_MULT)),
            .noise = Noise.init(Waveform.init(tnd_buffer, VOLUME_MULT)),
            .dmc = DMC.init(Waveform.init(tnd_buffer, @divTrunc(VOLUME_MULT, 3)), rom),
            .frame = .{},

            .pulse_buffer = pulse_buffer,
            .tnd_buffer = tnd_buffer,

            .device = device,

            .global_cycle = 0,
            .tick_cycle = 0,
            .next_tick_cycle = NTSC_TICK_LENGTH_TABLE[0][0],
            .next_transfer_cyc = clocks_needed,
            .last_frame_cyc = 0,

            .irq_interrupt = false,
            .jitter = Jitter.None,
        };
    }

    pub fn deinit(self: *Self) void {
        self.device.deinit(self.alloc);
        self.pulse_buffer.deinit(self.alloc);
        self.tnd_buffer.deinit(self.alloc);
    }

    pub fn reset(self: *Self) void {
        self.pulse_buffer.reset();
        self.tnd_buffer.reset();

        self.pulse1 = Pulse.init(false, Waveform.init(self.pulse_buffer, VOLUME_MULT));
        self.pulse2 = Pulse.init(true, Waveform.init(self.pulse_buffer, VOLUME_MULT));
        self.triangle = Triangle.init(Waveform.init(self.tnd_buffer, VOLUME_MULT));
        self.noise = Noise.init(Waveform.init(self.tnd_buffer, VOLUME_MULT));
        self.frame = .{};
        self.global_cycle = 0;
        self.tick_cycle = 0;
        self.next_tick_cycle = NTSC_TICK_LENGTH_TABLE[0][0];
        self.next_transfer_cyc = self.pulse_buffer.clocks_needed();
        self.last_frame_cyc = 0;
        self.irq_interrupt = false;
        self.jitter = Jitter.None;
    }

    pub fn saveState(self: *const Self) Snapshot {
        return .{
            .pulse1 = self.pulse1.saveState(),
            .pulse2 = self.pulse2.saveState(),
            .triangle = self.triangle.saveState(),
            .noise = self.noise.saveState(),
            .frame = self.frame,
            .dmc = self.dmc.saveState(),
            .global_cycle = self.global_cycle,
            .tick_cycle = self.tick_cycle,
            .next_tick_cycle = self.next_tick_cycle,
            .next_transfer_cyc = self.next_transfer_cyc,
            .last_frame_cyc = self.last_frame_cyc,
            .irq_interrupt = self.irq_interrupt,
            .jitter = self.jitter,
        };
    }

    pub fn loadState(self: *Self, snapshot: *const Snapshot) void {
        self.resetOutputBuffers();
        self.pulse1.loadState(snapshot.pulse1);
        self.pulse2.loadState(snapshot.pulse2);
        self.triangle.loadState(snapshot.triangle);
        self.noise.loadState(snapshot.noise);
        self.frame = snapshot.frame;
        self.dmc.loadState(snapshot.dmc);
        self.global_cycle = snapshot.global_cycle;
        self.tick_cycle = snapshot.tick_cycle;
        self.next_tick_cycle = snapshot.next_tick_cycle;
        self.next_transfer_cyc = snapshot.next_transfer_cyc;
        self.last_frame_cyc = snapshot.last_frame_cyc;
        self.irq_interrupt = snapshot.irq_interrupt;
        self.jitter = snapshot.jitter;
    }

    /// Clears presentation-only blip state without changing NES-visible APU
    /// state. Netplay calls this on the snapshot source so it matches the
    /// destination, whose output buffers are cleared by loadState().
    pub fn resetOutputBuffers(self: *Self) void {
        self.pulse_buffer.reset();
        self.tnd_buffer.reset();
    }

    pub fn irq_triggered(self: *const Self) bool {
        return self.irq_interrupt or self.dmc.irq_pending;
    }

    pub fn step(self: *Self) void {
        const current_cycle = self.global_cycle;
        const next_step = current_cycle + 1;

        switch (self.jitter) {
            .Delay => |data| {
                const time, const value = data;
                if (current_cycle == time) {
                    self.set_4017(value);
                    self.jitter = Jitter.None;
                }
            },
            else => {},
        }

        self.play(current_cycle, next_step);
        self.global_cycle = next_step;
        if (self.global_cycle == self.next_tick_cycle) {
            self.tick();
        }
        if (self.global_cycle == self.next_transfer_cyc) {
            self.transfer();
        }
    }

    fn tick(self: *Self) void {
        self.next_tick_cycle = self.global_cycle + NTSC_TICK_LENGTH_TABLE[@intFromBool(self.frame.mode)][self.tick_cycle];
        self.tick_cycle += 1;
        if (self.frame.mode) {
            switch (self.tick_cycle) {
                1 => {
                    self.envelope_tick();
                    self.length_tick();
                },
                2 => self.envelope_tick(),
                3 => {
                    self.envelope_tick();
                    self.length_tick();
                },
                4 => self.envelope_tick(),
                5 => self.tick_cycle = 0, //4 is the actual last tick in the cycle.
                else => self.tick_cycle = 0,
            }
        } else {
            switch (self.tick_cycle) {
                1 => self.envelope_tick(),
                2 => {
                    self.envelope_tick();
                    self.length_tick();
                },
                3 => self.envelope_tick(),
                4 => {
                    self.tick_cycle = 0;
                    self.envelope_tick();
                    self.length_tick();
                    self.raise_irq();
                },
                else => self.tick_cycle = 0,
            }
        }
    }

    fn envelope_tick(self: *Self) void {
        self.pulse1.envelope_tick();
        self.pulse2.envelope_tick();
        self.triangle.envelope_tick();
        self.noise.envelope_tick();
    }

    fn length_tick(self: *Self) void {
        self.pulse1.length_tick();
        self.pulse2.length_tick();
        self.triangle.length_tick();
        self.noise.length_tick();
    }

    fn raise_irq(self: *Self) void {
        if (!self.frame.suppress_irq) {
            self.irq_interrupt = true;
        }
    }

    fn play(self: *Self, from_cyc: u64, to_cyc: u64) void {
        const from: u32 = @intCast(from_cyc - self.last_frame_cyc);
        const to: u32 = @intCast(to_cyc - self.last_frame_cyc);
        self.pulse1.play(from, to);
        self.pulse2.play(from, to);
        self.triangle.play(from, to);
        self.noise.play(from, to);
        self.dmc.play(from, to);
    }

    fn transfer(self: *Self) void {
        const cpu_cyc = self.global_cycle;
        const cycles_since_last_frame: u32 = @intCast(cpu_cyc - self.last_frame_cyc);
        self.last_frame_cyc = cpu_cyc;

        const pulse_buffer = self.pulse_buffer;
        const tnd_buffer = self.tnd_buffer;
        pulse_buffer.end_frame(cycles_since_last_frame);
        tnd_buffer.end_frame(cycles_since_last_frame);

        const pulse_buffer_samples = pulse_buffer.read();
        const tnd_buffer_samples = tnd_buffer.read();

        std.debug.assert(pulse_buffer_samples.len == tnd_buffer_samples.len);

        var samples = std.ArrayList(buffer.Sample).initCapacity(
            self.alloc,
            // Can be either buffer length since they have the same size
            pulse_buffer_samples.len,
        ) catch @panic("Failed to initialize samples buffer\n");
        defer samples.deinit(self.alloc);

        for (pulse_buffer_samples, tnd_buffer_samples) |pulse_sample, tnd_sample| {
            samples.append(self.alloc, pulse_sample +| tnd_sample) catch
                @panic("Failed to append item to samples buffer\n");
        }

        self.next_transfer_cyc = cpu_cyc + pulse_buffer.clocks_needed();
        self.device.play(samples.items);
    }

    /// Returns the cycle number representing the next time the CPU should run
    /// the APU.
    /// Min of the next APU IRQ, the next DMC IRQ, and the next tick time. When
    /// the CPU cycle reaches this number, the CPU must run the APU.
    pub fn requested_run_cycle(self: Self) u64 {
        return self.next_tick_cycle;
    }

    fn set_4017(self: *Self, value: u8) void {
        self.frame = @bitCast(value);
        if (self.frame.suppress_irq) {
            self.irq_interrupt = false;
        }

        self.tick_cycle = 0;
        self.next_tick_cycle = self.global_cycle + NTSC_TICK_LENGTH_TABLE[@intFromBool(self.frame.mode)][0];
    }

    pub fn read_status(self: *Self) u8 {
        // self.run_to(cycle - 1);
        var status: u8 = 0;
        status |= self.pulse1.length_counter.active();
        status |= self.pulse2.length_counter.active() << 1;
        status |= self.triangle.length_counter.active() << 2;
        status |= self.noise.length_counter.active() << 3;
        status |= if (self.dmc.bytes_remaining > 0) @as(u8, 1 << 4) else 0;
        status |= if (self.irq_interrupt) @as(u8, 1 << 6) else 0;
        status |= if (self.dmc.irq_pending) @as(u8, 1 << 7) else 0;
        self.irq_interrupt = false;

        // self.run_to(cycle);
        return status;
    }

    pub fn peek_status(self: *const Self) u8 {
        var status: u8 = 0;
        status |= self.pulse1.length_counter.active();
        status |= self.pulse2.length_counter.active() << 1;
        status |= self.triangle.length_counter.active() << 2;
        status |= self.noise.length_counter.active() << 3;
        status |= if (self.dmc.bytes_remaining > 0) @as(u8, 1 << 4) else 0;
        status |= if (self.irq_interrupt) @as(u8, 1 << 6) else 0;
        status |= if (self.dmc.irq_pending) @as(u8, 1 << 7) else 0;
        return status;
    }

    pub fn write(self: *Self, addr: u16, value: u8) void {
        switch (addr) {
            0x4000, 0x4001, 0x4002, 0x4003 => self.pulse1.write(addr, value),
            0x4004, 0x4005, 0x4006, 0x4007 => self.pulse2.write(addr, value),
            0x4008, 0x4009, 0x400A, 0x400B => self.triangle.write(addr, value),
            0x400C, 0x400D, 0x400E, 0x400F => self.noise.write(addr, value),
            0x4010, 0x4011, 0x4012, 0x4013 => self.dmc.write(addr, value),
            0x4015 => {
                self.pulse1.length_counter.set_enable(value & 0b0000_0001 != 0);
                self.pulse2.length_counter.set_enable(value & 0b0000_0010 != 0);
                self.triangle.length_counter.set_enable(value & 0b0000_0100 != 0);
                self.noise.length_counter.set_enable(value & 0b0000_1000 != 0);
                self.dmc.set_enabled(value & 0b0001_0000 != 0);
                self.dmc.irq_pending = false;
            },
            0x4017 => {
                const write_cycle = self.global_cycle + 4;
                if (self.global_cycle % 2 == 0) {
                    self.jitter = .{ .Delay = .{ write_cycle, value } };
                } else {
                    self.jitter = .{ .Delay = .{ write_cycle + 1, value } };
                }
            },
            else => {
                std.log.warn("APU: attempt to write to addr 0x{X:04}", .{addr});
            },
        }
    }
};
