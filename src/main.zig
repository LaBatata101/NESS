const std = @import("std");

const ness = @import("8bit_emulator");
const Bus = ness.Bus;
const CPU = ness.CPU;
const Rom = ness.Rom;

const c = @cImport({
    @cInclude("SDL3/SDL.h");
    @cInclude("SDL3/SDL_main.h");
});

const Color = struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8,

    fn RGBA(r: u8, g: u8, b: u8, a: u8) @This() {
        return .{ .r = r, .g = g, .b = b, .a = a };
    }
};

const WHITE = Color.RGBA(255, 255, 255, 255);
const BLACK = Color.RGBA(0, 0, 0, 255);
const GRAY = Color.RGBA(128, 128, 128, 255);
const RED = Color.RGBA(255, 0, 0, 255);
const GREEN = Color.RGBA(0, 255, 0, 255);
const BLUE = Color.RGBA(0, 0, 255, 255);
const MAGENTA = Color.RGBA(255, 0, 255, 255);
const YELLOW = Color.RGBA(255, 255, 0, 255);
const CYAN = Color.RGBA(0, 255, 255, 255);

fn color(byte: u8) Color {
    return switch (byte) {
        0 => BLACK,
        1 => WHITE,
        2, 9 => GRAY,
        3, 10 => RED,
        4, 11 => GREEN,
        5, 12 => BLUE,
        6, 13 => MAGENTA,
        7, 14 => YELLOW,
        else => CYAN,
    };
}

const SCREEN_BUFFER_SIZE = 32 * 32 * 3;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    _ = args.skip();
    const rom_filepath = args.next() orelse {
        std.debug.print("ROM file path not provided\n", .{});
        std.process.exit(1);
    };

    const rom_abs_path = try std.fs.cwd().realpathAlloc(allocator, rom_filepath);
    defer allocator.free(rom_abs_path);

    const file = std.fs.openFileAbsolute(rom_abs_path, .{}) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.print("file not found!\n", .{});
            std.process.exit(1);
        },
        else => {
            std.debug.print("Error while opening file: {any}\n", .{err});
            std.process.exit(1);
        },
    };
    defer file.close();

    const file_size = try file.getEndPos();
    try file.seekTo(0);

    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.read(buffer);

    if (!c.SDL_Init(c.SDL_INIT_VIDEO | c.SDL_INIT_EVENTS)) {
        sdlPanic();
    }
    defer c.SDL_Quit();

    const window = c.SDL_CreateWindow("Snake Game Test", 320, 320, 0) orelse sdlPanic();
    defer c.SDL_DestroyWindow(window);

    const renderer = c.SDL_CreateRenderer(window, null) orelse sdlPanic();
    defer c.SDL_DestroyRenderer(renderer);

    _ = c.SDL_SetRenderScale(renderer, 10.0, 10.0);

    const texture = c.SDL_CreateTexture(renderer, c.SDL_PIXELFORMAT_RGB24, c.SDL_TEXTUREACCESS_TARGET, 32, 32);
    defer c.SDL_DestroyTexture(texture);

    _ = c.SDL_SetTextureScaleMode(texture, c.SDL_SCALEMODE_NEAREST);

    const T = struct {
        rnd: std.Random,
        screen_state: *[SCREEN_BUFFER_SIZE]u8,
        renderer: *c.struct_SDL_Renderer,
        texture: *c.struct_SDL_Texture,

        pub fn callback(self: @This(), cpu: *CPU) void {
            processInput(cpu);
            cpu.mem_write(0xFE, self.rnd.intRangeLessThan(u8, 1, 16));

            if (readScreenState(cpu, self.screen_state)) {
                _ = c.SDL_RenderClear(self.renderer);
                _ = c.SDL_UpdateTexture(self.texture, null, self.screen_state, 32 * 3);
                _ = c.SDL_RenderTexture(self.renderer, self.texture, null, null);
                _ = c.SDL_RenderPresent(self.renderer);
            }

            std.Thread.sleep(33_000);
        }
    };

    var seed: u64 = undefined;
    try std.posix.getrandom(std.mem.asBytes(&seed));
    var prng = std.Random.DefaultPrng.init(seed);
    const rnd = prng.random();

    var screen_state = [_]u8{0} ** SCREEN_BUFFER_SIZE;
    const t = T{
        .rnd = rnd,
        .screen_state = &screen_state,
        .renderer = renderer,
        .texture = texture,
    };

    const rom = Rom.load(buffer) catch |err| switch (err) {
        error.InvalidNesFormat => {
            std.debug.print("ROM format not supported!\n", .{});
            std.process.exit(1);
        },
        error.InvalidNesFormatVersion => {
            std.debug.print("NES2.0 format is not supported\n", .{});
            std.process.exit(1);
        },
    };
    const bus = Bus.init(rom);
    var cpu = CPU.init(bus);

    cpu.reset();
    cpu.run_with(t);
}

fn processInput(cpu: *CPU) void {
    var event: c.SDL_Event = undefined;
    while (c.SDL_PollEvent(&event)) {
        switch (event.type) {
            c.SDL_EVENT_QUIT => std.process.exit(0),
            c.SDL_EVENT_KEY_DOWN => switch (event.key.key) {
                c.SDLK_ESCAPE => std.process.exit(0),
                c.SDLK_W => cpu.mem_write(0xFF, 0x77),
                c.SDLK_S => cpu.mem_write(0xFF, 0x73),
                c.SDLK_A => cpu.mem_write(0xFF, 0x61),
                c.SDLK_D => cpu.mem_write(0xFF, 0x64),
                else => {},
            },
            else => {},
        }
    }
}

fn readScreenState(cpu: *CPU, frames: *[SCREEN_BUFFER_SIZE]u8) bool {
    var frame_idx: usize = 0;
    var update = false;

    for (0x0200..0x0600) |i| {
        const color_idx = cpu.mem_read(@intCast(i));
        const rgba = color(color_idx);

        if (frames[frame_idx] != rgba.r or frames[frame_idx + 1] != rgba.g or frames[frame_idx + 2] != rgba.b) {
            frames[frame_idx] = rgba.r;
            frames[frame_idx + 1] = rgba.g;
            frames[frame_idx + 2] = rgba.b;
            update = true;
        }
        frame_idx += 3;
    }

    return update;
}

fn sdlPanic() noreturn {
    const str = @as(?[*:0]const u8, c.SDL_GetError()) orelse "unknown error";
    @panic(std.mem.sliceTo(str, 0));
}
