const std = @import("std");
const ness = @import("ness");

const c = ness.c;
const ControllerButton = ness.controller.ControllerButton;
const Rom = ness.Rom;
const System = ness.System;
const UI = ness.ui.UI;

const rom_path = "sample_roms/Super Mario Bros. 3 (USA).nes";
const run_time_ns = 30 * std.time.ns_per_s;
const frame_time_ns = std.time.ns_per_s / 60;
const seconds: u64 = 60;

const InputStep = struct {
    input: ControllerButton = .{},
    frames: u64,
};

// each non-empty input is held for exactly the requested frame count.
const input_sequence = [_]InputStep{
    .{ .frames = 30 },
    .{ .input = .{ .START = true }, .frames = 5 },
    .{ .frames = 1 * seconds },
    .{ .input = .{ .START = true }, .frames = 5 },
    .{ .frames = 5 * seconds },
    .{ .input = .{ .RIGHT = true }, .frames = 5 },
    .{ .frames = 30 },
    .{ .input = .{ .UP = true }, .frames = 5 },
    .{ .frames = 30 },
    .{ .input = .{ .BUTTON_A = true }, .frames = 5 },
    .{ .frames = 7 * seconds }, // wait for Goomba
    .{ .input = .{ .BUTTON_A = true }, .frames = 1 * seconds },
    .{ .input = .{ .RIGHT = true, .BUTTON_B = true }, .frames = 4 * seconds },
    .{ .input = .{ .BUTTON_A = true, .RIGHT = true }, .frames = 1 * seconds },
    .{ .frames = 30 },
    .{ .input = .{ .LEFT = true }, .frames = 15 },
    .{ .input = .{ .BUTTON_A = true }, .frames = 30 },
    .{ .input = .{ .RIGHT = true, .BUTTON_B = true }, .frames = 1 * seconds },
    .{ .frames = 30 },
    .{ .input = .{ .BUTTON_A = true }, .frames = 30 },
    .{ .input = .{ .RIGHT = true, .BUTTON_B = true }, .frames = 40 },
    .{ .frames = 30 },
    .{ .input = .{ .BUTTON_A = true }, .frames = 30 },
    .{ .input = .{ .RIGHT = true, .BUTTON_B = true }, .frames = seconds },
    .{ .frames = 30 },
    .{ .input = .{ .BUTTON_A = true, .RIGHT = true }, .frames = 1 * seconds },
    .{ .frames = 30 },
    .{ .input = .{ .RIGHT = true, .BUTTON_B = true }, .frames = 2 * seconds },
};

pub const std_options: std.Options = .{
    .log_level = .err,
};

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa;
    const io = init.io;

    var args = try init.minimal.args.iterateAllocator(allocator);
    defer args.deinit();

    _ = args.skip();
    var show_gui = false;
    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--gui")) {
            show_gui = true;
        } else if (std.mem.eql(u8, arg, "--no-gui")) {
            show_gui = false;
        } else {
            std.debug.print("Usage: profiler [--gui|--no-gui]\n", .{});
            return error.InvalidArgument;
        }
    }

    std.debug.print("Profiling {s} for 30 seconds ({s})...\n", .{
        rom_path,
        if (show_gui) "GUI" else "headless",
    });

    const frame_count = if (show_gui)
        try runWithGui(allocator, io)
    else
        try runHeadless(allocator, io);

    std.debug.print("Profiler finished after {d} frames.\n", .{frame_count});
}

fn runHeadless(allocator: std.mem.Allocator, io: std.Io) !u64 {
    _ = c.SDL_SetHint(c.SDL_HINT_NO_SIGNAL_HANDLERS, "1");
    _ = c.SDL_SetHint(c.SDL_HINT_AUDIO_DRIVER, "dummy");
    if (!c.SDL_Init(c.SDL_INIT_AUDIO)) {
        std.debug.print("Failed to initialize SDL for profiling: {s}\n", .{c.SDL_GetError()});
        return error.SDLInitFailed;
    }
    defer c.SDL_Quit();

    const rom_bytes = try std.Io.Dir.cwd().readFileAlloc(io, rom_path, allocator, .unlimited);
    defer allocator.free(rom_bytes);

    var rom = try Rom.init(allocator, io, rom_path, rom_bytes);
    defer rom.deinit();

    var system = try System.init(allocator, io, &rom, .{ .disable_audio = true });
    defer system.deinit();
    system.reset();

    const started_at = std.Io.Timestamp.now(io, .awake);
    var frame: u64 = 0;
    while (started_at.untilNow(io, .awake).toNanoseconds() < run_time_ns) {
        system.applyControllerSnapshot(.{ .player1 = inputForFrame(frame) });
        system.run_frame();
        frame += 1;
        waitForFrame(io, started_at, frame);
    }

    return frame;
}

fn runWithGui(allocator: std.mem.Allocator, io: std.Io) !u64 {
    var ui = try UI.init(allocator, io, "NESkwik Profiler", 1280, 720);
    defer ui.deinit();

    const rom_bytes = try std.Io.Dir.cwd().readFileAlloc(io, rom_path, allocator, .unlimited);
    defer allocator.free(rom_bytes);

    var rom = try Rom.init(allocator, io, rom_path, rom_bytes);
    defer rom.deinit();

    var system = try System.init(allocator, io, &rom, .{ .disable_audio = true });
    defer system.deinit();
    system.reset();

    ui.setVSync(false);
    ui.setFramerate(.unlimited);

    const started_at = std.Io.Timestamp.now(io, .awake);
    var frame: u64 = 0;
    while (!ui.shouldClose() and started_at.untilNow(io, .awake).toNanoseconds() < run_time_ns) {
        system.applyControllerSnapshot(.{ .player1 = inputForFrame(frame) });
        system.run_frame();

        ui.beginFrame();
        const root = ui.column(.{
            .sizing = .grow,
            .bg_color = ness.render.Color.black,
        });
        _ = ui.canvas(.{
            .pixel_format = c.SDL_PIXELFORMAT_ABGR8888,
            .pixels = system.frame_buffer(),
            .w = ness.NES_WIDTH,
            .h = ness.NES_HEIGHT,
            .aspect_ratio = .@"4_3",
            .bg_color = ness.render.Color.black,
        });
        root.end();
        ui.endFrame();

        frame += 1;
        waitForFrame(io, started_at, frame);
    }

    return frame;
}

fn inputForFrame(frame: u64) ControllerButton {
    var first_frame: u64 = 0;
    for (input_sequence) |step| {
        const end_frame = first_frame + step.frames;
        if (frame < end_frame) return step.input;
        first_frame = end_frame;
    }

    return .{};
}

fn waitForFrame(io: std.Io, started_at: std.Io.Timestamp, completed_frames: u64) void {
    const target_elapsed_ns = @as(i96, @intCast(completed_frames)) * frame_time_ns;
    const elapsed_ns = started_at.untilNow(io, .awake).toNanoseconds();
    if (elapsed_ns >= target_elapsed_ns) return;

    std.Io.sleep(io, .fromNanoseconds(target_elapsed_ns - elapsed_ns), .awake) catch {};
}
