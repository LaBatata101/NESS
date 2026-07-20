const std = @import("std");
const builtin = @import("builtin");
const android = if (builtin.abi.isAndroid()) @import("android") else struct {};
const ness = @import("ness");
const logging = ness.logging;

const c = ness.c;
const gui = ness.gui;
const Rom = ness.Rom;
const UI = ness.ui.UI;
const System = ness.System;
const widgets = ness.ui.widgets;
const sdlError = ness.sdlError;
const customPanic = ness.customPanic;

pub const std_options: std.Options = .{
    .logFn = if (builtin.abi.isAndroid()) androidAndFileLogFn else logging.logFn,
};

pub const panic = std.debug.FullPanic(customPanic);

// Handles window resizes on Windows.
const CallbackParams = struct { ui: *UI, app_state: *gui.AppState };
fn handleWindowsResize(userdata: ?*anyopaque, event: [*c]c.SDL_Event) callconv(.c) bool {
    if (event == null or event.*.type != c.SDL_EVENT_WINDOW_EXPOSED) return true;

    const ctx: *CallbackParams = @ptrCast(@alignCast(userdata.?));
    if (event.*.window.windowID != ctx.ui.main_window.id()) return true;

    ctx.ui.beginFrameNoSDLEvents();
    gui.drawGUI(ctx.ui, ctx.app_state);
    ctx.ui.endFrame();
    return true;
}

fn androidAndFileLogFn(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    logging.logFn(message_level, scope, format, args);
    android.logFn(message_level, scope, format, args);
}

comptime {
    if (builtin.abi.isAndroid()) {
        @export(&SDL_main, .{ .name = "SDL_main", .linkage = .strong });
    }
}

fn SDL_main() callconv(.c) void {
    if (!comptime builtin.abi.isAndroid()) {
        @compileError("SDL_main should not be called outside of Android builds");
    }
    main() catch |err| {
        std.log.err("{t}", .{err});
        if (@errorReturnTrace()) |trace| {
            std.debug.dumpStackTrace(trace.*);
        }
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    logging.init(allocator) catch |err| {
        std.debug.print("Failed to initialize log file: {s}\n", .{@errorName(err)});
    };
    defer logging.deinit(allocator);

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    var ui = try UI.init(allocator, "NESkwik", 1280, 720);
    defer ui.deinit();
    var app_state = gui.AppState.init(allocator, ui);
    defer app_state.deinit();

    var live_resize_ctx = CallbackParams{ .ui = ui, .app_state = &app_state };
    if (builtin.os.tag == .windows) sdlError(c.SDL_AddEventWatch(handleWindowsResize, &live_resize_ctx));
    defer if (builtin.os.tag == .windows) c.SDL_RemoveEventWatch(handleWindowsResize, &live_resize_ctx);

    ui.setVSync(app_state.settings.vsync);

    if (!builtin.abi.isAndroid()) {
        _ = args.skip();
        if (args.next()) |arg0| {
            if (std.mem.eql(u8, arg0, "--debug")) {
                app_state.toggleDebug();

                if (args.next()) |arg1| {
                    try app_state.loadRom(arg1);
                } else {
                    std.debug.print("ROM file path not provided\n", .{});
                    std.process.exit(1);
                }
            } else {
                try app_state.loadRom(arg0);
            }
            app_state.render_home_ui = false;
        }
    }
    ui.setFramerate(.unlimited);

    // Load the "snow" shader to be displayed in the home screen
    try ui.loadShaderPreset("snow", "builtin://border-shaders/snow.slangp");
    var result = ui.pollShaderLoad("snow");
    while (result != .done) {
        result = ui.pollShaderLoad("snow");
    }

    ui.setShaderParam("snow", "A", 0.0);
    ui.setShaderParam("snow", "LAYERS", 10.0);
    ui.setShaderParam("snow", "SPEED", 0.005);
    ui.setShaderParam("snow", "FALL_DIRECTION", 0.0);

    while (!ui.shouldClose()) {
        app_state.update();

        ui.beginFrame();
        gui.drawGUI(ui, &app_state);
        ui.endFrame();
    }
}
