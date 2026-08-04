const c = @import("../root.zig").c;
const std = @import("std");
const builtin = @import("builtin");
const logging = @import("../logging.zig");

pub fn customPanic(msg: []const u8, first_trace_addr: ?usize) noreturn {
    const alloc = std.heap.page_allocator;
    var trace: std.Io.Writer.Allocating = .init(alloc);
    defer trace.deinit();
    const trace_writer = &trace.writer;

    var buffer: std.Io.Writer.Allocating = .init(alloc);
    defer buffer.deinit();
    const writer = &buffer.writer;

    var short_msg: std.Io.Writer.Allocating = .init(alloc);
    defer short_msg.deinit();
    const short_msg_writer = &short_msg.writer;

    if (builtin.abi.isAndroid()) {
        writer.print("{s}\n", .{msg}) catch {};
    } else {
        writer.print("The program crashed with the following message:\n\"{s}\"\n\n", .{msg}) catch {};
        if (logging.path()) |log_path| {
            writer.print("Check the log file at {s}\n\n", .{log_path}) catch {};
        } else {
            writer.print("Log file unavailable.\n\n", .{}) catch {};
        }
    }

    std.debug.writeCurrentStackTrace(.{
        .first_address = first_trace_addr,
        .allow_unsafe_unwind = true,
    }, .{ .writer = trace_writer, .mode = .no_color }) catch |err| {
        writer.print("Unable to dump stack trace: {s}\n", .{@errorName(err)}) catch {};
    };
    const stacktrace = trace.written();
    if (stacktrace.len > 1024) {
        short_msg_writer.print("Stacktrace:\n{s}...\n", .{stacktrace[0..1024]}) catch {};
    }
    writer.print("Stacktrace:\n{s}\n", .{stacktrace}) catch {};

    const short_panic_msg = alloc.dupeZ(u8, short_msg.written()) catch unreachable;
    defer alloc.free(short_panic_msg);

    const full_panic_msg = buffer.written();

    logging.writePanic(full_panic_msg);
    if (builtin.abi.isAndroid()) {
        std.log.err("{s}", .{full_panic_msg});
    } else {
        std.debug.print("{s}", .{full_panic_msg});
    }

    _ = c.SDL_ShowSimpleMessageBox(c.SDL_MESSAGEBOX_ERROR, "NESkwik Error", short_panic_msg.ptr, null);

    std.process.exit(1);
}
