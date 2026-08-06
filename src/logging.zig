const std = @import("std");
const builtin = @import("builtin");
const zeit = @import("zeit");

const paths = @import("utils/paths.zig");

const LOG_FILENAME = "NESkwik.log";
const MAX_LOG_FILE_SIZE_BYTES = 10 * 1024 * 1024;

const LogContext = struct {
    io: std.Io,
};

var context: LogContext = .{ .io = undefined };
var mutex: std.Io.Mutex = .init;
var log_file: ?std.Io.File = null;
var log_path: ?[]u8 = null;
var log_timezone: zeit.TimeZone = zeit.utc;

const FILE_WRITER_BUFFER_SIZE = 1024;

pub fn init(alloc: std.mem.Allocator, io: std.Io) !void {
    if (log_file != null) return;

    var local_timezone = zeit.local(alloc, io, .{}) catch zeit.utc;
    errdefer local_timezone.deinit();

    const log_dir = try paths.getLogDir(alloc);
    defer alloc.free(log_dir);

    try std.Io.Dir.cwd().createDirPath(io, log_dir);

    const resolved_log_path = try std.fs.path.join(alloc, &.{ log_dir, LOG_FILENAME });
    errdefer alloc.free(resolved_log_path);

    const file = try std.Io.Dir.createFileAbsolute(io, resolved_log_path, .{ .truncate = false });
    errdefer file.close(io);

    const file_size = try file.length(io);
    if (file_size > MAX_LOG_FILE_SIZE_BYTES) {
        try file.setLength(io, 0);
    }

    var file_writer = file.writerStreaming(io, &.{});
    try file_writer.seekTo(if (file_size > MAX_LOG_FILE_SIZE_BYTES) 0 else file_size);

    context.io = io;
    log_file = file;
    log_path = resolved_log_path;
    log_timezone = local_timezone;
}

pub fn deinit(alloc: std.mem.Allocator) void {
    const io = context.io;

    if (log_file) |file| {
        file.sync(io) catch {};
        file.close(io);
        log_file = null;
    }

    if (log_path) |resolved_log_path| {
        alloc.free(resolved_log_path);
        log_path = null;
    }

    log_timezone.deinit();
    log_timezone = zeit.utc;
}

pub fn path() ?[]const u8 {
    return log_path;
}

pub fn logFn(
    comptime message_level: std.log.Level,
    comptime scope: @EnumLiteral(),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = scope;

    const io = context.io;

    mutex.lockUncancelable(io);
    defer mutex.unlock(io);

    if (log_file) |file| {
        var buffer: [FILE_WRITER_BUFFER_SIZE]u8 = undefined;
        var file_writer = file.writerStreaming(io, &buffer);
        const writer = &file_writer.interface;
        writeLog(writer, message_level, format, args) catch {};
        writer.flush() catch {};
    }

    if (!builtin.abi.isAndroid()) {
        var stderr_buffer: [FILE_WRITER_BUFFER_SIZE]u8 = undefined;
        const locked_stderr = std.debug.lockStderr(&stderr_buffer);
        defer std.debug.unlockStderr();
        const writer = &locked_stderr.file_writer.interface;
        writeLog(writer, message_level, format, args) catch {};
    }
}

pub fn writePanic(message: []const u8) void {
    const io = context.io;

    mutex.lockUncancelable(io);
    defer mutex.unlock(io);

    const file = log_file orelse return;
    var buffer: [FILE_WRITER_BUFFER_SIZE]u8 = undefined;
    var file_writer = file.writerStreaming(io, &buffer);
    const writer = &file_writer.interface;

    writeTimestamp(writer) catch {};
    writer.print(" [FATAL] {s}\n", .{message}) catch {};
    writer.flush() catch {};
    file.sync(io) catch {};
}

fn writeLog(
    writer: *std.Io.Writer,
    comptime message_level: std.log.Level,
    comptime format: []const u8,
    args: anytype,
) !void {
    var prefixing_writer = LinePrefixingWriter.init(writer, levelText(message_level));
    try prefixing_writer.writer.print(format, args);
    try prefixing_writer.finish();
}

fn writeTimestamp(writer: *std.Io.Writer) !void {
    const now = zeit.instant(.{ .now = context.io }, &log_timezone);
    try now.time().strftime(writer, "%Y-%m-%d %H:%M:%S");
}

fn levelText(comptime level: std.log.Level) []const u8 {
    return switch (level) {
        .err => "ERROR",
        .warn => "WARN",
        .info => "INFO",
        .debug => "DEBUG",
    };
}

const LinePrefixingWriter = struct {
    out: *std.Io.Writer,
    level_text: []const u8,
    at_line_start: bool = true,
    wrote_anything: bool = false,
    writer: std.Io.Writer = .{
        .buffer = &.{},
        .vtable = &.{ .drain = drain },
    },

    fn init(out: *std.Io.Writer, level_text: []const u8) LinePrefixingWriter {
        return .{
            .out = out,
            .level_text = level_text,
        };
    }

    fn drain(writer: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
        const self: *LinePrefixingWriter = @alignCast(@fieldParentPtr("writer", writer));

        try self.writeBytes(writer.buffered());
        writer.end = 0;

        var consumed: usize = 0;
        for (data[0 .. data.len - 1]) |bytes| {
            try self.writeBytes(bytes);
            consumed += bytes.len;
        }

        const splat_bytes = data[data.len - 1];
        for (0..splat) |_| {
            try self.writeBytes(splat_bytes);
            consumed += splat_bytes.len;
        }

        return consumed;
    }

    fn writeBytes(self: *LinePrefixingWriter, bytes: []const u8) std.Io.Writer.Error!void {
        var start: usize = 0;
        while (start < bytes.len) {
            if (self.at_line_start) try self.writePrefix();

            const newline_index = std.mem.indexOfScalarPos(u8, bytes, start, '\n') orelse {
                try self.out.writeAll(bytes[start..]);
                self.at_line_start = false;
                return;
            };

            try self.out.writeAll(bytes[start..newline_index]);
            try self.out.writeByte('\n');
            self.at_line_start = true;
            start = newline_index + 1;
        }
    }

    fn finish(self: *LinePrefixingWriter) std.Io.Writer.Error!void {
        if (!self.wrote_anything) try self.writePrefix();
        if (!self.at_line_start) try self.out.writeByte('\n');
    }

    fn writePrefix(self: *LinePrefixingWriter) std.Io.Writer.Error!void {
        writeTimestamp(self.out) catch return error.WriteFailed;
        self.out.print(" [{s}] ", .{self.level_text}) catch return error.WriteFailed;
        self.at_line_start = false;
        self.wrote_anything = true;
    }
};
