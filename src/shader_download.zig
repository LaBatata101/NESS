const std = @import("std");

pub const url = "https://github.com/libretro/slang-shaders/archive/refs/heads/master.zip";
pub const archive_root_dir_name = "slang-shaders-master";
pub const zip_file_name = "slang-shaders.zip";
pub const unknown_total = std.math.maxInt(u64);

pub const State = enum(u8) {
    idle,
    downloading,
    extracting,
    done,
    failed,
};

pub const Progress = struct {
    state: *std.atomic.Value(u8),
    bytes: *std.atomic.Value(u64),
    total_bytes: *std.atomic.Value(u64),

    pub fn setState(self: Progress, state: State) void {
        self.state.store(@intFromEnum(state), .release);
    }

    pub fn setProgress(self: Progress, bytes: u64, total_bytes: u64) void {
        self.bytes.store(bytes, .release);
        self.total_bytes.store(total_bytes, .release);
    }

    pub fn addBytes(self: Progress, amount: usize) void {
        _ = self.bytes.fetchAdd(@intCast(amount), .release);
    }
};

pub fn stateFromInt(value: u8) State {
    return switch (value) {
        @intFromEnum(State.idle) => .idle,
        @intFromEnum(State.downloading) => .downloading,
        @intFromEnum(State.extracting) => .extracting,
        @intFromEnum(State.done) => .done,
        @intFromEnum(State.failed) => .failed,
        else => .failed,
    };
}

pub fn downloadAndExtract(io: std.Io, root_path: []const u8, progress: Progress) !void {
    const alloc = std.heap.smp_allocator;
    const parent_dir = std.fs.path.dirname(root_path) orelse return error.InvalidShaderPath;
    const zip_path = try std.fs.path.join(alloc, &.{ parent_dir, zip_file_name });
    defer alloc.free(zip_path);
    const extracted_root = try std.fs.path.join(alloc, &.{ parent_dir, archive_root_dir_name });
    defer alloc.free(extracted_root);

    try std.Io.Dir.cwd().createDirPath(io, parent_dir);
    deleteFileIfExists(io, zip_path) catch |err|
        std.log.warn("failed to remove old shader zip '{s}': {s}", .{ zip_path, @errorName(err) });
    deleteTreeIfExists(io, extracted_root) catch |err|
        std.log.warn("failed to remove old shader archive dir '{s}': {s}", .{ extracted_root, @errorName(err) });
    errdefer deleteFileIfExists(io, zip_path) catch {};
    errdefer deleteTreeIfExists(io, extracted_root) catch {};

    progress.setState(.downloading);
    progress.setProgress(0, unknown_total);
    try downloadZip(io, zip_path, progress);

    progress.setState(.extracting);
    try extractZip(io, zip_path, parent_dir);

    deleteTreeIfExists(io, root_path) catch |err|
        std.log.warn("failed to remove old shader dir '{s}': {s}", .{ root_path, @errorName(err) });
    try std.Io.Dir.renameAbsolute(extracted_root, root_path, io);
    deleteFileIfExists(io, zip_path) catch {};

    progress.setState(.done);
    const bytes = progress.bytes.load(.acquire);
    if (progress.total_bytes.load(.acquire) == unknown_total) {
        progress.total_bytes.store(bytes, .release);
    }
}

fn downloadZip(io: std.Io, zip_path: []const u8, progress: Progress) !void {
    var client: std.http.Client = .{
        .allocator = std.heap.smp_allocator,
        .io = io,
    };
    defer client.deinit();

    const uri = try std.Uri.parse(url);
    var req = try client.request(.GET, uri, .{});
    defer req.deinit();

    try req.sendBodiless();

    var redirect_buffer: [8 * 1024]u8 = undefined;
    var response = try req.receiveHead(&redirect_buffer);
    if (response.head.status != .ok) return error.ShaderDownloadHttpError;

    const total = response.head.content_length orelse unknown_total;
    progress.total_bytes.store(total, .release);

    const file = try std.Io.Dir.createFileAbsolute(io, zip_path, .{});
    defer file.close(io);

    var file_buffer: [64 * 1024]u8 = undefined;
    var writer = file.writer(io, &file_buffer);
    defer writer.interface.flush() catch {};

    var transfer_buffer: [64 * 1024]u8 = undefined;
    const reader = response.reader(&transfer_buffer);
    var buffer: [64 * 1024]u8 = undefined;

    while (true) {
        const read_len = reader.readSliceShort(&buffer) catch |err| switch (err) {
            error.ReadFailed => return response.bodyErr() orelse err,
            else => |e| return e,
        };
        if (read_len == 0) break;

        try writer.interface.writeAll(buffer[0..read_len]);
        progress.addBytes(read_len);
    }

    try writer.interface.flush();
}

fn extractZip(io: std.Io, zip_path: []const u8, dest_dir: []const u8) !void {
    try std.Io.Dir.cwd().createDirPath(io, dest_dir);

    const file = try std.Io.Dir.openFileAbsolute(io, zip_path, .{});
    defer file.close(io);

    var reader_buffer: [64 * 1024]u8 = undefined;
    var reader = file.reader(io, &reader_buffer);

    var dest = try std.Io.Dir.openDirAbsolute(io, dest_dir, .{});
    defer dest.close(io);

    try std.zip.extract(dest, &reader, .{});
}

fn deleteTreeIfExists(io: std.Io, path: []const u8) !void {
    try std.Io.Dir.cwd().deleteTree(io, path);
}

fn deleteFileIfExists(io: std.Io, path: []const u8) !void {
    std.Io.Dir.deleteFileAbsolute(io, path) catch |err| switch (err) {
        error.FileNotFound => return,
        else => return err,
    };
}
