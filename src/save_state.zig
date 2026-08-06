const std = @import("std");
const zeit = @import("zeit");

const utils = @import("utils/format.zig");
const System = @import("system.zig").System;
const paths = @import("utils/paths.zig");
const snapshot = @import("netplay/snapshot.zig");

pub const SLOT_COUNT = 10;

const MAGIC: [4]u8 = "NSST".*;
const VERSION: u32 = 1;
const STATE_DIR_NAME = "save-states";
const EXT = "nsst";

pub const SlotInfo = struct {
    display_time: [19]u8 = [_]u8{' '} ** 19,
};

pub fn saveSlot(alloc: std.mem.Allocator, io: std.Io, rom_name: []const u8, system: *System, slot: usize) !SlotInfo {
    std.debug.assert(slot <= SLOT_COUNT);

    var state_dir = try openStateDir(alloc, io, rom_name);
    defer state_dir.close(io);

    const slot_filename = try std.fmt.allocPrint(alloc, "slot-{}.{s}", .{ slot + 1, EXT });
    defer alloc.free(slot_filename);

    const file = try state_dir.createFile(io, slot_filename, .{});
    defer file.close(io);

    var snapshot_ = try system.saveState(alloc);
    defer snapshot_.deinit(alloc);

    snapshot_.saved_at = zeit.instant(.{ .now = io }, &zeit.utc).unixTimestamp();

    try writeSnapshot(alloc, io, file, &snapshot_);
    return .{ .display_time = utils.formatTimestamp(alloc, io, snapshot_.saved_at) };
}

pub fn loadSlot(alloc: std.mem.Allocator, io: std.Io, rom_name: []const u8, system: *System, slot: usize) !void {
    std.debug.assert(slot < SLOT_COUNT);

    const file = try openStateFile(alloc, io, rom_name, slot);
    defer file.close(io);

    const snapshot_ = try readSnapshot(alloc, io, file);
    defer {
        snapshot_.deinit(alloc);
        alloc.destroy(snapshot_);
    }

    try system.loadState(snapshot_);
}

pub fn info(alloc: std.mem.Allocator, io: std.Io, rom_name: []const u8, slot: usize) !SlotInfo {
    std.debug.assert(slot < SLOT_COUNT);

    const file = try openStateFile(alloc, io, rom_name, slot);
    defer file.close(io);

    return .{ .display_time = utils.formatTimestamp(alloc, io, try readHeaderTimestamp(io, file)) };
}

fn writeSnapshot(alloc: std.mem.Allocator, io: std.Io, file: std.Io.File, snapshot_: *const System.Snapshot) !void {
    var file_buffer: [4096]u8 = undefined;
    var file_writer = file.writer(io, &file_buffer);
    const writer = &file_writer.interface;

    try writer.writeAll(&MAGIC);
    try writeInt(writer, u32, VERSION);
    try writeInt(writer, i64, snapshot_.saved_at);

    const encoded = try snapshot.encode(alloc, snapshot_);
    defer alloc.free(encoded);

    try writeInt(writer, u32, @intCast(encoded.len));
    try writer.writeAll(encoded);
    try writer.flush();
}

fn readSnapshot(alloc: std.mem.Allocator, io: std.Io, file: std.Io.File) !*System.Snapshot {
    var file_buffer: [4096]u8 = undefined;
    var file_reader = file.reader(io, &file_buffer);
    const reader = &file_reader.interface;

    const header = try readHeader(reader);
    const encoded_len = try readInt(reader, u32);
    const encoded = try alloc.alloc(u8, encoded_len);
    defer alloc.free(encoded);
    try reader.readSliceAll(encoded);

    const snapshot_ = try snapshot.decode(alloc, encoded);
    snapshot_.saved_at = header.saved_at;
    return snapshot_;
}

const Header = struct {
    version: u32,
    saved_at: i64,
};

fn readHeader(reader: *std.Io.Reader) !Header {
    var magic: [4]u8 = undefined;
    try reader.readSliceAll(&magic);
    if (!std.mem.eql(u8, &magic, &MAGIC)) return error.InvalidSaveState;

    const version = try readInt(reader, u32);
    if (version != VERSION) return error.UnsupportedSaveStateVersion;

    return .{
        .version = version,
        .saved_at = try readInt(reader, i64),
    };
}

fn readHeaderTimestamp(io: std.Io, file: std.Io.File) !i64 {
    var file_buffer: [32]u8 = undefined;
    var file_reader = file.reader(io, &file_buffer);
    return (try readHeader(&file_reader.interface)).saved_at;
}

fn writeInt(writer: *std.Io.Writer, comptime T: type, value: T) !void {
    var buf: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &buf, value, .little);
    try writer.writeAll(&buf);
}

fn readInt(reader: *std.Io.Reader, comptime T: type) !T {
    var buf: [@sizeOf(T)]u8 = undefined;
    try reader.readSliceAll(&buf);
    return std.mem.readInt(T, &buf, .little);
}

fn openStateDir(alloc: std.mem.Allocator, io: std.Io, rom_name: []const u8) !std.Io.Dir {
    const data_dir_path = try paths.getDataDir(alloc);
    defer alloc.free(data_dir_path);

    std.Io.Dir.createDirAbsolute(io, data_dir_path, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var data_dir = try std.Io.Dir.openDirAbsolute(io, data_dir_path, .{});
    defer data_dir.close(io);

    data_dir.createDir(io, STATE_DIR_NAME, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var state_dir = try data_dir.openDir(io, STATE_DIR_NAME, .{});
    defer state_dir.close(io);

    state_dir.createDir(io, rom_name, .default_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    return try state_dir.openDir(io, rom_name, .{});
}

fn openStateFile(alloc: std.mem.Allocator, io: std.Io, rom_name: []const u8, slot: usize) !std.Io.File {
    var state_dir = try openStateDir(alloc, io, rom_name);
    defer state_dir.close(io);

    const slot_filename = try std.fmt.allocPrint(alloc, "slot-{}.{s}", .{ slot + 1, EXT });
    defer alloc.free(slot_filename);

    return try state_dir.openFile(io, slot_filename, .{});
}
