const std = @import("std");
const zeit = @import("zeit");

const utils = @import("utils/format.zig");
const APU = @import("apu/apu.zig").APU;
const Bus = @import("bus.zig").Bus;
const CPU = @import("cpu.zig").CPU;
const Mapper = @import("mappers/mapper.zig").Mapper;
const PPU = @import("ppu.zig").PPU;
const System = @import("system.zig").System;
const paths = @import("utils/paths.zig");
const compress = @import("utils/compress.zig");
const Mirroring = @import("rom.zig").Mirroring;
const Controllers = @import("controller.zig").Controllers;

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

    var snapshot = try system.saveState(alloc);
    defer snapshot.deinit(alloc);

    snapshot.saved_at = zeit.instant(.{ .now = io }, &zeit.utc).unixTimestamp();

    try writeSnapshot(alloc, io, file, &snapshot);
    return .{ .display_time = utils.formatTimestamp(alloc, io, snapshot.saved_at) };
}

pub fn loadSlot(alloc: std.mem.Allocator, io: std.Io, rom_name: []const u8, system: *System, slot: usize) !void {
    std.debug.assert(slot < SLOT_COUNT);

    const file = try openStateFile(alloc, io, rom_name, slot);
    defer file.close(io);

    const snapshot = try readSnapshot(alloc, io, file);
    defer {
        snapshot.deinit(alloc);
        alloc.destroy(snapshot);
    }

    try system.loadState(snapshot);
}

pub fn slotInfo(alloc: std.mem.Allocator, io: std.Io, rom_name: []const u8, slot: usize) !SlotInfo {
    std.debug.assert(slot < SLOT_COUNT);

    const file = try openStateFile(alloc, io, rom_name, slot);
    defer file.close(io);

    return .{ .display_time = utils.formatTimestamp(alloc, io, try readHeaderTimestamp(io, file)) };
}

fn writeSnapshot(alloc: std.mem.Allocator, io: std.Io, file: std.Io.File, snapshot: *const System.Snapshot) !void {
    var file_buffer: [4096]u8 = undefined;
    var file_writer = file.writer(io, &file_buffer);
    const writer = &file_writer.interface;

    try writer.writeAll(&MAGIC);
    try writeInt(writer, u32, VERSION);
    try writeInt(writer, i64, snapshot.saved_at);

    var body: std.Io.Writer.Allocating = .init(alloc);
    defer body.deinit();

    try writeValue(&body.writer, CPU.Snapshot, snapshot.cpu);
    try writeBusSnapshot(&body.writer, snapshot.bus);
    try writeValue(&body.writer, PPU.Snapshot, snapshot.ppu);
    try writeValue(&body.writer, APU.Snapshot, snapshot.apu);

    const compressed = try compressBytes(alloc, body.written());
    defer alloc.free(compressed);

    try writeInt(writer, u32, @intCast(body.written().len));
    try writeInt(writer, u32, @intCast(compressed.len));
    try writer.writeAll(compressed);
    try writer.flush();
}

fn readSnapshot(alloc: std.mem.Allocator, io: std.Io, file: std.Io.File) !*System.Snapshot {
    var file_buffer: [4096]u8 = undefined;
    var file_reader = file.reader(io, &file_buffer);
    const reader = &file_reader.interface;

    const header = try readHeader(reader);
    const body = try readSnapshotBody(alloc, reader, header.version);
    defer alloc.free(body);

    var body_reader: std.Io.Reader = .fixed(body);

    const snapshot = try alloc.create(System.Snapshot);
    errdefer alloc.destroy(snapshot);

    snapshot.saved_at = header.saved_at;
    snapshot.cpu = try readValue(CPU.Snapshot, &body_reader);
    snapshot.bus = try readBusSnapshot(alloc, &body_reader);
    snapshot.ppu = try readValue(PPU.Snapshot, &body_reader);
    snapshot.apu = try readValue(APU.Snapshot, &body_reader);

    return snapshot;
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

fn readSnapshotBody(alloc: std.mem.Allocator, reader: *std.Io.Reader, version: u32) ![]u8 {
    if (version != VERSION) return error.UnsupportedSaveStateVersion;

    const uncompressed_len = try readInt(reader, u32);
    const compressed_len = try readInt(reader, u32);
    const compressed = try alloc.alloc(u8, compressed_len);
    defer alloc.free(compressed);
    try reader.readSliceAll(compressed);

    return try decompressBytes(alloc, compressed, uncompressed_len);
}

fn writeBusSnapshot(writer: *std.Io.Writer, snapshot: Bus.Snapshot) !void {
    try writer.writeAll(&snapshot.ram);
    try writeInt(writer, u64, snapshot.cycles);
    try writeInt(writer, u8, snapshot.open_bus);
    try writeInt(writer, u8, snapshot.dma_start_delay);
    try writeInt(writer, u16, snapshot.dma_cycles);
    try writeValue(writer, Controllers, snapshot.controllers);
    try writeMapperSnapshot(writer, snapshot.rom.mapper);
}

fn readBusSnapshot(alloc: std.mem.Allocator, reader: *std.Io.Reader) !Bus.Snapshot {
    var ram: [2048]u8 = undefined;
    try reader.readSliceAll(&ram);
    return .{
        .ram = ram,
        .cycles = try readInt(reader, u64),
        .open_bus = try readInt(reader, u8),
        .dma_start_delay = try readInt(reader, u8),
        .dma_cycles = try readInt(reader, u16),
        .controllers = try readValue(Controllers, reader),
        .rom = .{ .mapper = try readMapperSnapshot(alloc, reader) },
    };
}

fn writeMapperSnapshot(writer: *std.Io.Writer, snapshot: Mapper.Snapshot) !void {
    switch (snapshot) {
        .mapper0 => |value| {
            try writeInt(writer, u8, 0);
            try writeSlice(writer, value.prg_ram);
            try writeSlice(writer, value.chr_ram);
        },
        .mapper1 => |value| {
            try writeInt(writer, u8, 1);
            try writeValue(writer, u8, value.load_register);
            try writeValue(writer, u8, value.write_index);
            try writeValue(writer, u8, value.control);
            try writeValue(writer, u8, value.prg_bank);
            try writeValue(writer, u8, value.chr_bank_1);
            try writeValue(writer, u8, value.chr_bank_2);
            try writeSlice(writer, value.prg_ram);
            try writeSlice(writer, value.chr_ram);
        },
        .mapper2 => |value| {
            try writeInt(writer, u8, 2);
            try writeValue(writer, u8, value.selected_bank);
            try writeSlice(writer, value.chr_ram);
        },
        .mapper3 => |value| {
            try writeInt(writer, u8, 3);
            try writeValue(writer, u8, value.selected_chr_bank);
            try writeSlice(writer, value.prg_ram);
        },
        .mapper4 => |value| {
            try writeInt(writer, u8, 4);
            try writeValue(writer, [10]usize, value.bank_registers);
            try writeValue(writer, u8, value.bank_select);
            try writeValue(writer, bool, value.prg_inversion);
            try writeValue(writer, bool, value.chr_inversion);
            try writeValue(writer, bool, value.irq_flag);
            try writeValue(writer, u8, value.irq_counter);
            try writeValue(writer, bool, value.irq_reload_flag);
            try writeValue(writer, u8, value.irq_counter_reload);
            try writeValue(writer, bool, value.irq_enabled);
            try writeValue(writer, bool, value.ppu_a12);
            try writeValue(writer, u64, value.ppu_a12_low_cycle);
            try writeValue(writer, Mirroring, value.mirroring_mode);
            try writeSlice(writer, value.prg_ram);
            try writeSlice(writer, value.chr_ram);
        },
    }
}

fn readMapperSnapshot(alloc: std.mem.Allocator, reader: *std.Io.Reader) !Mapper.Snapshot {
    return switch (try readInt(reader, u8)) {
        0 => .{
            .mapper0 = .{
                .prg_ram = try readSlice(alloc, reader),
                .chr_ram = try readSlice(alloc, reader),
            },
        },
        1 => .{
            .mapper1 = .{
                .load_register = try readValue(u8, reader),
                .write_index = try readValue(u8, reader),
                .control = try readValue(u8, reader),
                .prg_bank = try readValue(u8, reader),
                .chr_bank_1 = try readValue(u8, reader),
                .chr_bank_2 = try readValue(u8, reader),
                .prg_ram = try readSlice(alloc, reader),
                .chr_ram = try readSlice(alloc, reader),
            },
        },
        2 => .{
            .mapper2 = .{
                .selected_bank = try readValue(u8, reader),
                .chr_ram = try readSlice(alloc, reader),
            },
        },
        3 => .{
            .mapper3 = .{
                .selected_chr_bank = try readValue(u8, reader),
                .prg_ram = try readSlice(alloc, reader),
            },
        },
        4 => .{
            .mapper4 = .{
                .bank_registers = try readValue([10]usize, reader),
                .bank_select = try readValue(u8, reader),
                .prg_inversion = try readValue(bool, reader),
                .chr_inversion = try readValue(bool, reader),
                .irq_flag = try readValue(bool, reader),
                .irq_counter = try readValue(u8, reader),
                .irq_reload_flag = try readValue(bool, reader),
                .irq_counter_reload = try readValue(u8, reader),
                .irq_enabled = try readValue(bool, reader),
                .ppu_a12 = try readValue(bool, reader),
                .ppu_a12_low_cycle = try readValue(u64, reader),
                .mirroring_mode = try readValue(Mirroring, reader),
                .prg_ram = try readSlice(alloc, reader),
                .chr_ram = try readSlice(alloc, reader),
            },
        },
        else => error.UnsupportedMapperSnapshot,
    };
}

fn writeValue(writer: *std.Io.Writer, comptime T: type, value: T) !void {
    try writer.writeAll(std.mem.asBytes(&value));
}

fn readValue(comptime T: type, reader: *std.Io.Reader) !T {
    var value: T = undefined;
    try reader.readSliceAll(std.mem.asBytes(&value));
    return value;
}

fn writeSlice(writer: *std.Io.Writer, bytes: []const u8) !void {
    try writeInt(writer, u32, @intCast(bytes.len));
    try writer.writeAll(bytes);
}

fn readSlice(alloc: std.mem.Allocator, reader: *std.Io.Reader) ![]u8 {
    const len = try readInt(reader, u32);
    const bytes = try alloc.alloc(u8, len);
    try reader.readSliceAll(bytes);
    return bytes;
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

fn compressBytes(alloc: std.mem.Allocator, bytes: []const u8) ![]u8 {
    var reader: std.Io.Reader = .fixed(bytes);
    var writer: std.Io.Writer.Allocating = .init(alloc);
    errdefer writer.deinit();

    try compress.compressAlloc(alloc, &reader, &writer.writer, .{});
    return try writer.toOwnedSlice();
}

fn decompressBytes(alloc: std.mem.Allocator, compressed: []const u8, expected_len: usize) ![]u8 {
    var reader: std.Io.Reader = .fixed(compressed);
    const output = try alloc.alloc(u8, expected_len);
    errdefer alloc.free(output);
    var writer: std.Io.Writer = .fixed(output);

    var decompressor: std.compress.flate.Decompress = .init(&reader, .gzip, &.{});
    _ = decompressor.reader.streamRemaining(&writer) catch |err| switch (err) {
        error.ReadFailed, error.WriteFailed => return error.InvalidCompressedSaveState,
    };
    if (writer.end != expected_len) return error.InvalidCompressedSaveState;

    return output;
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
