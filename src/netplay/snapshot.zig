const std = @import("std");

const APU = @import("../apu/apu.zig").APU;
const Bus = @import("../bus.zig").Bus;
const Controllers = @import("../controller.zig").Controllers;
const CPU = @import("../cpu.zig").CPU;
const Mapper = @import("../mappers/mapper.zig").Mapper;
const PPU = @import("../ppu.zig").PPU;
const Mirroring = @import("../rom.zig").Mirroring;
const System = @import("../system.zig").System;
const compress = @import("../utils/compress.zig");
const protocol = @import("protocol.zig");

const MAGIC: [4]u8 = "NSNW".*;
const HEADER_SIZE = MAGIC.len + @sizeOf(u32) * 2;

pub const ComponentDigests = struct {
    cpu: protocol.Digest,
    bus: protocol.Digest,
    ppu: protocol.Digest,
    apu: protocol.Digest,
};

/// Encodes emulation state field-by-field in a fixed little-endian
/// representation, then gzip compresses it. `Snapshot.saved_at` is persistence
/// metadata and intentionally never enters the network representation.
pub fn encode(alloc: std.mem.Allocator, snapshot: *const System.Snapshot) ![]u8 {
    var body: std.Io.Writer.Allocating = .init(alloc);
    defer body.deinit();

    try writeCanonicalRef(&body.writer, CPU.Snapshot, &snapshot.cpu);
    try writeCanonicalBus(&body.writer, &snapshot.bus);
    try writeCanonicalRef(&body.writer, PPU.Snapshot, &snapshot.ppu);
    try writeCanonicalRef(&body.writer, APU.Snapshot, &snapshot.apu);
    if (body.written().len > protocol.max_snapshot_size) return error.SnapshotTooLarge;

    const compressed = try compressBytes(alloc, body.written());
    defer alloc.free(compressed);
    if (compressed.len > protocol.max_snapshot_size) return error.SnapshotTooLarge;

    const result = try alloc.alloc(u8, HEADER_SIZE + compressed.len);
    @memcpy(result[0..MAGIC.len], &MAGIC);
    std.mem.writeInt(u32, result[4..8], @intCast(body.written().len), .little);
    std.mem.writeInt(u32, result[8..12], @intCast(compressed.len), .little);
    @memcpy(result[HEADER_SIZE..], compressed);

    return result;
}

pub fn decode(alloc: std.mem.Allocator, encoded: []const u8) !*System.Snapshot {
    if (encoded.len < HEADER_SIZE or !std.mem.eql(u8, encoded[0..MAGIC.len], &MAGIC))
        return error.InvalidNetworkSnapshot;

    const uncompressed_len = std.mem.readInt(u32, encoded[4..8], .little);
    const compressed_len = std.mem.readInt(u32, encoded[8..12], .little);
    if (uncompressed_len > protocol.max_snapshot_size or compressed_len > protocol.max_snapshot_size)
        return error.SnapshotTooLarge;
    if (encoded.len != HEADER_SIZE + @as(usize, compressed_len))
        return error.InvalidNetworkSnapshot;

    const body = try decompressBytes(alloc, encoded[HEADER_SIZE..], uncompressed_len);
    defer alloc.free(body);

    var reader: std.Io.Reader = .fixed(body);
    const snapshot = try alloc.create(System.Snapshot);
    errdefer alloc.destroy(snapshot);

    // Network snapshots have no persistence timestamp.
    snapshot.saved_at = 0;
    try readCanonicalInto(alloc, CPU.Snapshot, &snapshot.cpu, &reader);
    snapshot.bus = try readCanonicalBus(alloc, &reader);
    errdefer snapshot.bus.deinit(alloc);
    try readCanonicalInto(alloc, PPU.Snapshot, &snapshot.ppu, &reader);
    errdefer snapshot.ppu.deinit(alloc);
    try readCanonicalInto(alloc, APU.Snapshot, &snapshot.apu, &reader);
    if (reader.seek != reader.end) return error.InvalidNetworkSnapshot;

    return snapshot;
}

/// Hashes only the emulation state represented on the network. In particular,
/// changing `Snapshot.saved_at` cannot affect this digest.
pub fn digest(snapshot: *const System.Snapshot) !protocol.Digest {
    var buffer: std.Io.Writer.Allocating = .init(std.heap.page_allocator);
    defer buffer.deinit();

    try writeCanonicalRef(&buffer.writer, CPU.Snapshot, &snapshot.cpu);
    try writeCanonicalBus(&buffer.writer, &snapshot.bus);
    try writeCanonicalRef(&buffer.writer, PPU.Snapshot, &snapshot.ppu);
    try writeCanonicalApuDigest(&buffer.writer, snapshot.apu);
    if (buffer.written().len > protocol.max_snapshot_size) return error.SnapshotTooLarge;

    var result: protocol.Digest = undefined;
    std.crypto.hash.Blake3.hash(buffer.written(), &result, .{});
    return result;
}

/// Diagnostic hashes using the same canonical encoders as the full netplay
/// digest. They identify a divergent subsystem without additional wire data.
pub fn componentDigests(snapshot: *const System.Snapshot) !ComponentDigests {
    return .{
        .cpu = try digestCanonical(CPU.Snapshot, &snapshot.cpu),
        .bus = try digestCanonicalBus(&snapshot.bus),
        .ppu = try digestCanonical(PPU.Snapshot, &snapshot.ppu),
        .apu = try digestCanonicalApu(&snapshot.apu),
    };
}

fn digestCanonical(comptime T: type, value: *const T) !protocol.Digest {
    var buffer: std.Io.Writer.Allocating = .init(std.heap.page_allocator);
    defer buffer.deinit();

    try writeCanonicalRef(&buffer.writer, T, value);

    var result: protocol.Digest = undefined;
    std.crypto.hash.Blake3.hash(buffer.written(), &result, .{});
    return result;
}

fn digestCanonicalBus(value: *const Bus.Snapshot) !protocol.Digest {
    var buffer: std.Io.Writer.Allocating = .init(std.heap.page_allocator);
    defer buffer.deinit();

    try writeCanonicalBus(&buffer.writer, value);

    var result: protocol.Digest = undefined;
    std.crypto.hash.Blake3.hash(buffer.written(), &result, .{});
    return result;
}

fn digestCanonicalApu(value: *const APU.Snapshot) !protocol.Digest {
    var buffer: std.Io.Writer.Allocating = .init(std.heap.page_allocator);
    defer buffer.deinit();

    try writeCanonicalApuDigest(&buffer.writer, value.*);

    var result: protocol.Digest = undefined;
    std.crypto.hash.Blake3.hash(buffer.written(), &result, .{});
    return result;
}

/// Hashes NES-visible APU state while excluding audio-renderer bookkeeping.
/// These fields depend on the local output-buffer phase and do not affect
/// CPU-visible APU behavior.
fn writeCanonicalApuDigest(writer: *std.Io.Writer, value: APU.Snapshot) !void {
    var core = value;
    core.pulse1.waveform_last_amp = 0;
    core.pulse2.waveform_last_amp = 0;
    core.triangle.waveform_last_amp = 0;
    core.noise.waveform_last_amp = 0;
    core.dmc.waveform_last_amp = 0;
    core.next_transfer_cyc = 0;
    core.last_frame_cyc = 0;
    try writeCanonical(writer, APU.Snapshot, core);
}

fn writeCanonicalBus(writer: *std.Io.Writer, snapshot: *const Bus.Snapshot) !void {
    try writer.writeAll(&snapshot.ram);
    try writeCanonical(writer, u64, snapshot.cycles);
    try writeCanonical(writer, u8, snapshot.open_bus);
    try writeCanonical(writer, u8, snapshot.dma_start_delay);
    try writeCanonical(writer, u16, snapshot.dma_cycles);
    try writeCanonical(writer, Controllers, snapshot.controllers);
    try writeCanonicalMapper(writer, snapshot.rom.mapper);
}

fn readCanonicalBus(alloc: std.mem.Allocator, reader: *std.Io.Reader) !Bus.Snapshot {
    var ram: [2048]u8 = undefined;
    try reader.readSliceAll(&ram);

    return .{
        .ram = ram,
        .cycles = try readCanonical(alloc, u64, reader),
        .open_bus = try readCanonical(alloc, u8, reader),
        .dma_start_delay = try readCanonical(alloc, u8, reader),
        .dma_cycles = try readCanonical(alloc, u16, reader),
        .controllers = try readCanonical(alloc, Controllers, reader),
        .rom = .{ .mapper = try readCanonicalMapper(alloc, reader) },
    };
}

fn writeCanonicalMapper(writer: *std.Io.Writer, snapshot: Mapper.Snapshot) !void {
    switch (snapshot) {
        .mapper0 => |value| {
            try writeCanonical(writer, u8, 0);
            try writeCanonicalSlice(writer, value.prg_ram);
            try writeCanonicalSlice(writer, value.chr_ram);
        },
        .mapper1 => |value| {
            try writeCanonical(writer, u8, 1);
            try writeCanonical(writer, u8, value.load_register);
            try writeCanonical(writer, u8, value.write_index);
            try writeCanonical(writer, u8, value.control);
            try writeCanonical(writer, u8, value.prg_bank);
            try writeCanonical(writer, u8, value.chr_bank_1);
            try writeCanonical(writer, u8, value.chr_bank_2);
            try writeCanonicalSlice(writer, value.prg_ram);
            try writeCanonicalSlice(writer, value.chr_ram);
        },
        .mapper2 => |value| {
            try writeCanonical(writer, u8, 2);
            try writeCanonical(writer, u8, value.selected_bank);
            try writeCanonicalSlice(writer, value.chr_ram);
        },
        .mapper3 => |value| {
            try writeCanonical(writer, u8, 3);
            try writeCanonical(writer, u8, value.selected_chr_bank);
            try writeCanonicalSlice(writer, value.prg_ram);
        },
        .mapper4 => |value| {
            try writeCanonical(writer, u8, 4);
            for (value.bank_registers) |bank| try writeCanonical(writer, u64, bank);
            try writeCanonical(writer, u8, value.bank_select);
            try writeCanonical(writer, bool, value.prg_inversion);
            try writeCanonical(writer, bool, value.chr_inversion);
            try writeCanonical(writer, bool, value.irq_flag);
            try writeCanonical(writer, u8, value.irq_counter);
            try writeCanonical(writer, bool, value.irq_reload_flag);
            try writeCanonical(writer, u8, value.irq_counter_reload);
            try writeCanonical(writer, bool, value.irq_enabled);
            try writeCanonical(writer, bool, value.ppu_a12);
            try writeCanonical(writer, u64, value.ppu_a12_low_cycle);
            try writeCanonical(writer, Mirroring, value.mirroring_mode);
            try writeCanonicalSlice(writer, value.prg_ram);
            try writeCanonicalSlice(writer, value.chr_ram);
        },
    }
}

fn readCanonicalMapper(alloc: std.mem.Allocator, reader: *std.Io.Reader) !Mapper.Snapshot {
    return switch (try readCanonical(alloc, u8, reader)) {
        0 => blk: {
            const prg = try readCanonicalSlice(alloc, reader);
            errdefer alloc.free(prg);
            const chr = try readCanonicalSlice(alloc, reader);
            break :blk .{ .mapper0 = .{ .prg_ram = prg, .chr_ram = chr } };
        },
        1 => blk: {
            const load_register = try readCanonical(alloc, u8, reader);
            const write_index = try readCanonical(alloc, u8, reader);
            const control = try readCanonical(alloc, u8, reader);
            const prg_bank = try readCanonical(alloc, u8, reader);
            const chr_bank_1 = try readCanonical(alloc, u8, reader);
            const chr_bank_2 = try readCanonical(alloc, u8, reader);
            const prg = try readCanonicalSlice(alloc, reader);
            errdefer alloc.free(prg);
            const chr = try readCanonicalSlice(alloc, reader);
            break :blk .{ .mapper1 = .{
                .load_register = load_register,
                .write_index = write_index,
                .control = control,
                .prg_bank = prg_bank,
                .chr_bank_1 = chr_bank_1,
                .chr_bank_2 = chr_bank_2,
                .prg_ram = prg,
                .chr_ram = chr,
            } };
        },
        2 => .{ .mapper2 = .{
            .selected_bank = try readCanonical(alloc, u8, reader),
            .chr_ram = try readCanonicalSlice(alloc, reader),
        } },
        3 => .{ .mapper3 = .{
            .selected_chr_bank = try readCanonical(alloc, u8, reader),
            .prg_ram = try readCanonicalSlice(alloc, reader),
        } },
        4 => blk: {
            var banks: [10]usize = undefined;
            for (&banks) |*bank| bank.* = @intCast(try readCanonical(alloc, u64, reader));
            const bank_select = try readCanonical(alloc, u8, reader);
            const prg_inversion = try readCanonical(alloc, bool, reader);
            const chr_inversion = try readCanonical(alloc, bool, reader);
            const irq_flag = try readCanonical(alloc, bool, reader);
            const irq_counter = try readCanonical(alloc, u8, reader);
            const irq_reload_flag = try readCanonical(alloc, bool, reader);
            const irq_counter_reload = try readCanonical(alloc, u8, reader);
            const irq_enabled = try readCanonical(alloc, bool, reader);
            const ppu_a12 = try readCanonical(alloc, bool, reader);
            const ppu_a12_low_cycle = try readCanonical(alloc, u64, reader);
            const mirroring_mode = try readCanonical(alloc, Mirroring, reader);
            const prg = try readCanonicalSlice(alloc, reader);
            errdefer alloc.free(prg);
            const chr = try readCanonicalSlice(alloc, reader);
            break :blk .{ .mapper4 = .{
                .bank_registers = banks,
                .bank_select = bank_select,
                .prg_inversion = prg_inversion,
                .chr_inversion = chr_inversion,
                .irq_flag = irq_flag,
                .irq_counter = irq_counter,
                .irq_reload_flag = irq_reload_flag,
                .irq_counter_reload = irq_counter_reload,
                .irq_enabled = irq_enabled,
                .ppu_a12 = ppu_a12,
                .ppu_a12_low_cycle = ppu_a12_low_cycle,
                .mirroring_mode = mirroring_mode,
                .prg_ram = prg,
                .chr_ram = chr,
            } };
        },
        else => error.UnsupportedMapperSnapshot,
    };
}

fn writeCanonicalSlice(writer: *std.Io.Writer, value: []const u8) !void {
    if (value.len > protocol.max_snapshot_size) return error.SnapshotTooLarge;
    try writeCanonical(writer, u32, @intCast(value.len));
    try writer.writeAll(value);
}

fn readCanonicalSlice(alloc: std.mem.Allocator, reader: *std.Io.Reader) ![]u8 {
    const len = try readCanonical(alloc, u32, reader);
    if (len > protocol.max_snapshot_size) return error.SnapshotTooLarge;

    const value = try alloc.alloc(u8, len);
    errdefer alloc.free(value);
    try reader.readSliceAll(value);
    return value;
}

fn writeCanonical(writer: *std.Io.Writer, comptime T: type, value: T) !void {
    try writeCanonicalRef(writer, T, &value);
}

fn writeCanonicalRef(writer: *std.Io.Writer, comptime T: type, value: *const T) !void {
    switch (@typeInfo(T)) {
        .void => {},
        .bool => try writer.writeByte(@intFromBool(value.*)),
        .int => |info| {
            const ValueBits = std.meta.Int(.unsigned, info.bits);
            const byte_len = (info.bits + 7) / 8;
            const Storage = std.meta.Int(.unsigned, byte_len * 8);
            const value_bits: ValueBits = @bitCast(value.*);
            var buffer: [byte_len]u8 = undefined;
            std.mem.writeInt(Storage, &buffer, @intCast(value_bits), .little);
            try writer.writeAll(&buffer);
        },
        .float => |info| {
            const U = std.meta.Int(.unsigned, info.bits);
            const bits: U = @bitCast(value.*);
            try writeCanonicalRef(writer, U, &bits);
        },
        .@"enum" => |info| {
            const tag: info.tag_type = @intFromEnum(value.*);
            try writeCanonicalRef(writer, info.tag_type, &tag);
        },
        .optional => |info| {
            if (value.*) |child| {
                try writer.writeByte(1);
                try writeCanonicalRef(writer, info.child, &child);
            } else try writer.writeByte(0);
        },
        .array => |info| for (value) |*element| try writeCanonicalRef(writer, info.child, element),
        .pointer => |info| {
            if (info.size != .one) @compileError("canonical snapshot pointers must point to one value");
            try writeCanonicalRef(writer, info.child, value.*);
        },
        .@"struct" => |info| if (info.layout == .@"packed") {
            inline for (info.fields) |field| {
                const field_value = @field(value.*, field.name);
                try writeCanonicalRef(writer, field.type, &field_value);
            }
        } else inline for (info.fields) |field| {
            try writeCanonicalRef(writer, field.type, &@field(value.*, field.name));
        },
        .@"union" => |info| {
            const Tag = info.tag_type orelse @compileError("canonical snapshot unions must be tagged");
            const tag = std.meta.activeTag(value.*);
            try writeCanonicalRef(writer, Tag, &tag);
            inline for (info.fields) |field| {
                if (tag == @field(Tag, field.name)) {
                    const payload = @field(value.*, field.name);
                    try writeCanonicalRef(writer, field.type, &payload);
                }
            }
        },
        else => @compileError("unsupported canonical snapshot type: " ++ @typeName(T)),
    }
}

fn readCanonical(alloc: std.mem.Allocator, comptime T: type, reader: *std.Io.Reader) !T {
    var result: T = undefined;
    try readCanonicalInto(alloc, T, &result, reader);
    return result;
}

fn readCanonicalInto(alloc: std.mem.Allocator, comptime T: type, result: *T, reader: *std.Io.Reader) !void {
    switch (@typeInfo(T)) {
        .void => result.* = {},
        .bool => result.* = switch (try reader.takeByte()) {
            0 => false,
            1 => true,
            else => return error.InvalidCanonicalBoolean,
        },
        .int => |info| {
            const ValueBits = std.meta.Int(.unsigned, info.bits);
            const byte_len = (info.bits + 7) / 8;
            const Storage = std.meta.Int(.unsigned, byte_len * 8);
            var buffer: [byte_len]u8 = undefined;
            try reader.readSliceAll(&buffer);
            const stored = std.mem.readInt(Storage, &buffer, .little);
            const bits: ValueBits = @truncate(stored);
            result.* = @bitCast(bits);
        },
        .float => |info| {
            const U = std.meta.Int(.unsigned, info.bits);
            var bits: U = undefined;
            try readCanonicalInto(alloc, U, &bits, reader);
            result.* = @bitCast(bits);
        },
        .@"enum" => |info| {
            var tag: info.tag_type = undefined;
            try readCanonicalInto(alloc, info.tag_type, &tag, reader);
            result.* = std.enums.fromInt(T, tag) orelse return error.InvalidCanonicalEnum;
        },
        .optional => |info| switch (try reader.takeByte()) {
            0 => result.* = null,
            1 => {
                var child: info.child = undefined;
                try readCanonicalInto(alloc, info.child, &child, reader);
                result.* = child;
            },
            else => return error.InvalidCanonicalBoolean,
        },
        .array => |info| for (result) |*element| try readCanonicalInto(alloc, info.child, element, reader),
        .pointer => |info| {
            if (info.size != .one) @compileError("canonical snapshot pointers must point to one value");
            const child = try alloc.create(info.child);
            errdefer alloc.destroy(child);
            try readCanonicalInto(alloc, info.child, child, reader);
            result.* = child;
        },
        .@"struct" => |info| if (info.layout == .@"packed") {
            inline for (info.fields) |field| {
                var field_value: field.type = undefined;
                try readCanonicalInto(alloc, field.type, &field_value, reader);
                @field(result.*, field.name) = field_value;
            }
        } else inline for (info.fields, 0..) |field, index| {
            readCanonicalInto(alloc, field.type, &@field(result.*, field.name), reader) catch |err| {
                inline for (info.fields[0..index]) |decoded_field| {
                    deinitCanonicalValue(decoded_field.type, &@field(result.*, decoded_field.name), alloc);
                }
                return err;
            };
        },
        .@"union" => |info| {
            const Tag = info.tag_type orelse @compileError("canonical snapshot unions must be tagged");
            const tag = try readCanonical(alloc, Tag, reader);
            inline for (info.fields) |field| {
                if (tag == @field(Tag, field.name)) {
                    var payload: field.type = undefined;
                    try readCanonicalInto(alloc, field.type, &payload, reader);
                    result.* = @unionInit(T, field.name, payload);
                    return;
                }
            }
            return error.InvalidCanonicalUnion;
        },
        else => @compileError("unsupported canonical snapshot type: " ++ @typeName(T)),
    }
}

fn deinitCanonicalValue(comptime T: type, value: *T, alloc: std.mem.Allocator) void {
    switch (@typeInfo(T)) {
        .optional => |info| if (value.*) |*child| deinitCanonicalValue(info.child, child, alloc),
        .array => |info| for (value) |*element| deinitCanonicalValue(info.child, element, alloc),
        .pointer => |info| {
            if (info.size != .one) return;
            const child: *info.child = @constCast(value.*);
            deinitCanonicalValue(info.child, child, alloc);
            alloc.destroy(child);
        },
        .@"struct" => |info| {
            if (info.layout == .@"packed") return;
            inline for (info.fields) |field| {
                deinitCanonicalValue(field.type, &@field(value.*, field.name), alloc);
            }
        },
        .@"union" => |info| {
            const Tag = info.tag_type orelse return;
            const tag = std.meta.activeTag(value.*);
            inline for (info.fields) |field| {
                if (tag == @field(Tag, field.name)) {
                    deinitCanonicalValue(field.type, &@field(value.*, field.name), alloc);
                }
            }
        },
        else => {},
    }
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
        error.ReadFailed, error.WriteFailed => return error.InvalidCompressedSnapshot,
    };
    if (writer.end != expected_len) return error.InvalidCompressedSnapshot;

    return output;
}

fn initTestSnapshot(snapshot: *System.Snapshot) void {
    @memset(std.mem.asBytes(snapshot), 0);
    snapshot.bus.rom.mapper = .{ .mapper0 = .{ .prg_ram = &.{}, .chr_ram = &.{} } };
}

test "canonical network snapshot encode decode" {
    const alloc = std.testing.allocator;
    var snapshot: System.Snapshot = undefined;
    initTestSnapshot(&snapshot);

    const SpriteData = @typeInfo(@TypeOf(snapshot.ppu.sprite_data)).pointer.child;
    const Frame = @typeInfo(@TypeOf(snapshot.ppu.frame_buffer)).pointer.child;
    var sprite_data: SpriteData = undefined;
    var frame_buffer: Frame = undefined;
    @memset(std.mem.asBytes(&sprite_data), 0);
    @memset(std.mem.asBytes(&frame_buffer), 0);
    snapshot.ppu.sprite_data = &sprite_data;
    snapshot.ppu.frame_buffer = &frame_buffer;

    const encoded = try encode(alloc, &snapshot);
    defer alloc.free(encoded);
    const decoded = try decode(alloc, encoded);
    defer {
        decoded.deinit(alloc);
        alloc.destroy(decoded);
    }

    try std.testing.expectEqual(@as(i64, 0), decoded.saved_at);
    try std.testing.expectEqual(snapshot.cpu.pc, decoded.cpu.pc);
    try std.testing.expectEqual(snapshot.bus.cycles, decoded.bus.cycles);
    try std.testing.expectEqual(try digest(&snapshot), try digest(decoded));
    try std.testing.expectEqual(try componentDigests(&snapshot), try componentDigests(decoded));

    var bad = try alloc.dupe(u8, encoded);
    defer alloc.free(bad);
    bad[0] = 'X';
    try std.testing.expectError(error.InvalidNetworkSnapshot, decode(alloc, bad));
}

test "network representation ignores saved_at" {
    const alloc = std.testing.allocator;
    var snapshot: System.Snapshot = undefined;
    initTestSnapshot(&snapshot);

    const SpriteData = @typeInfo(@TypeOf(snapshot.ppu.sprite_data)).pointer.child;
    const Frame = @typeInfo(@TypeOf(snapshot.ppu.frame_buffer)).pointer.child;
    var sprite_data: SpriteData = undefined;
    var frame_buffer: Frame = undefined;
    @memset(std.mem.asBytes(&sprite_data), 0);
    @memset(std.mem.asBytes(&frame_buffer), 0);
    snapshot.ppu.sprite_data = &sprite_data;
    snapshot.ppu.frame_buffer = &frame_buffer;

    const baseline_encoded = try encode(alloc, &snapshot);
    defer alloc.free(baseline_encoded);
    const baseline_digest = try digest(&snapshot);

    snapshot.saved_at = 1_750_000_000;
    const timestamped_encoded = try encode(alloc, &snapshot);
    defer alloc.free(timestamped_encoded);

    try std.testing.expectEqualSlices(u8, baseline_encoded, timestamped_encoded);
    try std.testing.expectEqual(baseline_digest, try digest(&snapshot));
}

test "canonical network snapshot supports every mapper variant" {
    const alloc = std.testing.allocator;
    const variants = [_]Mapper.Snapshot{
        .{ .mapper0 = .{ .prg_ram = @constCast("prg"), .chr_ram = @constCast("chr") } },
        .{ .mapper1 = .{
            .load_register = 1,
            .write_index = 2,
            .control = 3,
            .prg_bank = 4,
            .chr_bank_1 = 5,
            .chr_bank_2 = 6,
            .prg_ram = @constCast("prg"),
            .chr_ram = @constCast("chr"),
        } },
        .{ .mapper2 = .{ .selected_bank = 2, .chr_ram = @constCast("chr") } },
        .{ .mapper3 = .{ .selected_chr_bank = 3, .prg_ram = @constCast("prg") } },
        .{ .mapper4 = .{
            .bank_registers = .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 },
            .bank_select = 4,
            .prg_inversion = true,
            .chr_inversion = false,
            .irq_flag = true,
            .irq_counter = 7,
            .irq_reload_flag = false,
            .irq_counter_reload = 8,
            .irq_enabled = true,
            .ppu_a12 = false,
            .ppu_a12_low_cycle = 99,
            .mirroring_mode = .HORIZONTAL,
            .prg_ram = @constCast("prg"),
            .chr_ram = @constCast("chr"),
        } },
    };

    for (variants) |mapper| {
        var snapshot: System.Snapshot = undefined;
        initTestSnapshot(&snapshot);

        const SpriteData = @typeInfo(@TypeOf(snapshot.ppu.sprite_data)).pointer.child;
        const Frame = @typeInfo(@TypeOf(snapshot.ppu.frame_buffer)).pointer.child;
        var sprite_data: SpriteData = undefined;
        var frame_buffer: Frame = undefined;
        @memset(std.mem.asBytes(&sprite_data), 0);
        @memset(std.mem.asBytes(&frame_buffer), 0);
        snapshot.ppu.sprite_data = &sprite_data;
        snapshot.ppu.frame_buffer = &frame_buffer;
        snapshot.bus.rom.mapper = mapper;

        const encoded = try encode(alloc, &snapshot);
        defer alloc.free(encoded);
        const decoded = try decode(alloc, encoded);
        defer {
            decoded.deinit(alloc);
            alloc.destroy(decoded);
        }

        try std.testing.expectEqual(std.meta.activeTag(mapper), std.meta.activeTag(decoded.bus.rom.mapper));
        try std.testing.expectEqual(try digest(&snapshot), try digest(decoded));
    }
}

test "netplay digest excludes presentation-only APU bookkeeping" {
    var snapshot: System.Snapshot = undefined;
    initTestSnapshot(&snapshot);

    const SpriteData = @typeInfo(@TypeOf(snapshot.ppu.sprite_data)).pointer.child;
    const Frame = @typeInfo(@TypeOf(snapshot.ppu.frame_buffer)).pointer.child;
    var sprite_data: SpriteData = undefined;
    var frame_buffer: Frame = undefined;
    @memset(std.mem.asBytes(&sprite_data), 0);
    @memset(std.mem.asBytes(&frame_buffer), 0);
    snapshot.ppu.sprite_data = &sprite_data;
    snapshot.ppu.frame_buffer = &frame_buffer;

    const baseline = try digest(&snapshot);

    snapshot.apu.next_transfer_cyc = 1234;
    snapshot.apu.last_frame_cyc = 5678;
    snapshot.apu.pulse1.waveform_last_amp = 11;
    snapshot.apu.pulse2.waveform_last_amp = 12;
    snapshot.apu.triangle.waveform_last_amp = 13;
    snapshot.apu.noise.waveform_last_amp = 14;
    snapshot.apu.dmc.waveform_last_amp = 15;
    try std.testing.expectEqual(baseline, try digest(&snapshot));

    snapshot.apu.global_cycle = 1;
    const changed = try digest(&snapshot);
    try std.testing.expect(!std.mem.eql(u8, &baseline, &changed));
}
