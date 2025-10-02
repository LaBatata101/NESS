const std = @import("std");

const NES_TAG = [_]u8{ 0x4e, 0x45, 0x53, 0x1a };
const PRG_ROM_PAGE_SIZE = 16384;
const CHR_ROM_PAGE_SIZE = 8192;

pub const Mirroring = enum {
    VERTICAL,
    HORIZONTAL,
    FOUR_SCREEN,
};

pub const Rom = struct {
    /// PRG ROM is mapped to address `0x8000..0x10000`.
    prg_rom: []u8,
    /// CHR ROM is mapped to address `0..0x1FFF`
    chr_rom: []u8,
    mapper: u8,
    mirroring: Mirroring,

    const Self = @This();

    pub const InitError = error{ InvalidNesFormat, InvalidNesFormatVersion };

    pub fn load(raw: []u8) InitError!Self {
        if (!std.mem.eql(u8, raw[0..4], &NES_TAG)) {
            return InitError.InvalidNesFormat;
        }

        // Byte 6 and 7 of the NES file format contains information about the data in the file.
        // The uppper 4 bits of byte 6 contains the 4 lower bits of the ROM Mapper type and
        // the upper 4 bits of byte 7 contains the 4 upper bits of the ROM Mapper type.
        const mapper = raw[7] & 0b1111_0000 | raw[6] >> 4;

        // bits 2 and 3 of byte 7 contains information about the version of the NES file format. If these bits are
        // set, it means it's using the format version 2.0 which we don't support.
        if ((raw[7] >> 2) & 0b11 != 0) {
            return InitError.InvalidNesFormatVersion;
        }

        const is_four_screen = raw[6] & 0b1000 != 0;
        const is_vertical_mirroring = raw[6] & 0b1 != 0;
        var screen_mirroring: Mirroring = undefined;
        if (is_four_screen) {
            screen_mirroring = Mirroring.FOUR_SCREEN;
        } else if (is_vertical_mirroring) {
            screen_mirroring = Mirroring.VERTICAL;
        } else {
            screen_mirroring = Mirroring.HORIZONTAL;
        }

        // Byte 4 contains the number of 16KB ROM banks
        const prg_rom_size = @as(usize, raw[4]) * PRG_ROM_PAGE_SIZE;
        // Byte 5 contains the number of 8KB VROM banks
        const chr_rom_size = @as(usize, raw[5]) * CHR_ROM_PAGE_SIZE;

        // If bit 2 of byte 6 is set, there's a 512-byte trainer section in the file to skip
        const skip_trainer = raw[6] & 0b100 != 0;

        const prg_rom_start: usize = 16 + @as(usize, if (skip_trainer) 512 else 0);
        const chr_rom_start = prg_rom_start + prg_rom_size;

        return .{
            .mapper = mapper,
            .mirroring = screen_mirroring,
            .prg_rom = raw[prg_rom_start..(prg_rom_start + prg_rom_size)],
            .chr_rom = raw[chr_rom_start..(chr_rom_start + chr_rom_size)],
        };
    }
};

pub const TestRom = struct {
    header: []const u8,
    trainer: ?[]const u8,
    prg_rom: []const u8,
    chr_rom: []const u8,

    fn createRawRom(self: @This(), allocator: std.mem.Allocator) ![]u8 {
        var array = try std.ArrayList(u8).initCapacity(
            allocator,
            self.header.len + self.prg_rom.len + self.chr_rom.len + if (self.trainer) |trainer| trainer.len else 0,
        );
        try array.appendSlice(allocator, self.header);
        if (self.trainer) |trainer| {
            try array.appendSlice(allocator, trainer);
        }

        try array.appendSlice(allocator, self.prg_rom);
        try array.appendSlice(allocator, self.chr_rom);
        return try array.toOwnedSlice(allocator);
    }

    pub fn testRom(allocator: std.mem.Allocator, program: []const u8) ![]u8 {
        var prg_rom_contents = try std.ArrayList(u8).initCapacity(allocator, program.len);
        defer prg_rom_contents.deinit(allocator);

        try prg_rom_contents.appendSlice(allocator, program);
        try prg_rom_contents.resize(allocator, 2 * PRG_ROM_PAGE_SIZE);

        const test_rom = TestRom{
            .header = &[_]u8{ 0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
            .trainer = null,
            .prg_rom = prg_rom_contents.items,
            .chr_rom = &[_]u8{2} ** CHR_ROM_PAGE_SIZE,
        };
        return try test_rom.createRawRom(allocator);
    }
};

test "ROM creation" {
    const allocator = std.testing.allocator;
    var header = [_]u8{
        0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };
    var prg_rom = [_]u8{1} ** (2 * PRG_ROM_PAGE_SIZE);
    var chr_rom = [_]u8{2} ** CHR_ROM_PAGE_SIZE;
    const test_rom = TestRom{
        .header = header[0..],
        .trainer = null,
        .prg_rom = &prg_rom,
        .chr_rom = &chr_rom,
    };

    const raw_rom = try test_rom.createRawRom(allocator);
    defer allocator.free(raw_rom);
    const rom = try Rom.load(raw_rom);

    try std.testing.expect(std.mem.eql(u8, &prg_rom, rom.prg_rom));
    try std.testing.expect(std.mem.eql(u8, &chr_rom, rom.chr_rom));
    try std.testing.expectEqual(3, rom.mapper);
    try std.testing.expectEqual(Mirroring.VERTICAL, rom.mirroring);
}

test "ROM with trainer section" {
    const allocator = std.testing.allocator;
    var header = [_]u8{
        0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31 | 0b100, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };
    var prg_rom = [_]u8{1} ** (2 * PRG_ROM_PAGE_SIZE);
    var chr_rom = [_]u8{2} ** CHR_ROM_PAGE_SIZE;
    var trainer = [_]u8{0} ** 512;
    const test_rom = TestRom{
        .header = header[0..],
        .trainer = &trainer,
        .prg_rom = &prg_rom,
        .chr_rom = &chr_rom,
    };

    const raw_rom = try test_rom.createRawRom(allocator);
    defer allocator.free(raw_rom);
    const rom = try Rom.load(raw_rom);

    try std.testing.expect(std.mem.eql(u8, &prg_rom, rom.prg_rom));
    try std.testing.expect(std.mem.eql(u8, &chr_rom, rom.chr_rom));
    try std.testing.expectEqual(3, rom.mapper);
    try std.testing.expectEqual(Mirroring.VERTICAL, rom.mirroring);
}

test "ROM NES2.0 format not supported" {
    const allocator = std.testing.allocator;
    var header = [_]u8{
        0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };
    var prg_rom = [_]u8{1} ** PRG_ROM_PAGE_SIZE;
    var chr_rom = [_]u8{2} ** CHR_ROM_PAGE_SIZE;
    const test_rom = TestRom{
        .header = header[0..],
        .trainer = null,
        .prg_rom = &prg_rom,
        .chr_rom = &chr_rom,
    };

    const raw_rom = try test_rom.createRawRom(allocator);
    defer allocator.free(raw_rom);

    try std.testing.expectError(error.InvalidNesFormatVersion, Rom.load(raw_rom));
}
