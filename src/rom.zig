const std = @import("std");
const Mapper = @import("mappers/mapper.zig").Mapper;

const NES_TAG = [_]u8{ 0x4e, 0x45, 0x53, 0x1a };
const PRG_ROM_PAGE_SIZE = 16384;
const CHR_ROM_PAGE_SIZE = 8192;

pub const Mirroring = enum {
    VERTICAL,
    HORIZONTAL,
    FOUR_SCREEN,
    SINGLE_SCREEN_LOWER,
    SINGLE_SCREEN_UPPER,
};

const Flag6 = packed struct(u8) {
    /// 0: vertical arrangement ("horizontal mirrored") (CIRAM A10 = PPU A11)
    /// 1: horizontal arrangement ("vertically mirrored") (CIRAM A10 = PPU A10)
    nametable_arrangement: u1,
    /// Cartridge contains battery-backed PRG RAM ($6000-7FFF) or other persistent memory
    has_battery: bool,
    /// 512-byte trainer at $7000-$71FF (stored before PRG data)
    has_trainer: bool,
    alternative_nametable_layout: bool,
    /// Lower nybble of mapper number
    mapper_lo: u4,

    fn mirroring_type(self: @This()) Mirroring {
        if (self.alternative_nametable_layout) {
            return Mirroring.FOUR_SCREEN;
        } else {
            switch (self.nametable_arrangement) {
                0 => return Mirroring.HORIZONTAL,
                1 => return Mirroring.VERTICAL,
            }
        }
    }
};

const Flag7 = packed struct(u8) {
    /// 0: Nintendo Entertainment System/Family Computer
    /// 1: Nintendo Vs. System
    /// 2: Nintendo Playchoice 10
    /// 3: Extended Console Type
    console_type: u2,
    /// NES 2.0 identifier
    nes_version: u2,
    /// Upper nybble of mapper number
    mapper_hi: u4,

    fn is_nes2(self: @This()) bool {
        return self.nes_version != 0;
    }
};

pub const Rom = struct {
    /// PRG ROM is mapped to address `0x8000..0x10000`.
    prg_rom: []u8,
    /// CHR ROM is mapped to address `0..0x1FFF`
    chr_rom: []u8,
    mapper: Mapper,

    const Self = @This();

    pub const Snapshot = struct {
        mapper: Mapper.Snapshot,

        pub fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            self.mapper.deinit(alloc);
        }
    };

    pub fn init(allocator: std.mem.Allocator, io: std.Io, rom_path: []const u8, bytes: []u8) !Self {
        return initWithOptions(allocator, io, rom_path, bytes, .{});
    }

    const InitOptions = struct {
        /// Constructs a transferred ROM without ever opening battery-save files.
        disable_battery_ram: bool = false,
    };

    pub fn initWithOptions(allocator: std.mem.Allocator, io: std.Io, rom_path: []const u8, bytes: []u8, options: InitOptions) !Self {
        if (!std.mem.eql(u8, bytes[0..4], &NES_TAG)) {
            return error.InvalidNesFormat;
        }

        const flag6: Flag6 = @bitCast(bytes[6]);
        const flag7: Flag7 = @bitCast(bytes[7]);

        const mapper_id = (@as(u8, flag7.mapper_hi) << 4) | @as(u8, flag6.mapper_lo);

        // Byte 4 contains the number of 16KB PGR-ROM banks
        const prg_rom_banks = bytes[4];
        // Byte 5 contains the number of 8KB CHR-ROM banks
        const chr_rom_banks = bytes[5];

        const prg_rom_size = @as(usize, prg_rom_banks) * PRG_ROM_PAGE_SIZE;
        const chr_rom_size = @as(usize, chr_rom_banks) * CHR_ROM_PAGE_SIZE;

        const prg_rom_start: usize = 16 + @as(usize, if (flag6.has_trainer) 512 else 0);
        const chr_rom_start = prg_rom_start + prg_rom_size;

        const prg_ram_size: usize = blk: {
            const ram_size = bytes[8];
            if (ram_size == 0) { // value 0 defaults to 8KB
                break :blk 8192;
            } else {
                break :blk @as(usize, ram_size) * 8192;
            }
        };

        const prg_rom = bytes[prg_rom_start..(prg_rom_start + prg_rom_size)];
        const chr_rom = bytes[chr_rom_start..(chr_rom_start + chr_rom_size)];
        const has_persistent_battery = flag6.has_battery and !options.disable_battery_ram;

        std.log.info(
            \\{s}
            \\Mapper ID: {}
            \\Number of 16KB PRG-ROM banks: {}
            \\Number of 8KB CHR-ROM banks: {}
            \\PRG RAM size: {}
            \\Mirroring type: {s}
            \\Has battery-backed RAM: {s}
            \\
        , .{
            if (flag7.is_nes2()) "iNES 2.0" else "iNES 1.0",
            mapper_id,
            prg_rom_banks,
            chr_rom_banks,
            prg_ram_size,
            @tagName(flag6.mirroring_type()),
            if (has_persistent_battery) "YES" else "NO",
        });

        const mapper = try Mapper.init(allocator, io, mapper_id, .{
            .rom_path = rom_path,
            .prg_rom = prg_rom,
            .chr_rom = chr_rom,
            .prg_rom_banks = prg_rom_banks,
            .prg_ram_size = prg_ram_size,
            .has_battery_backed_ram = has_persistent_battery,
            .mirroring_mode = flag6.mirroring_type(),
        });

        return .{
            .prg_rom = prg_rom,
            .chr_rom = chr_rom,
            .mapper = mapper,
        };
    }

    pub fn deinit(self: *Self) void {
        self.mapper.deinit();
    }

    pub fn saveState(self: *const Self, alloc: std.mem.Allocator) !Snapshot {
        return .{ .mapper = try self.mapper.saveState(alloc) };
    }

    pub fn loadState(self: *Self, snapshot: Snapshot) !void {
        try self.mapper.loadState(snapshot.mapper);
    }

    /// Read from PRG ROM ($8000-$FFFF)
    pub fn prg_rom_read(self: *Self, addr: u16) u8 {
        return self.mapper.prg_rom_read(addr);
    }

    ///  Read from PRG RAM ($6000–$7FFF)
    pub fn prg_ram_read(self: *Self, addr: u16) u8 {
        return self.mapper.prg_ram_read(addr);
    }

    /// Write to PRG ROM ($8000-$FFFF) space
    pub fn prg_rom_write(self: *Self, addr: u16, value: u8) void {
        self.mapper.prg_rom_write(addr, value);
    }

    /// Write to PRG RAM ($6000–$7FFF) space
    pub fn prg_ram_write(self: *Self, addr: u16, value: u8) void {
        self.mapper.prg_ram_write(addr, value);
    }

    /// Read from CHR ROM/RAM space ($0000-$1FFF)
    pub fn chr_read(self: *Self, addr: u16) u8 {
        return self.mapper.chr_read(addr);
    }

    /// Write to CHR ROM/RAM space
    pub fn chr_write(self: *Self, addr: u16, value: u8) void {
        self.mapper.chr_write(addr, value);
    }

    /// Get current mirroring mode (can change with some mappers)
    pub fn get_mirroring(self: *const Self) Mirroring {
        return self.mapper.mirroring();
    }

    /// Check if mapper has an active IRQ
    pub fn mapper_irq_active(self: *Self) bool {
        return self.mapper.irq_active();
    }

    pub fn mapper_ppu_address_updated(self: *Self, addr: u16, ppu_cycle: u64) void {
        self.mapper.ppu_address_updated(addr, ppu_cycle);
    }
};

pub const TestRom = struct {
    prg_rom: []u8,
    rom: Rom,
    alloc: std.mem.Allocator,

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator, comptime opcodes: []const u8) Self {
        return Self.init_with_mirroring(alloc, opcodes, .HORIZONTAL);
    }

    pub fn init_with_mirroring(alloc: std.mem.Allocator, comptime opcodes: []const u8, mirroring: Mirroring) Self {
        const prg_rom = alloc.alloc(u8, 0x10000) catch @panic("Failed to allocate PRG ROM");
        @memset(prg_rom, 0);

        @memmove(prg_rom[0x8000 .. 0x8000 + opcodes.len], opcodes);

        const chr_rom = &[_]u8{0};
        return .{
            .alloc = alloc,
            .prg_rom = prg_rom,
            .rom = .{
                .prg_rom = prg_rom,
                .chr_rom = @constCast(chr_rom),
                .mapper = Mapper.init(alloc, std.testing.io, 0, .{
                    .rom_path = "test.rom",
                    .prg_rom = prg_rom,
                    .chr_rom = chr_rom,
                    .prg_rom_banks = 0,
                    .prg_ram_size = 0,
                    .has_battery_backed_ram = false,
                    .mirroring_mode = mirroring,
                }) catch @panic("Failed to init mapper\n"),
            },
        };
    }

    pub fn deinit(self: *Self) void {
        self.rom.deinit();
        self.alloc.free(self.prg_rom);
    }
};

test "network ROM battery RAM is disabled" {
    const alloc = std.testing.allocator;
    const bytes = try alloc.alloc(u8, 16 + PRG_ROM_PAGE_SIZE);
    defer alloc.free(bytes);
    @memset(bytes, 0);
    @memcpy(bytes[0..4], &NES_TAG);
    bytes[4] = 1;
    bytes[6] = 0x12; // Mapper 1 plus the battery-backed RAM flag.

    // A battery-backed local Mapper 1 ROM requires a filesystem path. A
    // transferred ROM with only a display name must still initialize because
    // initNetwork forces its mapper RAM to the volatile implementation.
    var network_rom = try Rom.initWithOptions(
        alloc,
        std.testing.io,
        "remote.nes",
        bytes,
        .{ .disable_battery_ram = true },
    );
    defer network_rom.deinit();
    network_rom.prg_ram_write(0, 0x5a);
    try std.testing.expectEqual(@as(u8, 0x5a), network_rom.prg_ram_read(0));
}
