const std = @import("std");
const Ram = @import("ram.zig").Ram;
const VolatileRam = @import("ram.zig").VolatileRam;
const BatteryBackedRam = @import("ram.zig").BatteryBackedRam;
const Mapper = @import("mapper.zig").Mapper;
const Mirroring = @import("../rom.zig").Mirroring;
const MapperParams = @import("mapper.zig").MapperParams;

/// See: https://www.nesdev.org/wiki/MMC1
/// Mapper 1 (MMC1)
/// Features:
/// - 2 switchable 16KB PRG ROM banks
/// - 2 switchable 4KB CHR ROM banks or non-switchable CHR ROM
/// - Configurable mirroring modes
/// - Serial write interface with shift register
///
/// The MMC1 uses a serial port to write to internal registers.
/// To write to MMC1 registers, the CPU writes to PRG ROM space ($8000-$FFFF).
/// Bit 7 of the written value controls whether to reset the shift register.
/// Five writes are needed to complete a transfer to an internal register.
pub const Mapper1 = struct {
    prg_rom: []const u8,
    chr_rom: []const u8,
    chr_ram: []u8,
    chr_is_ram: bool,

    /// Shift register for serial writes (5 bits)
    load_register: u8,
    /// Count of writes to the shift register (0-4)
    write_index: u8,

    /// Control register (internal register 0)
    /// Bits:
    /// 4-3: PRG ROM bank mode
    /// 2: CHR ROM bank mode
    /// 1-0: Mirroring mode
    control: u8,

    /// PRG bank select (internal register 3)
    prg_bank: u8,
    /// CHR bank 0 select (internal register 1)
    chr_bank_1: u8,
    /// CHR bank 1 select (internal register 2)
    chr_bank_2: u8,

    /// Calculated offsets for the two 16KB PRG banks
    prg_offsets: [2]u32,
    /// Calculated offsets for the two 4KB CHR banks
    chr_offsets: [2]u32,

    prg_ram: Ram,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub const Snapshot = struct {
        load_register: u8,
        write_index: u8,
        control: u8,
        prg_bank: u8,
        chr_bank_1: u8,
        chr_bank_2: u8,
        prg_ram: []u8,
        chr_ram: []u8,

        pub fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            alloc.free(self.prg_ram);
            alloc.free(self.chr_ram);
        }
    };

    pub fn init(allocator: std.mem.Allocator, io: std.Io, params: MapperParams) !*Self {
        const self = try allocator.create(Self);

        const chr_is_ram = params.chr_rom.len == 0;
        var chr_ram: []u8 = undefined;
        if (chr_is_ram) {
            chr_ram = try allocator.alloc(u8, 8192);
            @memset(chr_ram, 0);
        } else {
            chr_ram = &.{};
        }

        const prg_ram = if (params.has_battery_backed_ram)
            (try BatteryBackedRam.init(allocator, io, params.rom_path, params.prg_ram_size)).as_ram()
        else
            (try VolatileRam.init(allocator, params.prg_ram_size)).as_ram();

        self.* = .{
            .prg_rom = params.prg_rom,
            .chr_rom = params.chr_rom,
            .chr_ram = chr_ram,
            .chr_is_ram = chr_is_ram,
            .load_register = 0x10,
            .write_index = 0,
            // Default control: mode 3 (fix last bank at $C000)
            .control = 0x0C,
            .prg_bank = 0,
            .chr_bank_1 = 0,
            .chr_bank_2 = 0,
            .prg_offsets = [_]u32{ 0, 0 },
            .chr_offsets = [_]u32{ 0, 0 },
            .prg_ram = prg_ram,
            .allocator = allocator,
        };

        self.update_offsets();
        return self;
    }

    const VTableImpl = Mapper.VTable{
        .deinit = @ptrCast(&Self.deinit),
        .chr_read = @ptrCast(&Self.chr_read),
        .chr_write = @ptrCast(&Self.chr_write),
        .prg_rom_read = @ptrCast(&Self.prg_rom_read),
        .prg_rom_write = @ptrCast(&Self.prg_rom_write),
        .prg_ram_read = @ptrCast(&Self.prg_ram_read),
        .prg_ram_write = @ptrCast(&Self.prg_ram_write),
        .irq_active = @ptrCast(&Self.irq_active),
        .mirroring = @ptrCast(&Self.mirroring),
        .save_state = @ptrCast(&Self.saveState),
        .load_state = @ptrCast(&Self.loadState),
    };

    pub fn as_mapper(self: *Self) Mapper {
        return .{ .ptr = self, .vtable = &VTableImpl };
    }

    pub fn deinit(self: *Self) void {
        self.prg_ram.deinit();
        self.allocator.free(self.chr_ram);
        self.allocator.destroy(self);
    }

    /// Updates the PRG and CHR offset tables based on current banking configuration
    fn update_offsets(self: *Self) void {
        // PRG ROM bank mode (bits 2-3 of control register)
        switch ((self.control & 0x0C) >> 2) {
            // Mode 0, 1: switch 32 KB at $8000, ignoring low bit of bank number
            0, 1 => {
                self.prg_offsets[0] = self.prg_offset((self.prg_bank & 0x0E));
                self.prg_offsets[1] = self.prg_offset((self.prg_bank | 0x01) & 0x0F);
            },
            // Mode 2: fix first bank at $8000 and switch 16 KB bank at $C000
            2 => {
                self.prg_offsets[0] = 0;
                self.prg_offsets[1] = self.prg_offset(self.prg_bank & 0x0F);
            },
            // Mode 3: fix last bank at $C000 and switch 16 KB bank at $8000
            3 => {
                self.prg_offsets[0] = self.prg_offset(self.prg_bank & 0x0F);
                const last_bank = @as(u32, @intCast(self.prg_rom.len)) / 0x4000 - 1;
                self.prg_offsets[1] = self.prg_offset(last_bank);
            },
            else => unreachable,
        }

        // CHR ROM bank mode (bit 4 of control register)
        switch ((self.control & 0x10) >> 4) {
            // Mode 0: switch 8 KB at a time
            0 => {
                self.chr_offsets[0] = self.chr_offset((self.chr_bank_1 & 0x1E));
                self.chr_offsets[1] = self.chr_offset((self.chr_bank_1 | 0x01));
            },
            // Mode 1: switch two separate 4 KB banks
            1 => {
                self.chr_offsets[0] = self.chr_offset(self.chr_bank_1 & 0x1F);
                self.chr_offsets[1] = self.chr_offset(self.chr_bank_2 & 0x1F);
            },
            else => unreachable,
        }
    }

    /// Calculate PRG ROM offset for a given bank index
    fn prg_offset(self: *const Self, index: u32) u32 {
        const num_banks = @as(u32, @intCast(self.prg_rom.len)) / 0x4000;
        return (index % num_banks) * 0x4000;
    }

    /// Calculate CHR memory offset for a given bank index
    fn chr_offset(self: *const Self, index: u32) u32 {
        const chr_len = if (self.chr_is_ram) self.chr_ram.len else self.chr_rom.len;
        const num_banks = @as(u32, @intCast(chr_len)) / 0x1000;
        return (index % num_banks) * 0x1000;
    }

    pub fn prg_rom_read(self: *const Self, addr: u16) u8 {
        const rel = addr - @as(u16, 0x8000);
        const bank = rel / 0x4000;
        const offset = rel % 0x4000;
        const final_addr = self.prg_offsets[bank] + offset;
        return self.prg_rom[final_addr];
    }

    pub fn prg_rom_write(self: *Self, addr: u16, value: u8) void {
        // If bit 7 is set, reset the shift register
        if (value & 0x80 != 0) {
            self.load_register = 0x10;
            self.write_index = 0;
            self.control = 0x0C;
            return;
        }

        // Shift bit into the load register
        self.load_register >>= 1;
        self.load_register |= (value & 0x01) << 4;

        // On the 5th write, copy to the appropriate internal register
        self.write_index += 1;
        if (self.write_index == 5) {
            // Determine target register from address bits 13-14
            switch (addr & 0xE000) {
                0x8000 => self.control = self.load_register,
                0xA000 => self.chr_bank_1 = self.load_register,
                0xC000 => self.chr_bank_2 = self.load_register,
                0xE000 => self.prg_bank = self.load_register,
                else => std.debug.panic("Unexpected address: {X:04}\n", .{addr}),
            }

            self.load_register = 0x10;
            self.write_index = 0;
            self.update_offsets();
        }
    }

    pub fn prg_ram_read(self: *Self, addr: u16) u8 {
        return self.prg_ram.read(addr);
    }

    pub fn prg_ram_write(self: *Self, addr: u16, value: u8) void {
        self.prg_ram.write(addr, value);
    }

    pub fn chr_read(self: *const Self, addr: u16) u8 {
        const bank = addr / 0x1000;
        const offset = addr % 0x1000;
        const final_addr = self.chr_offsets[bank] + offset;

        if (self.chr_is_ram) {
            return self.chr_ram[final_addr];
        } else {
            return self.chr_rom[final_addr];
        }
    }

    pub fn chr_write(self: *Self, addr: u16, value: u8) void {
        if (!self.chr_is_ram) {
            return;
        }

        const bank = addr / 0x1000;
        const offset = addr % 0x1000;
        const final_addr = self.chr_offsets[bank] + offset;
        self.chr_ram[final_addr] = value;
    }

    pub fn mirroring(self: *const Self) Mirroring {
        // Bits 0-1 of control register determine mirroring
        return switch (self.control & 0x03) {
            0 => Mirroring.SINGLE_SCREEN_LOWER,
            1 => Mirroring.SINGLE_SCREEN_UPPER,
            2 => Mirroring.VERTICAL,
            3 => Mirroring.HORIZONTAL,
            else => unreachable,
        };
    }

    pub fn irq_active(self: *Self) bool {
        _ = self;
        return false;
    }

    pub fn saveState(self: *const Self, alloc: std.mem.Allocator) !Mapper.Snapshot {
        const prg_ram = try self.prg_ram.saveState(alloc);
        errdefer alloc.free(prg_ram);
        return .{ .mapper1 = .{
            .load_register = self.load_register,
            .write_index = self.write_index,
            .control = self.control,
            .prg_bank = self.prg_bank,
            .chr_bank_1 = self.chr_bank_1,
            .chr_bank_2 = self.chr_bank_2,
            .prg_ram = prg_ram,
            .chr_ram = try alloc.dupe(u8, self.chr_ram),
        } };
    }

    pub fn loadState(self: *Self, snapshot: Mapper.Snapshot) !void {
        const data = switch (snapshot) {
            .mapper1 => |value| value,
            else => return error.InvalidSnapshot,
        };
        if (data.chr_ram.len != self.chr_ram.len) return error.InvalidSnapshot;
        self.load_register = data.load_register;
        self.write_index = data.write_index;
        self.control = data.control;
        self.prg_bank = data.prg_bank;
        self.chr_bank_1 = data.chr_bank_1;
        self.chr_bank_2 = data.chr_bank_2;
        try self.prg_ram.loadState(data.prg_ram);
        @memcpy(self.chr_ram, data.chr_ram);
        self.update_offsets();
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Mapper1 initialization" {
    const allocator = std.testing.allocator;

    const prg_rom = try allocator.alloc(u8, 0x40000); // 256KB (16 banks)
    defer allocator.free(prg_rom);
    @memset(prg_rom, 0);

    const chr_rom = try allocator.alloc(u8, 0x8000); // 32KB (8 banks)
    defer allocator.free(chr_rom);
    @memset(chr_rom, 0);

    var mapper = try Mapper1.init(allocator, std.testing.io, .{
        .prg_rom = prg_rom,
        .chr_rom = chr_rom,
        .prg_rom_banks = 16,
        .prg_ram_size = 0x2000,
        .mirroring_mode = .HORIZONTAL,
        .has_battery_backed_ram = false,
        .rom_path = "test.rom",
    });
    defer mapper.deinit();

    try std.testing.expectEqual(@as(u8, 0x10), mapper.load_register);
    try std.testing.expectEqual(@as(u8, 0), mapper.write_index);
    try std.testing.expectEqual(@as(u8, 0x0C), mapper.control);
}

test "Mapper1 shift register write sequence" {
    const allocator = std.testing.allocator;

    const prg_rom = try allocator.alloc(u8, 0x8000); // 32KB
    defer allocator.free(prg_rom);
    @memset(prg_rom, 0);

    const chr_rom = try allocator.alloc(u8, 0x2000); // 8KB
    defer allocator.free(chr_rom);
    @memset(chr_rom, 0);

    var mapper = try Mapper1.init(allocator, std.testing.io, .{
        .prg_rom = prg_rom,
        .chr_rom = chr_rom,
        .prg_rom_banks = 2,
        .prg_ram_size = 0x2000,
        .mirroring_mode = .HORIZONTAL,
        .has_battery_backed_ram = false,
        .rom_path = "test.rom",
    });
    defer mapper.deinit();

    // Write 5 bits to control register (address $8000-$9FFF)
    // Writing 0b11100 (0x1C) in LSB-first order
    mapper.prg_rom_write(0x8000, 0); // bit 0 = 0
    mapper.prg_rom_write(0x8000, 0); // bit 1 = 0
    mapper.prg_rom_write(0x8000, 1); // bit 2 = 1
    mapper.prg_rom_write(0x8000, 1); // bit 3 = 1
    mapper.prg_rom_write(0x8000, 1); // bit 4 = 1

    try std.testing.expectEqual(@as(u8, 0x1C), mapper.control);
    try std.testing.expectEqual(@as(u8, 0), mapper.write_index);
}

test "Mapper1 reset with bit 7" {
    const allocator = std.testing.allocator;

    const prg_rom = try allocator.alloc(u8, 0x8000);
    defer allocator.free(prg_rom);
    @memset(prg_rom, 0);

    const chr_rom = try allocator.alloc(u8, 0x2000);
    defer allocator.free(chr_rom);
    @memset(chr_rom, 0);

    var mapper = try Mapper1.init(allocator, std.testing.io, .{
        .prg_rom = prg_rom,
        .chr_rom = chr_rom,
        .prg_rom_banks = 2,
        .prg_ram_size = 0x2000,
        .mirroring_mode = .HORIZONTAL,
        .has_battery_backed_ram = false,
        .rom_path = "test.rom",
    });
    defer mapper.deinit();

    // Start a write sequence
    mapper.prg_rom_write(0x8000, 1);
    mapper.prg_rom_write(0x8000, 1);
    try std.testing.expectEqual(@as(u8, 2), mapper.write_index);

    // Reset with bit 7 set
    mapper.prg_rom_write(0x8000, 0x80);

    try std.testing.expectEqual(@as(u8, 0), mapper.write_index);
    try std.testing.expectEqual(@as(u8, 0x10), mapper.load_register);
}

test "Mapper1 PRG banking mode 3" {
    const allocator = std.testing.allocator;

    // Create 4 banks of 16KB each
    var prg_rom = try allocator.alloc(u8, 0x10000); // 64KB
    defer allocator.free(prg_rom);

    // Fill each bank with its bank number
    for (0..4) |bank| {
        const bank_start = bank * 0x4000;
        const bank_end = bank_start + 0x4000;
        @memset(prg_rom[bank_start..bank_end], @as(u8, @truncate(bank)));
    }

    const chr_rom = try allocator.alloc(u8, 0x2000);
    defer allocator.free(chr_rom);
    @memset(chr_rom, 0);

    var mapper = try Mapper1.init(allocator, std.testing.io, .{
        .prg_rom = prg_rom,
        .chr_rom = chr_rom,
        .prg_rom_banks = 4,
        .prg_ram_size = 0x2000,
        .mirroring_mode = .HORIZONTAL,
        .has_battery_backed_ram = false,
        .rom_path = "test.rom",
    });
    defer mapper.deinit();

    // Mode 3 (default): switch first bank, fix last bank
    // Last bank (bank 3) should be at $C000
    try std.testing.expectEqual(@as(u8, 3), mapper.prg_rom_read(0xC000));

    // Switch to bank 1 at $8000
    for (0..5) |i| {
        mapper.prg_rom_write(0xE000, @as(u8, @intCast(std.math.shr(usize, 1, i) & 1)));
    }
    try std.testing.expectEqual(@as(u8, 1), mapper.prg_rom_read(0x8000));
    try std.testing.expectEqual(@as(u8, 3), mapper.prg_rom_read(0xC000));
}

test "Mapper1 CHR banking" {
    const allocator = std.testing.allocator;

    const prg_rom = try allocator.alloc(u8, 0x8000);
    defer allocator.free(prg_rom);
    @memset(prg_rom, 0);

    // Create 8 banks of 4KB CHR ROM
    var chr_rom = try allocator.alloc(u8, 0x8000); // 32KB
    defer allocator.free(chr_rom);

    for (0..8) |bank| {
        const bank_start = bank * 0x1000;
        const bank_end = bank_start + 0x1000;
        @memset(chr_rom[bank_start..bank_end], @as(u8, @truncate(bank)));
    }

    var mapper = try Mapper1.init(allocator, std.testing.io, .{
        .prg_rom = prg_rom,
        .chr_rom = chr_rom,
        .prg_rom_banks = 2,
        .prg_ram_size = 0x2000,
        .mirroring_mode = .HORIZONTAL,
        .has_battery_backed_ram = false,
        .rom_path = "test.rom",
    });
    defer mapper.deinit();

    // Set CHR mode to 1 (two separate 4KB banks)
    // Write control register: bits = 0b10000 (CHR mode = 1)
    for (0..5) |i| {
        mapper.prg_rom_write(0x8000, @as(u8, @intCast(std.math.shr(usize, 0x10, i) & 1)));
    }

    // Set CHR bank 0 to bank 2
    for (0..5) |i| {
        mapper.prg_rom_write(0xA000, @as(u8, @intCast(std.math.shr(usize, 2, i) & 1)));
    }

    // Set CHR bank 1 to bank 5
    for (0..5) |i| {
        mapper.prg_rom_write(0xC000, @as(u8, @intCast(std.math.shr(usize, 5, i) & 1)));
    }

    try std.testing.expectEqual(@as(u8, 2), mapper.chr_read(0x0000));
    try std.testing.expectEqual(@as(u8, 5), mapper.chr_read(0x1000));
}
