const std = @import("std");

const Ram = @import("ram.zig").Ram;
const Mapper = @import("mapper.zig").Mapper;
const Mirroring = @import("../rom.zig").Mirroring;
const VolatileRam = @import("ram.zig").VolatileRam;
const MapperParams = @import("mapper.zig").MapperParams;
const BatteryBackedRam = @import("ram.zig").BatteryBackedRam;

/// See: https://www.nesdev.org/wiki/MMC3
/// Mapper 4 (MMC3)
/// Features:
/// - 4x 8KB switchable PRG ROM banks
/// - 2x 2KB switchable CHR ROM banks (treated as 4x 1KB)
/// - 4x 1KB switchable CHR ROM banks
/// - IRQ counter for scanline counting
/// - Configurable mirroring
/// - PRG RAM support (8KB battery-backed)
///
/// The MMC3 is one of the most popular NES mappers, used by games like
/// Super Mario Bros. 3, Mega Man 3-6, and many others.
pub const Mapper4 = struct {
    prg_rom: []const u8,
    chr_rom: []const u8,
    chr_ram: []u8,
    chr_is_ram: bool,
    prg_ram: Ram,

    /// 8 bank registers (R0-R7) + 2 fixed banks (second-to-last and last PRG banks)
    bank_registers: [10]usize,
    /// Which bank register to update on next write to $8001
    bank_select: u8,
    /// PRG ROM bank mode (false = $8000 swappable, true = $C000 swappable)
    prg_inversion: bool,
    /// CHR A12 inversion
    chr_inversion: bool,

    /// IRQ flag - set when counter reaches 0
    irq_flag: bool,
    /// IRQ counter value
    irq_counter: u8,
    /// Flag to reload counter on next clock
    irq_reload_flag: bool,
    /// Value to reload counter with
    irq_counter_reload: u8,
    /// Whether IRQs are enabled
    irq_enabled: bool,

    /// Previous state of PPU A12 line (for edge detection)
    ppu_a12: bool,
    /// PPU cycle when A12 most recently went low (for MMC3 low-pass filtering)
    ppu_a12_low_cycle: u64,

    mirroring_mode: Mirroring,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub const Snapshot = struct {
        bank_registers: [10]usize,
        bank_select: u8,
        prg_inversion: bool,
        chr_inversion: bool,
        irq_flag: bool,
        irq_counter: u8,
        irq_reload_flag: bool,
        irq_counter_reload: u8,
        irq_enabled: bool,
        ppu_a12: bool,
        ppu_a12_low_cycle: u64,
        mirroring_mode: Mirroring,
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

        var bank_registers = [_]usize{0} ** 10;

        // Initialize fixed banks (second-to-last and last 8KB PRG banks)
        const num_banks = params.prg_rom.len / 0x2000;
        bank_registers[8] = (num_banks - 2) * 0x2000;
        bank_registers[9] = (num_banks - 1) * 0x2000;

        self.* = .{
            .prg_rom = params.prg_rom,
            .chr_rom = params.chr_rom,
            .chr_ram = chr_ram,
            .chr_is_ram = chr_is_ram,
            .prg_ram = prg_ram,
            .bank_registers = bank_registers,
            .bank_select = 0,
            .prg_inversion = false,
            .chr_inversion = false,
            .irq_flag = false,
            .irq_counter = 0,
            .irq_reload_flag = false,
            .irq_counter_reload = 0,
            .irq_enabled = false,
            .ppu_a12 = false,
            .ppu_a12_low_cycle = 0,
            .mirroring_mode = params.mirroring_mode,
            .allocator = allocator,
        };

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
        .ppu_address_updated = @ptrCast(&Self.ppu_address_updated),
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

    /// Clock the IRQ counter (called on A12 rising edge)
    fn clock_irq(self: *Self) void {
        if (self.irq_counter == 0 or self.irq_reload_flag) {
            self.irq_counter = self.irq_counter_reload;
            self.irq_reload_flag = false;
        } else {
            self.irq_counter -|= 1;
        }

        if (self.irq_counter == 0) {
            self.irq_flag = self.irq_enabled;
        }
    }

    fn prg_rom_read(self: *const Self, addr: u16) u8 {
        const bank_index: u8, const bank_size: u16 = switch (addr) {
            0x8000...0x9FFF => if (self.prg_inversion) .{ 8, 0x2000 } else .{ 6, 0x2000 },
            0xA000...0xBFFF => .{ 7, 0x2000 },
            0xC000...0xDFFF => if (self.prg_inversion) .{ 6, 0x2000 } else .{ 8, 0x2000 },
            0xE000...0xFFFF => .{ 9, 0x2000 },
            else => std.debug.panic("Invalid address: 0x{X:04}\n", .{addr}),
        };

        const base = self.bank_registers[bank_index];
        const offset = (addr % bank_size);
        return self.prg_rom[base + offset];
    }

    fn prg_rom_write(self: *Self, addr: u16, value: u8) void {
        // MMC3 has 4 pairs of registers at $8000-$9FFF, $A000-$BFFF, $C000-$DFFF, $E000-$FFFF
        // Even addresses select low register, odd addresses select high register
        const register_pair = addr & 0xE000;
        const is_odd = (addr & 0x01) != 0;
        switch (register_pair) {
            0x8000 => {
                if (is_odd) {
                    // $8001 (odd) - Bank data
                    if (self.bank_select >= 6) {
                        // PRG ROM bank (8KB, ignore top 2 bits)
                        self.bank_registers[self.bank_select] =
                            ((@as(usize, value & 0x3F) << 13) % self.prg_rom.len);
                    } else if (self.bank_select <= 1) {
                        // 2KB CHR bank - can only select even banks
                        const chr_len = if (self.chr_is_ram) self.chr_ram.len else self.chr_rom.len;
                        self.bank_registers[self.bank_select] =
                            ((@as(usize, value & 0xFE) << 10) % chr_len);
                    } else {
                        // 1KB CHR bank
                        const chr_len = if (self.chr_is_ram) self.chr_ram.len else self.chr_rom.len;
                        self.bank_registers[self.bank_select] =
                            ((@as(usize, value) << 10) % chr_len);
                    }
                } else {
                    // $8000 (even) - Bank select
                    self.bank_select = value & 0x07;
                    self.prg_inversion = (value & 0x40) != 0;
                    self.chr_inversion = (value & 0x80) != 0;
                }
            },
            0xA000 => {
                if (!is_odd) {
                    // $A000 (even) - Mirroring
                    self.mirroring_mode = if ((value & 0x01) == 0)
                        Mirroring.VERTICAL
                    else
                        Mirroring.HORIZONTAL;
                } else {
                    // $A001 (odd) - PRG RAM protect (unimplemented for MMC6 compatibility)
                }
            },
            0xC000 => {
                if (is_odd) {
                    // $C001 (odd) - IRQ reload
                    self.irq_reload_flag = true;
                    self.irq_counter = 0;
                } else {
                    // $C000 (even) - IRQ latch
                    self.irq_counter_reload = value;
                }
            },
            0xE000 => {
                if (is_odd) {
                    // $E001 (odd) - IRQ enable
                    self.irq_enabled = true;
                } else {
                    // $E000 (even) - IRQ disable
                    self.irq_enabled = false;
                    self.irq_flag = false;
                }
            },
            else => {},
        }
    }

    fn prg_ram_read(self: *Self, addr: u16) u8 {
        return self.prg_ram.read(addr);
    }

    fn prg_ram_write(self: *Self, addr: u16, value: u8) void {
        self.prg_ram.write(addr, value);
    }

    fn chr_read(self: *Self, addr: u16) u8 {
        const bank_index: u8, const bank_size: u16 = switch (addr) {
            0x0000...0x03FF => if (self.chr_inversion) .{ 2, 0x400 } else .{ 0, 0x800 },
            0x0400...0x07FF => if (self.chr_inversion) .{ 3, 0x400 } else .{ 0, 0x800 },
            0x0800...0x0BFF => if (self.chr_inversion) .{ 4, 0x400 } else .{ 1, 0x800 },
            0x0C00...0x0FFF => if (self.chr_inversion) .{ 5, 0x400 } else .{ 1, 0x800 },
            0x1000...0x13FF => if (self.chr_inversion) .{ 0, 0x800 } else .{ 2, 0x400 },
            0x1400...0x17FF => if (self.chr_inversion) .{ 0, 0x800 } else .{ 3, 0x400 },
            0x1800...0x1BFF => if (self.chr_inversion) .{ 1, 0x800 } else .{ 4, 0x400 },
            0x1C00...0x1FFF => if (self.chr_inversion) .{ 1, 0x800 } else .{ 5, 0x400 },
            else => std.debug.panic("Invalid address: 0x{X:04}\n", .{addr}),
        };

        const base = self.bank_registers[bank_index];
        const offset = (addr % bank_size);

        if (self.chr_is_ram) {
            return self.chr_ram[base + offset];
        } else {
            return self.chr_rom[base + offset];
        }
    }

    fn chr_write(self: *Self, addr: u16, value: u8) void {
        if (!self.chr_is_ram) {
            return;
        }
        self.chr_ram[addr % self.chr_ram.len] = value;
    }

    fn mirroring(self: *const Self) Mirroring {
        return self.mirroring_mode;
    }

    fn irq_active(self: *Self) bool {
        return self.irq_flag;
    }

    fn ppu_address_updated(self: *Self, addr: u16, ppu_cycle: u64) void {
        if (addr >= 0x2000) return;

        const a12 = addr & 0x1000 == 0x1000;
        if (a12 == self.ppu_a12) {
            return;
        }

        if (a12) {
            if (ppu_cycle -| self.ppu_a12_low_cycle >= 12) {
                self.clock_irq();
            }
        } else {
            self.ppu_a12_low_cycle = ppu_cycle;
        }
        self.ppu_a12 = a12;
    }

    fn saveState(self: *const Self, alloc: std.mem.Allocator) !Mapper.Snapshot {
        const prg_ram = try self.prg_ram.saveState(alloc);
        errdefer alloc.free(prg_ram);
        return .{ .mapper4 = .{
            .bank_registers = self.bank_registers,
            .bank_select = self.bank_select,
            .prg_inversion = self.prg_inversion,
            .chr_inversion = self.chr_inversion,
            .irq_flag = self.irq_flag,
            .irq_counter = self.irq_counter,
            .irq_reload_flag = self.irq_reload_flag,
            .irq_counter_reload = self.irq_counter_reload,
            .irq_enabled = self.irq_enabled,
            .ppu_a12 = self.ppu_a12,
            .ppu_a12_low_cycle = self.ppu_a12_low_cycle,
            .mirroring_mode = self.mirroring_mode,
            .prg_ram = prg_ram,
            .chr_ram = try alloc.dupe(u8, self.chr_ram),
        } };
    }

    fn loadState(self: *Self, snapshot: Mapper.Snapshot) !void {
        const data = switch (snapshot) {
            .mapper4 => |value| value,
            else => return error.InvalidSnapshot,
        };
        if (data.chr_ram.len != self.chr_ram.len) return error.InvalidSnapshot;
        self.bank_registers = data.bank_registers;
        self.bank_select = data.bank_select;
        self.prg_inversion = data.prg_inversion;
        self.chr_inversion = data.chr_inversion;
        self.irq_flag = data.irq_flag;
        self.irq_counter = data.irq_counter;
        self.irq_reload_flag = data.irq_reload_flag;
        self.irq_counter_reload = data.irq_counter_reload;
        self.irq_enabled = data.irq_enabled;
        self.ppu_a12 = data.ppu_a12;
        self.ppu_a12_low_cycle = data.ppu_a12_low_cycle;
        self.mirroring_mode = data.mirroring_mode;
        try self.prg_ram.loadState(data.prg_ram);
        @memcpy(self.chr_ram, data.chr_ram);
    }
};

test "Mapper4 initialization" {
    const allocator = std.testing.allocator;

    const prg_rom = try allocator.alloc(u8, 0x40000); // 256KB
    defer allocator.free(prg_rom);
    @memset(prg_rom, 0);

    const chr_rom = try allocator.alloc(u8, 0x20000); // 128KB
    defer allocator.free(chr_rom);
    @memset(chr_rom, 0);

    var mapper = try Mapper4.init(allocator, std.testing.io, .{
        .prg_rom = prg_rom,
        .chr_rom = chr_rom,
        .prg_rom_banks = 32,
        .has_battery_backed_ram = false,
        .prg_ram_size = 8192,
        .mirroring_mode = .HORIZONTAL,
        .rom_path = "teste.rom",
    });
    defer mapper.deinit();

    // Check that fixed banks are initialized correctly
    const num_banks = prg_rom.len / 0x2000;
    try std.testing.expectEqual((num_banks - 2) * 0x2000, mapper.bank_registers[8]);
    try std.testing.expectEqual((num_banks - 1) * 0x2000, mapper.bank_registers[9]);
}

test "Mapper4 bank select and data" {
    const allocator = std.testing.allocator;

    var prg_rom = try allocator.alloc(u8, 0x40000); // 256KB
    defer allocator.free(prg_rom);

    // Fill each 8KB bank with its bank number
    for (0..32) |bank| {
        const start = bank * 0x2000;
        const end = start + 0x2000;
        @memset(prg_rom[start..end], @as(u8, @truncate(bank)));
    }

    const chr_rom = try allocator.alloc(u8, 0x2000);
    defer allocator.free(chr_rom);
    @memset(chr_rom, 0);

    var mapper = try Mapper4.init(allocator, std.testing.io, .{
        .prg_rom = prg_rom,
        .chr_rom = chr_rom,
        .prg_rom_banks = 32,
        .has_battery_backed_ram = false,
        .prg_ram_size = 8192,
        .mirroring_mode = .HORIZONTAL,
        .rom_path = "teste.rom",
    });
    defer mapper.deinit();

    // Select bank register 6 (first switchable PRG bank)
    mapper.prg_rom_write(0x8000, 6);
    // Set it to bank 5
    mapper.prg_rom_write(0x8001, 5);

    // In non-inverted mode, bank 6 maps to $8000
    try std.testing.expectEqual(@as(u8, 5), mapper.prg_rom_read(0x8000));
}

test "Mapper4 PRG inversion" {
    const allocator = std.testing.allocator;

    var prg_rom = try allocator.alloc(u8, 0x20000); // 128KB (16 banks)
    defer allocator.free(prg_rom);

    for (0..16) |bank| {
        const start = bank * 0x2000;
        const end = start + 0x2000;
        @memset(prg_rom[start..end], @as(u8, @truncate(bank)));
    }

    const chr_rom = try allocator.alloc(u8, 0x2000);
    defer allocator.free(chr_rom);
    @memset(chr_rom, 0);

    var mapper = try Mapper4.init(allocator, std.testing.io, .{
        .prg_rom = prg_rom,
        .chr_rom = chr_rom,
        .prg_rom_banks = 16,
        .has_battery_backed_ram = false,
        .prg_ram_size = 8192,
        .mirroring_mode = .HORIZONTAL,
        .rom_path = "teste.rom",
    });
    defer mapper.deinit();

    // Set bank 6 to bank 3
    mapper.prg_rom_write(0x8000, 6);
    mapper.prg_rom_write(0x8001, 3);

    // Without inversion: bank 6 at $8000, fixed second-to-last at $C000
    try std.testing.expectEqual(@as(u8, 3), mapper.prg_rom_read(0x8000));
    try std.testing.expectEqual(@as(u8, 14), mapper.prg_rom_read(0xC000));

    // Enable PRG inversion (bit 6)
    mapper.prg_rom_write(0x8000, 0x46); // bank select 6, PRG inversion on

    // With inversion: fixed second-to-last at $8000, bank 6 at $C000
    try std.testing.expectEqual(@as(u8, 14), mapper.prg_rom_read(0x8000));
    try std.testing.expectEqual(@as(u8, 3), mapper.prg_rom_read(0xC000));
}

test "Mapper4 mirroring control" {
    const allocator = std.testing.allocator;

    const prg_rom = try allocator.alloc(u8, 0x8000);
    defer allocator.free(prg_rom);
    @memset(prg_rom, 0);

    const chr_rom = try allocator.alloc(u8, 0x2000);
    defer allocator.free(chr_rom);
    @memset(chr_rom, 0);

    var mapper = try Mapper4.init(allocator, std.testing.io, .{
        .prg_rom = prg_rom,
        .chr_rom = chr_rom,
        .prg_rom_banks = 4,
        .has_battery_backed_ram = false,
        .prg_ram_size = 8192,
        .mirroring_mode = .HORIZONTAL,
        .rom_path = "teste.rom",
    });
    defer mapper.deinit();

    try std.testing.expectEqual(Mirroring.HORIZONTAL, mapper.mirroring());

    // Set vertical mirroring
    mapper.prg_rom_write(0xA000, 0);
    try std.testing.expectEqual(Mirroring.VERTICAL, mapper.mirroring());

    // Set horizontal mirroring
    mapper.prg_rom_write(0xA000, 1);
    try std.testing.expectEqual(Mirroring.HORIZONTAL, mapper.mirroring());
}

test "Mapper4 IRQ functionality" {
    const allocator = std.testing.allocator;

    const prg_rom = try allocator.alloc(u8, 0x8000);
    defer allocator.free(prg_rom);
    @memset(prg_rom, 0);

    const chr_rom = try allocator.alloc(u8, 0x2000);
    defer allocator.free(chr_rom);
    @memset(chr_rom, 0);

    var mapper = try Mapper4.init(allocator, std.testing.io, .{
        .prg_rom = prg_rom,
        .chr_rom = chr_rom,
        .prg_rom_banks = 4,
        .has_battery_backed_ram = false,
        .prg_ram_size = 8192,
        .mirroring_mode = .HORIZONTAL,
        .rom_path = "teste.rom",
    });
    defer mapper.deinit();

    // Set IRQ counter reload value to 2
    mapper.prg_rom_write(0xC000, 2);
    // Reload counter
    mapper.prg_rom_write(0xC001, 0);
    // Enable IRQ
    mapper.prg_rom_write(0xE001, 0);

    try std.testing.expect(!mapper.irq_active());

    // Simulate 3 scanlines (A12 rising edges)
    mapper.ppu_a12_low_cycle = 0;
    mapper.ppu_address_updated(0x1000, 20); // A12 rises
    try std.testing.expect(!mapper.irq_active());

    mapper.ppu_a12 = false;
    mapper.ppu_a12_low_cycle = 20;
    mapper.ppu_address_updated(0x1000, 40); // A12 rises
    try std.testing.expect(!mapper.irq_active());

    mapper.ppu_a12 = false;
    mapper.ppu_a12_low_cycle = 40;
    mapper.ppu_address_updated(0x1000, 60); // A12 rises - counter should hit 0
    try std.testing.expect(mapper.irq_active());
}

test "Mapper4 CHR banking" {
    const allocator = std.testing.allocator;

    const prg_rom = try allocator.alloc(u8, 0x8000);
    defer allocator.free(prg_rom);
    @memset(prg_rom, 0);

    var chr_rom = try allocator.alloc(u8, 0x8000); // 32KB (32 1KB banks)
    defer allocator.free(chr_rom);

    for (0..32) |bank| {
        const start = bank * 0x400;
        const end = start + 0x400;
        @memset(chr_rom[start..end], @as(u8, @truncate(bank)));
    }

    var mapper = try Mapper4.init(allocator, std.testing.io, .{
        .prg_rom = prg_rom,
        .chr_rom = chr_rom,
        .prg_rom_banks = 4,
        .has_battery_backed_ram = false,
        .prg_ram_size = 8192,
        .mirroring_mode = .HORIZONTAL,
        .rom_path = "teste.rom",
    });
    defer mapper.deinit();

    // Set 2KB bank 0 (R0) to bank 4 (will use banks 4-5)
    mapper.prg_rom_write(0x8000, 0);
    mapper.prg_rom_write(0x8001, 4);

    // Without CHR inversion, R0 maps to $0000-$07FF
    try std.testing.expectEqual(@as(u8, 4), mapper.chr_read(0x0000));
    try std.testing.expectEqual(@as(u8, 4), mapper.chr_read(0x03FF));
}
