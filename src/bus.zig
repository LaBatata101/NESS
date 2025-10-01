const std = @import("std");
const Rom = @import("rom.zig").Rom;

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x2000;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x4000;

//  _______________ $10000  _______________
// | PRG-ROM       |       |               |
// | Upper Bank    |       |               |
// |_ _ _ _ _ _ _ _| $C000 | PRG-ROM       |
// | PRG-ROM       |       |               |
// | Lower Bank    |       |               |
// |_______________| $8000 |_______________|
// | SRAM          |       | SRAM          |
// |_______________| $6000 |_______________|
// | Expansion ROM |       | Expansion ROM |
// |_______________| $4020 |_______________|
// | I/O Registers |       |               |
// |_ _ _ _ _ _ _ _| $4000 |               |
// | Mirrors       |       | I/O Registers |
// | $2000-$2007   |       |               |
// |_ _ _ _ _ _ _ _| $2008 |               |
// | I/O Registers |       |               |
// |_______________| $2000 |_______________|
// | Mirrors       |       |               |
// | $0000-$07FF   |       |               |
// |_ _ _ _ _ _ _ _| $0800 |               |
// | RAM           |       | RAM           |
// |_ _ _ _ _ _ _ _| $0200 |               |
// | Stack         |       |               |
// |_ _ _ _ _ _ _ _| $0100 |               |
// | Zero Page     |       |               |
// |_______________| $0000 |_______________|
pub const Bus = struct {
    /// The CPU has `0x0000..0x2000` addressing space reserved for RAM space. The RAM address
    /// space `0x000..0x0800` (2KiB) is mirrored three times:
    /// - `0x800..0x1000`
    /// - `0x1000..0x1800`
    /// - `0x1800..0x2000`
    /// This means that there is no difference in accessing memory addresses at `0x0000` or `0x0800` or
    /// `0x1000` or `0x1800` for reads or writes.
    ram: [2048]u8,
    rom: Rom,

    const Self = @This();

    pub fn init(rom: Rom) Self {
        return .{ .ram = [_]u8{0} ** 2048, .rom = rom };
    }

    pub fn mem_read(self: Self, addr: u16) u8 {
        if (addr >= RAM and addr < RAM_MIRRORS_END) {
            const mirror_down_addr = addr & 0b00000111_11111111;
            return self.ram[mirror_down_addr];
        } else if (addr >= PPU_REGISTERS and addr < PPU_REGISTERS_MIRRORS_END) {
            _ = addr & 0b00100000_00000111;
            std.log.warn("PPU not supported yet!", .{});
            return 0;
        } else if (addr >= 0x8000 and addr < 0x10000) {
            return self.read_prg_rom(addr);
        } else {
            std.log.warn("Ignoring mem read at 0x{X:04}", .{addr});
            return 0;
        }
    }

    pub fn mem_write(self: *Self, addr: u16, data: u8) void {
        if (addr >= RAM and addr <= RAM_MIRRORS_END) {
            const mirror_down_addr = addr & 0b11111111111;
            self.ram[mirror_down_addr] = data;
        } else if (addr >= PPU_REGISTERS and addr <= PPU_REGISTERS_MIRRORS_END) {
            _ = addr & 0b00100000_00000111;
            std.log.warn("PPU not supported yet!", .{});
        } else if (addr >= 0x8000 and addr <= 0xFFFF) {
            @panic("Attempt to write to cartridge ROM memory space");
        } else {
            std.log.warn("Ignoring mem write at 0x{X:04}", .{addr});
        }
    }

    pub fn mem_read_u16(self: Self, addr: u16) u16 {
        const lo = @as(u16, self.mem_read(addr));
        const hi = @as(u16, self.mem_read(addr + 1));
        return (hi << 8) | lo;
    }

    pub fn mem_write_u16(self: *Self, addr: u16, data: u16) void {
        const hi: u8 = @truncate(data >> 8);
        const lo: u8 = @truncate(data);
        self.mem_write(addr, lo);
        self.mem_write(addr + 1, hi);
    }

    // PRG Rom Size might be 16 KiB or 32 KiB. Because `0x8000..0x10000` mapped region is 32 KiB of addressable space,
    // the upper 16 KiB needs to be mapped to the lower 16 KiB (if a game has only 16 KiB of PRG ROM)
    fn read_prg_rom(self: Self, addr: u16) u8 {
        var new_addr = addr - 0x8000;
        // Check if the PRG Rom Size is 16 KiB and we're trying to access memory pass the 16 KiB.
        if (self.rom.prg_rom.len == 0x4000 and new_addr >= 0x4000) {
            new_addr = new_addr % 0x4000;
        }
        return self.rom.prg_rom[new_addr];
    }
};
