const std = @import("std");
const Rom = @import("rom.zig").Rom;

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

pub const Bus = struct {
    ram: [2048]u8,
    rom: Rom,

    const Self = @This();

    pub fn init(rom: Rom) Self {
        return .{ .ram = [_]u8{0} ** 2048, .rom = rom };
    }

    pub fn mem_read(self: Self, addr: u16) u8 {
        if (addr >= RAM and addr <= RAM_MIRRORS_END) {
            const mirror_down_addr = addr & 0b00000111_11111111;
            return self.ram[mirror_down_addr];
        } else if (addr >= PPU_REGISTERS and addr <= PPU_REGISTERS_MIRRORS_END) {
            _ = addr & 0b00100000_00000111;
            @panic("PPU not supported yet!");
        } else if (addr >= 0x8000 and addr <= 0xFFFF) {
            return self.read_prg_rom(addr);
        } else {
            std.log.info("Ignoring mem read at 0x{X:04}", .{addr});
            return 0;
        }
    }

    pub fn mem_write(self: *Self, addr: u16, data: u8) void {
        if (addr >= RAM and addr <= RAM_MIRRORS_END) {
            const mirror_down_addr = addr & 0b11111111111;
            self.ram[mirror_down_addr] = data;
        } else if (addr >= PPU_REGISTERS and addr <= PPU_REGISTERS_MIRRORS_END) {
            _ = addr & 0b00100000_00000111;
            @panic("PPU not supported yet!");
        } else if (addr >= 0x8000 and addr <= 0xFFFF) {
            @panic("Attempt to write to cartridge ROM memory space");
        } else {
            std.log.info("Ignoring mem write at 0x{X:04}", .{addr});
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

    fn read_prg_rom(self: Self, addr: u16) u8 {
        var new_addr = addr - 0x8000;
        // Check if the PRG Rom Size is 16 KiB and we're trying to access memory pass the 16 KiB.
        if (self.rom.prg_rom.len == 0x4000 and new_addr >= 0x4000) {
            new_addr = new_addr % 0x4000;
        }
        return self.rom.prg_rom[new_addr];
    }
};
