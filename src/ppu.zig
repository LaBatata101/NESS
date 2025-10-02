const std = @import("std");
const Mirroring = @import("rom.zig").Mirroring;

const ControlRegister = packed struct(u8) {
    /// Base nametable address (false = 0x2000; true = 0x2400)
    name_table1: bool = false,
    /// Base nametable address (false = 0x2800; true = 0x2C00)
    name_table2: bool = false,

    /// VRAM address increment per CPU read/write of `PPUDATA` (`false`: add 1, going across; `true`: add 32, going down)
    vram_add_increment: bool = false,

    /// Sprite pattern table address for 8x8 sprites (`false`: 0x0000; `true`: 0x1000; ignored in 8x16 mode)
    sprite_pattern_addr: bool = false,

    /// Background pattern table address (`false`: 0x0000; `true`: 0x1000)
    background_pattern_addr: bool = false,

    /// Sprite size (`false`: 8x8 pixels; `true`: 8x16 pixels)
    sprite_size: bool = false,

    /// PPU master/slave select (`false`: read backdrop from EXT pins; `true`: output color on EXT pins)
    master_slave_select: bool = false,

    /// Generate an NMI at the start of the vertical blanking interval (`false`: off; `true`: on)
    generate_nmi: bool = false,

    fn vram_addr_increment(self: @This()) u8 {
        return if (self.vram_add_increment) 32 else 1;
    }
};

const MaskRegister = packed struct(u8) {
    /// (`false`: normal color, `true`: greyscale)
    greyscale: bool = false,

    /// `true`: Show background in leftmost 8 pixels of screen, `false`: Hide
    show_background: bool = false,

    /// `true`: Show sprites in leftmost 8 pixels of screen, `false`: Hide
    show_sprites: bool = false,

    /// `true`: Enable background rendering
    background_rendering: bool = false,

    /// `true`: Enable sprite rendering
    sprite_rendering: bool = false,

    /// (green on PAL/Dendy)
    emphasize_red: bool = false,

    /// (red on PAL/Dendy)
    emphasize_green: bool = false,
    emphasize_blue: bool = false,
};

const StatusRegister = packed struct(u8) {
    open_bus: u5 = 0,
    sprite_overflow: bool = false,
    sprite_zero_hit: bool = false,
    /// Vblank flag, cleared on read.
    vblank: bool = false,
};

const ScrollRegister = struct {
    scroll_x: u8 = 0,
    scroll_y: u8 = 0,
};

pub const PPU = struct {
    /// Visuals of a game stored on a cartridge
    chr_rom: []const u8,
    /// Internal memory to keep palette tables used by a screen
    palette_table: [32]u8,
    /// 2 KiB of space to hold background information.
    vram: [2048]u8,

    mirroring: Mirroring,

    /// `0x2000` - write: `PPUCTRL` - Miscellaneous settings
    ///
    /// Contains a mix of settings related to rendering, scroll position, vblank NMI, and dual-PPU configurations.
    ctrl_register: ControlRegister,

    /// `0x2001` - write:  `PPUMASK` - Rendering settings
    ///
    /// Controls the rendering of sprites and backgrounds, as well as color effects. Most commonly, PPUMASK is set to
    /// `0x00` outside of gameplay to allow transferring a large amount of data to VRAM, and `0x1E` during gameplay to
    /// enable all rendering with no color effects.
    mask_register: MaskRegister,

    /// `0x2002` - read: `PPUSTATUS` - Rendering events
    ///
    /// Reflects the state of rendering-related events and is primarily used for timing. The three flags in this
    /// register are automatically cleared on dot 1 of the prerender scanline.
    ///
    /// Reading this register has the side effect of clearing the PPU's `internal w register`. It is commonly read
    /// before writes to `PPUSCROLL` and `PPUADDR` to ensure the writes occur in the correct order.
    status_register: StatusRegister,

    /// `0x2003` - write: `OAMADDR` - Sprite RAM address
    ///
    /// Write the address of OAM you want to access here. Most games just write 0x00 here and then use OAMDMA. (DMA is
    /// implemented in the 2A03/7 chip and works by repeatedly writing to OAMDATA).
    oam_addr_register: u8,

    /// `0x2004` - read/write: `OAMDATA` - Sprite RAM data
    ///
    /// Write OAM data here. Writes will increment `OAMADDR` after the write; reads do not. Reads during vertical or
    /// forced blanking return the value from OAM at that address.
    oam_data_register: [256]u8,

    /// `0x2005` - write: `PPUSCROLL` - X and Y scroll
    ///
    /// This register is used to change the scroll position, telling the PPU which pixel of the nametable selected
    /// through `PPUCTRL` should be at the top left corner of the rendered screen. `PPUSCROLL` takes two writes: the
    /// first is the X scroll and the second is the Y scroll. Whether this is the first or second write is tracked
    /// internally by the `w register`, which is shared with `PPUADDR`.
    scroll_register: ScrollRegister,

    /// `0x2006` - write : `PPUADDR` - VRAM address (two writes: most significant byte, then least significant byte).
    ///
    /// The 16-bit address is written to PPUADDR one byte at a time. Whether this is the first or second write is
    /// tracked by the PPU's internal `w` register.
    addr_register: u16,

    /// `Internal register W`: Toggles on each write to either `PPUSCROLL` or `PPUADDR`, indicating whether this is the
    /// first or second write. Clears on reads of `PPUSTATUS`. Sometimes called the 'write latch' or 'write toggle'.
    write_toggle: bool,

    internal_data_buf: u8,

    const Self = @This();

    pub fn init(chr_rom: []const u8, mirroring: Mirroring) Self {
        return .{
            .chr_rom = chr_rom,
            .palette_table = [_]u8{0} ** 32,
            .vram = [_]u8{0} ** 2048,
            .mirroring = mirroring,
            .write_toggle = true,
            .ctrl_register = .{},
            .mask_register = .{},
            .status_register = .{},
            .oam_addr_register = 0,
            .oam_data_register = [_]u8{0} ** 256,
            .scroll_register = .{},
            .addr_register = 0,
            .internal_data_buf = 0,
        };
    }

    /// Write a value to the PPUADDR register.
    pub fn addr_write(self: *Self, value: u8) void {
        if (self.write_toggle) {
            self.addr_register = self.addr_register & ~@as(u16, 0xFF00) | (@as(u16, value) << 8);
        } else {
            self.addr_register = self.addr_register & ~@as(u16, 0xFF) | value;
        }

        // mirror down addr above 0x3fff
        if (self.addr_register > 0x3FFF) {
            self.addr_register &= 0b11111111111111;
        }
        self.write_toggle = !self.write_toggle;
    }

    /// Increment the PPUADDR by `inc`.
    pub fn addr_increment(self: *Self, inc: u8) void {
        self.addr_register +%= inc;

        // mirror down addr above 0x3fff
        if (self.addr_register > 0x3FFF) {
            self.addr_register &= 0b11111111111111;
        }
    }

    fn increment_vram_addr(self: *Self) void {
        self.addr_increment(self.ctrl_register.vram_addr_increment());
    }

    fn reset_write(self: *Self) void {
        self.write_toggle = true;
    }

    /// Horizontal:
    ///   [ A ] [ a ]
    ///   [ B ] [ b ]
    ///
    /// Vertical:
    ///   [ A ] [ B ]
    ///   [ a ] [ b ]
    fn mirror_vram_addr(self: Self, addr: u16) u16 {
        // mirror down 0x3000-0x3eff to 0x2000 - 0x2eff
        const mirrored_vram = addr & 0b10111111111111;
        // to vram vector
        const vram_index = mirrored_vram - 0x2000;
        // to the name table index
        const name_table = vram_index / 0x400;

        return switch (self.mirroring) {
            Mirroring.VERTICAL => switch (name_table) {
                2, 3 => vram_index - 0x800,
                else => vram_index,
            },
            Mirroring.HORIZONTAL => switch (name_table) {
                1, 2 => vram_index - 0x400,
                3 => vram_index - 0x800,
                else => vram_index,
            },
            Mirroring.FOUR_SCREEN => vram_index,
        };
    }

    /// `PPUDATA` - VRAM data
    ///
    /// Get value from VRAM data register. After access, the video memory address will increment by an amount
    /// determined by bit 2 of 0x2000 (`PPUCTRL`).
    ///
    /// Reading from PPUDATA does not directly return the value at the current VRAM address, but instead returns the
    /// contents of an internal read buffer. This read buffer is updated on every PPUDATA read, but only after the
    /// previous contents have been returned to the CPU, effectively delaying PPUDATA reads by one.
    pub fn data_read(self: *Self) u8 {
        const addr = self.addr_register;
        self.increment_vram_addr();

        if (addr >= 0 and addr <= 0x1FFF) {
            const result = self.internal_data_buf;
            self.internal_data_buf = self.chr_rom[addr];
            return result;
        } else if (addr >= 0x2000 and addr <= 0x2FFF) {
            const result = self.internal_data_buf;
            self.internal_data_buf = self.vram[self.mirror_vram_addr(addr)];
            return result;
        } else if (addr >= 0x3000 and addr <= 0x3EFF) {
            std.debug.panic("addr space 0x3000..0x3eff is not expected to be used, requested = 0x{X:04}\n", .{addr});
        } else if (addr >= 0x3F00 and addr <= 0x3FFF) {
            return self.palette_table[(addr - 0x3F00)];
        } else {
            std.debug.panic("unexpected access to mirrored space 0x{X:04}\n", .{addr});
        }
    }

    /// `PPUDATA` - VRAM data
    pub fn data_write(self: *Self, value: u8) void {
        const addr = self.addr_register;

        if (addr >= 0 and addr <= 0x1FFF) {
            std.log.err("Attempt to write to CHR ROM space 0x{X:04}", .{addr});
        } else if (addr >= 0x2000 and addr <= 0x2FFF) {
            self.vram[self.mirror_vram_addr(addr)] = value;
        } else if (addr == 0x3F10 or addr == 0x3F14 or addr == 0x3F18 or addr == 0x3F1C) {
            // Addresses 0x3F10/0x3F14/0x3F18/0x3F1C are mirrors of 0x3F00/0x3F04/0x3F08/0x3F0C
            self.palette_table[addr - 0x3F10] = value;
        } else if (addr >= 0x3F00 and addr <= 0x3FFF) {
            self.palette_table[addr - 0x3F00] = value;
        } else {
            std.debug.panic("unexpected access to mirrored space 0x{X:04}\n", .{addr});
        }
        self.increment_vram_addr();
    }

    /// Write a value to the `PPUCTRL` register.
    pub fn ctrl_write(self: *Self, value: u8) void {
        self.ctrl_register = @bitCast(value);
    }

    /// Write a value to the `PPUMASK` register.
    pub fn mask_write(self: *Self, value: u8) void {
        self.mask_register = @bitCast(value);
    }

    /// Write a value to the `OAMADDR` register.
    pub fn oam_addr_write(self: *Self, value: u8) void {
        self.oam_addr_register = value;
    }

    pub fn oam_dma_write(self: *Self, data: [256]u8) void {
        for (data) |byte| {
            self.oam_data_register[self.oam_addr_register] = byte;
            self.oam_addr_register +%= 1;
        }
    }

    pub fn oam_data_write(self: *Self, value: u8) void {
        self.oam_data_register[self.oam_addr_register] = value;
        self.oam_addr_register +%= 1;
    }

    pub fn oam_data_read(self: Self) u8 {
        return self.oam_data_register[self.oam_addr_register];
    }

    pub fn scroll_write(self: *Self, data: u8) void {
        if (!self.write_toggle) {
            self.scroll_register.scroll_x = data;
        } else {
            self.scroll_register.scroll_y = data;
        }
        self.write_toggle = !self.write_toggle;
    }

    pub fn status_read(self: *Self) StatusRegister {
        const status = self.status_register;

        self.write_toggle = true;
        self.status_register.vblank = false;

        return status;
    }
};

test "PPU VRAM write" {
    var ppu = PPU.init(&[_]u8{0} ** 2048, Mirroring.HORIZONTAL);
    ppu.addr_write(0x23);
    ppu.addr_write(0x05);
    ppu.data_write(0x42);

    try std.testing.expectEqual(0x42, ppu.vram[0x0305]);
}

test "PPU VRAM read" {
    var ppu = PPU.init(&[_]u8{0} ** 2048, Mirroring.HORIZONTAL);
    ppu.ctrl_write(0);
    ppu.vram[0x0305] = 0x42;

    ppu.addr_write(0x23);
    ppu.addr_write(0x05);
    _ = ppu.data_read(); // load data to internal buffer

    try std.testing.expectEqual(0x2306, ppu.addr_register);
    try std.testing.expectEqual(0x42, ppu.data_read());
}

test "PPU VRAM read cross page" {
    var ppu = PPU.init(&[_]u8{0} ** 2048, Mirroring.HORIZONTAL);
    ppu.ctrl_write(0);
    ppu.vram[0x01ff] = 0x42;
    ppu.vram[0x0200] = 0x69;

    ppu.addr_write(0x21);
    ppu.addr_write(0xFF);
    _ = ppu.data_read(); // load data to internal buffer

    try std.testing.expectEqual(0x42, ppu.data_read());
    try std.testing.expectEqual(0x69, ppu.data_read());
}

test "PPU VRAM read - step 32" {
    var ppu = PPU.init(&[_]u8{0} ** 2048, Mirroring.HORIZONTAL);
    ppu.ctrl_register.vram_add_increment = true;
    ppu.vram[0x01ff] = 0x12;
    ppu.vram[0x01ff + 32] = 0x13;
    ppu.vram[0x01ff + 64] = 0x22;

    ppu.addr_write(0x21);
    ppu.addr_write(0xff);
    _ = ppu.data_read(); // load data to internal buffer

    try std.testing.expectEqual(0x12, ppu.data_read());
    try std.testing.expectEqual(0x13, ppu.data_read());
    try std.testing.expectEqual(0x22, ppu.data_read());
}

// Horizontal: https://wiki.nesdev.com/w/index.php/Mirroring
//   [0x2000 A ] [0x2400 a ]
//   [0x2800 B ] [0x2C00 b ]
test "PPU VRAM horizontal mirror" {
    var ppu = PPU.init(&[_]u8{0} ** 2048, Mirroring.HORIZONTAL);
    ppu.addr_write(0x24);
    ppu.addr_write(0x05);

    ppu.data_write(0x66); // write to A

    ppu.addr_write(0x28);
    ppu.addr_write(0x05);

    ppu.data_write(0x77); // write to B

    ppu.addr_write(0x20);
    ppu.addr_write(0x05);

    _ = ppu.data_read(); // load data to internal buffer
    try std.testing.expectEqual(0x66, ppu.data_read()); // read from a

    ppu.addr_write(0x2C);
    ppu.addr_write(0x05);

    _ = ppu.data_read(); // load data to internal buffer
    try std.testing.expectEqual(0x77, ppu.data_read()); // read from b
}

// Vertical: https://wiki.nesdev.com/w/index.php/Mirroring
//   [0x2000 A ] [0x2400 B ]
//   [0x2800 a ] [0x2C00 b ]
test "PPU VRAM vertical mirror" {
    var ppu = PPU.init(&[_]u8{0} ** 2048, Mirroring.VERTICAL);
    ppu.addr_write(0x20);
    ppu.addr_write(0x05);

    ppu.data_write(0x66); // write to A

    ppu.addr_write(0x2C);
    ppu.addr_write(0x05);

    ppu.data_write(0x77); // write to B

    ppu.addr_write(0x28);
    ppu.addr_write(0x05);

    _ = ppu.data_read(); // load data to internal buffer
    try std.testing.expectEqual(0x66, ppu.data_read()); // read from a

    ppu.addr_write(0x24);
    ppu.addr_write(0x05);

    _ = ppu.data_read(); // load data to internal buffer
    try std.testing.expectEqual(0x77, ppu.data_read()); // read from b
}

test "read status resets write toggle (internal w register)" {
    var ppu = PPU.init(&[_]u8{0} ** 2048, Mirroring.HORIZONTAL);
    ppu.vram[0x0305] = 0x66;

    ppu.addr_write(0x21);
    ppu.addr_write(0x23);
    ppu.addr_write(0x05); // write toggle state is false after this

    _ = ppu.data_read(); // load data to internal buffer
    try std.testing.expect(ppu.data_read() != 0x66);

    _ = ppu.status_read();

    ppu.addr_write(0x23);
    ppu.addr_write(0x05);

    _ = ppu.data_read(); // load data to internal buffer
    try std.testing.expectEqual(0x66, ppu.data_read());
}

test "PPU VRAM mirroring" {
    var ppu = PPU.init(&[_]u8{0} ** 2048, Mirroring.HORIZONTAL);
    ppu.vram[0x0305] = 0x66;

    ppu.addr_write(0x63); // 0x6305 -> 0x2305
    ppu.addr_write(0x05);

    _ = ppu.data_read(); // load data to internal buffer
    try std.testing.expectEqual(0x66, ppu.data_read());
}

test "OAM read write" {
    var ppu = PPU.init(&[_]u8{0} ** 2048, Mirroring.HORIZONTAL);
    ppu.oam_addr_write(0x10);
    ppu.oam_data_write(0x42);
    ppu.oam_data_write(0x69);

    ppu.oam_addr_write(0x10);
    try std.testing.expectEqual(0x42, ppu.oam_data_read());

    ppu.oam_addr_write(0x11);
    try std.testing.expectEqual(0x69, ppu.oam_data_read());
}

test "read status resets vblank" {
    var ppu = PPU.init(&[_]u8{0} ** 2048, Mirroring.HORIZONTAL);
    ppu.status_register.vblank = true;

    const status = ppu.status_read();

    try std.testing.expect(status.vblank);
    try std.testing.expect(!ppu.status_register.vblank);
}

test "OAM DMA" {
    var ppu = PPU.init(&[_]u8{0} ** 2048, Mirroring.HORIZONTAL);
    var data = [_]u8{0x69} ** 256;
    data[0] = 0x42;
    data[255] = 0x13;

    ppu.oam_addr_write(0x10);
    ppu.oam_dma_write(data);

    ppu.oam_addr_write(0xF); // wrap around
    try std.testing.expectEqual(0x13, ppu.oam_data_read());

    ppu.oam_addr_write(0x10);
    try std.testing.expectEqual(0x42, ppu.oam_data_read());

    ppu.oam_data_write(0x11);
    try std.testing.expectEqual(0x69, ppu.oam_data_read());
}
