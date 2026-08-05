const std = @import("std");

const Mirroring = @import("rom.zig").Mirroring;
const render = @import("render.zig");
const Rom = @import("rom.zig").Rom;
const Frame = render.Frame;
const PatternTableFrame = render.PatternTableFrame;

/// Aprox. amount of CPU cycles equivalent to 1 second.
const CPU_ONE_SEC_CYLES: u64 = 1_790_000;
/// Aprox. amount of PPU cycles equivalent to 1 second.
const PPU_ONE_SEC_CYLES: u64 = CPU_ONE_SEC_CYLES / 3;
const CYCLES_PER_SCANLINE: u16 = 341;
const SCANLINES_PER_FRAME: u16 = 262;
const CYCLES_PER_FRAME: u64 = @as(u64, CYCLES_PER_SCANLINE) * @as(u64, SCANLINES_PER_FRAME);

const ControlRegister = packed struct(u8) {
    /// Base nametable address (0 = 0x2000; 1 = 0x2400; 2 = 0x2800; 3 = 0x2C00)
    name_table_addr: u2 = 0,

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

    pub fn bg_pattern_addr(self: @This()) u16 {
        return if (self.background_pattern_addr) 0x1000 else 0;
    }

    pub fn sprt_pattern_addr(self: @This()) u16 {
        return if (self.sprite_pattern_addr) 0x1000 else 0;
    }

    pub fn nametable_addr(self: @This()) u16 {
        return switch (self.name_table_addr) {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2C00,
        };
    }
};

const MaskRegister = packed struct(u8) {
    /// (`false`: normal color, `true`: greyscale)
    greyscale: bool = false,

    /// `true`: Show background in leftmost 8 pixels of screen, `false`: Hide
    render_background_left: bool = false,

    /// `true`: Show sprites in leftmost 8 pixels of screen, `false`: Hide
    render_sprite_left: bool = false,

    /// `true`: Enable background rendering
    render_background: bool = false,

    /// `true`: Enable sprite rendering
    render_sprite: bool = false,

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
    fine_x: u8 = 0,
    fine_y: u8 = 0,
};

const AddrRegister = packed struct(u16) {
    coarse_x: u5 = 0,
    coarse_y: u5 = 0,
    nametable_x: u1 = 0,
    nametable_y: u1 = 0,
    fine_y: u3 = 0,
    unused: u1 = 0,

    fn addr(self: @This()) u16 {
        return @bitCast(self);
    }
};

const BgData = struct {
    // Two 16-bit shift registers containing bitmap data for 2 tiles. Every 8 cycles the data for the next tile is
    // loaded into the upper 8 bits of the register, meanwhile the pixel to render is fetched from the lower 8 bits.
    shifter_pattern_lo: u16 = 0,
    shifter_pattern_hi: u16 = 0,

    tile_addr: u16 = 0,
    next_tile_lo: u8 = 0,
    next_tile_hi: u8 = 0,
    next_tile_id: u8 = 0,
    next_tile_attr: u8 = 0,

    // Two 8-bit shift registers containing the palette attributes for the lower 8 pixels of the 16-bit register. These
    // registers are fed by a latch which contains the palette attribute for the next tile.
    shifter_attr_lo: u8 = 0,
    shifter_attr_hi: u8 = 0,
    next_attr_lo: u8 = 0,
    next_attr_hi: u8 = 0,
};

const OamAttr = packed struct(u8) {
    /// Palette (4 to 7) of sprite
    palette: u2 = 0,
    unimplemented: u3 = 0,
    /// Priority (false: in front of background; true: behind background)
    priority: bool = false,
    /// Flip sprite horizontally
    horizontally_flipped: bool = false,
    /// Flip sprite vertically
    vertically_flipped: bool = false,
};

const OamEntry = struct {
    /// Y position of sprite
    y: u8 = 0,
    /// ID of tile from pattern memory
    id: u8 = 0,
    /// Flags define how sprite should be rendered
    attr: OamAttr = .{},
    /// X position of sprite
    x: u8 = 0,

    fn init(data: []const u8) @This() {
        return .{
            .y = data[0],
            .id = data[1],
            .attr = @bitCast(data[2]),
            .x = data[3],
        };
    }
};

const SpriteData = struct {
    secondary_oam: [8]OamEntry = [_]OamEntry{.{}} ** 8,
    count: u8 = 0,
    copied: u8 = 0,
    oam_byte: u8 = 0,
    shifter_pattern_lo: [8]u8 = [_]u8{0} ** 8,
    shifter_pattern_hi: [8]u8 = [_]u8{0} ** 8,

    attributes: [8]OamAttr = [_]OamAttr{.{}} ** 8,
    x_pos: [8]u8 = [_]u8{0} ** 8,
    has_sprite_at_x: [256]bool = [_]bool{false} ** 256,

    zero_hit_possible: bool = false,
    zero_being_rendered: bool = false,

    queued_copies: u8 = 0,
    eval_phase: u8 = 0,
    /// OAM byte m (0-3)
    m: u8 = 0,
    /// Sprite n (0-63)
    n: u8 = 0,

    /// Write a value to the secondary OAM.
    fn write_to_oam(self: *@This(), pos: u16, value: u8) void {
        var bytes: []u8 = std.mem.asBytes(&self.secondary_oam);
        bytes[pos] = value;
    }

    comptime {
        std.debug.assert(@sizeOf(OamEntry) == 4);
        std.debug.assert(@sizeOf([8]OamEntry) == 32);
    }
};

fn div_rem(num: u64, den: u64) struct { u64, u64 } {
    return .{ @divFloor(num, den), num % den };
}

fn ppu_to_cpu_cycle(ppu_cyc: u64) u64 {
    const div, const rem = div_rem(ppu_cyc, 3);
    return if (rem == 0) div else div + 1;
}

fn cpu_to_ppu_cycle(cpu_cyc: u64) u64 {
    return cpu_cyc * 3;
}

/// - PPU memory map
/// The PPU addresses a 14-bit (16kB) address space, `0x0000-0x3FFF`, completely separate from the CPU's address bus. It
/// is either directly accessed by the PPU itself, or via the CPU with memory mapped registers at `0x2006` and `0x2007`.
///
/// *Address range*       *Size*    Description             Mapped by
/// `$0000-$0FFF`         `$1000`   Pattern table 0         Cartridge
/// `$1000-$1FFF`         `$1000`   Pattern table 1         Cartridge
/// `$2000-$23BF`         `$03C0`   Nametable 0             Cartridge
/// `$23C0-$23FF`         `$0040`   Attribute table 0       Cartridge
/// `$2400-$27BF`         `$03C0`   Nametable 1             Cartridge
/// `$27C0-$27FF`         `$0040`   Attribute table 1       Cartridge
/// `$2800-$2BBF`         `$03C0`   Nametable 2             Cartridge
/// `$2BC0-$2BFF`         `$0040`   Attribute table 2       Cartridge
/// `$2C00-$2FBF`         `$03C0`   Nametable 3             Cartridge
/// `$2FC0-$2FFF`         `$0040`   Attribute table 3       Cartridge
/// `$3000-$3EFF`         `$0F00`   Unused                  Cartridge
/// `$3F00-$3F1F`         `$0020`   Palette RAM indexes     Internal to PPU
/// `$3F20-$3FFF`         `$00E0`   Mirrors of $3F00-$3F1F  Internal to PPU
pub const PPU = struct {
    rom: *Rom,
    /// Internal memory to keep palette tables used by a screen
    palette_table: [32]u8,
    /// Palette RAM resolved to RGBA colors, including mirroring and greyscale.
    resolved_palette: [32]render.Color,
    /// 2 KiB of space to hold background information, can hold two nametables.
    vram: [2048]u8,

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

    /// `0x2006` - write: `PPUADDR` - VRAM address (two writes: most significant byte, then least significant byte).
    ///
    /// The 16-bit address is written to PPUADDR one byte at a time. Whether this is the first or second write is
    /// tracked by the PPU's internal `w` register.
    addr_register: AddrRegister,
    tmp_addr: AddrRegister,

    /// `Internal register W`: Toggles on each write to either `PPUSCROLL` or `PPUADDR`, indicating whether this is the
    /// first or second write. Clears on reads of `PPUSTATUS`. Sometimes called the 'write latch' or 'write toggle'.
    write_toggle: bool,
    fine_x: u8,

    /// `0x4014` - write: `Sprite DMA`
    /// OAMDMA is a CPU register that suspends the CPU so it can quickly copy a page of CPU memory to PPU OAM using DMA.
    /// The value written to this register is the high byte of the source address, and the copy begins on the cycle
    /// immediately after the write.
    oam_dma_addr: u8,

    internal_data_buf: u8,

    cycle: u16,
    scanline: u16,

    bg_data: BgData,
    sprite_data: SpriteData,

    nmi_interrupt: bool,
    nmi_interrupt_delay: bool,
    nmi_interrupt_cycle: u64,

    frame_buffer: Frame,
    frame_complete: bool,

    global_cycle: u64,
    next_vblank_ppu_cycle: u64,
    next_vblank_cpu_cycle: u64,
    pending_mapper_addr: ?u16,
    pending_mapper_cycle: u64,
    suppress_vblank_set: bool,

    /// A fake dynamic latch representing the capacitance of the wires in the
    /// PPU that we have to emulate.
    dynamic_latch: u8,
    // Stores the last time the data passed through the bus.
    last_cycle_written: u64,

    frame_is_odd: bool,
    skip_odd_frame_cycle: bool,
    current_frame_buffer_index: usize,

    const Self = @This();
    const PRE_RENDER_SCANLINE: u16 = 261;
    // CPU-visible VBL/NMI edge timings used by Blargg's ppu_vbl_nmi tests.
    const VBL_NMI_DELAY: u64 = 15;
    const VBL_NMI_CANCEL_LAST_CYCLE: u16 = 3;
    // Rendering enable is sampled before dot 339, then the odd-frame dot is skipped after dot 339.
    const ODD_FRAME_SKIP_SAMPLE_CYCLE: u16 = 338;
    const ODD_FRAME_SKIP_APPLY_CYCLE: u16 = 339;

    pub const Snapshot = struct {
        palette_table: [32]u8,
        vram: [2048]u8,
        ctrl_register: ControlRegister,
        mask_register: MaskRegister,
        status_register: StatusRegister,
        oam_addr_register: u8,
        oam_data_register: [256]u8,
        scroll_register: ScrollRegister,
        addr_register: AddrRegister,
        tmp_addr: AddrRegister,
        write_toggle: bool,
        fine_x: u8,
        oam_dma_addr: u8,
        internal_data_buf: u8,
        cycle: u16,
        scanline: u16,
        bg_data: BgData,
        sprite_data: *const SpriteData,
        nmi_interrupt: bool,
        nmi_interrupt_delay: bool,
        nmi_interrupt_cycle: u64,
        frame_buffer: *const Frame,
        frame_complete: bool,
        global_cycle: u64,
        next_vblank_ppu_cycle: u64,
        next_vblank_cpu_cycle: u64,
        pending_mapper_addr: ?u16,
        pending_mapper_cycle: u64,
        suppress_vblank_set: bool,
        dynamic_latch: u8,
        last_cycle_written: u64,
        frame_is_odd: bool,
        skip_odd_frame_cycle: bool,
        current_frame_buffer_index: usize,

        pub fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            alloc.destroy(self.frame_buffer);
            alloc.destroy(self.sprite_data);
        }
    };

    pub fn init(rom: *Rom) Self {
        return .{
            .rom = rom,
            .palette_table = [_]u8{0} ** 32,
            .resolved_palette = [_]render.Color{render.SYSTEM_PALETTE[0]} ** 32,
            .vram = [_]u8{0} ** 2048,
            .write_toggle = false,
            .ctrl_register = .{},
            .mask_register = .{},
            .status_register = .{},
            .oam_addr_register = 0,
            .oam_data_register = [_]u8{0} ** 256,
            .oam_dma_addr = 0,
            .scroll_register = .{},
            .addr_register = .{},
            .tmp_addr = .{},
            .internal_data_buf = 0,
            .scanline = PRE_RENDER_SCANLINE,
            .cycle = 0,
            .nmi_interrupt = false,
            .nmi_interrupt_delay = false,
            .nmi_interrupt_cycle = 0,
            .bg_data = .{},
            .sprite_data = .{},
            .fine_x = 0,
            .frame_buffer = Frame.init(),
            .frame_complete = false,
            .global_cycle = 0,
            .next_vblank_ppu_cycle = 1,
            .next_vblank_cpu_cycle = ppu_to_cpu_cycle(1),
            .pending_mapper_addr = null,
            .pending_mapper_cycle = 0,
            .suppress_vblank_set = false,
            .dynamic_latch = 0,
            .frame_is_odd = false,
            .skip_odd_frame_cycle = false,
            .last_cycle_written = 0,
            .current_frame_buffer_index = 0,
        };
    }

    pub fn reset(self: *Self) void {
        @memset(&self.palette_table, 0);
        self.resolved_palette = [_]render.Color{render.SYSTEM_PALETTE[0]} ** 32;
        @memset(&self.vram, 0);
        @memset(&self.oam_data_register, 0);
        self.addr_register = .{};
        self.bg_data = .{};
        self.ctrl_register = .{};
        self.cycle = 0;
        self.dynamic_latch = 0;
        self.fine_x = 0;
        self.frame_buffer.clear();
        self.frame_complete = false;
        self.global_cycle = 0;
        self.internal_data_buf = 0;
        self.mask_register = .{};
        self.next_vblank_cpu_cycle = ppu_to_cpu_cycle(1);
        self.next_vblank_ppu_cycle = 1;
        self.pending_mapper_addr = null;
        self.pending_mapper_cycle = 0;
        self.suppress_vblank_set = false;
        self.nmi_interrupt = false;
        self.nmi_interrupt_delay = false;
        self.nmi_interrupt_cycle = 0;
        self.oam_addr_register = 0;
        self.scanline = 0;
        self.scroll_register = .{};
        self.sprite_data = .{};
        self.status_register = .{};
        self.tmp_addr = .{};
        self.write_toggle = false;
        self.frame_is_odd = false;
        self.skip_odd_frame_cycle = false;
        self.last_cycle_written = 0;
    }

    pub fn saveState(self: *const Self, alloc: std.mem.Allocator) !Snapshot {
        const frame_buffer = try alloc.create(Frame);
        const sprite_data = try alloc.create(SpriteData);

        frame_buffer.* = self.frame_buffer;
        sprite_data.* = self.sprite_data;

        return .{
            .palette_table = self.palette_table,
            .vram = self.vram,
            .ctrl_register = self.ctrl_register,
            .mask_register = self.mask_register,
            .status_register = self.status_register,
            .oam_addr_register = self.oam_addr_register,
            .oam_data_register = self.oam_data_register,
            .scroll_register = self.scroll_register,
            .addr_register = self.addr_register,
            .tmp_addr = self.tmp_addr,
            .write_toggle = self.write_toggle,
            .fine_x = self.fine_x,
            .oam_dma_addr = self.oam_dma_addr,
            .internal_data_buf = self.internal_data_buf,
            .cycle = self.cycle,
            .scanline = self.scanline,
            .bg_data = self.bg_data,
            .sprite_data = sprite_data,
            .nmi_interrupt = self.nmi_interrupt,
            .nmi_interrupt_delay = self.nmi_interrupt_delay,
            .nmi_interrupt_cycle = self.nmi_interrupt_cycle,
            .frame_buffer = frame_buffer,
            .frame_complete = self.frame_complete,
            .global_cycle = self.global_cycle,
            .next_vblank_ppu_cycle = self.next_vblank_ppu_cycle,
            .next_vblank_cpu_cycle = self.next_vblank_cpu_cycle,
            .pending_mapper_addr = self.pending_mapper_addr,
            .pending_mapper_cycle = self.pending_mapper_cycle,
            .suppress_vblank_set = self.suppress_vblank_set,
            .dynamic_latch = self.dynamic_latch,
            .last_cycle_written = self.last_cycle_written,
            .frame_is_odd = self.frame_is_odd,
            .skip_odd_frame_cycle = self.skip_odd_frame_cycle,
            .current_frame_buffer_index = self.current_frame_buffer_index,
        };
    }

    pub fn loadState(self: *Self, snapshot: *const Snapshot) void {
        self.palette_table = snapshot.palette_table;
        self.vram = snapshot.vram;
        self.ctrl_register = snapshot.ctrl_register;
        self.mask_register = snapshot.mask_register;
        self.refresh_resolved_palette();
        self.status_register = snapshot.status_register;
        self.oam_addr_register = snapshot.oam_addr_register;
        self.oam_data_register = snapshot.oam_data_register;
        self.scroll_register = snapshot.scroll_register;
        self.addr_register = snapshot.addr_register;
        self.tmp_addr = snapshot.tmp_addr;
        self.write_toggle = snapshot.write_toggle;
        self.fine_x = snapshot.fine_x;
        self.oam_dma_addr = snapshot.oam_dma_addr;
        self.internal_data_buf = snapshot.internal_data_buf;
        self.cycle = snapshot.cycle;
        self.scanline = snapshot.scanline;
        self.bg_data = snapshot.bg_data;
        self.sprite_data = snapshot.sprite_data.*;
        self.nmi_interrupt = snapshot.nmi_interrupt;
        self.nmi_interrupt_delay = snapshot.nmi_interrupt_delay;
        self.nmi_interrupt_cycle = snapshot.nmi_interrupt_cycle;
        self.frame_buffer = snapshot.frame_buffer.*;
        self.frame_complete = snapshot.frame_complete;
        self.global_cycle = snapshot.global_cycle;
        self.next_vblank_ppu_cycle = snapshot.next_vblank_ppu_cycle;
        self.next_vblank_cpu_cycle = snapshot.next_vblank_cpu_cycle;
        self.pending_mapper_addr = snapshot.pending_mapper_addr;
        self.pending_mapper_cycle = snapshot.pending_mapper_cycle;
        self.suppress_vblank_set = snapshot.suppress_vblank_set;
        self.dynamic_latch = snapshot.dynamic_latch;
        self.last_cycle_written = snapshot.last_cycle_written;
        self.frame_is_odd = snapshot.frame_is_odd;
        self.skip_odd_frame_cycle = snapshot.skip_odd_frame_cycle;
        self.current_frame_buffer_index = snapshot.current_frame_buffer_index;
    }

    pub fn run_to(self: *Self, cpu_cycle: u64) void {
        const stop = cpu_to_ppu_cycle(cpu_cycle);
        while (self.global_cycle < stop) {
            self.tick();
        }
    }

    pub fn requested_run_cycle(self: *const Self) u64 {
        return self.next_vblank_cpu_cycle;
    }

    pub fn tick(self: *Self) void {
        if (self.pending_mapper_addr) |addr| {
            if (self.global_cycle >= self.pending_mapper_cycle) {
                self.rom.mapper_ppu_address_updated(addr, self.global_cycle);
                self.pending_mapper_addr = null;
            }
        }

        switch (self.scanline) {
            // All but 1 of the scanlines is visible to the user. The pre-render scanline
            // at 261, is used to configure the "shifters" for the first visible scanline, 0.
            0...239, 261 => self.render_scanline(),
            // PPU does nothing on the idle scanline.
            240 => {},
            241...260 => self.vblank_scanline(),
            else => std.debug.panic("Scanline shouldn't be bigger than 261. Got {}\n", .{self.scanline}),
        }
        const skip_after_this_dot = self.scanline == PRE_RENDER_SCANLINE and
            self.cycle == ODD_FRAME_SKIP_APPLY_CYCLE and self.skip_odd_frame_cycle;
        if (self.scanline == PRE_RENDER_SCANLINE and self.cycle == ODD_FRAME_SKIP_SAMPLE_CYCLE and self.frame_is_odd) {
            self.skip_odd_frame_cycle = self.is_rendering_enabled();
        }

        self.cycle += 1;
        self.global_cycle += 1;

        const dots_this_scanline: u16 = CYCLES_PER_SCANLINE;

        if (skip_after_this_dot) {
            self.skip_odd_frame_cycle = false;
            self.cycle = 0;
            self.scanline = 0;
            self.frame_is_odd = !self.frame_is_odd;
            self.frame_complete = true;
        } else if (self.cycle >= dots_this_scanline) {
            self.cycle = 0;
            self.scanline = (self.scanline + 1) % SCANLINES_PER_FRAME;

            // Check if we just wrapped to a new frame
            if (self.scanline == 0) {
                self.frame_is_odd = !self.frame_is_odd;
                self.frame_complete = true;
            } else {
                self.frame_complete = false;
            }
        }
    }

    fn vblank_scanline(self: *Self) void {
        if (self.scanline == 241 and self.cycle == 1) {
            const cycles_this_frame = if (self.frame_is_odd) CYCLES_PER_FRAME - 1 else CYCLES_PER_FRAME;
            self.next_vblank_ppu_cycle += cycles_this_frame;
            self.next_vblank_cpu_cycle = ppu_to_cpu_cycle(self.next_vblank_ppu_cycle);

            if (self.suppress_vblank_set) {
                self.suppress_vblank_set = false;
                self.nmi_interrupt = false;
                self.nmi_interrupt_delay = false;
                self.nmi_interrupt_cycle = 0;
                return;
            }

            self.status_register.vblank = true;
            if (self.ctrl_register.generate_nmi) {
                self.nmi_interrupt = true;
                self.nmi_interrupt_delay = false;
                self.nmi_interrupt_cycle = self.global_cycle + VBL_NMI_DELAY;
            }
        }
    }

    fn render_scanline(self: *Self) void {
        if (self.cycle == 257) {
            @memset(&self.sprite_data.has_sprite_at_x, false);
        }

        switch (self.cycle) {
            // Idle cycle. Just set the index for the frame buffer.
            0 => {
                self.current_frame_buffer_index = @as(usize, self.scanline) * 256 * 4;
            },
            // The data for each tile is fetched during this phase.
            1...256 => self.render_cycle(),
            // The tile data for the sprites on the next scanline are fetched here.
            257...320 => self.fetch_next_sprite_cycle(),
            // This is where the first two tiles for the next scanline are fetched, and loaded into the shift registers.
            321...336 => self.prefetch_tiles_cycle(),
            // Two bytes are fetched, but the purpose for this is unknown.
            337...340 => self.unknown_fetch(),
            else => std.debug.panic("PPU cycle should be bigger than 340. Got {}\n", .{self.cycle}),
        }

        // Sprite evaluation doesn't happen on the pre-render scanline or if the rendering is disabled.
        if (self.scanline != PRE_RENDER_SCANLINE and self.is_rendering_enabled()) {
            self.sprite_evaluation();
        }

        self.handle_scrolling();

        if (self.scanline == PRE_RENDER_SCANLINE and self.cycle == 1) {
            self.status_register.vblank = false;
            self.status_register.sprite_overflow = false;
            self.status_register.sprite_zero_hit = false;
        }
    }

    fn render_cycle(self: *Self) void {
        // Reload shift registers from the latches on cycles 1, 9, 17, ..., 257.
        if (self.cycle % 8 == 1) {
            self.reload_shift_registers();
        }
        self.fetch_tile_data();

        // Render the pixel if we're not on the pre-render scanline.
        if (self.scanline != PRE_RENDER_SCANLINE) {
            const palette, const pixel = self.render_pixel();
            const color = self.get_color(palette, pixel);

            const idx = self.current_frame_buffer_index;
            self.frame_buffer.data[idx..][0..4].* = .{ color.r, color.g, color.b, color.a };

            self.current_frame_buffer_index += 4;
        }

        self.shift_registers();
    }

    fn fetch_next_sprite_cycle(self: *Self) void {
        // MMC3 scanline IRQs depend on sprite pattern A12 rising during the pre-render line too.
        if (self.is_rendering_enabled()) {
            self.sprite_fetch_cycle();
        }
    }

    fn unknown_fetch(self: *Self) void {
        self.bg_data.next_tile_id = self.ppu_read(0x2000 | (self.addr_register.addr() & 0x0FFF));
    }

    fn prefetch_tiles_cycle(self: *Self) void {
        if (self.cycle % 8 == 1) {
            self.reload_shift_registers();
        }
        self.fetch_tile_data();
        self.shift_registers();
    }

    fn sprite_evaluation(self: *Self) void {
        switch (self.cycle) {
            0 => {
                self.sprite_data.zero_being_rendered = self.sprite_data.zero_hit_possible;
                self.sprite_data.count = self.sprite_data.copied;
                self.sprite_data.zero_hit_possible = false;
                self.sprite_data.queued_copies = 0;
                self.sprite_data.eval_phase = 0;
                self.sprite_data.oam_byte = 0;
                self.sprite_data.copied = 0;
                self.sprite_data.n = 0;
                self.sprite_data.m = 0;
            },
            1...64 => if (self.scanline != PRE_RENDER_SCANLINE and self.cycle % 2 == 0) {
                self.sprite_data.write_to_oam((self.cycle / 2) - 1, 0xFF);
            },
            65...256 => if (self.scanline != PRE_RENDER_SCANLINE) self.sprite_evaluation_cycle(),
            else => {},
        }
    }

    fn sprite_evaluation_cycle(self: *Self) void {
        // Read from primary OAM on odd cycles
        if (self.cycle % 2 == 1) {
            self.sprite_data.oam_byte = self.oam_data_register[self.sprite_data.n * 4 + self.sprite_data.m];
            return;
        }

        const sprite_size: u16 = if (self.ctrl_register.sprite_size) 16 else 8;
        const min_y = self.scanline -| (sprite_size - 1);
        const max_y = self.scanline;

        switch (self.sprite_data.eval_phase) {
            // Phase 0: Copy sprites
            0 => {
                self.sprite_data.write_to_oam(
                    (self.sprite_data.copied * 4) + self.sprite_data.m,
                    self.sprite_data.oam_byte,
                );
                if (self.sprite_data.queued_copies > 0) {
                    self.sprite_data.m += 1;
                    self.sprite_data.queued_copies -= 1;
                } else if (self.sprite_data.oam_byte < min_y or self.sprite_data.oam_byte > max_y) {
                    self.sprite_data.n += 1;
                } else {
                    if (self.sprite_data.n == 0) {
                        self.sprite_data.zero_hit_possible = true;
                    }
                    self.sprite_data.m += 1;
                    self.sprite_data.queued_copies = 3;
                }

                // Handle overflows
                if (self.sprite_data.m >= 4) {
                    self.sprite_data.n += 1;
                    self.sprite_data.m = 0;
                    self.sprite_data.copied += 1;
                }
                if (self.sprite_data.n >= 64) {
                    self.sprite_data.n = 0;
                    self.sprite_data.eval_phase = 2;
                }
                if (self.sprite_data.copied == 8) {
                    self.sprite_data.eval_phase = 1;
                }
            },
            // Phase 1: Sprite overflow
            1 => {
                if (self.sprite_data.queued_copies > 0) {
                    self.sprite_data.m += 1;
                    self.sprite_data.queued_copies -= 1;
                } else if (self.sprite_data.oam_byte < min_y or self.sprite_data.oam_byte > max_y) {
                    self.sprite_data.n += 1;
                    self.sprite_data.m += 1;
                    self.sprite_data.m %= 4;
                } else {
                    self.status_register.sprite_overflow = true;
                    self.sprite_data.m += 1;
                    self.sprite_data.queued_copies = 3;
                }
                if (self.sprite_data.m >= 4) {
                    self.sprite_data.n += 1;
                    self.sprite_data.m = 0;
                }
                if (self.sprite_data.n >= 64) {
                    self.sprite_data.n = 0;
                    self.sprite_data.eval_phase = 2;
                }
            },
            // Phase 2: finish
            2 => {},
            else => std.debug.panic("Reached unexpected sprite evaluation phase: {}\n", .{self.sprite_data.eval_phase}),
        }
    }

    fn handle_scrolling(self: *Self) void {
        // No scrolling if rendering is disabled.
        if (!self.is_rendering_enabled()) {
            return;
        }

        // Increment Y position on cycle 256 of each scanline.
        if (self.cycle == 256) {
            self.increment_scroll_y();
        }

        // Copy the X position from `t` to `v` on cycle 256 of each scanline.
        if (self.cycle == 257) {
            self.addr_register.coarse_x = self.tmp_addr.coarse_x;
            self.addr_register.nametable_x = self.tmp_addr.nametable_x;
        }

        // Copy the Y position from `t` to `v` between the cycles 280 and 304 of the pre-render scanline.
        if (self.scanline == PRE_RENDER_SCANLINE and self.cycle >= 280 and self.cycle <= 304) {
            self.addr_register.fine_y = self.tmp_addr.fine_y;
            self.addr_register.coarse_y = self.tmp_addr.coarse_y;
            self.addr_register.nametable_y = self.tmp_addr.nametable_y;
        }

        // Increment X position on every cycle multiple of 8 except 0, between the cycle 328 of current scanline, and
        // the cycle 256 of the next scanline.
        if (((self.cycle > 0 and self.cycle <= 256) or self.cycle >= 328) and (self.cycle % 8 == 0)) {
            self.increment_scroll_x();
        }
    }

    // Load sprite data for the next scanline into registers.
    fn sprite_fetch_cycle(self: *Self) void {
        if (self.cycle % 8 != 5) {
            return;
        }

        const sprite_idx = (self.cycle - 256) / 8;
        const oam_entry = self.sprite_data.secondary_oam[sprite_idx];

        var bank = self.ctrl_register.sprt_pattern_addr();
        var tile_idx: u16 = oam_entry.id;

        const sprite_height: u8 = if (self.ctrl_register.sprite_size) 15 else 7;
        var offset = self.scanline -| oam_entry.y;
        if (oam_entry.attr.vertically_flipped) { // vertically flipped?
            offset = sprite_height -| offset;
        }

        if (self.ctrl_register.sprite_size) { // 8x16 sprites
            bank = @as(u16, oam_entry.id & 1) << 12;
            tile_idx &= 0xFE;

            if (offset >= 8) {
                tile_idx += 1;
                offset -= 8;
            }
        }
        const tile_addr_lo = bank | tile_idx << 4 | offset;
        const tile_addr_hi = tile_addr_lo + 8;

        const tile_byte_lo = self.rom.chr_read(tile_addr_lo);
        const tile_byte_hi = self.rom.chr_read(tile_addr_hi);
        self.schedule_mapper_ppu_update(tile_addr_lo);

        self.sprite_data.x_pos[sprite_idx] = oam_entry.x;
        self.sprite_data.attributes[sprite_idx] = oam_entry.attr;
        self.sprite_data.shifter_pattern_lo[sprite_idx] = tile_byte_lo;
        self.sprite_data.shifter_pattern_hi[sprite_idx] = tile_byte_hi;

        const sprite_x = oam_entry.x;
        for (0..8) |i| {
            if (sprite_x + i < 256) {
                self.sprite_data.has_sprite_at_x[sprite_x + i] = true;
            }
        }
    }

    fn render_pixel(self: *Self) struct { u8, u8 } {
        var bg_pixel: u8 = 0; // 2-bit pixel to be rendered
        var bg_palette: u8 = 0; // 3-bit index of the palette the pixel indexes
        if (self.mask_register.render_background and (self.mask_register.render_background_left or self.cycle > 8)) {
            const pixel_lo = (self.bg_data.shifter_pattern_lo >> @as(u4, @intCast(15 - self.fine_x))) & 1;
            const pixel_hi = (self.bg_data.shifter_pattern_hi >> @as(u4, @intCast(15 - self.fine_x))) & 1;
            bg_pixel = @truncate((pixel_hi << 1) | pixel_lo);

            const palette_lo = (self.bg_data.shifter_attr_lo >> @as(u3, @intCast(7 - self.fine_x))) & 1;
            const palette_hi = (self.bg_data.shifter_attr_hi >> @as(u3, @intCast(7 - self.fine_x))) & 1;
            bg_palette = (palette_hi << 1) | palette_lo;
        }

        var sprite_pixel: u8 = 0;
        var sprite_palette: u8 = 0;
        var sprite_priority: bool = false;
        if (self.sprite_data.has_sprite_at_x[self.cycle - 1] and
            self.mask_register.render_sprite and
            (self.mask_register.render_sprite_left or self.cycle > 8))
        {
            for (0..self.sprite_data.count) |i| {
                const sprite_x = self.sprite_data.x_pos[i];

                const diff = @as(i16, @bitCast(self.cycle)) - @as(i16, @intCast(sprite_x)) - 1;

                if (diff >= 0 and diff < 8) {
                    const attr = self.sprite_data.attributes[i];
                    const shift: u3 = if (attr.horizontally_flipped) @intCast(diff) else @intCast(7 - diff);

                    const sprite_pixel_lo = (self.sprite_data.shifter_pattern_lo[i] >> shift) & 1;
                    const sprite_pixel_hi = (self.sprite_data.shifter_pattern_hi[i] >> shift) & 1;
                    sprite_pixel = (sprite_pixel_hi << 1) | sprite_pixel_lo;

                    if (sprite_pixel != 0) {
                        // Extract the palette from the bottom two bits. The foreground palettes are the latter 4 in the
                        // palette memory.
                        sprite_palette = @as(u8, self.sprite_data.attributes[i].palette) + 4;
                        sprite_priority = !self.sprite_data.attributes[i].priority;

                        if (bg_pixel != 0 and i == 0 and self.cycle != 256 and self.sprite_data.zero_being_rendered) {
                            self.status_register.sprite_zero_hit = true;
                        }
                        break;
                    }
                }
            }
        }

        var pixel: u8 = 0;
        var palette: u8 = 0;
        if (sprite_pixel != 0 and (sprite_priority or bg_pixel == 0)) {
            palette = sprite_palette;
            pixel = sprite_pixel;
        } else if (bg_pixel != 0) {
            pixel = bg_pixel;
            palette = bg_palette;
        }
        return .{ palette, pixel };
    }

    inline fn fetch_tile_data(self: *Self) void {
        const slot: u3 = @truncate(self.cycle);
        if (slot & 1 == 0) return;

        const phase: u2 = @truncate(slot >> 1);
        switch (phase) {
            // Fetch the next background tile ID
            0 => { // cycle 1
                const tile_addr = 0x2000 | (self.addr_register.addr() & 0x0FFF);
                self.bg_data.next_tile_id = self.vram_read(tile_addr);
                self.bg_data.tile_addr = self.ctrl_register.bg_pattern_addr() +
                    (@as(u16, self.bg_data.next_tile_id) << 4) +
                    self.addr_register.fine_y;
            },
            // Fetch the next background tile attribute.
            1 => { // cycle 3
                const attr_addr = 0x23C0 |
                    (@as(u16, self.addr_register.nametable_y) << 11) |
                    (@as(u16, self.addr_register.nametable_x) << 10) |
                    ((@as(u16, self.addr_register.coarse_y) << 1) & 0b111000) |
                    ((@as(u16, self.addr_register.coarse_x) >> 2) & 0b111);
                const shift = ((self.addr_register.coarse_y << 1) & 0b100) | (self.addr_register.coarse_x & 0b10);
                self.bg_data.next_tile_attr = std.math.shr(u8, self.vram_read(attr_addr), shift);
            },
            2 => { // cycle 5
                // Fetch the next background tile LSB bit plane from the pattern memory.
                self.bg_data.next_tile_lo = self.chr_read(self.bg_data.tile_addr);
            },
            3 => { // cycle 7
                // Fetch the next background tile MSB bit plane from the pattern memory.
                self.bg_data.next_tile_hi = self.chr_read(self.bg_data.tile_addr + 8);
            },
        }
    }

    // Increment the background tile "pointer" one tile/column horizontally
    fn increment_scroll_x(self: *Self) void {
        if (self.addr_register.coarse_x == 31) {
            self.addr_register.coarse_x = 0;
            self.addr_register.nametable_x = ~self.addr_register.nametable_x;
        } else {
            self.addr_register.coarse_x += 1;
        }
    }

    // Increment the background tile "pointer" one scanline vertically
    fn increment_scroll_y(self: *Self) void {
        if (self.addr_register.fine_y < 7) {
            self.addr_register.fine_y += 1;
        } else {
            self.addr_register.fine_y = 0;
            if (self.addr_register.coarse_y == 29) {
                self.addr_register.coarse_y = 0;
                self.addr_register.nametable_y = ~self.addr_register.nametable_y;
            } else if (self.addr_register.coarse_y == 31) {
                self.addr_register.coarse_y = 0;
            } else {
                self.addr_register.coarse_y += 1;
            }
        }
    }

    fn reload_shift_registers(self: *Self) void {
        self.bg_data.shifter_pattern_lo = (self.bg_data.shifter_pattern_lo & 0xFF00) | self.bg_data.next_tile_lo;
        self.bg_data.shifter_pattern_hi = (self.bg_data.shifter_pattern_hi & 0xFF00) | self.bg_data.next_tile_hi;

        // Mux the correct bits and load into the bit-latches.
        self.bg_data.next_attr_lo = self.bg_data.next_tile_attr & 1;
        self.bg_data.next_attr_hi = (self.bg_data.next_tile_attr >> 1) & 1;
    }

    fn shift_registers(self: *Self) void {
        self.bg_data.shifter_pattern_lo <<= 1;
        self.bg_data.shifter_pattern_hi <<= 1;

        // Attribute registers pull in bits from the latch.
        self.bg_data.shifter_attr_lo <<= 1;
        self.bg_data.shifter_attr_hi <<= 1;
        self.bg_data.shifter_attr_lo |= self.bg_data.next_attr_lo;
        self.bg_data.shifter_attr_hi |= self.bg_data.next_attr_hi;
    }

    fn is_rendering_enabled(self: *const Self) bool {
        return self.mask_register.render_background or self.mask_register.render_sprite;
    }

    fn is_in_vblank(self: *const Self) bool {
        return self.scanline >= 241 and self.scanline <= 260;
    }

    fn is_rendering(self: *const Self) bool {
        return !self.is_in_vblank() and self.is_rendering_enabled();
    }

    fn get_color(self: *Self, palette_index: u8, pixel_index: u8) render.Color {
        const addr: u5 = @truncate((@as(u16, palette_index) << 2) + pixel_index);
        return self.resolved_palette[addr];
    }

    fn refresh_resolved_palette(self: *Self) void {
        const color_mask: u8 = if (self.mask_register.greyscale) 0x30 else 0x3F;
        for (0..self.resolved_palette.len) |addr| {
            const palette_addr = switch (addr) {
                0x10, 0x14, 0x18, 0x1C => addr - 0x10,
                else => addr,
            };
            const color_idx = self.palette_table[palette_addr] & color_mask;
            self.resolved_palette[addr] = render.SYSTEM_PALETTE[color_idx];
        }
    }

    fn refresh_resolved_palette_entry(self: *Self, palette_addr: usize) void {
        const color_mask: u8 = if (self.mask_register.greyscale) 0x30 else 0x3F;
        const color_idx = self.palette_table[palette_addr] & color_mask;
        const color = render.SYSTEM_PALETTE[color_idx];
        self.resolved_palette[palette_addr] = color;

        // Universal background entries are mirrored into the sprite palettes.
        if (palette_addr < 0x10 and palette_addr & 0x03 == 0) {
            self.resolved_palette[palette_addr + 0x10] = color;
        }
    }

    fn ppu_read(self: *Self, addr: u16) u8 {
        const new_addr = addr & 0x3FFF;
        return switch (new_addr) {
            0...0x1FFF => {
                self.rom.mapper_ppu_address_updated(new_addr, self.global_cycle);
                return self.rom.chr_read(new_addr);
            },
            0x2000...0x3EFF => self.vram_read(new_addr),
            0x3F00...0x3FFF => {
                var palette_addr = new_addr & 0x1F;
                palette_addr = switch (palette_addr) {
                    0x10, 0x14, 0x18, 0x1C => palette_addr - 0x10,
                    else => palette_addr,
                };
                return self.palette_table[palette_addr] & if (self.mask_register.greyscale) @as(u8, 0x30) else 0x3F;
            },
            else => {
                std.log.warn("PPU: unexpected access to mirrored space 0x{X:04} while reading\n", .{new_addr});
                return 0;
            },
        };
    }

    fn chr_read(self: *Self, addr: u16) u8 {
        if (self.is_rendering()) {
            self.schedule_mapper_ppu_update(addr);
        } else {
            self.rom.mapper_ppu_address_updated(addr, self.global_cycle);
        }
        return self.rom.chr_read(addr);
    }

    fn schedule_mapper_ppu_update(self: *Self, addr: u16) void {
        if (self.pending_mapper_addr == null) {
            self.pending_mapper_addr = addr;
            // CHR data is fetched here for rendering, but MMC3 observes the
            // PPU A12 transition later in the fetch slot.
            self.pending_mapper_cycle = self.global_cycle + 14;
        }
    }

    fn vram_read(self: *const Self, addr: u16) u8 {
        return self.vram[self.mirror_vram_addr(addr)];
    }

    pub fn cpu_read(self: *Self, addr: u16) u8 {
        const decayed_value = if (self.global_cycle > PPU_ONE_SEC_CYLES + self.last_cycle_written)
            0
        else
            self.dynamic_latch;
        const mirrored_addr = 0x2000 | (addr & 0x0007);
        const value = switch (mirrored_addr) {
            // If it has passed 1 second return 0 to emulate the decay of the open bus.
            0x2000, 0x2001, 0x2003, 0x2005, 0x2006 => decayed_value,
            0x2002 => @as(u8, @bitCast(self.status_read())) | (decayed_value & 0b0001_1111),
            0x2004 => self.oam_data_read(),
            0x2007 => self.data_read(),
            else => unreachable,
        };
        self.dynamic_latch = value;
        return value;
    }

    pub fn cpu_peek(self: *Self, addr: u16) u8 {
        const mirrored_addr = 0x2000 | (addr & 0x0007);
        return switch (mirrored_addr) {
            0x2000, 0x2001, 0x2003, 0x2005, 0x2006 => self.dynamic_latch,
            0x2002 => @as(u8, @bitCast(self.status_register)) | (self.dynamic_latch & 0b0001_1111),
            0x2004 => self.oam_data_read(),
            0x2007 => self.ppu_read(self.addr_register.addr()),
            else => unreachable,
        };
    }

    pub fn cpu_write(self: *Self, addr: u16, data: u8) void {
        const mirrored_addr = 0x2000 | (addr & 0x0007);
        self.dynamic_latch = data;
        self.last_cycle_written = self.global_cycle;
        switch (mirrored_addr) {
            0x2000 => self.ctrl_write(data),
            0x2001 => self.mask_write(data),
            0x2002 => {},
            0x2003 => self.oam_addr_write(data),
            0x2004 => self.oam_data_write(data),
            0x2005 => self.scroll_write(data),
            0x2006 => self.addr_write(data),
            0x2007 => self.data_write(data),
            else => unreachable,
        }
    }

    /// Writes a byte to the `PPUADDR` register (`0x2006`) to set the PPU memory address.
    ///
    /// The PPU uses a 16-bit address pointer to access its memory space, but the CPU
    /// can only write 8 bits at a time through the `PPUADDR` register. This requires two
    /// sequential writes to set a complete address:
    /// 1. First write: Sets the high byte (bits 8-15) of the address
    /// 2. Second write: Sets the low byte (bits 0-7) of the address
    ///
    /// An internal write toggle tracks whether the next write should update the high
    /// or low byte. This toggle is shared with `PPUSCROLL` and is reset by reading
    /// `PPUSTATUS` (`0x2002`).
    ///
    /// ## Write Sequence
    ///
    /// ```
    /// First write:  `0xAB` -> address becomes `0xAB00`
    /// Second write: `0xCD` -> address becomes `0xABCD`
    /// Third write:  `0x12` -> address becomes `0x12CD` (high byte again)
    /// Fourth write: `0x34` -> address becomes `0x1234` (low byte again)
    /// ```
    ///
    /// ## Address Mirroring
    ///
    /// PPU address space is only 14 bits (`0x0000-0x3FFF`). Addresses above `0x3FFF`
    /// are automatically mirrored down by masking to 14 bits. For example:
    /// - `0x4000` mirrors to `0x0000`
    /// - `0x7FFF` mirrors to `0x3FFF`
    ///
    /// ## Side Effects
    /// - Toggles the write latch for the next write
    ///
    /// ## Important Notes
    /// - The write toggle is shared with `PPUSCROLL` (`0x2005`)
    /// - Reading `PPUSTATUS` (`0x2002`) resets the write toggle to "high byte" state
    /// - After setting an address, subsequent `PPUDATA` reads/writes will use this address
    fn addr_write(self: *Self, value: u8) void {
        if (!self.write_toggle) {
            // The high byte write only sets bits 8-13. Bit 14 is cleared.
            const tmp_addr = (self.tmp_addr.addr() & 0x00FF) | (@as(u16, value & 0x3F) << 8);
            self.tmp_addr = @bitCast(tmp_addr);
        } else {
            const tmp_addr = (self.tmp_addr.addr() & 0xFF00) | value;
            self.tmp_addr = @bitCast(tmp_addr);
            self.addr_register = self.tmp_addr;

            self.rom.mapper_ppu_address_updated(tmp_addr & 0x3FFF, self.global_cycle);
        }

        self.write_toggle = !self.write_toggle;
    }

    fn increment_vram_addr(self: *Self) void {
        if (self.scanline >= 240 or !self.is_rendering_enabled()) {
            const new_addr = self.addr_register.addr() +% self.ctrl_register.vram_addr_increment();
            self.addr_register = @bitCast(new_addr);
            self.rom.mapper_ppu_address_updated(new_addr & 0x3FFF, self.global_cycle);
        } else {
            // "During rendering (on the pre-render line and the visible lines 0-239, provided either background or
            // sprite rendering is enabled), " it will update v in an odd way, triggering a coarse X increment and a
            // Y increment simultaneously"
            self.increment_scroll_x();
            self.increment_scroll_y();
        }
    }

    /// Mirrors PPU VRAM addresses according to the configured mirroring mode.
    ///
    /// The NES PPU has 4 nametable address ranges (`0x2000-0x23FF`, `0x2400-0x27FF`,
    /// `0x2800-0x2BFF`, `0x2C00-0x2FFF`), but only 2KB of actual VRAM. The mirroring
    /// mode determines how these 4 logical nametables map to the 2KB of physical memory.
    ///
    /// Address range `0x3000-0x3EFF` is mirrored down to `0x2000-0x2EFF` before processing.
    ///
    /// ## Mirroring Modes
    ///
    /// **Horizontal Mirroring** (vertical arrangement):
    /// ```
    ///   [ A ] [ B ]
    ///   [ a ] [ b ]
    /// ```
    /// - Nametables 0 and 1 map to the first 1KB (top row)
    /// - Nametables 2 and 3 map to the second 1KB (bottom row)
    ///
    /// **Vertical Mirroring** (horizontal arrangement):
    /// ```
    ///   [ A ] [ a ]
    ///   [ B ] [ b ]
    /// ```
    /// - Nametables 0 and 2 map to the first 1KB (left column)
    /// - Nametables 1 and 3 map to the second 1KB (right column)
    ///
    /// **Four-Screen**: All 4 nametables are unique (requires extra RAM on cartridge)
    ///
    /// **Single-Screen Lower**: All nametables map to first 1KB
    /// **Single-Screen Upper**: All nametables map to second 1KB
    fn mirror_vram_addr(self: *const Self, addr: u16) u16 {
        // mirror down 0x2000 - 0x2eff
        const mirrored_addr = addr & 0x2FFF;
        // to index (0x0000 - 0x0FFF)
        const vram_index = mirrored_addr - 0x2000;
        // to the name table index
        const name_table_idx = vram_index / 0x400;

        return switch (self.rom.get_mirroring()) {
            Mirroring.VERTICAL => switch (name_table_idx) {
                0, 2 => vram_index & 0x03FF,
                1, 3 => (vram_index & 0x03FF) + 0x0400,
                else => unreachable,
            },
            Mirroring.HORIZONTAL => switch (name_table_idx) {
                0, 1 => vram_index & 0x03FF,
                2, 3 => (vram_index & 0x03FF) + 0x0400,
                else => unreachable,
            },
            Mirroring.FOUR_SCREEN => vram_index & 0x07FF,
            // All nametables map to first 1KB
            Mirroring.SINGLE_SCREEN_LOWER => vram_index & 0x03FF,
            // All nametables map to second 1KB
            Mirroring.SINGLE_SCREEN_UPPER => (vram_index & 0x03FF) + 0x0400,
        };
    }

    /// Reads a byte from PPU memory through the `PPUDATA` register (`0x2007`).
    ///
    /// This function handles reads from different regions of PPU address space based on
    /// the current value in the PPU address register (set via PPUADDR writes). After
    /// each read, the VRAM address is automatically incremented based on bit 2 of
    /// `PPUCTRL` (+1 for horizontal, +32 for vertical).
    ///
    /// ## Internal Read Buffer Behavior
    ///
    /// **IMPORTANT**: Reads from PPUDATA are buffered with a one-read delay for most
    /// address ranges. When reading from `0x0000-0x3EFF`:
    /// 1. The function returns the value from the *previous* read (internal buffer)
    /// 2. The internal buffer is updated with the value at the *current* address
    /// 3. The first read after setting an address returns stale buffer data
    ///
    /// **Exception**: Palette RAM (`0x3F00-0x3FFF`) returns data immediately without
    /// buffering, allowing direct palette reads.
    ///
    /// ## Address Space Mapping
    ///
    /// - **0x0000-0x1FFF**: Pattern Tables (CHR ROM/RAM)
    ///   - Returns buffered data from CHR ROM
    ///   - Subject to one-read delay
    ///
    /// - **0x2000-0x2FFF**: Nametables (VRAM)
    ///   - Returns buffered data from nametable memory
    ///   - Mirroring is applied based on cartridge configuration
    ///   - Subject to one-read delay
    ///
    /// - **0x3000-0x3EFF**: Nametable Mirror
    ///   - Mirrors 0x2000-0x2EFF (rarely used in practice)
    ///   - Generates a warning but processes the read
    ///   - Subject to one-read delay
    ///
    /// - **0x3F00-0x3FFF**: Palette RAM
    ///   - Returns data immediately (no buffering!)
    ///   - 32 bytes with internal mirroring
    ///   - Used for background and sprite color palettes
    fn data_read(self: *Self) u8 {
        const addr = self.addr_register.addr();
        const byte = self.ppu_read(addr);

        self.increment_vram_addr();

        if ((addr & 0x3FFF) >= 0x3F00) { // palette address don't have buffering
            // Palette read should also read VRAM into internal data buffer
            self.internal_data_buf = self.ppu_read(addr & 0x2FFF);
            // The high 2 bits of palette are from whatever it was on open bus.
            return (byte & 0b0011_1111) | (self.dynamic_latch & 0b1100_0000);
        } else {
            const result = self.internal_data_buf;
            self.internal_data_buf = byte;
            return result;
        }
    }

    /// Writes a byte to PPU memory through the `PPUDATA` register (`0x2007`).
    ///
    /// This function handles writes to different regions of PPU address space based on
    /// the current value in the PPU address register (set via PPUADDR writes). After
    /// each write, the VRAM address is automatically incremented based on the control
    /// register settings (`+1` or `+32`).
    ///
    /// ## Address Space Mapping
    ///
    /// - **0x0000-0x1FFF**: Pattern Tables (CHR ROM/RAM)
    ///   - Writes to this region are logged as errors if CHR ROM is present
    ///   - CHR RAM writes would be handled by the cartridge mapper
    ///
    /// - **0x2000-0x2FFF**: Nametables (VRAM)
    ///   - Writes to nametable memory with mirroring applied
    ///   - Actual storage uses 2KB VRAM with hardware mirroring
    ///
    /// - **0x3000-0x3EFF**: Nametable Mirror
    ///   - Mirrors 0x2000-0x2EFF (not commonly used)
    ///   - Generates a warning but processes the write
    ///
    /// - **0x3F00-0x3FFF**: Palette RAM
    ///   - 32 bytes of palette data with mirroring
    ///   - Stores background and sprite color palettes
    fn data_write(self: *Self, value: u8) void {
        const addr = self.addr_register.addr() & 0x3FFF;

        if (addr >= 0 and addr <= 0x1FFF) {
            self.rom.chr_write(addr, value);
        } else if (addr >= 0x2000 and addr <= 0x3EFF) {
            self.vram[self.mirror_vram_addr(addr)] = value;
        } else if (addr >= 0x3F00 and addr <= 0x3FFF) {
            var palette_addr = addr & 0x001F;
            if (palette_addr == 0x10 or palette_addr == 0x14 or palette_addr == 0x18 or palette_addr == 0x1C) {
                palette_addr -= 0x10;
            }
            self.palette_table[palette_addr] = value;
            self.refresh_resolved_palette_entry(palette_addr);
        } else {
            std.debug.panic("PPU: unexpected access to mirrored space 0x{X:04} while writing\n", .{addr});
        }

        self.increment_vram_addr();
    }

    /// Write a value to the `PPUCTRL` register.
    fn ctrl_write(self: *Self, value: u8) void {
        const old_ctrl = self.ctrl_register;
        self.ctrl_register = @bitCast(value);

        self.tmp_addr.nametable_x = @as(u1, @truncate(self.ctrl_register.name_table_addr)) & 1;
        self.tmp_addr.nametable_y = @as(u1, @truncate(self.ctrl_register.name_table_addr >> 1)) & 1;

        if (!self.ctrl_register.generate_nmi) {
            if (!self.nmi_interrupt or (self.scanline == 241 and self.cycle <= VBL_NMI_CANCEL_LAST_CYCLE)) {
                self.nmi_interrupt = false;
                self.nmi_interrupt_delay = false;
                self.nmi_interrupt_cycle = 0;
            } else if (self.nmi_interrupt_cycle > self.global_cycle) {
                self.nmi_interrupt_cycle = self.global_cycle;
            }
        } else if (!old_ctrl.generate_nmi and self.status_register.vblank and
            !(self.scanline == PRE_RENDER_SCANLINE and self.cycle >= 1))
        {
            self.nmi_interrupt = true;
            self.nmi_interrupt_delay = true;
            self.nmi_interrupt_cycle = self.global_cycle;
        }
    }

    /// Writes a byte to the `PPUMASK` register (`0x2001`) to configure rendering options.
    ///
    /// The `PPUMASK` register controls various rendering features including:
    /// - Grayscale mode
    /// - Background and sprite visibility in leftmost 8 pixels
    /// - Background and sprite rendering enable/disable
    /// - Color emphasis (tint) for red, green, and blue channels
    ///
    /// ## Bit Layout
    /// ```
    /// Bit 0: Grayscale (0 = normal color, 1 = grayscale)
    /// Bit 1: Show background in leftmost 8 pixels (1 = show, 0 = hide)
    /// Bit 2: Show sprites in leftmost 8 pixels (1 = show, 0 = hide)
    /// Bit 3: Show background (1 = enabled, 0 = disabled)
    /// Bit 4: Show sprites (1 = enabled, 0 = disabled)
    /// Bit 5: Emphasize red
    /// Bit 6: Emphasize green
    /// Bit 7: Emphasize blue
    /// ```
    fn mask_write(self: *Self, value: u8) void {
        const old_greyscale = self.mask_register.greyscale;
        self.mask_register = @bitCast(value);
        if (old_greyscale != self.mask_register.greyscale) {
            self.refresh_resolved_palette();
        }
    }

    /// Writes a byte to the `OAMADDR` register (`0x2003`) to set the OAM write address.
    ///
    /// Object Attribute Memory (`OAM`) stores sprite data for up to 64 sprites (256 bytes
    /// total, 4 bytes per sprite). `OAMADDR` sets the starting address within OAM for
    /// subsequent `OAMDATA` writes.
    ///
    /// ## Important Notes
    /// - Most games do not use `OAMADDR`/`OAMDATA` for sprite updates
    /// - Instead, they use `OAMDMA` (register `0x4014`) for faster bulk transfers
    /// - `OAMADDR` is often set to `0x00` before VBLANK to ensure clean sprite rendering
    /// - Writing to `OAMADDR` during rendering can cause sprite corruption
    fn oam_addr_write(self: *Self, value: u8) void {
        self.oam_addr_register = value;
    }

    /// Writes a byte to the `OAMDATA` register (`0x2004`) to update a single `OAM` byte.
    ///
    /// Writes one byte to `OAM` at the current `OAMADDR` position and automatically
    /// increments `OAMADDR` afterward. This is rarely used in practice as `OAM DMA`
    /// is much more efficient for updating sprite data.
    ///
    /// ## Side Effects
    /// - Increments OAMADDR by 1 (wraps from 0xFF to 0x00)
    ///
    /// ## Important Notes
    /// - Writing during rendering can cause sprite corruption
    /// - Most games use `OAMDMA` instead of individual `OAMDATA` writes
    /// - Each sprite occupies 4 consecutive bytes in `OAM`
    pub fn oam_data_write(self: *Self, value: u8) void {
        var data = value;
        // Check if the current address points to the attribute byte and mask out bits 2, 3 and 4.
        if (self.oam_addr_register % 4 == 2) {
            data &= 0xE3;
        }
        self.oam_data_register[self.oam_addr_register] = data;
        self.oam_addr_register +%= 1;
    }

    fn oam_data_read(self: Self) u8 {
        return self.oam_data_register[self.oam_addr_register];
    }

    /// Writes a byte to the `PPUSCROLL` register (`0x2005`) to set screen scroll position.
    ///
    /// The `PPUSCROLL` register controls the starting position for background rendering,
    /// enabling smooth scrolling effects. Like `PPUADDR`, it requires two sequential writes:
    /// 1. First write: Sets the X scroll offset (0-255)
    /// 2. Second write: Sets the Y scroll offset (0-239)
    ///
    /// The write toggle is shared with `PPUADDR` and is reset by reading `PPUSTATUS`.
    ///
    /// ## Side Effects
    /// - Toggles the write latch for the next write
    ///
    /// ## Important Notes
    /// - Reading `PPUSTATUS` (`0x2002`) resets the toggle to "X scroll" state
    /// - Scroll changes typically occur during `VBLANK` to avoid visual glitches
    /// - Fine scroll updates affect background rendering immediately
    fn scroll_write(self: *Self, data: u8) void {
        if (!self.write_toggle) {
            // self.scroll_register.fine_x = data;
            self.fine_x = data & 0x07;
            self.tmp_addr.coarse_x = @truncate(data >> 3);
        } else {
            // self.scroll_register.fine_y = data;
            self.tmp_addr.fine_y = @truncate(data & 0x07);
            self.tmp_addr.coarse_y = @truncate(data >> 3);
        }
        self.write_toggle = !self.write_toggle;
    }

    /// Reads the `PPUSTATUS` register (`0x2002`) and returns its current state.
    ///
    /// The `PPUSTATUS` register provides important timing and status information:
    /// - Bit 7: VBLANK flag (1 = in VBLANK period)
    /// - Bit 6: Sprite 0 hit flag (1 = sprite 0 overlapped background)
    /// - Bit 5: Sprite overflow flag (1 = more than 8 sprites on a scanline)
    /// - Bits 0-4: Open bus (return last value on PPU data bus)
    ///
    /// ## Side Effects
    /// - **Clears the VBLANK flag** (bit 7) after reading
    /// - **Resets the write toggle** for `PPUADDR` and `PPUSCROLL` to "first write" state
    ///
    /// ## Important Notes
    /// - Reading PPUSTATUS clears VBLANK
    /// - The VBLANK flag is automatically set at the start of VBLANK (scanline 241)
    fn status_read(self: *Self) StatusRegister {
        const status = self.status_register;

        // A read immediately before the VBL flag is set suppresses the set
        // event for this frame.
        if (self.scanline == 241 and self.cycle == 1) {
            self.suppress_vblank_set = true;
        }

        self.write_toggle = false;
        self.status_register.vblank = false;
        if (self.scanline == 241 and self.cycle <= VBL_NMI_CANCEL_LAST_CYCLE) {
            self.nmi_interrupt = false;
            self.nmi_interrupt_delay = false;
            self.nmi_interrupt_cycle = 0;
        }

        return status;
    }

    pub fn get_pattern_table(self: *Self, i: u8, palette: u8) PatternTableFrame {
        var frame = PatternTableFrame{};
        for (0..16) |tile_y| {
            for (0..16) |tile_x| {
                const offset: u16 = @intCast(tile_y * 256 + tile_x * 16);
                for (0..8) |row| {
                    var tile_lsb = self.ppu_read(@intCast(@as(u16, i) * 0x1000 + offset + row));
                    var tile_msb = self.ppu_read(@intCast(@as(u16, i) * 0x1000 + offset + row + 0x0008));

                    for (0..8) |col| {
                        const pixel = (tile_lsb & 1) | ((tile_msb & 1) << 1);
                        tile_lsb >>= 1;
                        tile_msb >>= 1;

                        frame.set_pixel(tile_x * 8 + (7 - col), tile_y * 8 + row, self.get_color(palette, pixel));
                    }
                }
            }
        }
        return frame;
    }
};

// "flips" a byte so 0b11100000 becomes 0b00000111.
// source: https://stackoverflow.com/a/2602885
fn flip_byte(byte: u8) u8 {
    var result = (byte & 0xF0) >> 4 | (byte & 0x0F) << 4;
    result = (result & 0xCC) >> 2 | (result & 0x33) << 2;
    result = (result & 0xAA) >> 1 | (result & 0x55) << 1;
    return result;
}

test "resolved palette follows writes, greyscale, and state loads" {
    const alloc = std.testing.allocator;
    const TestRom = @import("rom.zig").TestRom;
    var test_rom = TestRom.init(alloc, &[_]u8{});
    defer test_rom.deinit();

    var ppu = PPU.init(&test_rom.rom);

    ppu.addr_write(0x3F);
    ppu.addr_write(0x00);
    ppu.data_write(0x21);
    try std.testing.expectEqual(render.SYSTEM_PALETTE[0x21], ppu.get_color(0, 0));
    try std.testing.expectEqual(render.SYSTEM_PALETTE[0x21], ppu.get_color(4, 0));

    // Writes through a mirrored address update both logical palette entries.
    ppu.addr_write(0x3F);
    ppu.addr_write(0x10);
    ppu.data_write(0x12);
    try std.testing.expectEqual(render.SYSTEM_PALETTE[0x12], ppu.get_color(0, 0));
    try std.testing.expectEqual(render.SYSTEM_PALETTE[0x12], ppu.get_color(4, 0));

    ppu.addr_write(0x3F);
    ppu.addr_write(0x11);
    ppu.data_write(0x2F);
    ppu.mask_write(0x01);
    try std.testing.expectEqual(render.SYSTEM_PALETTE[0x20], ppu.get_color(4, 1));

    var snapshot = try ppu.saveState(alloc);
    defer snapshot.deinit(alloc);

    ppu.mask_write(0x00);
    ppu.addr_write(0x3F);
    ppu.addr_write(0x11);
    ppu.data_write(0x01);
    try std.testing.expectEqual(render.SYSTEM_PALETTE[0x01], ppu.get_color(4, 1));

    ppu.loadState(&snapshot);
    try std.testing.expectEqual(render.SYSTEM_PALETTE[0x20], ppu.get_color(4, 1));
}

test "PPU VRAM write" {
    const alloc = std.testing.allocator;
    const TestRom = @import("rom.zig").TestRom;
    var test_rom = TestRom.init(alloc, &[_]u8{});
    defer test_rom.deinit();

    var ppu = PPU.init(&test_rom.rom);
    ppu.addr_write(0x23);
    ppu.addr_write(0x05);
    ppu.data_write(0x42);

    try std.testing.expectEqual(0x42, ppu.vram[0x0305]);
}

test "PPU VRAM read" {
    const alloc = std.testing.allocator;
    const TestRom = @import("rom.zig").TestRom;
    var test_rom = TestRom.init(alloc, &[_]u8{});
    defer test_rom.deinit();

    var ppu = PPU.init(&test_rom.rom);
    ppu.ctrl_write(0);
    ppu.vram[0x0305] = 0x42;

    ppu.addr_write(0x23);
    ppu.addr_write(0x05);
    _ = ppu.data_read(); // load data to internal buffer

    try std.testing.expectEqual(0x2306, @as(u16, @bitCast(ppu.addr_register)));
    try std.testing.expectEqual(0x42, ppu.data_read());
}

test "PPU VRAM read cross page" {
    const alloc = std.testing.allocator;
    const TestRom = @import("rom.zig").TestRom;
    var test_rom = TestRom.init(alloc, &[_]u8{});
    defer test_rom.deinit();

    var ppu = PPU.init(&test_rom.rom);
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
    const alloc = std.testing.allocator;
    const TestRom = @import("rom.zig").TestRom;
    var test_rom = TestRom.init(alloc, &[_]u8{});
    defer test_rom.deinit();

    var ppu = PPU.init(&test_rom.rom);
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
    const alloc = std.testing.allocator;
    const TestRom = @import("rom.zig").TestRom;
    var test_rom = TestRom.init(alloc, &[_]u8{});
    defer test_rom.deinit();

    var ppu = PPU.init(&test_rom.rom);
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
    const alloc = std.testing.allocator;
    const TestRom = @import("rom.zig").TestRom;
    var test_rom = TestRom.init_with_mirroring(alloc, &[_]u8{}, .VERTICAL);
    defer test_rom.deinit();

    var ppu = PPU.init(&test_rom.rom);
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
    const alloc = std.testing.allocator;
    const TestRom = @import("rom.zig").TestRom;
    var test_rom = TestRom.init(alloc, &[_]u8{});
    defer test_rom.deinit();

    var ppu = PPU.init(&test_rom.rom);
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

test "OAM read write" {
    const alloc = std.testing.allocator;
    const TestRom = @import("rom.zig").TestRom;
    var test_rom = TestRom.init(alloc, &[_]u8{});
    defer test_rom.deinit();

    var ppu = PPU.init(&test_rom.rom);
    ppu.oam_addr_write(0x10);
    ppu.oam_data_write(0x42);
    ppu.oam_data_write(0x69);

    ppu.oam_addr_write(0x10);
    try std.testing.expectEqual(0x42, ppu.oam_data_read());

    ppu.oam_addr_write(0x11);
    try std.testing.expectEqual(0x69, ppu.oam_data_read());
}

test "read status resets vblank" {
    const alloc = std.testing.allocator;
    const TestRom = @import("rom.zig").TestRom;
    var test_rom = TestRom.init(alloc, &[_]u8{});
    defer test_rom.deinit();

    var ppu = PPU.init(&test_rom.rom);
    ppu.status_register.vblank = true;

    const status = ppu.status_read();

    try std.testing.expect(status.vblank);
    try std.testing.expect(!ppu.status_register.vblank);
}
