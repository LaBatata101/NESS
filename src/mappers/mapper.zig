const std = @import("std");
const Mirroring = @import("../rom.zig").Mirroring;
const Mapper0 = @import("mapper0.zig").Mapper0;
const Mapper1 = @import("mapper1.zig").Mapper1;
const Mapper2 = @import("mapper2.zig").Mapper2;
const Mapper3 = @import("mapper3.zig").Mapper3;
const Mapper4 = @import("mapper4.zig").Mapper4;

pub const MapperParams = struct {
    rom_path: []const u8,
    prg_rom: []const u8,
    chr_rom: []const u8,
    prg_rom_banks: u8,
    prg_ram_size: usize,
    has_battery_backed_ram: bool,
    mirroring_mode: Mirroring,
};

pub const Mapper = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    const DefaultImpl = struct {
        fn ppu_address_updated(ptr: *anyopaque, addr: u16, ppu_cycle: u64) void {
            _ = ptr;
            _ = addr;
            _ = ppu_cycle;
        }
    };
    pub const VTable = struct {
        deinit: *const fn (ptr: *anyopaque) void,
        prg_rom_read: *const fn (ptr: *anyopaque, addr: u16) u8,
        prg_rom_write: *const fn (ptr: *anyopaque, addr: u16, value: u8) void,
        prg_ram_read: *const fn (ptr: *anyopaque, addr: u16) u8,
        prg_ram_write: *const fn (ptr: *anyopaque, addr: u16, value: u8) void,
        chr_read: *const fn (ptr: *anyopaque, addr: u16) u8,
        chr_write: *const fn (ptr: *anyopaque, addr: u16, value: u8) void,
        mirroring: *const fn (ptr: *const anyopaque) Mirroring,
        irq_active: *const fn (ptr: *anyopaque) bool,
        ppu_address_updated: *const fn (ptr: *anyopaque, addr: u16, ppu_cycle: u64) void = @ptrCast(&DefaultImpl.ppu_address_updated),
        save_state: *const fn (ptr: *const anyopaque, alloc: std.mem.Allocator) anyerror!Snapshot,
        load_state: *const fn (ptr: *anyopaque, snapshot: Snapshot) anyerror!void,
    };

    const Self = @This();

    pub const Snapshot = union(enum(u8)) {
        mapper0: Mapper0.Snapshot,
        mapper1: Mapper1.Snapshot,
        mapper2: Mapper2.Snapshot,
        mapper3: Mapper3.Snapshot,
        mapper4: Mapper4.Snapshot,

        pub fn deinit(self: *Snapshot, alloc: std.mem.Allocator) void {
            switch (self.*) {
                .mapper0 => |*snapshot| snapshot.deinit(alloc),
                .mapper1 => |*snapshot| snapshot.deinit(alloc),
                .mapper2 => |*snapshot| snapshot.deinit(alloc),
                .mapper3 => |*snapshot| snapshot.deinit(alloc),
                .mapper4 => |*snapshot| snapshot.deinit(alloc),
            }
        }
    };

    pub fn init(allocator: std.mem.Allocator, io: std.Io, mapper_id: u8, params: MapperParams) !Self {
        switch (mapper_id) {
            0 => {
                const mapper = try Mapper0.init(allocator, params);
                return mapper.as_mapper();
            },
            1 => {
                const mapper = try Mapper1.init(allocator, io, params);
                return mapper.as_mapper();
            },
            2 => {
                const mapper = try Mapper2.init(allocator, params);
                return mapper.as_mapper();
            },
            3 => {
                const mapper = try Mapper3.init(allocator, params);
                return mapper.as_mapper();
            },
            4 => {
                const mapper = try Mapper4.init(allocator, io, params);
                return mapper.as_mapper();
            },
            else => return error.UnsupportedMapper,
        }
    }

    pub fn deinit(self: *Self) void {
        self.vtable.deinit(self.ptr);
    }

    /// Read from PRG ROM space ($8000-$FFFF)
    pub fn prg_rom_read(self: *Self, addr: u16) u8 {
        return self.vtable.prg_rom_read(self.ptr, addr);
    }

    /// Write to PRG ROM space ($8000-$FFFF)
    pub fn prg_rom_write(self: *Self, addr: u16, value: u8) void {
        self.vtable.prg_rom_write(self.ptr, addr, value);
    }

    /// Read from PRG RAM space ($6000–$7FFF)
    pub fn prg_ram_read(self: *Self, addr: u16) u8 {
        return self.vtable.prg_ram_read(self.ptr, addr);
    }

    /// Write to PRG RAM space ($6000–$7FFF)
    pub fn prg_ram_write(self: *Self, addr: u16, value: u8) void {
        self.vtable.prg_ram_write(self.ptr, addr, value);
    }

    /// Read from CHR ROM/RAM space ($0000-$1FFF)
    pub fn chr_read(self: *Self, addr: u16) u8 {
        return self.vtable.chr_read(self.ptr, addr);
    }

    /// Write to CHR ROM/RAM space
    pub fn chr_write(self: *Self, addr: u16, value: u8) void {
        self.vtable.chr_write(self.ptr, addr, value);
    }

    /// Get current mirroring mode
    pub fn mirroring(self: *const Self) Mirroring {
        return self.vtable.mirroring(self.ptr);
    }

    /// Check if mapper requests IRQ
    pub fn irq_active(self: *Self) bool {
        return self.vtable.irq_active(self.ptr);
    }

    pub fn ppu_address_updated(self: *Self, addr: u16, ppu_cycle: u64) void {
        self.vtable.ppu_address_updated(self.ptr, addr, ppu_cycle);
    }

    pub fn saveState(self: *const Self, alloc: std.mem.Allocator) !Snapshot {
        return self.vtable.save_state(self.ptr, alloc);
    }

    pub fn loadState(self: *Self, snapshot: Snapshot) !void {
        return self.vtable.load_state(self.ptr, snapshot);
    }
};
