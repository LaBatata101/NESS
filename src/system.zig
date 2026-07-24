const std = @import("std");

const Rom = @import("rom.zig").Rom;
const Bus = @import("bus.zig").Bus;
const CPU = @import("cpu.zig").CPU;
const PPU = @import("ppu.zig").PPU;
const APU = @import("apu/apu.zig").APU;
const SDLAudioOut = @import("sdl_audio.zig").SDLAudioOut;
const ControllerButton = @import("controller.zig").ControllerButton;
const trace = @import("trace.zig").trace;

pub const Settings = struct {
    trace_cpu: bool = false,
    disable_audio: bool = false,
};

pub const System = struct {
    allocator: std.mem.Allocator,

    bus: *Bus,
    cpu: *CPU,
    apu: *APU,
    ppu: *PPU,

    settings: Settings,

    trace_file: ?std.fs.File,
    trace_file_writer: ?std.fs.File.Writer,
    trace_file_buffer: [4096]u8,

    const Self = @This();
    pub const Snapshot = struct {
        saved_at: i64 = 0,
        cpu: CPU.Snapshot,
        bus: Bus.Snapshot,
        ppu: PPU.Snapshot,
        apu: APU.Snapshot,

        pub fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            self.bus.deinit(alloc);
            self.ppu.deinit(alloc);
        }
    };

    pub const ControllerSnapshot = struct {
        player1: ControllerButton = .{},
        player2: ControllerButton = .{},
    };

    pub fn init(allocator: std.mem.Allocator, rom: *Rom, settings: Settings) !Self {
        const cpu = try allocator.create(CPU);
        const apu = try allocator.create(APU);
        const ppu = try allocator.create(PPU);
        const bus = try allocator.create(Bus);

        ppu.* = PPU.init(rom);
        apu.* = try APU.init(allocator, try SDLAudioOut.init(allocator), rom);
        bus.* = Bus.init(rom, ppu, apu);
        cpu.* = CPU.init(bus);

        apu.device.disable = settings.disable_audio;

        var trace_file: ?std.fs.File = null;
        var trace_file_writer: ?std.fs.File.Writer = null;
        // const trace_file_buffer = try allocator.alloc(u8, 4096);
        if (settings.trace_cpu) {
            trace_file = try std.fs.cwd().createFile("cpu.trace", .{});
            trace_file_writer = .initStreaming(trace_file.?, &.{});
        }

        return .{
            .allocator = allocator,
            .bus = bus,
            .cpu = cpu,
            .ppu = ppu,
            .apu = apu,
            .settings = settings,
            .trace_file = trace_file,
            .trace_file_writer = trace_file_writer,
            .trace_file_buffer = undefined,
        };
    }

    pub fn deinit(self: *Self) void {
        self.apu.deinit();

        self.allocator.destroy(self.cpu);
        self.allocator.destroy(self.ppu);
        self.allocator.destroy(self.apu);
        self.allocator.destroy(self.bus);

        if (self.settings.trace_cpu) {
            self.trace_file.?.close();
        }
    }

    pub fn tick(self: *Self) void {
        self.ppu.tick();
        self.ppu.tick();
        self.ppu.tick();

        if (self.bus.dma_start_delay > 0) {
            self.bus.dma_start_delay -= 1;
        }
        const dma_stalling = self.bus.dma_start_delay == 0 and self.bus.dma_cycles > 0;

        if (!dma_stalling and self.cpu.cycles_wait == 0 and (self.bus.rom.mapper_irq_active() or self.apu.irq_triggered())) {
            self.cpu.interrupt(CPU.IRQ);
        }

        if (self.ppu.nmi_interrupt and self.ppu.global_cycle >= self.ppu.nmi_interrupt_cycle) {
            if (self.ppu.nmi_interrupt_delay) {
                if (!dma_stalling and self.cpu.cycles_wait == 0) {
                    self.ppu.nmi_interrupt_delay = false;
                }
            } else if (!dma_stalling and self.cpu.cycles_wait == 0 and !self.cpu.shouldDeferNmi()) {
                self.ppu.nmi_interrupt = false;
                self.cpu.interrupt(CPU.NMI);
            } else if (!dma_stalling and self.cpu.hijackNmiVector()) {
                self.ppu.nmi_interrupt = false;
            }
        }

        if (dma_stalling) {
            self.bus.dma_cycles -= 1;
        } else {
            self.cpu.step();
        }
        self.apu.step();
        self.bus.cycles += 1;
    }

    pub fn run_frame(self: *Self) void {
        while (!self.is_frame_complete()) {
            if (self.settings.trace_cpu) {
                var writer = self.trace_file_writer.?;
                trace(&writer.interface, self.cpu) catch @panic("Failed to trace CPU");
            }
            self.tick();
        }
    }

    fn run_ppu(self: *Self) void {
        self.ppu.run_to(self.bus.cycles);
        if (self.ppu.nmi_interrupt and self.ppu.global_cycle >= self.ppu.nmi_interrupt_cycle) {
            if (self.ppu.nmi_interrupt_delay) {
                self.ppu.nmi_interrupt_delay = false;
            } else {
                self.ppu.nmi_interrupt = false;
                self.cpu.interrupt(CPU.NMI);
            }
        }
    }

    fn run_apu(self: *Self) void {
        self.apu.step(self.bus.cycles);
        if (self.apu.irq_interrupt) {
            self.cpu.interrupt(CPU.IRQ);
        }
    }

    pub fn reset(self: *Self) void {
        self.cpu.reset();
        self.bus.reset();
    }

    pub fn setAudioPaused(self: *Self, paused: bool) void {
        self.apu.device.setPaused(paused);
    }

    pub fn is_frame_complete(self: *Self) bool {
        if (self.ppu.frame_complete) {
            self.ppu.frame_complete = false;
            if (self.settings.trace_cpu) {
                self.dump_trace();
            }
            return true;
        }
        return false;
    }

    fn dump_trace(self: *const Self) void {
        var writer = self.trace_file_writer.?;
        writer.interface.flush() catch |err| std.debug.panic("Flush failed: {any}\n", .{err});
    }

    pub fn frame_buffer(self: *const Self) []const u8 {
        return &self.ppu.frame_buffer.data;
    }

    pub fn applyControllerSnapshot(self: *Self, snapshot: ControllerSnapshot) void {
        self.bus.controllers.cntrl1_status = snapshot.player1;
        self.bus.controllers.cntrl2_status = snapshot.player2;
    }

    pub fn saveState(self: *const Self, alloc: std.mem.Allocator) !Snapshot {
        return .{
            .cpu = self.cpu.saveState(),
            .bus = try self.bus.saveState(alloc),
            .ppu = try self.ppu.saveState(alloc),
            .apu = self.apu.saveState(),
        };
    }

    pub fn loadState(self: *Self, snapshot: *const Snapshot) !void {
        try self.bus.loadState(&snapshot.bus);
        self.ppu.loadState(&snapshot.ppu);
        self.apu.loadState(&snapshot.apu);
        self.cpu.loadState(snapshot.cpu);
    }
};
