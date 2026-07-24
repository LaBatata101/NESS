const std = @import("std");
const cpu = @import("cpu.zig");
const rom = @import("rom.zig");
pub const render = @import("render.zig");
pub const opcodes = @import("opcodes.zig");
pub const controller = @import("controller.zig");
pub const ui = @import("ui/core/ui.zig");
pub const gui = @import("ui/gui.zig");
pub const logging = @import("logging.zig");
pub const netplay_protocol = @import("netplay/protocol.zig");
pub const netplay_snapshot = @import("netplay/snapshot.zig");
pub const save_state = @import("save_state.zig");
pub const netplay_session = @import("netplay/session.zig");

pub const customPanic = @import("utils/panic.zig").customPanic;

pub const PPU = @import("ppu.zig").PPU;
pub const APU = @import("apu/apu.zig").APU;
pub const trace = @import("trace.zig");
pub const System = @import("system.zig").System;
pub const SDLAudioOut = @import("sdl_audio.zig").SDLAudioOut;

pub const CPU = cpu.CPU;
pub const Rom = rom.Rom;
pub const SYSTEM_PALLETE = render.SYSTEM_PALETTE;

pub const sdlError = @import("utils/sdl.zig").sdlError;
pub const mmap = @import("utils/mmap.zig");
pub const ThreadPool = @import("utils/pool.zig");
pub const vulkan = @import("utils/vulkan.zig");

pub const c = @cImport({
    @cInclude("SDL3/SDL.h");
    @cInclude("SDL3/SDL_system.h");
    @cInclude("blip_buf.h");
    @cInclude("glslang/Include/glslang_c_interface.h");
    @cInclude("glslang/Public/resource_limits_c.h");
    @cInclude("vulkan/vulkan.h");
    @cInclude("SDL3/SDL_vulkan.h");
    @cInclude("spirv_cross_c.h");
});

pub const NES_WIDTH = 256;
pub const NES_HEIGHT = 240;
pub const OVERSCAN_TOP = 8;
pub const OVERSCAN_BOTTOM = 8;
pub const NES_VISIBLE_HEIGHT = NES_HEIGHT - OVERSCAN_TOP - OVERSCAN_BOTTOM;
pub const DEBUG_WIDTH = 250;

test {
    std.testing.refAllDecls(@This());
}
