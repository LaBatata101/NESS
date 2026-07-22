const std = @import("std");
const builtin = @import("builtin");

const game_history = @import("../game_history.zig");
const c = @import("../root.zig").c;
const Key = @import("core/ui.zig").Key;
const UI = @import("core/ui.zig").UI;
const Rom = @import("../rom.zig").Rom;
const viewport = @import("core/viewport.zig");
const System = @import("../system.zig").System;
const Frame = @import("../render.zig").Frame;
const PatternTableFrame = @import("../render.zig").PatternTableFrame;
const ProcessorStatus = @import("../cpu.zig").ProcessorStatus;
const ControllerButton = @import("../controller.zig").ControllerButton;
const bindings = @import("bindings.zig");
const settings = @import("settings.zig");
const paths = @import("../utils/paths.zig");
const shader_download = @import("../shader_download.zig");
const save_state = @import("../save_state.zig");
const file = @import("../utils/file.zig");
const android = @import("../utils/android.zig");
const ness = @import("../root.zig");
const clay = @import("core/clay.zig");
const sdlError = ness.sdlError;

const NES_WIDTH = ness.NES_WIDTH;
const NES_HEIGHT = ness.NES_HEIGHT;
const OVERSCAN_TOP = ness.OVERSCAN_TOP;
const NES_VISIBLE_HEIGHT = ness.NES_VISIBLE_HEIGHT;
const NES_PIXEL_BYTES = NES_WIDTH * NES_HEIGHT * 4;
const OVERSCAN_PIXEL_OFFSET = OVERSCAN_TOP * NES_WIDTH * 4;
const NES_VISIBLE_PIXEL_BYTES = NES_WIDTH * NES_VISIBLE_HEIGHT * 4;
const NES_CONTROLLER_IMG = @embedFile("nes_controller_img");
const NO_FRAME: u8 = std.math.maxInt(u8);
const CURSOR_HIDE_DELAY_MS = 3000;
const NES_TARGET_FPS: u64 = 60;
const SPEED_SAMPLE_MS: u64 = 1000;

const Player = bindings.Player;
const ControllerAction = bindings.ControllerAction;
const ControllerKeyBindings = bindings.ControllerKeyBindings;
const ControllerBindingTarget = bindings.ControllerBindingTarget;
const InputDevice = bindings.InputDevice;
const GeneralAction = bindings.GeneralAction;
const GeneralKeyBindings = bindings.GeneralKeyBindings;
const GamepadKeyBindings = bindings.GamepadKeyBindings;
const ParamTarget = settings.ParamTarget;
const ShaderParamSetting = settings.ShaderParamSetting;
const EmulationSpeed = settings.EmulationSpeed;
const BorderShaderOpts = settings.BorderShaderOpts;

pub const ShaderFilePickerEntry = struct {
    kind: Kind,
    label: []u8,

    pub const Kind = enum {
        directory,
        file,
    };
};

pub const SettingsCategory = enum {
    general,
    video,
    shader,
    controls,

    pub fn displayName(self: @This()) []const u8 {
        return switch (self) {
            .general => "General",
            .video => "Video",
            .shader => "Shader",
            .controls => "Controls",
        };
    }
};

pub const AppState = struct {
    alloc: std.mem.Allocator,
    ui: *UI,
    /// Wheter to skip drawing the home screen.
    render_home_ui: bool = true,
    render_debug_ui: bool = false,
    show_android_settings_ui: bool = false,
    show_android_sidepanel: bool = false,
    show_android_save_state_dialog: bool = false,
    show_android_load_state_dialog: bool = false,
    emulation_running: bool = false,

    step_mode: bool = false,

    rom_bytes: ?[]u8 = null,
    rom: ?Rom = null,
    system: ?System = null,

    history: game_history.GameHistory = undefined,
    current_rom_path: ?[]u8 = null,
    save_state_info: [save_state.SLOT_COUNT]?save_state.SlotInfo = .{null} ** save_state.SLOT_COUNT,
    game_start_time_ms: i64 = 0,

    paused: bool = false,
    lifecycle_suspended: std.atomic.Value(bool) = .init(false),

    is_cursor_hidden: bool = false,

    /// Set to true to trigger loading the shader preset on the next frame.
    should_load_shader: bool = false,
    /// Set to true to trigger clearing the shader preset on the next frame.
    should_clear_shader: bool = false,
    /// True while an async shader compile is in progress.
    shader_loading: bool = false,
    /// Last shader load error message to display in the settings window (owned).
    shader_error: ?[]u8 = null,
    /// Set to true to trigger loading the border shader preset on the next frame.
    should_load_border_shader: bool = false,
    /// Set to true to trigger clearing the border shader preset on the next frame.
    should_clear_border_shader: bool = false,
    /// True while an async border shader compile is in progress.
    border_shader_loading: bool = false,
    /// Last border shader load error message (owned).
    border_shader_error: ?[]u8 = null,
    /// Background Android shader library download, if active.
    shader_download_thread: ?std.Thread = null,
    shader_download_root_path: ?[]u8 = null,
    shader_download_result: ?anyerror = null,
    shader_download_error: ?[]u8 = null,
    shader_download_state: std.atomic.Value(u8) = .init(@intFromEnum(shader_download.State.idle)),
    shader_download_bytes: std.atomic.Value(u64) = .init(0),
    shader_download_total_bytes: std.atomic.Value(u64) = .init(shader_download.unknown_total),

    // for the custom Android file picker
    show_custom_file_picker: bool = false,
    shader_target: settings.ParamTarget = .main,
    shader_file_picker_current_dir: []u8 = &.{},
    shader_file_picker_entries: std.ArrayList(ShaderFilePickerEntry) = .{},
    shader_file_picker_error: ?[]u8 = null,

    // Edit the on-screen controls on Android
    android_edit_mode: bool = false,
    android_onscreen_controller: OnScreenController = .{},

    show_fps: bool = false,

    settings: EmulatorSettings = .{},
    saved_settings: EmulatorSettings = .{},
    config_dir: ?[]u8 = null,
    controller_img: LoadedImage,

    selected_input_device: [2]InputDevice = .{ .keyboard, .keyboard },
    tmp_selected_input_device: [2]InputDevice = .{ .keyboard, .keyboard },
    input_devices: std.ArrayList(InputDevice) = .empty,

    emulation_thread: ?std.Thread = null,
    emulation_stop: std.atomic.Value(bool) = .init(false),
    emulation_lock: std.Thread.RwLock = .{},
    controller1_bits: std.atomic.Value(u8) = .init(0),
    controller2_bits: std.atomic.Value(u8) = .init(0),
    frame_buffers: [2]Frame = .{ Frame.init(), Frame.init() },
    published_frame_idx: std.atomic.Value(u8) = .init(0),
    ui_frame_idx: std.atomic.Value(u8) = .init(0),
    writing_frame_idx: std.atomic.Value(u8) = .init(NO_FRAME),
    render_frame_idx: u8 = 0,
    emulation_speed_percent: std.atomic.Value(u64) = .init(0),
    /// Currently selected category in the settings sidebar.
    selected_category: SettingsCategory = .general,

    const Self = @This();
    pub const DebugSnapshot = struct {
        const CPU_MEMORY_SIZE = 0x10000;

        register_a: u8 = 0,
        register_x: u8 = 0,
        register_y: u8 = 0,
        pc: u16 = 0,
        sp: u8 = 0,
        status: ProcessorStatus = .{},
        scanline: u16 = 0,
        cycle: u16 = 0,
        global_cycle: u64 = 0,
        palette_ram: [32]u8 = [_]u8{0} ** 32,
        pattern_table_0: PatternTableFrame = .{},
        pattern_table_1: PatternTableFrame = .{},
        memory: [CPU_MEMORY_SIZE]u8 = [_]u8{0} ** CPU_MEMORY_SIZE,

        pub fn memPeek(self: *const @This(), addr: u16) u8 {
            return self.memory[addr];
        }

        pub fn memPeekU16(self: *const @This(), addr: u16) u16 {
            const lo = self.memPeek(addr);
            const hi = @as(u16, self.memPeek(addr +% 1));
            return (hi << 8) | lo;
        }

        fn capture(system: *const System) @This() {
            var snapshot: @This() = .{
                .register_a = system.cpu.register_a,
                .register_x = system.cpu.register_x,
                .register_y = system.cpu.register_y,
                .pc = system.cpu.pc,
                .sp = system.cpu.sp,
                .status = system.cpu.status,
                .scanline = system.ppu.scanline,
                .cycle = system.ppu.cycle,
                .global_cycle = system.ppu.global_cycle,
                .palette_ram = system.ppu.palette_table,
                .pattern_table_0 = system.ppu.get_pattern_table(0, 0),
                .pattern_table_1 = system.ppu.get_pattern_table(1, 0),
            };

            for (&snapshot.memory, 0..) |*value, addr| {
                value.* = system.cpu.bus.mem_peek(@intCast(addr));
            }

            return snapshot;
        }
    };

    pub const LoadedImage = struct {
        raw: [*c]c.SDL_Surface,

        pub fn w(self: *const @This()) u32 {
            return @intCast(self.raw.*.w);
        }
        pub fn h(self: *const @This()) u32 {
            return @intCast(self.raw.*.h);
        }
        pub fn format(self: *const @This()) c.SDL_PixelFormat {
            return self.raw.*.format;
        }
        pub fn pixels(self: *const @This()) []const u8 {
            const len: usize = @as(usize, @intCast(self.raw.*.pitch)) * @as(usize, @intCast(self.raw.*.h));
            const ptr: [*]const u8 = @ptrCast(self.raw.*.pixels);
            return ptr[0..len];
        }
    };

    pub const OnScreenController = struct {
        portrait: Layout = .{},
        landscape: Layout = .{},

        pub const Layout = struct {
            dpad: Pos = .{},
            start_btn: Pos = .{},
            select_btn: Pos = .{},
            action_btn_A: Pos = .{},
            action_btn_B: Pos = .{},
        };

        pub const Pos = struct {
            scale: f32 = 1,
            offset: clay.Vector2 = .{ .x = 0, .y = 0 },
        };

        pub fn forOrientation(self: *@This(), orientation: android.ScreenOrientation) *Layout {
            return switch (orientation) {
                .portrait, .portrait_flipped => &self.portrait,
                else => &self.landscape,
            };
        }
    };

    pub const EmulatorSettings = struct {
        aspect_ratio: viewport.AspectRatio = .@"4_3",
        /// Path to the active shader preset (owned by this struct).
        shader_preset_path: ?[]u8 = null,
        /// Active shader parameter overrides (names are owned by this struct).
        shader_params: std.ArrayList(ShaderParamSetting) = .{},
        /// Active bundled border shader preset.
        border_shader: BorderShaderOpts = .none,
        /// Active border shader parameter overrides (names are owned by this struct).
        border_shader_params: std.ArrayList(ShaderParamSetting) = .{},
        vsync: bool = true,
        hide_mouse_on_inactivity: bool = false,
        emulation_speed: EmulationSpeed = .normal,
        selected_player: Player = .one,
        controller_bindings: ControllerKeyBindings = .{},
        capture_binding: ?ControllerBindingTarget = null,
        general_bindings: GeneralKeyBindings = .{},
        capture_general_binding: ?GeneralAction = null,
        gamepad_bindings: GamepadKeyBindings = .{},
        capture_gamepad_binding: ?ControllerBindingTarget = null,
        gamepad_deadzone: u8 = 25,
        show_home_screen_snow_effect: bool = true,
        hide_android_onscreen_controller: bool = false,
        android_onscreen_controller: OnScreenController = .{},
    };

    pub fn init(alloc: std.mem.Allocator, ui: *UI) Self {
        var hist = game_history.GameHistory.init(alloc);
        hist.load();

        const img_bytes = c.SDL_IOFromConstMem(NES_CONTROLLER_IMG, NES_CONTROLLER_IMG.len);
        const surface = c.SDL_LoadPNG_IO(img_bytes, true);

        const config_dir = paths.getConfigDir(alloc) catch |err| blk: {
            std.log.warn("settings directory unavailable: {s}", .{@errorName(err)});
            break :blk null;
        };

        var state = Self{
            .alloc = alloc,
            .ui = ui,
            .history = hist,
            .config_dir = config_dir,
            .controller_img = .{ .raw = surface },
        };

        if (!builtin.abi.isAndroid()) {
            state.input_devices.append(alloc, .keyboard) catch @panic("OOM");
        }

        state.loadSettings();
        state.snapshotSettings() catch @panic("Failed to snapshot loaded settings");
        return state;
    }

    pub fn deinit(self: *Self) void {
        self.stopEmulationThread();

        // Save before freeing the system.
        if (self.emulation_running) self.saveCurrentGame();
        self.history.deinit();

        if (self.config_dir) |path| self.alloc.free(path);
        if (self.current_rom_path) |p| self.alloc.free(p);
        if (self.rom_bytes) |rom_bytes| self.alloc.free(rom_bytes);
        deinitShaderRuntimeState(self.alloc, self);
        deinitEmulatorSettings(self.alloc, &self.settings);
        deinitEmulatorSettings(self.alloc, &self.saved_settings);
        self.input_devices.deinit(self.alloc);
        c.SDL_DestroySurface(self.controller_img.raw);

        if (self.emulation_running) {
            self.rom.?.deinit();
            self.system.?.deinit();
        }
    }

    fn startEmulationThread(self: *Self) !void {
        self.stopEmulationThread();
        self.emulation_stop.store(false, .release);
        self.emulation_thread = try std.Thread.spawn(.{}, emulationThreadMain, .{self});
    }

    pub fn stopEmulationThread(self: *Self) void {
        if (self.emulation_thread) |thread| {
            self.emulation_stop.store(true, .release);
            thread.join();
            self.emulation_thread = null;
            self.emulation_stop.store(false, .release);
        }
    }

    fn emulationThreadMain(self: *Self) void {
        var frame_acc: f32 = 0.0;
        var speed_frame_count: u64 = 0;
        var speed_window_start: u64 = c.SDL_GetTicks();

        while (!self.emulation_stop.load(.acquire)) {
            self.emulation_lock.lock();
            const can_run = self.emulation_running and
                self.system != null and
                !self.lifecycle_suspended.load(.acquire) and
                !self.paused and
                !self.step_mode;
            if (can_run) {
                const speed = self.settings.emulation_speed;
                const multiplier = speed.multiplier();
                frame_acc += multiplier;
                const frames_to_run: u32 = @intFromFloat(frame_acc);
                frame_acc -= @floatFromInt(frames_to_run);

                self.system.?.apu.device.setSpeed(multiplier);
                for (0..frames_to_run) |_| {
                    self.system.?.applyControllerSnapshot(self.controllerSnapshot());
                    self.system.?.run_frame();
                    self.publishFrame(self.system.?.frame_buffer());
                    speed_frame_count += 1;
                }

                const now = c.SDL_GetTicks();
                const diff = now -% speed_window_start;
                if (diff >= SPEED_SAMPLE_MS) {
                    const percent = @divFloor(speed_frame_count * c.SDL_MS_PER_SECOND * 100, diff * NES_TARGET_FPS);
                    self.emulation_speed_percent.store(percent, .release);
                    speed_frame_count = 0;
                    speed_window_start = now;
                }
            }
            self.emulation_lock.unlock();

            c.SDL_Delay(1);
        }
    }

    pub fn currentEmulationSpeedPercent(self: *const Self) u64 {
        return self.emulation_speed_percent.load(.acquire);
    }

    fn publishFrame(self: *Self, pixels: []const u8) void {
        while (true) {
            const read_idx = self.ui_frame_idx.load(.acquire);
            const write_idx: u8 = if (read_idx == 0) 1 else 0;

            self.writing_frame_idx.store(write_idx, .release);
            if (self.ui_frame_idx.load(.acquire) == write_idx) {
                self.writing_frame_idx.store(NO_FRAME, .release);
                continue;
            }

            @memcpy(self.frame_buffers[write_idx].data[0..], pixels);
            self.published_frame_idx.store(write_idx, .release);
            self.writing_frame_idx.store(NO_FRAME, .release);
            return;
        }
    }

    fn handleInput(self: *Self, ui: *UI) void {
        if (ui.androidBackRequested()) {
            self.handleAndroidBack(ui);
            return;
        }

        if (self.emulation_running) {
            const main_window_active = ui.current_window == ui.main_window;
            if (main_window_active) {
                self.syncControllers(ui);

                if (ui.isKeyPressed(self.generalBinding(.quick_save))) self.saveStateSlot(0);
                if (ui.isKeyPressed(self.generalBinding(.quick_load))) self.loadStateSlot(0);
                if (ui.isKeyPressed(self.generalBinding(.quit))) ui.quit = true;
                if (ui.isKeyPressed(self.generalBinding(.toggle_step_mode))) self.toggleDebug();
                if (ui.isKeyPressed(self.generalBinding(.restart))) self.resetSystem();
                if (ui.isKeyPressed(self.generalBinding(.toggle_pause))) self.togglePause();
                if (ui.isKeyPressed(self.generalBinding(.stop))) {
                    self.unloadCurrentRom();
                    ui.setWindowFullscreen(false);
                }
                if (self.step_mode and ui.isKeyPressed(self.generalBinding(.run_tick))) self.runSystemTick();
                if (self.step_mode and ui.isKeyPressed(self.generalBinding(.run_frame))) {
                    self.runSystemFrame();
                }
                if (ui.isKeyPressed(self.generalBinding(.toggle_fullscreen))) {
                    if (ui.isWindowFullscreen()) {
                        ui.setWindowFullscreen(false);
                    } else {
                        ui.setWindowFullscreen(true);
                    }
                }
            }

            if (ui.mouseMotion()) {
                ui.setTimer("hide_cursor", CURSOR_HIDE_DELAY_MS);
                if (self.is_cursor_hidden) {
                    sdlError(c.SDL_ShowCursor());
                    self.is_cursor_hidden = false;
                }
            }

            if (self.settings.hide_mouse_on_inactivity and !self.render_debug_ui) {
                if (!self.is_cursor_hidden and ui.hasTimerExpired("hide_cursor").unwrap_or(false)) {
                    sdlError(c.SDL_HideCursor());
                    self.is_cursor_hidden = true;
                }
            } else if (self.is_cursor_hidden) { // Always show cursor if not in fullscreen
                sdlError(c.SDL_ShowCursor());
                self.is_cursor_hidden = false;
            }
        }
    }

    fn handleAndroidBack(self: *Self, ui: *UI) void {
        if (self.show_custom_file_picker) {
            self.closeShaderFilePicker();
            return;
        }

        if (self.android_edit_mode) {
            self.android_onscreen_controller = self.settings.android_onscreen_controller;
            self.android_edit_mode = false;
            self.show_android_settings_ui = true;
            ui.setWindowFullscreen(false);
            return;
        }

        if (self.show_android_sidepanel) {
            self.show_android_sidepanel = false;
            return;
        }

        if (self.show_android_settings_ui) {
            self.show_android_settings_ui = false;
            self.render_home_ui = !self.emulation_running;
            return;
        }

        if (self.emulation_running) {
            self.show_android_sidepanel = true;
            // Set a timer of 250ms to avoid closing the sidepanel as soon as it's opened
            ui.setTimer("android_sidepanel", 250);
            return;
        }

        ui.quit = true;
    }

    pub fn update(self: *Self) void {
        self.handleInput(self.ui);
        self.updateShaderState(self.ui);

        // Track connected/disconnected gamepads
        const prev_input_devices: isize = @intCast(self.input_devices.items.len - if (builtin.abi.isAndroid()) 0 else 1);
        if (@as(isize, @intCast(self.ui.gamepads.items.len)) - prev_input_devices != 0) {
            self.input_devices.clearRetainingCapacity();
            if (!builtin.abi.isAndroid()) {
                self.input_devices.append(self.alloc, .keyboard) catch @panic("OOM");
            }

            for (self.ui.gamepads.items, 0..) |gamepad, i| {
                self.input_devices.append(self.alloc, .{ .gamepad = .{ .id = i, .name = gamepad.name } }) catch
                    @panic("OOM");
            }

            if (builtin.abi.isAndroid() and self.ui.gamepads.items.len > 0) {
                // Switch the input to the first connected gamepad
                self.selected_input_device[0] = .{ .gamepad = self.input_devices.items[0].gamepad };
                self.tmp_selected_input_device[0] = .{ .gamepad = self.input_devices.items[0].gamepad };
            }
        }

        if (builtin.abi.isAndroid()) self.pollShaderDownload();

        while (true) {
            const idx = self.published_frame_idx.load(.acquire);
            self.ui_frame_idx.store(idx, .release);
            if (self.writing_frame_idx.load(.acquire) != idx) {
                self.render_frame_idx = idx;
                return;
            }
            std.atomic.spinLoopHint();
        }
    }

    pub fn selectedTmpInputDevice(self: *Self) *InputDevice {
        return &self.tmp_selected_input_device[self.settings.selected_player.value()];
    }

    fn updateShaderState(self: *Self, ui: *UI) void {
        // Handle deferred shader preset load/clear requests.
        if (self.should_load_shader) {
            self.should_load_shader = false;

            if (self.settings.shader_preset_path) |path| {
                if (self.shader_error) |old| {
                    self.alloc.free(old);
                    self.shader_error = null;
                }

                ui.loadShaderPreset("main", path) catch |err| {
                    std.log.err("Failed to start shader load '{s}': {s}", .{ path, @errorName(err) });
                    self.shader_error = std.fmt.allocPrint(
                        self.alloc,
                        "Load failed: {s}",
                        .{@errorName(err)},
                    ) catch null;
                    // Clear the bad path so it doesn't show as "active".
                    self.alloc.free(path);
                    self.settings.shader_preset_path = null;
                    settings.clearShaderParamSettings(self.alloc, &self.settings.shader_params);
                };

                if (self.shader_error == null) {
                    self.shader_loading = true;
                }
            }
        } else if (self.should_clear_shader) {
            self.should_clear_shader = false;
            ui.clearShaderPreset("main");
            self.shader_loading = false;

            if (self.shader_error) |old| {
                self.alloc.free(old);
                self.shader_error = null;
            }
        }

        // Poll an in-progress async shader compile.
        if (self.shader_loading) {
            switch (ui.pollShaderLoad("main")) {
                .idle => self.shader_loading = false,
                .done => {
                    self.shader_loading = false;
                    self.applyShaderParamSettings(ui, .main);
                },
                .compiling => {},
                .failed => |msg| {
                    self.shader_loading = false;
                    if (self.shader_error) |old| self.alloc.free(old);
                    self.shader_error = self.alloc.dupe(u8, msg) catch null;
                    if (self.settings.shader_preset_path) |path| {
                        self.alloc.free(path);
                        self.settings.shader_preset_path = null;
                        settings.clearShaderParamSettings(self.alloc, &self.settings.shader_params);
                    }
                },
            }
        }

        // Handle deferred border shader preset load/clear requests.
        if (self.should_load_border_shader) {
            self.should_load_border_shader = false;
            if (self.settings.border_shader.presetPath()) |path| {
                if (self.border_shader_error) |old| {
                    self.alloc.free(old);
                    self.border_shader_error = null;
                }

                ui.loadShaderPreset("border", path) catch |err| {
                    std.log.err("Failed to start border shader load '{s}': {s}", .{ path, @errorName(err) });
                    self.border_shader_error = std.fmt.allocPrint(
                        self.alloc,
                        "Load failed: {s}",
                        .{@errorName(err)},
                    ) catch null;
                    self.settings.border_shader = .none;
                    settings.clearShaderParamSettings(self.alloc, &self.settings.border_shader_params);
                };

                if (self.border_shader_error == null) {
                    self.border_shader_loading = true;
                }
            }
        } else if (self.should_clear_border_shader) {
            self.should_clear_border_shader = false;
            ui.clearShaderPreset("border");
            self.border_shader_loading = false;

            if (self.border_shader_error) |old| {
                self.alloc.free(old);
                self.border_shader_error = null;
            }
        }

        // Poll an in-progress async border shader compile.
        if (self.border_shader_loading) {
            switch (ui.pollShaderLoad("border")) {
                .idle => self.border_shader_loading = false,
                .done => {
                    self.border_shader_loading = false;
                    self.applyShaderParamSettings(ui, .border);
                },
                .compiling => {},
                .failed => |msg| {
                    self.border_shader_loading = false;
                    if (self.border_shader_error) |old| self.alloc.free(old);
                    self.border_shader_error = self.alloc.dupe(u8, msg) catch null;
                    if (self.settings.border_shader != .none) {
                        self.settings.border_shader = .none;
                        settings.clearShaderParamSettings(self.alloc, &self.settings.border_shader_params);
                    }
                },
            }
        }
    }

    pub fn framePixels(self: *Self, offset: usize, len: usize) []const u8 {
        return self.frame_buffers[self.render_frame_idx].data[offset..][0..len];
    }

    pub fn debugSnapshot(self: *Self) DebugSnapshot {
        self.emulation_lock.lockShared();
        defer self.emulation_lock.unlockShared();

        return DebugSnapshot.capture(&self.system.?);
    }

    pub fn syncControllers(self: *Self, ui: *UI) void {
        const snapshot = self.pollControllers(ui);
        self.controller1_bits.store(@bitCast(snapshot.player1), .release);
        self.controller2_bits.store(@bitCast(snapshot.player2), .release);
    }

    fn clearControllerState(self: *Self) void {
        self.controller1_bits.store(0, .release);
        self.controller2_bits.store(0, .release);
    }

    fn controllerSnapshot(self: *Self) System.ControllerSnapshot {
        return .{
            .player1 = @bitCast(self.controller1_bits.load(.acquire)),
            .player2 = @bitCast(self.controller2_bits.load(.acquire)),
        };
    }

    fn pollControllers(self: *Self, ui: *UI) System.ControllerSnapshot {
        return .{
            .player1 = self.pollController(ui, .one),
            .player2 = self.pollController(ui, .two),
        };
    }

    fn pollController(self: *Self, ui: *UI, player_id: Player) ControllerButton {
        var status: ControllerButton = .{};

        const input_device = self.selected_input_device[player_id.value()];
        if (input_device == .gamepad) {
            self.pollGamepadButtons(ui, player_id, input_device.gamepad.id, &status);
        } else if (builtin.abi.isAndroid()) {
            status.insert(ui.onScreenControllerStatus());
        } else {
            const key_bindings = self.settings.controller_bindings.forPlayer(player_id);
            inline for (@typeInfo(ControllerAction).@"enum".fields) |field| {
                const action = @field(ControllerAction, field.name);
                if (ui.isKeyDown(key_bindings.get(action))) {
                    status.insert(action.button());
                }
            }
        }

        return status;
    }

    fn pollGamepadButtons(self: *Self, ui: *UI, player: Player, gamepad_idx: usize, status: *ControllerButton) void {
        const gamepad_bindings = self.settings.gamepad_bindings.forPlayer(player);

        if (ui.isGamepadButtonDown(gamepad_idx, gamepad_bindings.a)) status.insert(.{ .BUTTON_A = true });
        if (ui.isGamepadButtonDown(gamepad_idx, gamepad_bindings.b)) status.insert(.{ .BUTTON_B = true });
        if (ui.isGamepadButtonDown(gamepad_idx, gamepad_bindings.start)) status.insert(.{ .START = true });
        if (ui.isGamepadButtonDown(gamepad_idx, gamepad_bindings.select)) status.insert(.{ .SELECT = true });
        if (ui.isGamepadButtonDown(gamepad_idx, gamepad_bindings.up)) status.insert(.{ .UP = true });
        if (ui.isGamepadButtonDown(gamepad_idx, gamepad_bindings.down)) status.insert(.{ .DOWN = true });
        if (ui.isGamepadButtonDown(gamepad_idx, gamepad_bindings.left)) status.insert(.{ .LEFT = true });
        if (ui.isGamepadButtonDown(gamepad_idx, gamepad_bindings.right)) status.insert(.{ .RIGHT = true });

        const threshold: i16 = @intCast(@as(u32, self.settings.gamepad_deadzone) * 32767 / 100);
        const lx = ui.getGamepadAxis(gamepad_idx, c.SDL_GAMEPAD_AXIS_LEFTX);
        const ly = ui.getGamepadAxis(gamepad_idx, c.SDL_GAMEPAD_AXIS_LEFTY);
        if (lx < -threshold) status.insert(.{ .LEFT = true });
        if (lx > threshold) status.insert(.{ .RIGHT = true });
        if (ly < -threshold) status.insert(.{ .UP = true });
        if (ly > threshold) status.insert(.{ .DOWN = true });
    }

    pub fn resetSystem(self: *Self) void {
        self.emulation_lock.lock();
        defer self.emulation_lock.unlock();
        self.system.?.reset();
    }

    pub fn runSystemTick(self: *Self) void {
        self.emulation_lock.lock();
        defer self.emulation_lock.unlock();
        self.system.?.tick();
    }

    pub fn runSystemFrame(self: *Self) void {
        self.emulation_lock.lock();
        defer self.emulation_lock.unlock();
        self.system.?.run_frame();
    }

    pub fn setEmulationSpeed(self: *Self, speed: EmulationSpeed) void {
        self.emulation_lock.lock();
        defer self.emulation_lock.unlock();
        self.settings.emulation_speed = speed;

    pub fn romDisplayName(self: *const Self) []const u8 {
        return if (builtin.abi.isAndroid())
            (android.displayName(self.alloc, self.current_rom_path.?) catch @panic("JNI error")).?
        else
            self.alloc.dupe(u8, std.fs.path.stem(self.current_rom_path.?)) catch @panic("OOM");
    }

    pub fn saveStateSlot(self: *Self, slot: usize) void {
        std.debug.assert(self.current_rom_path != null);
        std.debug.assert(slot < save_state.SLOT_COUNT);

        const name = self.romDisplayName();
        defer self.alloc.free(name);

        self.emulation_lock.lock();
        defer self.emulation_lock.unlock();

        std.log.info("Saving state to slot {} for \"{s}\"", .{ slot + 1, name });

        const info = save_state.saveSlot(self.alloc, name, &self.system.?, slot) catch |err| {
            std.log.err("save state slot {} failed: {s}", .{ slot + 1, @errorName(err) });
            return;
        };

        self.save_state_info[slot] = info;
        self.ui.setTimer("save_state_toast", 1000);
    }

    pub fn loadStateSlot(self: *Self, slot: usize) void {
        std.debug.assert(self.current_rom_path != null);

        const name = self.romDisplayName();
        defer self.alloc.free(name);

        self.emulation_lock.lock();
        defer self.emulation_lock.unlock();

        std.log.info("Loading state from slot {} for \"{s}\"", .{ slot + 1, name });

        save_state.loadSlot(self.alloc, name, &self.system.?, slot) catch |err| {
            std.log.err("load state slot {} failed: {s}", .{ slot + 1, @errorName(err) });
        };
        self.ui.setTimer("load_state_toast", 1000);
    }

    pub fn saveStateSlotInfo(self: *const Self, slot: usize) ?save_state.SlotInfo {
        std.debug.assert(slot < save_state.SLOT_COUNT);
        return self.save_state_info[slot];
    }

    pub fn loadRom(self: *Self, path: []const u8) !void {
        // Save previous game's progress before replacing it.
        if (self.emulation_running) {
            self.stopEmulationThread();
            self.saveCurrentGame();
        }

        if (self.rom) |*rom| rom.deinit();
        if (self.system) |*system| system.deinit();

        const rom_fullpath = if (builtin.abi.isAndroid()) path else blk: {
            const cwd = try std.process.getCwdAlloc(self.alloc);
            defer self.alloc.free(cwd);
            const rom_fullpath = try std.fs.path.resolve(self.alloc, &.{ cwd, path });

            break :blk rom_fullpath;
        };
        defer if (!builtin.abi.isAndroid()) self.alloc.free(rom_fullpath);

        std.log.debug("Reading file: {s}", .{rom_fullpath});

        if (self.rom_bytes) |bytes| self.alloc.free(bytes);
        self.rom_bytes = try file.readFile(self.alloc, rom_fullpath);

        self.rom = try Rom.init(self.alloc, rom_fullpath, self.rom_bytes.?);
        self.system = try System.init(self.alloc, &self.rom.?, .{});
        self.system.?.reset();
        self.publishFrame(self.system.?.frame_buffer());
        self.emulation_running = true;
        self.render_home_ui = false;
        self.render_debug_ui = false;
        self.show_android_settings_ui = false;
        self.show_android_sidepanel = false;

        if (self.current_rom_path) |p| self.alloc.free(p);
        self.current_rom_path = self.alloc.dupe(u8, rom_fullpath) catch null;
        self.game_start_time_ms = std.time.milliTimestamp();
        self.refreshSaveStateInfo();
        try self.startEmulationThread();
    }

    pub fn unloadCurrentRom(self: *Self) void {
        self.clearControllerState();
        self.stopEmulationThread();
        self.saveCurrentGame();

        self.rom.?.deinit();
        self.system.?.deinit();
        self.alloc.free(self.current_rom_path.?);
        self.alloc.free(self.rom_bytes.?);

        self.rom = null;
        self.system = null;
        self.rom_bytes = null;
        self.current_rom_path = null;
        self.emulation_running = false;
        self.render_home_ui = true;
        self.render_debug_ui = false;

        self.show_android_settings_ui = false;
        self.show_android_sidepanel = false;
        self.clearSaveStateInfo();
    }

    fn saveCurrentGame(self: *Self) void {
        const path = self.current_rom_path orelse return;

        const name = self.romDisplayName();
        defer self.alloc.free(name);

        const elapsed_ms = std.time.milliTimestamp() - self.game_start_time_ms;
        const elapsed_secs: u64 = if (elapsed_ms > 0) @intCast(@divFloor(elapsed_ms, 1000)) else 0;

        var existing_secs: u64 = 0;
        for (self.history.entries.items) |entry| {
            if (std.mem.eql(u8, entry.rom_path, path)) {
                existing_secs = entry.play_time_secs;
                break;
            }
        }

        const pixels = self.framePixels(OVERSCAN_PIXEL_OFFSET, NES_VISIBLE_PIXEL_BYTES);
        self.history.save(name, path, existing_secs + elapsed_secs, pixels);

        // Reset so back-to-back saves (loadRom then deinit) don't double-count.
        self.game_start_time_ms = std.time.milliTimestamp();
    }

    fn refreshSaveStateInfo(self: *Self) void {
        const rom_path = self.current_rom_path orelse {
            self.clearSaveStateInfo();
            return;
        };

        const name = if (builtin.abi.isAndroid())
            (android.displayName(self.alloc, rom_path) catch @panic("JNI error")).?
        else
            std.fs.path.stem(rom_path);
        defer if (builtin.abi.isAndroid()) self.alloc.free(name);

        for (&self.save_state_info, 0..) |*info, slot| {
            if (save_state.slotInfo(self.alloc, name, slot)) |slot_info| {
                info.* = slot_info;
            } else |_| {}
        }
    }

    fn clearSaveStateInfo(self: *Self) void {
        @memset(self.save_state_info[0..], null);
    }

    pub fn generalBinding(self: *const Self, action: GeneralAction) Key {
        return self.settings.general_bindings.get(action);
    }

    pub fn hasSettingsChanges(self: *const Self) bool {
        return !isSettingsEqual(self.settings, self.saved_settings) or hasInputDeviceChanged(self.selected_input_device, self.tmp_selected_input_device);
    }

    pub fn saveSettings(self: *Self) void {
        self.saveSettingsImpl() catch |err|
            std.log.err("settings save failed: {s}", .{@errorName(err)});
        self.snapshotSettings() catch @panic("Failed to snapshot saved settings");
        self.selected_input_device = self.tmp_selected_input_device;
    }

    pub fn restoreSavedSettings(self: *Self) void {
        const restored = clonePersistedSettings(self.alloc, self.saved_settings) catch
            @panic("Failed to restore loaded settings");

        self.emulation_lock.lock();
        defer self.emulation_lock.unlock();

        deinitEmulatorSettings(self.alloc, &self.settings);
        self.settings = restored;
        self.tmp_selected_input_device = self.selected_input_device;

        resetShaderRuntimeState(self);
    }

    fn loadSettings(self: *Self) void {
        settings.load(self.alloc, self.config_dir, &self.settings) catch |err|
            std.log.err("settings load failed: {s}", .{@errorName(err)});
        self.should_load_shader = self.settings.shader_preset_path != null;
        self.should_load_border_shader = self.settings.border_shader != .none;
    }

    fn saveSettingsImpl(self: *Self) !void {
        try settings.save(self.alloc, self.config_dir, self.settings);
    }

    fn snapshotSettings(self: *Self) !void {
        var snapshot = try clonePersistedSettings(self.alloc, self.settings);
        errdefer deinitEmulatorSettings(self.alloc, &snapshot);

        deinitEmulatorSettings(self.alloc, &self.saved_settings);
        self.saved_settings = snapshot;
    }

    pub fn applyShaderParamSettings(self: *const Self, ui: *UI, target: ParamTarget) void {
        const items = switch (target) {
            .main => self.settings.shader_params.items,
            .border => self.settings.border_shader_params.items,
        };

        for (items) |item| {
            switch (target) {
                .main => ui.setShaderParam("main", item.name, item.value),
                .border => ui.setShaderParam("border", item.name, item.value),
            }
        }
    }

    pub fn setShaderParamSetting(self: *Self, target: ParamTarget, name: []const u8, value: f32) void {
        const param_settings = switch (target) {
            .main => &self.settings.shader_params,
            .border => &self.settings.border_shader_params,
        };

        settings.setShaderParamSetting(self.alloc, param_settings, name, value) catch {
            std.log.err("failed to persist shader parameter '{s}': out of memory", .{name});
            return;
        };
    }

    pub fn openShaderFilePicker(self: *Self, target: settings.ParamTarget) void {
        self.shader_target = target;
        self.show_custom_file_picker = true;

        self.loadShaderFilePickerEntries() catch |err| {
            std.log.err("failed to load shader file picker entries: {s}", .{@errorName(err)});
            self.shader_file_picker_error = std.fmt.allocPrint(
                self.alloc,
                "Failed to list shaders: {s}",
                .{@errorName(err)},
            ) catch null;
        };
    }

    pub fn closeShaderFilePicker(self: *Self) void {
        self.show_custom_file_picker = false;

        if (self.shader_file_picker_current_dir.len > 0) {
            self.alloc.free(self.shader_file_picker_current_dir);
            self.shader_file_picker_current_dir = &.{};
        }
        self.clearShaderFilePickerEntries();
        if (self.shader_file_picker_error) |old| {
            self.alloc.free(old);
            self.shader_file_picker_error = null;
        }
    }

    pub fn shaderFilePickerEntries(self: *const Self) []const ShaderFilePickerEntry {
        return self.shader_file_picker_entries.items;
    }

    pub fn selectShaderFilePickerEntry(self: *Self, index: usize) void {
        std.debug.assert(index <= self.shader_file_picker_entries.items.len);

        const entry = self.shader_file_picker_entries.items[index];
        std.debug.assert(entry.kind == .file);

        const root_path = paths.shaderDownloadAndroidPath(self.alloc) catch |err| {
            std.log.err("failed to resolve shader root path: {s}", .{@errorName(err)});
            return;
        };
        defer self.alloc.free(root_path);
        const shader_path = if (self.shader_file_picker_current_dir.len == 0)
            std.fs.path.join(self.alloc, &.{ root_path, entry.label }) catch @panic("Failed to allocate!")
        else
            std.fs.path.join(self.alloc, &.{ root_path, self.shader_file_picker_current_dir, entry.label }) catch @panic("Failed to allocate!");
        defer self.alloc.free(shader_path);

        switch (self.shader_target) {
            .main => {
                if (self.settings.shader_preset_path) |old| self.alloc.free(old);
                self.settings.shader_preset_path = self.alloc.dupe(u8, shader_path) catch @panic("Failed to allocate!");
                settings.clearShaderParamSettings(self.alloc, &self.settings.shader_params);
                self.should_load_shader = true;
                self.should_clear_shader = false;
            },
            .border => {},
        }

        self.closeShaderFilePicker();
    }

    pub fn openShaderFilePickerEntry(self: *Self, index: usize) void {
        std.debug.assert(index <= self.shader_file_picker_entries.items.len);

        const entry = self.shader_file_picker_entries.items[index];
        std.debug.assert(entry.kind == .directory);

        const next_dir = if (self.shader_file_picker_current_dir.len == 0)
            self.alloc.dupe(u8, entry.label) catch @panic("OOM")
        else
            std.fs.path.join(self.alloc, &.{ self.shader_file_picker_current_dir, entry.label }) catch @panic("OOM");
        defer self.alloc.free(next_dir);

        self.setShaderFilePickerCurrentDir(next_dir);
    }

    pub fn shaderFilePickerCanGoUp(self: *const Self) bool {
        return self.shader_file_picker_current_dir.len > 0;
    }

    pub fn shaderFilePickerGoUp(self: *Self) void {
        if (self.shader_file_picker_current_dir.len == 0) return;

        const parent = std.fs.path.dirname(self.shader_file_picker_current_dir) orelse "";
        self.setShaderFilePickerCurrentDir(parent);
    }

    fn loadShaderFilePickerEntries(self: *Self) !void {
        self.clearShaderFilePickerEntries();

        const root_path = try paths.shaderDownloadAndroidPath(self.alloc);
        defer self.alloc.free(root_path);
        const dir_path = if (self.shader_file_picker_current_dir.len == 0)
            try self.alloc.dupe(u8, root_path)
        else
            try std.fs.path.join(self.alloc, &.{ root_path, self.shader_file_picker_current_dir });
        defer self.alloc.free(dir_path);

        var dir = try std.fs.openDirAbsolute(dir_path, .{ .iterate = true });
        defer dir.close();

        try self.collectShaderFilePickerEntries(dir);
        std.mem.sort(ShaderFilePickerEntry, self.shader_file_picker_entries.items, {}, lessThanShaderFilePickerEntry);
    }

    fn collectShaderFilePickerEntries(self: *Self, dir: std.fs.Dir) !void {
        var it = dir.iterate();
        while (try it.next()) |entry| {
            switch (entry.kind) {
                .file => {
                    if (!std.mem.endsWith(u8, entry.name, ".slangp")) continue;
                    const label = try self.alloc.dupe(u8, entry.name);
                    errdefer self.alloc.free(label);

                    try self.shader_file_picker_entries.append(self.alloc, .{
                        .kind = .file,
                        .label = label,
                    });
                },
                .directory => {
                    const label = try self.alloc.dupe(u8, entry.name);
                    errdefer self.alloc.free(label);

                    try self.shader_file_picker_entries.append(self.alloc, .{
                        .kind = .directory,
                        .label = label,
                    });
                },
                else => {},
            }
        }
    }

    fn setShaderFilePickerCurrentDir(self: *Self, rel_dir: []const u8) void {
        if (self.shader_file_picker_current_dir.len > 0) {
            self.alloc.free(self.shader_file_picker_current_dir);
            self.shader_file_picker_current_dir = &.{};
        }

        self.clearShaderFilePickerEntries();
        if (self.shader_file_picker_error) |old| {
            self.alloc.free(old);
            self.shader_file_picker_error = null;
        }

        if (rel_dir.len > 0) {
            self.shader_file_picker_current_dir = self.alloc.dupe(u8, rel_dir) catch @panic("Failed to allocate!");
        }

        self.loadShaderFilePickerEntries() catch |err| {
            std.log.err("failed to load shader file picker entries: {s}", .{@errorName(err)});
            self.shader_file_picker_error = std.fmt.allocPrint(
                self.alloc,
                "Failed to list shaders: {s}",
                .{@errorName(err)},
            ) catch null;
        };
    }

    pub fn shaderDownloadInstalled(self: *Self) bool {
        const root_path = paths.shaderDownloadAndroidPath(self.alloc) catch |err| {
            std.log.warn("failed to resolve shader download path: {s}", .{@errorName(err)});
            return false;
        };
        defer self.alloc.free(root_path);

        var dir = std.fs.openDirAbsolute(root_path, .{}) catch |err| switch (err) {
            error.FileNotFound => return false,
            else => {
                std.log.warn("failed to inspect shader download path '{s}': {s}", .{ root_path, @errorName(err) });
                return false;
            },
        };
        dir.close();
        return true;
    }

    pub fn shaderDownloadStatus(self: *Self) struct {
        state: shader_download.State,
        active: bool,
        bytes: u64,
        total_bytes: u64,
        error_message: ?[]const u8,
    } {
        return .{
            .state = shader_download.stateFromInt(self.shader_download_state.load(.acquire)),
            .active = self.shader_download_thread != null,
            .bytes = self.shader_download_bytes.load(.acquire),
            .total_bytes = self.shader_download_total_bytes.load(.acquire),
            .error_message = self.shader_download_error,
        };
    }

    pub fn startShaderDownload(self: *Self) !void {
        if (!builtin.abi.isAndroid()) return error.UnsupportedPlatform;
        if (self.shader_download_thread != null) return error.ShaderDownloadInProgress;
        if (self.shaderDownloadInstalled()) return error.ShaderDownloadAlreadyInstalled;

        if (self.shader_download_error) |old| {
            self.alloc.free(old);
            self.shader_download_error = null;
        }

        const root_path = try paths.shaderDownloadAndroidPath(self.alloc);
        errdefer self.alloc.free(root_path);

        self.shader_download_root_path = root_path;
        self.shader_download_result = null;
        self.shader_download_bytes.store(0, .release);
        self.shader_download_total_bytes.store(shader_download.unknown_total, .release);
        self.shader_download_state.store(@intFromEnum(shader_download.State.idle), .release);

        self.shader_download_thread = std.Thread.spawn(.{}, shaderDownloadThreadMain, .{self}) catch |err| {
            self.alloc.free(root_path);
            self.shader_download_root_path = null;
            return err;
        };
    }

    fn pollShaderDownload(self: *Self) void {
        const state = shader_download.stateFromInt(self.shader_download_state.load(.acquire));
        if (self.shader_download_thread == null or (state != .done and state != .failed)) return;

        self.joinShaderDownloadThread();

        if (state == .failed) {
            if (self.shader_download_error) |old| self.alloc.free(old);
            const result = self.shader_download_result orelse error.ShaderDownloadFailed;
            self.shader_download_error = std.fmt.allocPrint(
                self.alloc,
                "Download failed: {s}",
                .{@errorName(result)},
            ) catch null;
        } else if (self.shader_download_error) |old| {
            self.alloc.free(old);
            self.shader_download_error = null;
        }
    }

    fn joinShaderDownloadThread(self: *Self) void {
        if (self.shader_download_thread) |thread| {
            thread.join();
            self.shader_download_thread = null;
        }

        if (self.shader_download_root_path) |path| {
            self.alloc.free(path);
            self.shader_download_root_path = null;
        }
    }

    fn clearShaderFilePickerEntries(self: *Self) void {
        for (self.shader_file_picker_entries.items) |entry| {
            self.alloc.free(entry.label);
        }
        self.shader_file_picker_entries.clearRetainingCapacity();
    }

    pub fn togglePause(self: *Self) void {
        self.emulation_lock.lock();
        defer self.emulation_lock.unlock();
        self.paused = !self.paused;
    }

    pub fn setLifecycleSuspended(self: *Self, suspended: bool) void {
        if (self.lifecycle_suspended.swap(suspended, .acq_rel) == suspended) return;

        if (suspended) {
            self.clearControllerState();
        }

        if (self.system) |*system| {
            system.setAudioPaused(suspended);
        }
    }

    pub fn toggleDebug(self: *Self) void {
        self.emulation_lock.lock();
        defer self.emulation_lock.unlock();
        self.step_mode = !self.step_mode;
        self.render_debug_ui = !self.render_debug_ui;
    }

    pub fn toggleStepMode(self: *Self) void {
        self.emulation_lock.lock();
        defer self.emulation_lock.unlock();
        self.step_mode = !self.step_mode;
    }
};

fn shaderDownloadThreadMain(app_state: *AppState) void {
    const root_path = app_state.shader_download_root_path orelse {
        app_state.shader_download_result = error.InvalidShaderPath;
        app_state.shader_download_state.store(@intFromEnum(shader_download.State.failed), .release);
        return;
    };

    const progress = shader_download.Progress{
        .state = &app_state.shader_download_state,
        .bytes = &app_state.shader_download_bytes,
        .total_bytes = &app_state.shader_download_total_bytes,
    };

    shader_download.downloadAndExtract(root_path, progress) catch |err| {
        app_state.shader_download_result = err;
        progress.setState(.failed);
        std.log.err("shader download failed: {s}", .{@errorName(err)});
    };
}

fn lessThanShaderFilePickerEntry(_: void, lhs: ShaderFilePickerEntry, rhs: ShaderFilePickerEntry) bool {
    if (lhs.kind != rhs.kind) return lhs.kind == .directory;
    return std.mem.lessThan(u8, lhs.label, rhs.label);
}

fn clonePersistedSettings(
    alloc: std.mem.Allocator,
    source: AppState.EmulatorSettings,
) !AppState.EmulatorSettings {
    var result = AppState.EmulatorSettings{};
    errdefer deinitEmulatorSettings(alloc, &result);

    inline for (std.meta.fields(settings.SettingsConfig)) |field| {
        if (@hasField(AppState.EmulatorSettings, field.name)) {
            try cloneSettingsField(alloc, &@field(result, field.name), @field(source, field.name));
        }
    }

    return result;
}

fn cloneSettingsField(alloc: std.mem.Allocator, dest: anytype, source: anytype) !void {
    const Dest = @typeInfo(@TypeOf(dest)).pointer.child;
    const Source = @TypeOf(source);

    if (Dest == ?[]u8 and Source == ?[]u8) {
        if (source) |value| {
            dest.* = try alloc.dupe(u8, value);
        }
    } else if (Dest == std.ArrayList(ShaderParamSetting) and Source == std.ArrayList(ShaderParamSetting)) {
        try cloneShaderParamSettings(alloc, dest, source.items);
    } else {
        dest.* = source;
    }
}

fn cloneShaderParamSettings(
    alloc: std.mem.Allocator,
    dest: *std.ArrayList(ShaderParamSetting),
    source: []const ShaderParamSetting,
) !void {
    errdefer settings.clearShaderParamSettings(alloc, dest);

    for (source) |item| {
        const owned_name = try alloc.dupe(u8, item.name);
        errdefer alloc.free(owned_name);

        try dest.append(alloc, .{
            .name = owned_name,
            .value = item.value,
        });
    }
}

fn deinitShaderGroup(
    alloc: std.mem.Allocator,
    preset_path: *?[]u8,
    params: *std.ArrayList(ShaderParamSetting),
) void {
    if (preset_path.*) |path| {
        alloc.free(path);
        preset_path.* = null;
    }
    settings.clearShaderParamSettings(alloc, params);
    params.deinit(alloc);
}

fn deinitEmulatorSettings(alloc: std.mem.Allocator, s: *AppState.EmulatorSettings) void {
    deinitShaderGroup(alloc, &s.shader_preset_path, &s.shader_params);
    settings.clearShaderParamSettings(alloc, &s.border_shader_params);
    s.border_shader_params.deinit(alloc);
}

fn resetShaderRuntimeState(app_state: *AppState) void {
    app_state.should_load_shader = app_state.settings.shader_preset_path != null;
    app_state.should_clear_shader = app_state.settings.shader_preset_path == null;
    app_state.shader_loading = false;
    if (app_state.shader_error) |old| {
        app_state.alloc.free(old);
        app_state.shader_error = null;
    }

    app_state.should_load_border_shader = app_state.settings.border_shader != .none;
    app_state.should_clear_border_shader = app_state.settings.border_shader == .none;
    app_state.border_shader_loading = false;
    if (app_state.border_shader_error) |old| {
        app_state.alloc.free(old);
        app_state.border_shader_error = null;
    }
}

fn deinitShaderRuntimeState(alloc: std.mem.Allocator, app_state: *AppState) void {
    app_state.joinShaderDownloadThread();
    app_state.clearShaderFilePickerEntries();
    app_state.shader_file_picker_entries.deinit(alloc);

    if (app_state.shader_error) |msg| {
        alloc.free(msg);
        app_state.shader_error = null;
    }
    if (app_state.border_shader_error) |msg| {
        alloc.free(msg);
        app_state.border_shader_error = null;
    }
    if (app_state.shader_download_error) |msg| {
        alloc.free(msg);
        app_state.shader_download_error = null;
    }
    if (app_state.shader_file_picker_error) |msg| {
        alloc.free(msg);
        app_state.shader_file_picker_error = null;
    }
    if (app_state.shader_file_picker_current_dir.len > 0) {
        alloc.free(app_state.shader_file_picker_current_dir);
        app_state.shader_file_picker_current_dir = &.{};
    }
}

fn isSettingsEqual(a: AppState.EmulatorSettings, b: AppState.EmulatorSettings) bool {
    inline for (std.meta.fields(settings.SettingsConfig)) |field| {
        if (@hasField(AppState.EmulatorSettings, field.name)) {
            if (!settingsFieldEqual(@field(a, field.name), @field(b, field.name))) {
                return false;
            }
        }
    }

    return true;
}

fn hasInputDeviceChanged(a: [2]InputDevice, b: [2]InputDevice) bool {
    return !(a[0].eql(b[0]) and a[1].eql(b[1]));
}

fn settingsFieldEqual(a: anytype, b: @TypeOf(a)) bool {
    const T = @TypeOf(a);
    if (T == ?[]u8) {
        return optionalStringsEqual(a, b);
    } else if (T == std.ArrayList(ShaderParamSetting)) {
        return shaderParamSettingsEqual(a.items, b.items);
    } else {
        return std.meta.eql(a, b);
    }
}

fn optionalStringsEqual(a: ?[]const u8, b: ?[]const u8) bool {
    if (a == null and b == null) return true;
    if (a == null or b == null) return false;
    return std.mem.eql(u8, a.?, b.?);
}

fn shaderParamSettingsEqual(a: []const ShaderParamSetting, b: []const ShaderParamSetting) bool {
    if (a.len != b.len) return false;
    for (a, b) |a_item, b_item| {
        if (!std.mem.eql(u8, a_item.name, b_item.name)) return false;
        if (a_item.value != b_item.value) return false;
    }
    return true;
}
