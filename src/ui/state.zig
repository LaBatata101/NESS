const std = @import("std");
const builtin = @import("builtin");

const game_history = @import("../game_history.zig");
const c = @import("../root.zig").c;
const Key = @import("core/ui.zig").Key;
const UI = @import("core/ui.zig").UI;
const Window = @import("core/ui.zig").Window;
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
const netplay_protocol = @import("../netplay/protocol.zig");
const netplay_snapshot = @import("../netplay/snapshot.zig");
const file = @import("../utils/file.zig");
const android = @import("../utils/android.zig");
const ness = @import("../root.zig");
const clay = @import("core/clay.zig");
const sdlError = ness.sdlError;
const SessionManager = ness.netplay_session.SessionManager;

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
const CONNECTION_STATS_SAMPLE_MS: i64 = 500;
/// Keep the SDL event loop responsive when the transport delivers a burst of
/// authoritative frames (for example after a checkpoint or scheduler stall).
const MAX_NETPLAY_FRAMES_PER_UPDATE: usize = 4;

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

const Netplay = struct {
    session_manager: ness.netplay_session.SessionManager,
    session_code: ?[]u8 = null,
    session_error: ?[]const u8 = null,
    session_preview_name: ?[]const u8 = null,
    session_preview_size: u32 = 0,
    session_preview_hash: ?netplay_protocol.Digest = null,
    session_preview_frame: ?[]const u8 = null,
    session_peer: ?[32]u8 = null,
    connection_stats: ?ness.netplay_session.ConnectionStats = null,
    connection_stats_sample_time_ms: i64 = 0,
    session_window_handle: ?*Window = null,
    active_session_role: ness.netplay_session.Role = .none,
    network_rom: bool = false,

    epoch: u32 = 0,
    frame: u64 = 0,
    last_ack: u64 = 0,
    remote_player2: std.atomic.Value(u8) = .init(0),
    checkpoint_frame: u64 = 0,
    checkpoint_digest: ?[32]u8 = null,
    resyncing: bool = false,
    ready: bool = false,
    lead_paused: bool = false,
    rebase_times: [3]i64 = .{ 0, 0, 0 },
    client_saved_speed: ?EmulationSpeed = null,

    fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
        self.session_manager.deinit();
        self.clearPresentation(alloc);
    }

    fn clearPresentation(self: *@This(), alloc: std.mem.Allocator) void {
        if (self.session_code) |value| alloc.free(value);
        self.session_code = null;
        if (self.session_error) |value| alloc.free(value);
        self.session_error = null;
        if (self.session_preview_name) |value| alloc.free(value);
        self.session_preview_name = null;
        if (self.session_preview_frame) |value| alloc.free(value);
        self.session_preview_frame = null;
        self.session_preview_size = 0;
        self.session_preview_hash = null;
        self.session_peer = null;
        self.connection_stats = null;
        self.connection_stats_sample_time_ms = 0;
    }
};

pub const AppState = struct {
    alloc: std.mem.Allocator,
    io: std.Io,
    ui: *UI,
    /// Wheter to skip drawing the home screen.
    render_home_ui: bool = true,
    render_debug_ui: bool = false,
    show_android_settings_ui: bool = false,
    show_android_multiplayer_ui: bool = false,
    show_android_sidepanel: bool = false,
    show_android_save_state_dialog: bool = false,
    show_android_load_state_dialog: bool = false,
    emulation_running: bool = false,

    netplay: Netplay,

    step_mode: bool = false,

    rom_bytes: ?[]u8 = null,
    rom: ?Rom = null,
    system: ?System = null,

    history: game_history.GameHistory = undefined,
    current_rom_path: ?[]u8 = null,
    save_state_info: [save_state.SLOT_COUNT]?save_state.SlotInfo = @splat(null),
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
    shader_file_picker_entries: std.ArrayList(ShaderFilePickerEntry) = .empty,
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
    emulation_lock: std.Io.RwLock = .init,
    controller1_bits: std.atomic.Value(u8) = .init(0),
    controller2_bits: std.atomic.Value(u8) = .init(0),
    frame_buffers: [2]Frame = .{ Frame.init(), Frame.init() },
    published_frame_idx: std.atomic.Value(u8) = .init(0),
    ui_frame_idx: std.atomic.Value(u8) = .init(0),
    writing_frame_idx: std.atomic.Value(u8) = .init(NO_FRAME),
    render_frame_idx: u8 = 0,
    emulation_speed_frame_count: std.atomic.Value(u64) = .init(0),
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
        shader_params: std.ArrayList(ShaderParamSetting) = .empty,
        /// Active bundled border shader preset.
        border_shader: BorderShaderOpts = .none,
        /// Active border shader parameter overrides (names are owned by this struct).
        border_shader_params: std.ArrayList(ShaderParamSetting) = .empty,
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

    pub fn init(alloc: std.mem.Allocator, io: std.Io, ui: *UI) Self {
        var hist = game_history.GameHistory.init(alloc, io);
        hist.load();

        const img_bytes = c.SDL_IOFromConstMem(NES_CONTROLLER_IMG, NES_CONTROLLER_IMG.len);
        const surface = c.SDL_LoadPNG_IO(img_bytes, true);

        const config_dir = paths.getConfigDir(alloc) catch |err| blk: {
            std.log.warn("settings directory unavailable: {s}", .{@errorName(err)});
            break :blk null;
        };

        var state: Self = .{
            .alloc = alloc,
            .io = io,
            .ui = ui,
            .netplay = .{ .session_manager = SessionManager.init(alloc, io) },
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
        if (self.netplay.session_manager.isActive()) {
            // Give the peer a protocol-level disconnect before deinit closes
            // the transport, so a normal application exit is not reported as
            // an unexplained connection failure.
            self.netplay.session_manager.disconnect();
        }
        self.stopEmulationThread();

        self.netplay.deinit(self.alloc);

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
        self.emulation_speed_frame_count.store(0, .release);
        self.emulation_speed_percent.store(0, .release);
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
        var speed_window_start: u64 = c.SDL_GetTicks();

        while (!self.emulation_stop.load(.acquire)) {
            self.emulation_lock.lockUncancelable(self.io);
            const authoritative_client = self.netplay.active_session_role == .client and self.netplay.network_rom;
            const can_run = self.emulation_running and
                self.system != null and
                !self.lifecycle_suspended.load(.acquire) and
                !self.paused and
                !self.step_mode and
                !self.netplay.resyncing and
                !authoritative_client;
            if (can_run) {
                const speed = self.settings.emulation_speed;
                const multiplier = speed.multiplier();
                frame_acc += multiplier;
                const frames_to_run: u32 = @intFromFloat(frame_acc);
                frame_acc -= @floatFromInt(frames_to_run);

                self.system.?.apu.device.setSpeed(multiplier);
                for (0..frames_to_run) |_| {
                    var controllers = self.controllerSnapshot();
                    const connected_host = self.isConnectedHost();
                    if (connected_host) {
                        const frame_lead = self.netplay.frame -| self.netplay.last_ack;
                        if (frame_lead >= 12) {
                            if (!self.netplay.lead_paused) {
                                std.log.warn("netplay: host reached lead limit; pausing emulation (epoch={d}, frame={d}, last_ack={d}, lead={d})", .{
                                    self.netplay.epoch,
                                    self.netplay.frame,
                                    self.netplay.last_ack,
                                    frame_lead,
                                });
                                self.netplay.lead_paused = true;
                            }
                            self.system.?.setAudioPaused(true);
                            break;
                        }

                        if (self.netplay.lead_paused) {
                            // Use hysteresis so normal acknowledgement jitter does not
                            // alternate pause/resume for every individual frame.
                            if (frame_lead > 6) {
                                self.system.?.setAudioPaused(true);
                                break;
                            }
                            std.log.info("netplay: client caught up; host resuming emulation (epoch={d}, frame={d}, last_ack={d})", .{
                                self.netplay.epoch,
                                self.netplay.frame,
                                self.netplay.last_ack,
                            });
                            self.netplay.lead_paused = false;
                        }
                        self.system.?.setAudioPaused(false);
                        controllers.player2 = @bitCast(self.netplay.remote_player2.load(.acquire));
                    }

                    self.system.?.applyControllerSnapshot(controllers);
                    self.system.?.run_frame();
                    self.publishFrame(self.system.?.frame_buffer());
                    if (connected_host) self.publishAuthoritativeFrame(controllers);
                    _ = self.emulation_speed_frame_count.fetchAdd(1, .monotonic);
                }
            }

            const now = c.SDL_GetTicks();
            const diff = now -% speed_window_start;
            if (diff >= SPEED_SAMPLE_MS) {
                const speed_frame_count = self.emulation_speed_frame_count.swap(0, .acq_rel);
                const percent = @divFloor(speed_frame_count * c.SDL_MS_PER_SECOND * 100, diff * NES_TARGET_FPS);
                self.emulation_speed_percent.store(percent, .release);
                speed_window_start = now;
            }
            self.emulation_lock.unlock(self.io);

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
            if (main_window_active or self.sessionActive()) self.syncControllers(ui);
            if (main_window_active) {
                const client_restricted = self.isConnectedClient();
                if (!client_restricted and ui.isKeyPressed(self.generalBinding(.quick_save))) self.saveStateSlot(0);
                if (!client_restricted and ui.isKeyPressed(self.generalBinding(.quick_load))) self.loadStateSlot(0);
                if (ui.isKeyPressed(self.generalBinding(.quit))) ui.quit = true;
                if (!client_restricted and !self.sessionActive() and ui.isKeyPressed(self.generalBinding(.toggle_step_mode))) self.toggleDebug();
                if (!client_restricted and ui.isKeyPressed(self.generalBinding(.restart))) self.resetSystem();
                if (!client_restricted and ui.isKeyPressed(self.generalBinding(.toggle_pause))) self.togglePause();
                if (!client_restricted and ui.isKeyPressed(self.generalBinding(.stop))) {
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

        if (self.show_android_multiplayer_ui) {
            self.closeAndroidSessionUI();
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
        self.updateNetplay();
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
                self.selected_input_device[1] = .{ .gamepad = self.input_devices.items[0].gamepad };
                self.tmp_selected_input_device[1] = .{ .gamepad = self.input_devices.items[0].gamepad };
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
        self.emulation_lock.lockSharedUncancelable(self.io);
        defer self.emulation_lock.unlockShared(self.io);

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
            const touch_player = if (self.isConnectedClient()) Player.two else Player.one;
            if (player_id == touch_player) status.insert(ui.onScreenControllerStatus());
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
        if (self.isConnectedClient()) return;

        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);

        self.system.?.reset();

        if (self.isConnectedHost()) {
            std.log.info("netplay: host reset system; scheduling authoritative rebase", .{});
            self.sendRebase() catch |err| {
                std.log.err("netplay: failed to send reset rebase: {s}", .{@errorName(err)});
                self.netplay.session_manager.disconnect();
            };
        }
    }

    pub fn runSystemTick(self: *Self) void {
        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);
        self.system.?.tick();
    }

    pub fn runSystemFrame(self: *Self) void {
        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);
        self.system.?.run_frame();
    }

    pub fn setEmulationSpeed(self: *Self, speed: EmulationSpeed) void {
        if (self.isConnectedClient()) return;

        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);

        self.settings.emulation_speed = speed;

        if (self.isConnectedHost()) {
            std.log.info("netplay: host changed emulation speed to {s}", .{@tagName(speed)});
            self.netplay.session_manager.send(.init(&.{
                .control = .{ .speed = @intFromEnum(speed) },
            })) catch |err| {
                std.log.err("netplay: failed to send speed control: {s}", .{@errorName(err)});
                self.netplay.session_manager.disconnect();
            };
        }
    }

    pub fn romDisplayName(self: *const Self) []const u8 {
        return if (builtin.abi.isAndroid())
            (android.displayName(self.alloc, self.current_rom_path.?) catch @panic("JNI error")).?
        else
            self.alloc.dupe(u8, std.fs.path.stem(self.current_rom_path.?)) catch @panic("OOM");
    }

    pub fn saveStateSlot(self: *Self, slot: usize) void {
        if (self.isConnectedClient()) return;
        std.debug.assert(self.current_rom_path != null);
        std.debug.assert(slot < save_state.SLOT_COUNT);

        const name = self.romDisplayName();
        defer self.alloc.free(name);

        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);

        std.log.info("Saving state to slot {} for \"{s}\"", .{ slot + 1, name });

        const info = save_state.saveSlot(self.alloc, self.io, name, &self.system.?, slot) catch |err| {
            std.log.err("save state slot {} failed: {s}", .{ slot + 1, @errorName(err) });
            return;
        };

        self.save_state_info[slot] = info;
        self.ui.setTimer("save_state_toast", 1000);
    }

    pub fn loadStateSlot(self: *Self, slot: usize) void {
        if (self.isConnectedClient()) return;
        std.debug.assert(self.current_rom_path != null);

        const name = self.romDisplayName();
        defer self.alloc.free(name);

        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);

        std.log.info("Loading state from slot {} for \"{s}\"", .{ slot + 1, name });

        save_state.loadSlot(self.alloc, self.io, name, &self.system.?, slot) catch |err| {
            std.log.err("load state slot {} failed: {s}", .{ slot + 1, @errorName(err) });
        };
        if (self.isConnectedHost()) {
            std.log.info("netplay: host loaded save state slot {d}; scheduling authoritative rebase", .{slot + 1});
            self.sendRebase() catch |err| {
                std.log.err("netplay: failed to send load-state rebase: {s}", .{@errorName(err)});
                self.netplay.session_manager.disconnect();
            };
        }
        self.ui.setTimer("load_state_toast", 1000);
    }

    pub fn saveStateInfo(self: *const Self, slot: usize) ?save_state.SlotInfo {
        std.debug.assert(slot < save_state.SLOT_COUNT);
        return self.save_state_info[slot];
    }

    pub fn loadRom(self: *Self, path: []const u8) !void {
        if (self.sessionActive()) self.netplay.session_manager.disconnect();
        // Save previous game's progress before replacing it.
        if (self.emulation_running) {
            self.stopEmulationThread();
            self.saveCurrentGame();
        }

        if (self.rom) |*rom| rom.deinit();
        if (self.system) |*system| system.deinit();

        const rom_fullpath = if (builtin.abi.isAndroid()) path else blk: {
            const cwd = try std.process.currentPathAlloc(self.io, self.alloc);
            defer self.alloc.free(cwd);
            const rom_fullpath = try std.fs.path.resolve(self.alloc, &.{ cwd, path });

            break :blk rom_fullpath;
        };
        defer if (!builtin.abi.isAndroid()) self.alloc.free(rom_fullpath);

        std.log.debug("Reading file: {s}", .{rom_fullpath});

        if (self.rom_bytes) |bytes| self.alloc.free(bytes);
        self.rom_bytes = try file.readFile(self.alloc, self.io, rom_fullpath);

        self.rom = try Rom.init(self.alloc, self.io, rom_fullpath, self.rom_bytes.?);
        self.system = try System.init(self.alloc, self.io, &self.rom.?, .{});
        self.system.?.reset();
        self.publishFrame(self.system.?.frame_buffer());
        self.emulation_running = true;
        self.netplay.network_rom = false;
        self.render_home_ui = false;
        self.render_debug_ui = false;
        self.show_android_settings_ui = false;
        self.show_android_multiplayer_ui = false;
        self.show_android_sidepanel = false;

        if (self.current_rom_path) |p| self.alloc.free(p);
        self.current_rom_path = self.alloc.dupe(u8, rom_fullpath) catch null;
        self.game_start_time_ms = std.Io.Timestamp.now(self.io, .real).toMilliseconds();
        self.refreshSaveStateInfo();
        try self.startEmulationThread();
    }

    pub fn unloadCurrentRom(self: *Self) void {
        if (self.sessionActive()) self.netplay.session_manager.disconnect();
        self.unloadCurrentRomInternal();
    }

    fn unloadCurrentRomInternal(self: *Self) void {
        if (!self.emulation_running) return;
        self.clearControllerState();
        self.stopEmulationThread();
        if (!self.netplay.network_rom) self.saveCurrentGame();

        self.rom.?.deinit();
        self.system.?.deinit();
        self.alloc.free(self.current_rom_path.?);
        self.alloc.free(self.rom_bytes.?);

        self.rom = null;
        self.system = null;
        self.rom_bytes = null;
        self.current_rom_path = null;
        self.emulation_running = false;
        self.netplay.network_rom = false;
        self.render_home_ui = true;
        self.render_debug_ui = false;

        self.show_android_settings_ui = false;
        self.show_android_multiplayer_ui = false;
        self.show_android_sidepanel = false;
        @memset(self.save_state_info[0..], null);
    }

    fn saveCurrentGame(self: *Self) void {
        if (self.netplay.network_rom) return;
        const path = self.current_rom_path orelse return;

        const name = self.romDisplayName();
        defer self.alloc.free(name);

        const elapsed_ms = std.Io.Timestamp.now(self.io, .real).toMilliseconds() - self.game_start_time_ms;
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
        self.game_start_time_ms = std.Io.Timestamp.now(self.io, .real).toMilliseconds();
    }

    fn refreshSaveStateInfo(self: *Self) void {
        @memset(self.save_state_info[0..], null);

        const name = self.romDisplayName();
        defer self.alloc.free(name);

        for (&self.save_state_info, 0..) |*info, slot| {
            info.* = save_state.info(self.alloc, self.io, name, slot) catch null;
        }
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

        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);

        deinitEmulatorSettings(self.alloc, &self.settings);
        self.settings = restored;
        self.tmp_selected_input_device = self.selected_input_device;

        resetShaderRuntimeState(self);
    }

    fn loadSettings(self: *Self) void {
        settings.load(self.alloc, self.io, self.config_dir, &self.settings) catch |err|
            std.log.err("settings load failed: {s}", .{@errorName(err)});
        self.should_load_shader = self.settings.shader_preset_path != null;
        self.should_load_border_shader = self.settings.border_shader != .none;
    }

    fn saveSettingsImpl(self: *Self) !void {
        try settings.save(self.alloc, self.io, self.config_dir, self.settings);
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

        var dir = try std.Io.Dir.openDirAbsolute(self.io, dir_path, .{ .iterate = true });
        defer dir.close(self.io);

        try self.collectShaderFilePickerEntries(dir);
        std.mem.sort(ShaderFilePickerEntry, self.shader_file_picker_entries.items, {}, lessThanShaderFilePickerEntry);
    }

    fn collectShaderFilePickerEntries(self: *Self, dir: std.Io.Dir) !void {
        var it = dir.iterate();
        while (try it.next(self.io)) |entry| {
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

        var dir = std.Io.Dir.openDirAbsolute(self.io, root_path, .{}) catch |err| switch (err) {
            error.FileNotFound => return false,
            else => {
                std.log.warn("failed to inspect shader download path '{s}': {s}", .{ root_path, @errorName(err) });
                return false;
            },
        };
        dir.close(self.io);
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
        if (self.isConnectedClient()) return;

        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);

        self.paused = !self.paused;

        if (self.isConnectedHost()) {
            std.log.info("netplay: host changed pause state to {any}", .{self.paused});
            self.netplay.session_manager.send(.init(&.{
                .control = .{ .paused = self.paused },
            })) catch |err| {
                std.log.err("netplay: failed to send pause control: {s}", .{@errorName(err)});
                self.netplay.session_manager.disconnect();
            };
        }
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
        if (self.sessionActive()) return;
        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);
        self.step_mode = !self.step_mode;
        self.render_debug_ui = !self.render_debug_ui;
    }

    pub fn toggleStepMode(self: *Self) void {
        if (self.sessionActive()) return;
        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);
        self.step_mode = !self.step_mode;
    }

    pub fn sessionActive(self: *Self) bool {
        return self.netplay.session_manager.isActive();
    }

    pub fn sessionRole(self: *Self) ness.netplay_session.Role {
        return self.netplay.active_session_role;
    }

    pub fn sessionState(self: *Self) ness.netplay_session.State {
        return self.netplay.session_manager.getState();
    }

    pub fn isConnectedClient(self: *Self) bool {
        return self.netplay.active_session_role == .client and self.netplay.session_manager.getState() == .connected;
    }

    pub fn isConnectedHost(self: *Self) bool {
        return self.netplay.active_session_role == .host and self.netplay.ready and self.netplay.session_manager.getState() == .connected;
    }

    pub fn startHostSession(self: *Self) !void {
        if (!self.emulation_running or self.rom_bytes == null or self.current_rom_path == null) return error.NoGameRunning;
        if (self.rom_bytes.?.len > netplay_protocol.max_rom_size) return error.RomTooLarge;

        self.clearSessionPresentation();

        const framebuffer = blk: {
            self.emulation_lock.lockSharedUncancelable(self.io);
            defer self.emulation_lock.unlockShared(self.io);
            break :blk try self.alloc.dupe(u8, self.system.?.frame_buffer());
        };
        errdefer self.alloc.free(framebuffer);

        var rom_hash: [32]u8 = undefined;
        std.crypto.hash.Blake3.hash(self.rom_bytes.?, &rom_hash, .{});

        const rom_name = self.romDisplayName();
        defer self.alloc.free(rom_name);

        var name_len = @min(rom_name.len, netplay_protocol.max_display_name);
        while (name_len > 0 and !std.unicode.utf8ValidateSlice(rom_name[0..name_len])) name_len -= 1;
        const display_name = if (name_len == 0) "game.nes" else rom_name[0..name_len];
        const preview_name = try self.alloc.dupe(u8, display_name);
        errdefer self.alloc.free(preview_name);
        const protocol_name = try self.alloc.dupe(u8, display_name);
        errdefer self.alloc.free(protocol_name);

        std.log.info("netplay: preparing host session for '{s}' (rom={d} bytes, hash={x})", .{
            display_name,
            self.rom_bytes.?.len,
            rom_hash[0..8],
        });

        try self.netplay.session_manager.startHost(.init(.{
            .name = protocol_name,
            .rom_size = @intCast(self.rom_bytes.?.len),
            .rom_hash = rom_hash,
            .framebuffer = framebuffer,
        }));

        self.netplay.session_preview_name = preview_name;

        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);

        self.netplay.active_session_role = .host;
        self.netplay.epoch = 0;
        self.netplay.frame = 0;
        self.netplay.last_ack = 0;
        self.netplay.ready = false;
        self.netplay.lead_paused = false;

        std.log.info("netplay: host session startup accepted", .{});
    }

    pub fn connectSession(self: *Self, code: []const u8) !void {
        std.log.info("netplay: preparing client connection (code_length={d})", .{std.mem.trim(u8, code, " \t\r\n").len});

        self.clearSessionPresentation();
        try self.netplay.session_manager.connect(code);

        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);

        self.netplay.active_session_role = .client;

        std.log.info("netplay: client connection startup accepted", .{});
    }

    pub fn joinSession(self: *Self) !void {
        std.log.info("netplay: join confirmed from preview UI", .{});
        try self.netplay.session_manager.acceptPreview();
    }

    pub fn leaveSession(self: *Self) void {
        std.log.info("netplay: leave session requested from UI", .{});

        if (builtin.abi.isAndroid()) {
            self.show_android_multiplayer_ui = false;
            self.render_home_ui = !self.emulation_running;
            if (self.emulation_running and !(self.netplay.active_session_role == .client and self.netplay.network_rom)) {
                self.ui.setWindowFullscreen(true);
            }
        }

        self.netplay.session_manager.disconnect();
    }

    pub fn closeAndroidSessionUI(self: *Self) void {
        self.show_android_multiplayer_ui = false;
        self.handleSessionWindowClosed();
        self.render_home_ui = !self.emulation_running;
        if (self.emulation_running) self.ui.setWindowFullscreen(true);
    }

    fn closeSessionWindow(self: *Self, window: *Window) void {
        std.debug.assert(self.netplay.session_window_handle == window);
        self.netplay.session_window_handle = null;
        self.ui.closeWindow(window.id());
    }

    pub fn handleSessionWindowClosed(self: *Self) void {
        self.netplay.session_window_handle = null;

        const current = self.netplay.session_manager.getState();
        if (self.netplay.active_session_role == .client and
            (current == .connecting or current == .preview or current == .joining))
        {
            std.log.info("netplay: pending client setup cancelled because connection window closed (state={s})", .{@tagName(current)});
            if (current == .preview) {
                self.netplay.session_manager.disconnect();
            } else {
                self.netplay.session_manager.cancel();
            }
        }
    }

    fn clearSessionPresentation(self: *Self) void {
        self.netplay.clearPresentation(self.alloc);
    }

    fn updateNetplay(self: *Self) void {
        self.updateConnectionStats();

        var authoritative_frames_processed: usize = 0;

        while (self.netplay.session_manager.pollEvent()) |event_value| {
            var event = event_value;
            defer event.value.deinit(self.alloc);

            switch (event.value) {
                .state => |state| {
                    std.log.debug("netplay: application observed state {s}", .{@tagName(state)});

                    if (state == .waiting and self.netplay.active_session_role == .host) {
                        self.netplay.session_peer = null;
                    }

                    if (state == .connected and self.netplay.active_session_role == .client) {
                        self.ui.main_window.ctx.setTimer("connected_to_host_toast", 2500);

                        if (builtin.abi.isAndroid()) {
                            self.show_android_multiplayer_ui = false;
                            self.render_home_ui = false;
                            if (self.emulation_running) self.ui.setWindowFullscreen(true);
                        } else if (self.netplay.session_window_handle) |window| {
                            self.closeSessionWindow(window);
                        }
                    } else if (state == .connected and
                        self.netplay.active_session_role == .host and
                        !builtin.abi.isAndroid())
                    {
                        if (self.netplay.session_window_handle) |window| window.setWindowSize(600, 400);
                    }
                },
                .session_code => {
                    const value = event.value.takeSessionCode();

                    std.log.info("netplay: host session code is ready (length={d})", .{value.value.len});

                    if (self.netplay.session_code) |old| self.alloc.free(old);
                    self.netplay.session_code = value.value;
                },
                .preview => {
                    const preview = event.value.takePreview();

                    std.log.info("netplay: application received preview (name='{s}', rom_size={d}, hash={x})", .{
                        preview.value.name,
                        preview.value.rom_size,
                        preview.value.rom_hash[0..8],
                    });

                    if (self.netplay.session_preview_name) |old| self.alloc.free(old);
                    if (self.netplay.session_preview_frame) |old| self.alloc.free(old);

                    self.netplay.session_preview_name = preview.value.name;
                    self.netplay.session_preview_frame = preview.value.framebuffer;
                    self.netplay.session_preview_size = preview.value.rom_size;
                    self.netplay.session_preview_hash = preview.value.rom_hash;
                },
                .peer => |peer| {
                    std.log.info("netplay: application registered peer {x}", .{peer[0..8]});

                    self.netplay.session_peer = peer;
                },
                .join_requested => self.provideJoinData() catch |err| {
                    std.log.err("netplay: failed to prepare join data: {s}", .{@errorName(err)});
                    self.setSessionError(@errorName(err));
                    self.netplay.session_manager.cancel();
                },
                .message => |*message| {
                    const session_active = self.netplay.session_manager.isActive();
                    const process_authoritative_frame = session_active and
                        self.netplay.active_session_role == .client and message.* == .frame;

                    if (!session_active) {
                        std.log.debug("netplay: discarding queued {s} message after transport closed", .{@tagName(message.*)});
                    } else {
                        self.handleNetplayMessage(message) catch |err| {
                            if (err == error.SessionClosed) {
                                std.log.debug("netplay: {s} message processing was interrupted by session shutdown", .{@tagName(message.*)});
                            } else {
                                std.log.err("netplay: failed to handle {s} message: {s}", .{ @tagName(message.*), @errorName(err) });
                                self.setSessionError(@errorName(err));
                                self.netplay.session_manager.disconnect();
                            }
                        };
                    }

                    if (process_authoritative_frame) {
                        authoritative_frames_processed += 1;
                        if (authoritative_frames_processed >= MAX_NETPLAY_FRAMES_PER_UPDATE) return;
                    }
                },
                .failed => |message| {
                    std.log.err("netplay: session manager reported failure: {s}", .{message});
                    self.setSessionError(message);
                },
                .peer_disconnected => {
                    if (self.netplay.active_session_role == .host) {
                        self.ui.main_window.ctx.setTimer("client_disconnected_toast", 2500);
                    }
                },
                .disconnected => {
                    std.log.info("netplay: application received session-ended event", .{});
                    self.handleSessionEnded();
                },
            }
        }
    }

    fn updateConnectionStats(self: *Self) void {
        const session_state = self.netplay.session_manager.getState();
        if (session_state != .connected and session_state != .resyncing) {
            self.netplay.connection_stats = null;
            self.netplay.connection_stats_sample_time_ms = 0;
            return;
        }

        const now = std.Io.Timestamp.now(self.io, .real).toMilliseconds();
        if (self.netplay.connection_stats_sample_time_ms != 0 and
            now - self.netplay.connection_stats_sample_time_ms < CONNECTION_STATS_SAMPLE_MS)
        {
            return;
        }
        self.netplay.connection_stats_sample_time_ms = now;

        self.netplay.connection_stats = self.netplay.session_manager.getConnectionStats() catch |err| {
            std.log.warn("netplay: failed to sample connection statistics: {s}", .{@errorName(err)});
            return;
        };
    }

    pub fn setSessionError(self: *Self, message: []const u8) void {
        std.log.err("netplay: session error shown to user: {s}", .{message});

        if (self.netplay.session_error) |old| self.alloc.free(old);
        self.netplay.session_error = self.alloc.dupe(u8, message) catch @panic("OOM");
    }

    fn provideJoinData(self: *Self) !void {
        if (self.netplay.active_session_role != .host or self.rom_bytes == null) return error.InvalidSessionState;

        std.log.info("netplay: capturing host state at frame boundary for client join", .{});

        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);

        self.netplay.resyncing = true;
        self.netplay.ready = false;
        self.netplay.lead_paused = false;
        if (self.system) |*system| system.setAudioPaused(true);

        std.log.info("netplay: host emulation paused at join snapshot boundary until client is ready", .{});
        errdefer {
            self.netplay.resyncing = false;
            if (self.system) |*system| system.setAudioPaused(self.paused);
        }

        var snapshot = try self.system.?.saveState(self.alloc);
        defer snapshot.deinit(self.alloc);

        self.system.?.apu.resetOutputBuffers();

        const encoded = try netplay_snapshot.encode(self.alloc, &snapshot);
        defer self.alloc.free(encoded);

        var rom_hash: [32]u8 = undefined;
        std.crypto.hash.Blake3.hash(self.rom_bytes.?, &rom_hash, .{});

        self.netplay.epoch +%= 1;
        self.netplay.frame = 0;
        self.netplay.last_ack = 0;

        const name = self.romDisplayName();
        defer self.alloc.free(name);

        var name_len = @min(name.len, netplay_protocol.max_display_name);
        while (name_len > 0 and !std.unicode.utf8ValidateSlice(name[0..name_len])) name_len -= 1;
        const safe_name = if (name_len == 0) "game.nes" else name[0..name_len];

        std.log.info("netplay: sending initial state (name='{s}', rom={d} bytes, snapshot={d} bytes, epoch={d}, frame={d}, speed={s})", .{
            safe_name,
            self.rom_bytes.?.len,
            encoded.len,
            self.netplay.epoch,
            self.netplay.frame,
            @tagName(self.settings.emulation_speed),
        });

        try self.netplay.session_manager.send(.init(&.{ .join_data = .{
            .name = safe_name,
            .rom = self.rom_bytes.?,
            .rom_hash = rom_hash,
            .snapshot = encoded,
            .speed = @intFromEnum(self.settings.emulation_speed),
            .epoch = self.netplay.epoch,
            .frame = self.netplay.frame,
        } }));
    }

    fn handleNetplayMessage(self: *Self, message: *netplay_protocol.Message) !void {
        const incoming_role: netplay_protocol.IncomingRole = switch (self.netplay.active_session_role) {
            .host => .host,
            .client => .client,
            .none => return error.UnexpectedSessionMessage,
        };

        try netplay_protocol.validateIncomingMessage(incoming_role, std.meta.activeTag(message.*));

        switch (message.*) {
            .join_data => |*data| {
                if (self.netplay.network_rom) return error.UnexpectedSessionMessage;

                try self.installNetworkGame(data);
            },
            .ready => |ready| {
                self.emulation_lock.lockUncancelable(self.io);
                defer self.emulation_lock.unlock(self.io);

                try netplay_protocol.validateReady(
                    self.netplay.epoch,
                    self.netplay.frame,
                    self.netplay.resyncing and !self.netplay.ready,
                    ready,
                );

                self.netplay.last_ack = ready.frame;
                self.netplay.remote_player2.store(ready.player2, .release);
                self.netplay.resyncing = false;
                self.netplay.ready = true;
                self.netplay.lead_paused = false;
                if (self.system) |*system| system.setAudioPaused(self.paused);

                std.log.info("netplay: peer is ready; authoritative play active (epoch={d}, frame={d}, player2=0x{x})", .{
                    ready.epoch,
                    ready.frame,
                    ready.player2,
                });

                self.netplay.session_manager.markConnected();
            },
            .ack => |ack| {
                self.emulation_lock.lockUncancelable(self.io);
                defer self.emulation_lock.unlock(self.io);

                switch (try netplay_protocol.validateAcknowledgement(
                    self.netplay.epoch,
                    self.netplay.frame,
                    ack.epoch,
                    ack.frame,
                )) {
                    .stale => {
                        std.log.debug("netplay: discarded stale acknowledgement after rebase (ack_epoch={d}, ack_frame={d}, current_epoch={d})", .{
                            ack.epoch,
                            ack.frame,
                            self.netplay.epoch,
                        });
                        return;
                    },
                    .current => {},
                }

                self.netplay.last_ack = @max(self.netplay.last_ack, ack.frame);
                self.netplay.remote_player2.store(ack.player2, .release);

                if (ack.digest) |actual| {
                    if (ack.frame == self.netplay.checkpoint_frame) {
                        if (self.netplay.checkpoint_digest) |expected| {
                            if (!std.mem.eql(u8, &actual, &expected)) {
                                std.log.err("netplay: state digest mismatch (epoch={d}, frame={d}, expected={x}, actual={x})", .{
                                    ack.epoch,
                                    ack.frame,
                                    expected[0..8],
                                    actual[0..8],
                                });

                                try self.recoverDesyncLocked();
                            } else {
                                std.log.debug("netplay: checkpoint verified (epoch={d}, frame={d}, digest={x})", .{
                                    ack.epoch,
                                    ack.frame,
                                    actual[0..8],
                                });
                            }
                        }
                    }
                }
            },
            .frame => |frame| try self.applyAuthoritativeFrame(frame),
            .control => |control| switch (control) {
                .paused => |paused| {
                    std.log.info("netplay: applying host pause state {any}", .{paused});

                    self.paused = paused;
                    if (self.system) |*system| system.setAudioPaused(paused);
                },
                .speed => |value| {
                    const speed = std.enums.fromInt(EmulationSpeed, value) orelse return error.InvalidEmulationSpeed;

                    std.log.info("netplay: applying host emulation speed {s}", .{@tagName(speed)});

                    self.settings.emulation_speed = speed;
                    if (self.system) |*system| system.apu.device.setSpeed(speed.multiplier());
                },
            },
            .rebase => |rebase| try self.applyRebase(rebase),
            else => return error.UnexpectedSessionMessage,
        }
    }

    fn installNetworkGame(self: *Self, data: *netplay_protocol.JoinData) !void {
        std.log.info("netplay: validating network game (name='{s}', rom={d} bytes, snapshot={d} bytes, hash={x}, epoch={d}, frame={d})", .{
            data.name,
            data.rom.len,
            data.snapshot.len,
            data.rom_hash[0..8],
            data.epoch,
            data.frame,
        });

        const speed = std.enums.fromInt(EmulationSpeed, data.speed) orelse return error.InvalidEmulationSpeed;
        const preview_hash = self.netplay.session_preview_hash orelse return error.MissingSessionPreview;

        if (data.rom.len != self.netplay.session_preview_size) {
            std.log.err("netplay: transferred ROM size differs from approved preview (preview={d}, transfer={d})", .{
                self.netplay.session_preview_size,
                data.rom.len,
            });
            return error.PreviewRomMismatch;
        }

        var actual_hash: [32]u8 = undefined;
        std.crypto.hash.Blake3.hash(data.rom, &actual_hash, .{});

        if (!std.mem.eql(u8, &actual_hash, &data.rom_hash)) {
            std.log.err("netplay: transferred ROM hash mismatch (expected={x}, actual={x})", .{
                data.rom_hash[0..8],
                actual_hash[0..8],
            });
            return error.RomHashMismatch;
        }

        if (!std.mem.eql(u8, &actual_hash, &preview_hash)) {
            std.log.err("netplay: transferred ROM hash differs from approved preview (preview={x}, transfer={x})", .{
                preview_hash[0..8],
                actual_hash[0..8],
            });
            return error.PreviewRomMismatch;
        }

        std.log.debug("netplay: transferred ROM hash verified", .{});

        var snapshot = try netplay_snapshot.decode(self.alloc, data.snapshot);
        std.log.debug("netplay: network snapshot decoded successfully", .{});
        defer {
            snapshot.deinit(self.alloc);
            self.alloc.destroy(snapshot);
        }

        const rom_bytes = try self.alloc.dupe(u8, data.rom);
        var committed = false;
        errdefer if (!committed) self.alloc.free(rom_bytes);

        var rom = try Rom.initWithOptions(self.alloc, self.io, data.name, rom_bytes, .{ .disable_battery_ram = true });
        errdefer if (!committed) rom.deinit();

        const new_system = try self.alloc.create(System);
        var new_system_initialized = false;
        var new_system_moved = false;
        errdefer if (!new_system_moved) {
            if (new_system_initialized) new_system.deinit();
            self.alloc.destroy(new_system);
        };

        new_system.* = try System.init(self.alloc, self.io, &rom, .{});
        new_system_initialized = true;
        try new_system.loadState(snapshot);

        std.log.debug("netplay: network snapshot applied to new system", .{});

        const new_path = try self.alloc.dupe(u8, data.name);
        errdefer if (!committed) self.alloc.free(new_path);

        if (self.emulation_running) {
            self.stopEmulationThread();
            if (!self.netplay.network_rom) self.saveCurrentGame();
            self.rom.?.deinit();
            self.system.?.deinit();
            self.alloc.free(self.current_rom_path.?);
            self.alloc.free(self.rom_bytes.?);
        }

        self.rom_bytes = rom_bytes;
        self.rom = rom;

        new_system.bus.rom = &self.rom.?;
        new_system.ppu.rom = &self.rom.?;
        new_system.apu.dmc.rom = &self.rom.?;

        self.system = new_system.*;
        new_system_moved = true;
        self.alloc.destroy(new_system);
        self.current_rom_path = new_path;
        committed = true;

        self.netplay.network_rom = true;
        self.emulation_running = true;
        self.render_home_ui = false;
        self.render_debug_ui = false;
        self.paused = false;

        self.netplay.client_saved_speed = self.settings.emulation_speed;
        self.settings.emulation_speed = speed;
        self.system.?.apu.device.setSpeed(speed.multiplier());
        self.system.?.apu.device.setProducerBlocking(false);

        std.log.debug("netplay: authoritative client audio backpressure disabled", .{});

        self.netplay.epoch = data.epoch;
        self.netplay.frame = data.frame;
        self.netplay.lead_paused = false;

        self.publishFrame(self.system.?.frame_buffer());
        try self.startEmulationThread();

        const player2: u8 = @bitCast(self.controllerSnapshot().player2);

        std.log.info("netplay: network game installed; sending ready (speed={s}, epoch={d}, frame={d})", .{
            @tagName(speed),
            data.epoch,
            data.frame,
        });

        try self.netplay.session_manager.send(.init(&.{ .ready = .{
            .epoch = data.epoch,
            .frame = data.frame,
            .player2 = player2,
        } }));
    }

    fn applyAuthoritativeFrame(self: *Self, frame: netplay_protocol.Frame) !void {
        if (self.netplay.active_session_role != .client or !self.netplay.network_rom) return error.InvalidSessionState;
        try netplay_protocol.validateNext(self.netplay.epoch, self.netplay.frame + 1, frame.epoch, frame.frame);

        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);

        self.system.?.applyControllerSnapshot(.{ .player1 = @bitCast(frame.player1), .player2 = @bitCast(frame.player2) });
        self.system.?.run_frame();
        _ = self.emulation_speed_frame_count.fetchAdd(1, .monotonic);
        self.publishFrame(self.system.?.frame_buffer());
        self.netplay.frame = frame.frame;

        var digest_value: ?[32]u8 = null;
        if (frame.digest != null) {
            var snapshot = try self.system.?.saveState(self.alloc);
            defer snapshot.deinit(self.alloc);

            digest_value = try netplay_snapshot.digest(&snapshot);

            std.log.debug("netplay: client computed checkpoint digest (epoch={d}, frame={d}, digest={x})", .{
                self.netplay.epoch,
                self.netplay.frame,
                digest_value.?[0..8],
            });

            if (builtin.mode == .Debug) {
                logCheckpointComponents("client", self.netplay.epoch, self.netplay.frame, &snapshot);
            }
        }

        const local_player2: u8 = @bitCast(self.controllerSnapshot().player2);

        try self.netplay.session_manager.send(.init(&.{ .ack = .{
            .epoch = self.netplay.epoch,
            .frame = self.netplay.frame,
            .player2 = local_player2,
            .digest = digest_value,
        } }));
    }

    fn publishAuthoritativeFrame(self: *Self, controllers: System.ControllerSnapshot) void {
        self.netplay.frame +%= 1;

        var digest_value: ?[32]u8 = null;
        if (self.netplay.frame % 60 == 0) {
            var snapshot = self.system.?.saveState(self.alloc) catch |err| {
                std.log.err("netplay: failed to capture host checkpoint at frame {d}: {s}", .{ self.netplay.frame, @errorName(err) });
                self.netplay.session_manager.disconnect();
                return;
            };
            defer snapshot.deinit(self.alloc);

            digest_value = netplay_snapshot.digest(&snapshot) catch |err| {
                std.log.err("netplay: failed to hash host checkpoint at frame {d}: {s}", .{ self.netplay.frame, @errorName(err) });
                self.netplay.session_manager.disconnect();
                return;
            };

            self.netplay.checkpoint_frame = self.netplay.frame;
            self.netplay.checkpoint_digest = digest_value;

            std.log.debug("netplay: host created checkpoint (epoch={d}, frame={d}, digest={x})", .{
                self.netplay.epoch,
                self.netplay.frame,
                digest_value.?[0..8],
            });

            if (builtin.mode == .Debug) {
                logCheckpointComponents("host", self.netplay.epoch, self.netplay.frame, &snapshot);
            }
        }

        self.netplay.session_manager.send(.init(&.{ .frame = .{
            .epoch = self.netplay.epoch,
            .frame = self.netplay.frame,
            .player1 = @bitCast(controllers.player1),
            .player2 = @bitCast(controllers.player2),
            .digest = digest_value,
        } })) catch |err| {
            std.log.err("netplay: failed to queue authoritative frame (epoch={d}, frame={d}): {s}", .{
                self.netplay.epoch,
                self.netplay.frame,
                @errorName(err),
            });
            self.netplay.session_manager.disconnect();
        };
    }

    fn logCheckpointComponents(side: []const u8, epoch: u32, frame: u64, snapshot: *const System.Snapshot) void {
        const components = netplay_snapshot.componentDigests(snapshot) catch |err| {
            std.log.warn("netplay: failed to compute {s} checkpoint component diagnostics: {s}", .{ side, @errorName(err) });
            return;
        };

        std.log.debug("netplay: {s} checkpoint components (epoch={d}, frame={d}, cpu={x}, bus={x}, ppu={x}, apu={x})", .{
            side,
            epoch,
            frame,
            components.cpu[0..8],
            components.bus[0..8],
            components.ppu[0..8],
            components.apu[0..8],
        });
    }

    fn sendRebase(self: *Self) !void {
        const previous_epoch = self.netplay.epoch;

        std.log.info("netplay: capturing authoritative rebase (previous_epoch={d}, frame={d})", .{
            previous_epoch,
            self.netplay.frame,
        });

        var snapshot = try self.system.?.saveState(self.alloc);
        defer snapshot.deinit(self.alloc);

        self.system.?.apu.resetOutputBuffers();

        const encoded = try netplay_snapshot.encode(self.alloc, &snapshot);
        defer self.alloc.free(encoded);

        self.netplay.epoch +%= 1;
        self.netplay.frame = 0;
        self.netplay.last_ack = 0;
        self.netplay.resyncing = true;
        self.netplay.ready = false;
        self.netplay.lead_paused = false;
        if (self.system) |*system| system.setAudioPaused(true);
        self.netplay.session_manager.markResyncing();

        std.log.info("netplay: sending authoritative rebase (epoch={d}, snapshot={d} bytes)", .{
            self.netplay.epoch,
            encoded.len,
        });

        try self.netplay.session_manager.send(.init(&.{ .rebase = .{
            .epoch = self.netplay.epoch,
            .frame = 0,
            .snapshot = encoded,
        } }));
    }

    fn applyRebase(self: *Self, rebase: netplay_protocol.Rebase) !void {
        if (rebase.epoch <= self.netplay.epoch) return error.UnexpectedEpoch;

        std.log.info("netplay: applying authoritative rebase (old_epoch={d}, new_epoch={d}, frame={d}, snapshot={d} bytes)", .{
            self.netplay.epoch,
            rebase.epoch,
            rebase.frame,
            rebase.snapshot.len,
        });

        self.netplay.session_manager.markResyncing();

        const snapshot = try netplay_snapshot.decode(self.alloc, rebase.snapshot);
        defer {
            snapshot.deinit(self.alloc);
            self.alloc.destroy(snapshot);
        }

        self.emulation_lock.lockUncancelable(self.io);
        defer self.emulation_lock.unlock(self.io);

        try self.system.?.loadState(snapshot);

        self.netplay.epoch = rebase.epoch;
        self.netplay.frame = rebase.frame;

        self.publishFrame(self.system.?.frame_buffer());

        const player2: u8 = @bitCast(self.controllerSnapshot().player2);

        try self.netplay.session_manager.send(.init(&.{ .ready = .{
            .epoch = rebase.epoch,
            .frame = rebase.frame,
            .player2 = player2,
        } }));

        self.netplay.session_manager.markConnected();

        std.log.info("netplay: authoritative rebase applied and acknowledged (epoch={d}, frame={d})", .{
            rebase.epoch,
            rebase.frame,
        });
    }

    fn recoverDesyncLocked(self: *Self) !void {
        const now = std.Io.Timestamp.now(self.io, .real).toSeconds();

        self.netplay.rebase_times[0] = self.netplay.rebase_times[1];
        self.netplay.rebase_times[1] = self.netplay.rebase_times[2];
        self.netplay.rebase_times[2] = now;

        if (self.netplay.rebase_times[0] != 0 and now - self.netplay.rebase_times[0] <= 60) {
            std.log.err("netplay: automatic desync recovery limit exceeded (3 rebases within 60 seconds)", .{});
            return error.RepeatedDesync;
        }

        std.log.warn("netplay: starting automatic desync recovery", .{});
        try self.sendRebase();
    }

    fn handleSessionEnded(self: *Self) void {
        const was_host = self.netplay.active_session_role == .host;
        const was_client = self.netplay.active_session_role == .client;
        const stopped_client_game = was_client and self.netplay.network_rom;

        std.log.info("netplay: cleaning up ended session (role={s}, unload_network_game={any})", .{
            @tagName(self.netplay.active_session_role),
            stopped_client_game,
        });

        if (stopped_client_game) {
            self.unloadCurrentRomInternal();
            if (builtin.abi.isAndroid()) self.ui.setWindowFullscreen(false);
        } else {
            self.emulation_lock.lockUncancelable(self.io);
        }

        if (self.netplay.client_saved_speed) |speed| self.settings.emulation_speed = speed;

        self.netplay.client_saved_speed = null;
        self.netplay.remote_player2.store(0, .release);
        self.netplay.resyncing = false;
        self.netplay.ready = false;
        self.netplay.lead_paused = false;
        self.netplay.active_session_role = .none;

        if (self.system) |*system| system.setAudioPaused(false);

        if (!stopped_client_game) self.emulation_lock.unlock(self.io);

        if (was_host) {
            if (builtin.abi.isAndroid()) {
                if (self.show_android_multiplayer_ui) {
                    self.show_android_multiplayer_ui = false;
                    self.render_home_ui = !self.emulation_running;
                    if (self.emulation_running) self.ui.setWindowFullscreen(true);
                }
            } else if (self.netplay.session_window_handle) |window| {
                self.closeSessionWindow(window);
            }
        }

        std.log.info("netplay: session cleanup completed", .{});
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

    shader_download.downloadAndExtract(app_state.io, root_path, progress) catch |err| {
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
