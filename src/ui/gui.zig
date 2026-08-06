const std = @import("std");
const builtin = @import("builtin");

const game_history = @import("../game_history.zig");
const debug = @import("debug.zig");
const c = @import("../root.zig").c;
const ui_core = @import("core/ui.zig");
const UI = ui_core.UI;
const Key = ui_core.Key;
const clay = @import("core/clay.zig");
const viewport = @import("core/viewport.zig");
const theme = @import("common.zig").theme;
const widgets = @import("core/widgets.zig");
const Color = @import("core/color.zig").Color;
const android = @import("../utils/android.zig");
const pipeline = @import("../shaders/pipeline.zig");
const shader_download = @import("../shader_download.zig");
const bindings = @import("bindings.zig");
const settings = @import("settings.zig");
const utils = @import("../utils/format.zig");
const save_state = @import("../save_state.zig");
const state = @import("state.zig");
const ness = @import("../root.zig");
const sdlError = ness.sdlError;
const NES_WIDTH = ness.NES_WIDTH;
const NES_HEIGHT = ness.NES_HEIGHT;
const OVERSCAN_TOP = ness.OVERSCAN_TOP;
const OVERSCAN_BOTTOM = ness.OVERSCAN_BOTTOM;
const NES_VISIBLE_HEIGHT = ness.NES_VISIBLE_HEIGHT;
const OVERSCAN_PIXEL_OFFSET = OVERSCAN_TOP * NES_WIDTH * 4;
const NES_VISIBLE_PIXEL_BYTES = NES_WIDTH * NES_VISIBLE_HEIGHT * 4;

const Player = bindings.Player;
const ControllerAction = bindings.ControllerAction;
const ControllerBindingTarget = bindings.ControllerBindingTarget;
const GeneralAction = bindings.GeneralAction;
const GamepadButton = bindings.GamepadButton;
const InputDevice = bindings.InputDevice;
const ParamTarget = settings.ParamTarget;
const SettingsCategory = state.SettingsCategory;
pub const AppState = state.AppState;
const EmulationSpeed = settings.EmulationSpeed;
const BorderShaderOpts = settings.BorderShaderOpts;

const dialog_filter_list: [2]c.SDL_DialogFileFilter = [_]c.SDL_DialogFileFilter{
    .{ .name = "NES ROMs", .pattern = "nes" },
    .{ .name = "All files", .pattern = "*" },
};

pub fn drawGUI(ui: *UI, app_state: *AppState) void {
    const root = ui.column(.{
        .bg_color = theme.bg_base,
    });
    {
        const safe_area_padding = ui.main_window.safeAreaPadding();

        if (builtin.abi.isAndroid()) {
            if (!ui.isWindowFullscreen()) drawAndroidHeader(ui, app_state);
        } else {
            if (!ui.isWindowFullscreen()) drawDesktopMenu(ui, app_state);
        }

        if (app_state.show_fps) {
            const f = ui.float(.{
                .attach_to = .to_element_with_id,
                .z_index = 15,
                .parentId = root.id.id,
                .attach_points = .{ .parent = .left_top, .element = .left_top },
                .offset = .{
                    .x = 5 + @as(f32, @floatFromInt(safe_area_padding.left)),
                    .y = 30 + @as(f32, @floatFromInt(safe_area_padding.bottom)),
                },
            });
            _ = ui.label(.{
                .text = std.fmt.allocPrint(
                    ui.main_window.ctx.frameAlloc(),
                    "FPS: {}\nSpeed: {}%",
                    .{ ui.fps_manager.getFPS(), app_state.currentEmulationSpeedPercent() },
                ) catch @panic("OOM"),
                .font_size = 20,
                .color = .red,
            });
            f.end();
        }

        if (builtin.abi.isAndroid() and app_state.show_android_settings_ui) {
            drawAndroidSettingsUI(ui, app_state, safe_area_padding);
        } else if (builtin.abi.isAndroid() and app_state.android_edit_mode) {
            drawAndroidEditMode(ui, app_state, safe_area_padding);
        } else if (app_state.render_home_ui) {
            drawHomeUI(ui, app_state, safe_area_padding);
        } else if (app_state.render_debug_ui and !builtin.abi.isAndroid()) {
            debug.drawUI(ui, app_state);
        } else {
            const orientation: android.ScreenOrientation = if (builtin.abi.isAndroid())
                android.currentScreenOrientation().?
            else
                .unknown;
            const is_portrait = orientation == .portrait or orientation == .portrait_flipped;

            const border_shader = ui.shaderMode(.{
                .id = "border",
                .target = .canvas,
                .composition = .original_on_top,
            });
            const main_shader = ui.shaderMode(.{ .id = "main" });
            const padding: clay.Padding = if (builtin.abi.isAndroid()) blk: {
                break :blk switch (orientation) {
                    .portrait => .{ .top = safe_area_padding.top },
                    .portrait_flipped => .{ .bottom = safe_area_padding.bottom },
                    .landscape => .{ .left = safe_area_padding.left },
                    .landscape_flipped => .{ .right = safe_area_padding.right },
                    else => unreachable,
                };
            } else .{};
            const canvas = ui.canvas(.{
                .pixel_format = c.SDL_PIXELFORMAT_ABGR8888,
                .pixels = app_state.framePixels(OVERSCAN_PIXEL_OFFSET, NES_VISIBLE_PIXEL_BYTES),
                .w = NES_WIDTH,
                .h = NES_VISIBLE_HEIGHT,
                .padding = padding,
                .aspect_ratio = if (is_portrait) .@"4_3" else app_state.settings.aspect_ratio,
                .viewport_alignment = if (is_portrait) .top else .center,
                .bg_color = Color.black,
            });
            main_shader.end();
            border_shader.end();

            if (builtin.abi.isAndroid() and !(app_state.settings.hide_android_onscreen_controller and ui.getGamepadCount() > 0)) {
                drawOnScreenGamepad(ui, app_state, canvas.id, orientation);
            }

            if (app_state.paused) {
                const f = ui.float(.{
                    .attach_to = .to_element_with_id,
                    .attach_points = .{ .parent = .center_center, .element = .center_center },
                    .z_index = 5,
                    .sizing = .grow,
                    .parentId = canvas.id.id,
                });
                {
                    const col = ui.column(.{
                        .bg_color = Color.black.withAlpha(0.8),
                        .child_alignment = .center,
                    });
                    _ = ui.label(.{ .text = "Paused", .font_size = 42, .color = .white });
                    col.end();
                }
                f.end();
            }
        }

        if (builtin.abi.isAndroid() and app_state.show_android_multiplayer_ui) {
            drawAndroidSessionUI(ui, app_state, root.id, safe_area_padding);
        }

        if (ui.hasTimerExpired("save_state_toast").is_some_and(check_timer_result)) toast(ui, .{ .text = "State saved" });
        if (ui.hasTimerExpired("load_state_toast").is_some_and(check_timer_result)) toast(ui, .{ .text = "State loaded" });
        if (ui.hasTimerExpired("gamepad_connected_toast").is_some_and(check_timer_result)) {
            toast(ui, .{ .text = "Gamepad connected", .icon = .controller });
        }
        if (ui.hasTimerExpired("gamepad_disconnected_toast").is_some_and(check_timer_result)) {
            toast(ui, .{ .text = "Gamepad disconnected", .icon = .controller });
        }
        if (ui.hasTimerExpired("connected_to_host_toast").is_some_and(check_timer_result)) {
            toast(ui, .{ .text = "Connected to host" });
        }
        if (ui.hasTimerExpired("client_disconnected_toast").is_some_and(check_timer_result)) {
            toast(ui, .{ .text = "Client disconnected" });
        }

        if (builtin.abi.isAndroid()) {
            if (app_state.show_android_sidepanel) drawAndroidSidepanel(ui, app_state, root.id);
        }
    }
    root.end();
}

fn check_timer_result(result: bool) bool {
    return !result;
}

fn toastTransition(state_: clay.TransitionData, _: clay.TransitionProperty) callconv(.c) clay.TransitionData {
    var s = state_;
    s.bounding_box.y += s.bounding_box.height;
    return s;
}

fn toast(ui: *UI, params: struct {
    text: []const u8,
    icon: ?UI.Icon = null,
    style: struct {
        font_size: u16 = 25,
        bg_color: Color = Color.black.withAlpha(0.8),
    } = .{},
    attach_point: clay.FloatingAttachPointType = if (builtin.abi.isAndroid()) .center_bottom else .left_bottom,
}) void {
    const f = ui.float(.{
        .attach_to = .to_root,
        .attach_points = .{ .parent = params.attach_point, .element = params.attach_point },
        .z_index = std.math.maxInt(i16),
        .sizing = .fit,
        .offset = .{ .x = 15, .y = -15 },
        .transition = .{
            .handler = clay.easeOut,
            .duration = 0.25,
            .properties = clay.TransitionProperty.position,
            .enter = .{
                .set_initial_state = toastTransition,
                .trigger = .trigger_on_first_parent_frame,
            },
            .exit = .{
                .set_final_state = toastTransition,
                .sibling_ordering = .natural_order,
            },
        },
    });
    {
        const row = ui.row(.{
            .bg_color = params.style.bg_color,
            .child_alignment = .center,
            .gap = 10,
        });
        if (params.icon) |icon| {
            _ = ui.icon(.{ .icon = ui.icons.get(icon) });
        }
        _ = ui.label(.{ .text = params.text, .font_size = params.style.font_size, .color = .white });
        row.end();
    }
    f.end();
}

fn drawDesktopMenu(ui: *UI, app_state: *AppState) void {
    const menubar = ui.menuBar(.{
        .bg_color = theme.bg_panel,
        .border_color = theme.border_dim,
    });
    {
        const sys_menu = ui.dropdownMenu(.{
            .label = "System",
            .bg_color = theme.bg_panel,
            .hover_color = theme.bg_hover,
            .text_color = theme.text_secondary,
            .list_bg_color = theme.bg_section,
            .list_border_color = theme.border,
        });
        if (ui.menuItem(.{
            .label = "Open",
            .enabled = !app_state.isConnectedClient(),
            .bg_color = theme.bg_section,
            .hover_color = theme.accent_blue,
            .text_color = theme.text_secondary,
        }).clicked(ui.main_window.ctx)) {
            openRomDialog(ui, app_state);
        }
        if (ui.menuItem(.{
            .label = "Exit",
            .bg_color = theme.bg_section,
            .hover_color = theme.accent_blue,
            .text_color = theme.text_secondary,
            .shortcut = app_state.generalBinding(.quit).keyName(),
        }).clicked(ui.main_window.ctx)) {
            ui.quit = true;
        }
        sys_menu.end();

        const emulation_menu = ui.dropdownMenu(.{
            .label = "Emulation",
            .bg_color = theme.bg_panel,
            .hover_color = theme.bg_hover,
            .text_color = theme.text_secondary,
            .list_bg_color = theme.bg_section,
            .list_border_color = theme.border,
        });
        if (ui.menuItem(.{
            .label = if (app_state.paused) "Continue" else "Pause",
            .enabled = app_state.emulation_running and !app_state.isConnectedClient(),
            .bg_color = theme.bg_section,
            .hover_color = theme.accent_blue,
            .text_color = theme.text_secondary,
            .shortcut = app_state.generalBinding(.toggle_pause).keyName(),
        }).clicked(ui.main_window.ctx)) {
            app_state.togglePause();
        }
        if (ui.menuItem(.{
            .label = "Stop",
            .enabled = app_state.emulation_running and !app_state.isConnectedClient(),
            .bg_color = theme.bg_section,
            .hover_color = theme.accent_blue,
            .text_color = theme.text_secondary,
            .shortcut = app_state.generalBinding(.stop).keyName(),
        }).clicked(ui.main_window.ctx)) {
            app_state.unloadCurrentRom();
        }
        if (ui.menuItem(.{
            .label = "Restart",
            .enabled = app_state.emulation_running and !app_state.isConnectedClient(),
            .bg_color = theme.bg_section,
            .hover_color = theme.accent_blue,
            .text_color = theme.text_secondary,
            .shortcut = app_state.generalBinding(.restart).keyName(),
        }).clicked(ui.main_window.ctx)) {
            app_state.resetSystem();
        }

        _ = ui.separator(.{ .color = theme.border, .thickness = 2 });
        const save_state_item = ui.menuItem(.{
            .label = "Save State",
            .enabled = app_state.emulation_running and !app_state.isConnectedClient(),
            .bg_color = theme.bg_section,
            .hover_color = theme.accent_blue,
            .text_color = theme.text_secondary,
            .has_submenu = true,
        });
        {
            const save_state_submenu = save_state_item.submenu(.{
                .width = 270,
                .bg_color = theme.bg_section,
                .border_color = theme.border,
            });
            {
                const col = ui.column(.{ .gap = 5 });
                {
                    if (ui.menuItem(.{
                        .label = "Quick Save",
                        .enabled = app_state.emulation_running and !app_state.isConnectedClient(),
                        .bg_color = theme.bg_section,
                        .hover_color = theme.accent_blue,
                        .text_color = theme.text_secondary,
                    }).clicked(ui.main_window.ctx)) {
                        app_state.saveStateSlot(0);
                    }
                    _ = ui.separator(.{ .color = theme.border, .thickness = 2 });
                    drawStateSlotItems(ui, app_state, .save);
                }
                col.end();
            }
            save_state_submenu.end();
        }
        save_state_item.end();

        const load_state_item = ui.menuItem(.{
            .label = "Load State",
            .enabled = app_state.emulation_running and !app_state.isConnectedClient(),
            .bg_color = theme.bg_section,
            .hover_color = theme.accent_blue,
            .text_color = theme.text_secondary,
            .has_submenu = true,
        });
        {
            const load_state_submenu = load_state_item.submenu(.{
                .width = 270,
                .bg_color = theme.bg_section,
                .border_color = theme.border,
            });
            {
                const col = ui.column(.{ .gap = 5 });
                {
                    if (ui.menuItem(.{
                        .label = "Quick Load",
                        .enabled = app_state.emulation_running and !app_state.isConnectedClient(),
                        .bg_color = theme.bg_section,
                        .hover_color = theme.accent_blue,
                        .text_color = theme.text_secondary,
                    }).clicked(ui.main_window.ctx)) {
                        app_state.loadStateSlot(0);
                    }
                    _ = ui.separator(.{ .color = theme.border, .thickness = 2 });
                    drawStateSlotItems(ui, app_state, .load);
                }
                col.end();
            }
            load_state_submenu.end();
        }
        load_state_item.end();

        _ = ui.separator(.{ .color = theme.border, .thickness = 2 });
        if (ui.menuItem(.{
            .label = "Debug",
            .enabled = app_state.emulation_running and !app_state.sessionActive(),
            .bg_color = theme.bg_section,
            .hover_color = theme.accent_blue,
            .text_color = theme.text_secondary,
            .shortcut = app_state.generalBinding(.toggle_step_mode).keyName(),
        }).clicked(ui.main_window.ctx)) {
            app_state.toggleDebug();
        }
        if (ui.menuItem(.{
            .label = if (app_state.show_fps) "Hide FPS" else "Show FPS",
            .bg_color = theme.bg_section,
            .hover_color = theme.accent_blue,
            .text_color = theme.text_secondary,
        }).clicked(ui.main_window.ctx)) {
            app_state.show_fps = !app_state.show_fps;
        }
        _ = ui.separator(.{ .color = theme.border, .thickness = 2 });

        if (ui.menuItem(.{
            .label = "Settings",
            .bg_color = theme.bg_section,
            .hover_color = theme.accent_blue,
            .text_color = theme.text_secondary,
        }).clicked(ui.main_window.ctx)) {
            _ = ui.createWindow(
                "Settings",
                680,
                640,
                .{ .draw_fn = drawSettingsWindowUI, .user_data = @ptrCast(app_state) },
            );
        }
        emulation_menu.end();

        const multiplayer_menu = ui.dropdownMenu(.{
            .label = "Multiplayer",
            .bg_color = theme.bg_panel,
            .hover_color = theme.bg_hover,
            .text_color = theme.text_secondary,
            .list_bg_color = theme.bg_section,
            .list_border_color = theme.border,
        });
        if (app_state.sessionActive()) {
            if (ui.menuItem(.{
                .label = "Session Details…",
                .bg_color = theme.bg_section,
                .hover_color = theme.accent_blue,
                .text_color = theme.text_secondary,
            }).clicked(ui.main_window.ctx)) openSessionWindow(ui, app_state, "Session Details");
        } else {
            if (ui.menuItem(.{
                .label = "Connect to Session…",
                .bg_color = theme.bg_section,
                .hover_color = theme.accent_blue,
                .text_color = theme.text_secondary,
            }).clicked(ui.main_window.ctx)) openSessionWindow(ui, app_state, "Connect to Session");
            if (ui.menuItem(.{
                .label = "Host Session…",
                .enabled = app_state.emulation_running,
                .bg_color = theme.bg_section,
                .hover_color = theme.accent_blue,
                .text_color = theme.text_secondary,
            }).clicked(ui.main_window.ctx)) {
                app_state.startHostSession() catch |err| std.log.err("failed to host session: {s}", .{@errorName(err)});
                openSessionWindow(ui, app_state, "Host Session");
            }
        }
        multiplayer_menu.end();
    }
    menubar.end();
}

fn openSessionWindow(ui: *UI, app_state: *AppState, title: []const u8) void {
    const session_state = app_state.sessionState();
    const height: i32 = if (session_state == .connected or session_state == .resyncing) 400 else 220;
    app_state.netplay.session_window_handle = ui.createWindow(
        title,
        600,
        height,
        .{
            .user_data = @ptrCast(app_state),
            .draw_fn = drawSessionWindowUI,
            .on_close = onSessionWindowClosed,
        },
    );
}

fn onSessionWindowClosed(_: *UI, user_data: ?*anyopaque) void {
    const app_state: *AppState = @ptrCast(@alignCast(user_data.?));
    app_state.handleSessionWindowClosed();
}

fn drawSessionWindowUI(ui: *UI, user_data: ?*anyopaque) void {
    const app_state: *AppState = @ptrCast(@alignCast(user_data.?));
    drawSessionUI(ui, app_state, false);
}

fn drawAndroidSessionUI(ui: *UI, app_state: *AppState, root_id: clay.ElementId, safe_area_padding: clay.Padding) void {
    const scrim_padding = clay.Padding{
        .left = safe_area_padding.left + 14,
        .right = safe_area_padding.right + 14,
        .top = safe_area_padding.top + 18,
        .bottom = safe_area_padding.bottom + 18,
    };
    const available_w = @max(
        1.0,
        ui.main_window.logical_width - @as(f32, @floatFromInt(scrim_padding.left + scrim_padding.right)),
    );
    const available_h = @max(
        1.0,
        ui.main_window.logical_height - @as(f32, @floatFromInt(scrim_padding.top + scrim_padding.bottom)),
    );
    const max_panel_w = @min(560.0, available_w);
    const max_panel_h = @min(620.0, available_h);
    const max_body_h = @max(1.0, max_panel_h - 56.0);

    const overlay = ui.float(.{
        .attach_to = .to_element_with_id,
        .parentId = root_id.id,
        .attach_points = .{ .parent = .left_top, .element = .left_top },
        .z_index = 90,
        .sizing = .grow,
    });
    {
        const scrim = ui.column(.{
            .sizing = .grow,
            .bg_color = Color.black.withAlpha(0.72),
            .padding = scrim_padding,
            .child_alignment = .center,
        });
        {
            const panel = ui.column(.{
                .sizing = .{
                    .w = .growMinMax(.{ .max = max_panel_w }),
                    .h = .fitMinMax(.{ .max = max_panel_h }),
                },
                .bg_color = theme.bg_section,
                .border = .{ .width = .outside(1), .color = theme.border.toClay() },
                .corner_radius = 4,
                .padding = .all(12),
                .gap = 10,
                .child_alignment = .{ .x = .left, .y = .top },
            });
            {
                const header = ui.row(.{
                    .sizing = .{ .w = .grow, .h = .fit },
                    .gap = 8,
                    .child_alignment = .{ .y = .center },
                });
                {
                    _ = ui.label(.{
                        .text = androidSessionTitle(app_state),
                        .font_size = 18,
                        .color = theme.text_primary,
                    });
                    _ = ui.spacer(.{ .sizing = .grow });
                    if (ui.button(.{
                        .text = "Close",
                        .font_size = 15,
                        .text_color = theme.text_primary,
                        .bg_color = theme.bg_hover,
                        .hover_color = theme.border,
                        .padding = .{ .left = 10, .right = 10, .top = 6, .bottom = 6 },
                        .corner_radius = 3,
                    }).clicked(ui.main_window.ctx)) {
                        app_state.closeAndroidSessionUI();
                    }
                }
                header.end();

                const scroll = ui.scrollArea(.{
                    .id = "android_multiplayer_scroll",
                    .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .max = max_body_h }) },
                    .padding = .{ .right = 4 },
                });
                {
                    drawSessionUI(ui, app_state, true);
                }
                scroll.end();
            }
            panel.end();
        }
        scrim.end();
    }
    overlay.end();
}

fn androidSessionTitle(app_state: *AppState) []const u8 {
    return switch (app_state.sessionRole()) {
        .host => switch (app_state.sessionState()) {
            .connected, .resyncing, .disconnecting => "Session Details",
            else => "Host Session",
        },
        .client => switch (app_state.sessionState()) {
            .preview => "Join Session",
            .connected, .resyncing, .disconnecting => "Session Details",
            else => "Connect to Session",
        },
        .none => "Connect to Session",
    };
}

fn drawSessionUI(ui: *UI, app_state: *AppState, is_android: bool) void {
    const session_state = app_state.sessionState();
    const root = ui.column(.{
        .sizing = if (is_android) .{ .w = .grow, .h = .fit } else .grow,
        .bg_color = if (is_android) null else theme.bg_base,
        .padding = .all(if (is_android) 0 else 24),
        .gap = 14,
        .child_alignment = .{ .x = .left, .y = .center },
    });
    {
        _ = ui.label(.{ .text = sessionStateLabel(session_state), .font_size = 15, .color = theme.text_secondary });
        if (app_state.netplay.session_error) |message| {
            _ = ui.label(.{ .text = message, .font_size = 14, .color = theme.accent_red });
        }

        if (app_state.sessionRole() == .host) {
            drawHostSessionWindow(ui, app_state);
            drawSessionConnectionDetails(ui, app_state);
            if (session_state != .disconnecting) {
                const label = if (session_state == .waiting or session_state == .creating) "Cancel Hosting" else "Disconnect";
                drawSessionLeaveAction(ui, app_state, label);
            }
        } else if (session_state == .preview and app_state.netplay.session_preview_name != null and
            app_state.netplay.session_preview_frame != null)
        {
            drawClientPreview(ui, app_state, is_android);
        } else if (app_state.sessionRole() == .client and session_state != .idle and session_state != .failed) {
            drawSessionConnectionDetails(ui, app_state);
            if (session_state != .disconnecting) {
                const label = if (session_state == .connected or session_state == .resyncing) "Disconnect" else "Cancel Connection";
                drawSessionLeaveAction(ui, app_state, label);
            }
        } else {
            const code_scroll = ui.scrollArea(.{
                .id = "multiplayer_session_code_scroll",
                .sizing = .{ .w = .grow, .h = .fit },
                .vertical = false,
                .horizontal = true,
            });
            const session_code_field = ui.textField(.{
                .id = "multiplayer_session_code",
                .placeholder = "neskwik:…",
                .padding_val = .all(12),
            });
            code_scroll.end();

            if (ui.button(.{
                .text = "Connect",
                .bg_color = if (session_code_field.value().len != 0) theme.accent_blue else theme.bg_hover,
                .hover_color = theme.accent_blue.lighten(0.12),
                .text_color = Color.white,
                .enabled = session_code_field.value().len != 0,
            }).clicked(ui.current_window.ctx)) {
                if (app_state.connectSession(session_code_field.value())) {
                    if (!builtin.abi.isAndroid()) app_state.netplay.session_window_handle.?.setWindowSize(600, 550);
                } else |err| {
                    app_state.setSessionError(std.fmt.allocPrint(
                        ui.current_window.ctx.frameAlloc(),
                        "Failed to connect: {s}",
                        .{@errorName(err)},
                    ) catch @panic("OOM"));
                    std.log.err("failed to connect: {s}", .{@errorName(err)});
                }
            }
        }
    }
    root.end();
}

fn drawHostSessionWindow(ui: *UI, app_state: *AppState) void {
    if (app_state.netplay.session_preview_name) |name| {
        _ = ui.label(.{ .text = name, .font_size = 15, .color = theme.text_value });
    }

    if (app_state.netplay.session_code) |code| {
        const code_row = ui.row(.{ .sizing = .{ .w = .grow, .h = .fit }, .gap = 8 });
        {
            const code_scroll = ui.scrollArea(.{
                .id = "multiplayer_host_code_scroll",
                .sizing = .{ .w = .grow, .h = .fit },
                .vertical = false,
                .horizontal = true,
            });
            {
                _ = ui.textField(.{
                    .id = "multiplayer_host_code",
                    .value = code,
                    .read_only = true,
                    .width = .fit,
                });
            }
            code_scroll.end();
            if (ui.iconButton(.{
                .icon = ui.icons.get(.copy),
                .size = 24,
                .padding = .all(10),
                .bg_color = theme.bg_hover,
                .hover_color = theme.accent_blue,
                .overlay_color = theme.text_primary,
            }).clicked(ui.current_window.ctx)) {
                ui.setClipboardText(code) catch {};
                ui.setTimer("session_code_copied", 1400);
            }
        }
        code_row.end();

        if (!builtin.abi.isAndroid() and !ui.hasTimerExpired("session_code_copied").unwrap_or(true)) {
            toast(
                ui,
                .{
                    .text = "Code copied!",
                    .style = .{ .font_size = 18, .bg_color = .black },
                    .attach_point = .center_bottom,
                },
            );
        }
    }
}

fn drawSessionConnectionDetails(ui: *UI, app_state: *AppState) void {
    const frame_alloc = ui.current_window.ctx.frameAlloc();

    if (app_state.netplay.session_peer) |peer| {
        const peer_text = std.fmt.allocPrint(frame_alloc, "Peer: {x}", .{peer[0..8]}) catch "Peer connected";
        _ = ui.label(.{ .text = peer_text, .font_size = 13, .color = theme.text_secondary });
    }

    const stats = app_state.netplay.connection_stats orelse return;
    const ping = if (stats.has_selected_path)
        std.fmt.allocPrint(frame_alloc, "Ping: {d} ms", .{stats.rtt_ms}) catch "Ping: unavailable"
    else
        "Ping: unavailable";
    const route = switch (stats.route) {
        .direct => "Route: Direct",
        .relay => "Route: Relay",
        .custom => "Route: Custom",
        .unknown => "Route: Unknown",
    };
    const traffic = std.fmt.allocPrint(
        frame_alloc,
        "Traffic: {s} sent / {s} received",
        .{ utils.formatByteCount(frame_alloc, stats.udp_tx_bytes), utils.formatByteCount(frame_alloc, stats.udp_rx_bytes) },
    ) catch "Traffic: unavailable";
    const packet_loss = std.fmt.allocPrint(
        frame_alloc,
        "Packet loss: {d} packets ({s})",
        .{ stats.lost_packets, utils.formatByteCount(frame_alloc, stats.lost_bytes) },
    ) catch "Packet loss: unavailable";

    _ = ui.label(.{ .text = ping, .font_size = 13, .color = theme.text_secondary });
    _ = ui.label(.{ .text = route, .font_size = 13, .color = theme.text_secondary });
    _ = ui.label(.{ .text = traffic, .font_size = 13, .color = theme.text_secondary });
    _ = ui.label(.{ .text = packet_loss, .font_size = 13, .color = theme.text_secondary });
}

fn drawClientPreview(ui: *UI, app_state: *AppState, is_android: bool) void {
    _ = ui.label(.{ .text = app_state.netplay.session_preview_name.?, .font_size = 18, .color = theme.text_primary });
    const size_text = std.fmt.allocPrint(
        ui.current_window.ctx.frameAlloc(),
        "{d:.2} MiB",
        .{@as(f64, @floatFromInt(app_state.netplay.session_preview_size)) / (1024.0 * 1024.0)},
    ) catch "";

    _ = ui.label(.{ .text = size_text, .font_size = 13, .color = theme.text_secondary });
    _ = ui.canvas(.{
        .id = "multiplayer_preview",
        .pixel_format = c.SDL_PIXELFORMAT_ABGR8888,
        .pixels = app_state.netplay.session_preview_frame.?,
        .w = NES_WIDTH,
        .h = NES_HEIGHT,
        .sizing = .{
            .w = .grow,
            .h = .fixed(if (is_android)
                @min(360, @max(180, ui.main_window.logical_height * 0.45))
            else
                360),
        },
        .aspect_ratio = .@"4_3",
        .bg_color = Color.black,
    });

    const actions = ui.row(.{ .sizing = .{ .w = .grow, .h = .fit }, .gap = 10 });
    {
        if (ui.button(.{
            .text = "Join",
            .bg_color = theme.accent_green,
            .hover_color = theme.accent_green.lighten(0.1),
            .text_color = Color.white,
        }).clicked(ui.current_window.ctx)) app_state.joinSession() catch {};
        if (ui.button(.{
            .text = "Cancel",
            .bg_color = theme.bg_hover,
            .hover_color = theme.border,
            .text_color = theme.text_primary,
        }).clicked(ui.current_window.ctx)) app_state.leaveSession();
    }
    actions.end();
}

fn drawSessionLeaveAction(ui: *UI, app_state: *AppState, label: []const u8) void {
    if (ui.button(.{
        .text = label,
        .bg_color = theme.accent_red,
        .hover_color = theme.accent_red.lighten(0.1),
        .text_color = Color.white,
    }).clicked(ui.current_window.ctx)) app_state.leaveSession();
}

fn sessionStateLabel(value: ness.netplay_session.State) []const u8 {
    return switch (value) {
        .idle => "Enter the session code to join a session.",
        .creating => "Creating a secure endpoint…",
        .waiting => "Waiting for a guest…",
        .connecting => "Connecting…",
        .preview => "Review the host game before joining.",
        .joining => "Transferring and loading game state…",
        .connected => "Connected",
        .resyncing => "Resynchronizing…",
        .disconnecting => "Disconnecting…",
        .failed => "Connection failed",
    };
}

fn drawAndroidHeader(ui: *UI, app_state: *AppState) void {
    const safe_padding = ui.main_window.safeAreaPadding();

    const header = ui.column(.{
        .bg_color = theme.bg_panel,
        .border = .{ .width = .outside(1), .color = theme.border_dim.toClay() },
        .sizing = .{ .h = .fit, .w = .grow },
    });
    {
        _ = ui.spacer(.{ .sizing = .{ .h = .fixed(@floatFromInt(safe_padding.top)) } });
        const inner = ui.row(.{
            .bg_color = theme.bg_panel,
            .padding = .{ .left = 8, .right = 12 },
            .gap = 8,
            .child_alignment = .{ .x = .left, .y = .center },
        });
        {
            if (ui.iconButton(.{
                .icon = ui.icons.get(.menu),
                .size = 28,
                .padding = .all(10),
                .bg_color = Color.transparent,
                .hover_color = theme.bg_hover,
                .overlay_color = theme.text_primary,
                .corner_radius = 8,
            }).clicked(ui.main_window.ctx)) {
                app_state.show_android_sidepanel = true;
                // Set a timer of 250ms to avoid closing the sidepanel as soon as it's opened
                ui.setTimer("android_sidepanel", 250);
            }

            _ = ui.label(.{
                .text = "NESkwik",
                .font_size = 18,
                .line_height = 20,
                .color = theme.text_primary,
            });

            _ = ui.spacer(.{ .sizing = .{ .w = .grow } });

            if (!app_state.show_android_settings_ui and
                !app_state.show_android_multiplayer_ui and
                !app_state.isConnectedClient())
            {
                if (ui.button(.{
                    .text = "Open ROM",
                    .font_size = 14,
                    .text_color = Color.white,
                    .bg_color = theme.accent_blue,
                    .hover_color = theme.accent_blue.lighten(0.12),
                    .padding = .{ .left = 14, .right = 14, .top = 9, .bottom = 9 },
                    .corner_radius = 6,
                }).clicked(ui.main_window.ctx)) {
                    openRomDialog(ui, app_state);
                }
            }
        }
        inner.end();
    }
    header.end();
}

fn androidSidePanelTransitionState(state_: clay.TransitionData, _: clay.TransitionProperty) callconv(.c) clay.TransitionData {
    var s = state_;
    s.bounding_box.x -= s.bounding_box.width;
    return s;
}

fn drawAndroidSidepanel(ui: *UI, app_state: *AppState, root_id: clay.ElementId) void {
    const safe_padding = ui.main_window.safeAreaPadding();
    const dims = clay.getLayoutDimensions();
    const sidepanel_w = @min(340.0, dims.w * 0.60);
    const is_open = app_state.show_android_sidepanel;
    var pointer_over_state_dialog = false;
    const sidepanel_transition = clay.TransitionElementConfig{
        .handler = clay.easeOut,
        .duration = 0.18,
        .properties = clay.TransitionProperty.position,
        .enter = .{
            .set_initial_state = androidSidePanelTransitionState,
            .trigger = .trigger_on_first_parent_frame,
        },
        .exit = .{
            .set_final_state = androidSidePanelTransitionState,
            .sibling_ordering = .natural_order,
        },
    };

    const sidepanel = ui.float(.{
        .id = "android_sidepanel",
        .attach_to = .to_element_with_id,
        .parentId = root_id.id,
        .attach_points = .{ .parent = .left_top, .element = .left_top },
        .z_index = 60,
        .sizing = .{ .w = .fixed(sidepanel_w), .h = .grow },
        .transition = sidepanel_transition,
        .no_render = !is_open,
    });
    {
        const col = ui.column(.{
            .sizing = .{ .w = .fixed(sidepanel_w), .h = .grow },
            .bg_color = theme.bg_panel,
            .border = .{ .width = .outside(1), .color = theme.border.toClay() },
            .child_alignment = .{ .x = .left, .y = .top },
            .padding = .{
                .top = safe_padding.top,
                .left = safe_padding.left,
                .right = safe_padding.right,
            },
        });
        {
            const scroll = ui.scrollArea(.{
                .sizing = .grow,
                .vertical = true,
                .padding = .{ .top = 8, .bottom = @intCast(@as(u16, safe_padding.bottom) + 8) },
            });
            {
                drawAndroidDrawerSectionLabel(ui, "Home");
                if (drawAndroidDrawerAction(ui, "Home", !app_state.isConnectedClient()).clicked(ui.main_window.ctx)) {
                    app_state.show_android_settings_ui = false;
                    app_state.show_android_multiplayer_ui = false;
                    app_state.render_home_ui = true;
                    app_state.show_android_sidepanel = false;

                    if (app_state.emulation_running) {
                        ui.setWindowFullscreen(false);
                        app_state.unloadCurrentRom();
                    }
                }
                if (drawAndroidDrawerAction(ui, "Open ROM", !app_state.isConnectedClient()).clicked(ui.main_window.ctx)) {
                    app_state.show_android_sidepanel = false;
                    openRomDialog(ui, app_state);
                }

                drawAndroidDrawerSectionLabel(ui, "Emulation");
                if (drawAndroidDrawerAction(
                    ui,
                    if (app_state.paused) "Resume" else "Pause",
                    app_state.emulation_running and !app_state.isConnectedClient(),
                ).clicked(ui.main_window.ctx)) {
                    app_state.togglePause();
                    app_state.show_android_sidepanel = false;
                }
                if (drawAndroidDrawerAction(ui, "Restart", app_state.emulation_running and !app_state.isConnectedClient()).clicked(ui.main_window.ctx)) {
                    app_state.resetSystem();
                    app_state.show_android_sidepanel = false;
                }
                if (drawAndroidDrawerAction(ui, "Stop", app_state.emulation_running and !app_state.isConnectedClient()).clicked(ui.main_window.ctx)) {
                    app_state.unloadCurrentRom();
                    ui.setWindowFullscreen(false);
                }

                drawAndroidDrawerSectionLabel(ui, "State");
                const save_state_btn = drawAndroidDrawerAction(ui, "Save State", app_state.emulation_running and !app_state.isConnectedClient());
                if (save_state_btn.clicked(ui.main_window.ctx)) {
                    ui.setTimer("android_state_dialog", 250);
                    app_state.show_android_save_state_dialog = true;
                }

                const load_state_btn = drawAndroidDrawerAction(ui, "Load State", app_state.emulation_running and !app_state.isConnectedClient());
                if (load_state_btn.clicked(ui.main_window.ctx)) {
                    // Set a timer of 250ms to avoid closing the sidepanel as soon as it's opened
                    ui.setTimer("android_state_dialog", 250);
                    app_state.show_android_load_state_dialog = true;
                }

                if (app_state.show_android_save_state_dialog) {
                    pointer_over_state_dialog = drawAndroidStateDialog(ui, app_state, save_state_btn.id, .save);
                } else if (app_state.show_android_load_state_dialog) {
                    pointer_over_state_dialog = drawAndroidStateDialog(ui, app_state, load_state_btn.id, .load);
                }

                drawAndroidDrawerSectionLabel(ui, "Multiplayer");
                if (app_state.sessionActive()) {
                    if (drawAndroidDrawerAction(ui, "Session Details", true).clicked(ui.main_window.ctx)) {
                        openAndroidSessionUI(ui, app_state);
                    }
                } else {
                    if (drawAndroidDrawerAction(ui, "Connect to Session", true).clicked(ui.main_window.ctx)) {
                        openAndroidSessionUI(ui, app_state);
                    }
                    if (drawAndroidDrawerAction(ui, "Host Session", app_state.emulation_running).clicked(ui.main_window.ctx)) {
                        app_state.startHostSession() catch |err| std.log.err("failed to host session: {s}", .{@errorName(err)});
                        openAndroidSessionUI(ui, app_state);
                    }
                }

                drawAndroidDrawerSectionLabel(ui, "Tools");
                if (drawAndroidDrawerAction(
                    ui,
                    if (app_state.show_fps) "Hide FPS" else "Show FPS",
                    true,
                ).clicked(ui.main_window.ctx)) {
                    app_state.show_fps = !app_state.show_fps;
                    app_state.show_android_sidepanel = false;
                }

                if (drawAndroidDrawerAction(
                    ui,
                    std.fmt.allocPrint(
                        ui.main_window.ctx.frameAlloc(),
                        "Speed: {s}",
                        .{app_state.settings.emulation_speed.label()},
                    ) catch @panic("OOM"),
                    !app_state.isConnectedClient(),
                ).clicked(ui.main_window.ctx)) {
                    app_state.setEmulationSpeed(nextEmulationSpeed(app_state.settings.emulation_speed));
                }
                if (drawAndroidDrawerAction(ui, "Settings", true).clicked(ui.main_window.ctx)) {
                    app_state.show_android_settings_ui = true;
                    app_state.render_home_ui = false;
                    app_state.show_android_sidepanel = false;
                }
                if (drawAndroidDrawerAction(ui, "Exit", true).clicked(ui.main_window.ctx)) {
                    ui.quit = true;
                }
            }
            scroll.end();
        }
        col.end();
    }
    sidepanel.end();

    // close the sidepanel when a click happens outside of it
    if (is_open and !clay.pointerOver(sidepanel.id) and !pointer_over_state_dialog and
        ui.current_window.ctx.frame.mouse_down)
    {
        if (ui.hasTimerExpired("android_sidepanel").unwrap_or(false)) {
            app_state.show_android_sidepanel = false;
            app_state.show_android_save_state_dialog = false;
            app_state.show_android_load_state_dialog = false;
        }
    }
}

fn openAndroidSessionUI(ui: *UI, app_state: *AppState) void {
    app_state.show_android_multiplayer_ui = true;
    app_state.show_android_settings_ui = false;
    app_state.show_android_sidepanel = false;
    app_state.render_home_ui = !app_state.emulation_running;
    ui.setWindowFullscreen(false);
}

fn drawAndroidStateDialog(ui: *UI, app_state: *AppState, parent_id: clay.ElementId, mode: SaveStateMenuMode) bool {
    const is_open = app_state.show_android_save_state_dialog or app_state.show_android_load_state_dialog;
    const safe_padding = ui.main_window.safeAreaPadding();
    const offset = clay.Vector2{ .x = 20, .y = 40 };
    const bottom_margin = @as(f32, @floatFromInt(safe_padding.bottom)) + 8.0;

    const parent_data = clay.getElementData(parent_id);
    const dialog_top = if (parent_data.found)
        parent_data.bounding_box.y + offset.y
    else
        @as(f32, @floatFromInt(safe_padding.top)) + offset.y;
    const max_dialog_h = @max(48.0, ui.main_window.logical_height - bottom_margin - dialog_top);
    const max_slot_list_h = @max(48.0, max_dialog_h - 58.0);

    const float_panel = ui.float(.{
        .attach_to = .to_element_with_id,
        .parentId = parent_id.id,
        .z_index = 65,
        .offset = offset,
    });
    {
        const panel = ui.column(.{
            .bg_color = theme.bg_section,
            .border = .{ .width = .outside(1), .color = theme.border.toClay() },
            .corner_radius = 8,
            .padding = .{ .left = 8, .right = 8, .top = 8, .bottom = 8 },
            .gap = 8,
            .sizing = .{ .w = .fixed(260), .h = .fitMinMax(.{ .max = max_dialog_h }) },
            .child_alignment = .{ .x = .left, .y = .top },
        });
        {
            const scroll = ui.scrollArea(.{
                .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .max = max_slot_list_h }) },
                .gap = 5,
                .padding = .{ .right = 3 },
            });
            {
                for (0..save_state.SLOT_COUNT) |slot| {
                    const slot_info = app_state.saveStateInfo(slot);
                    if (drawAndroidStateSlotRow(ui, slot, slot_info, mode)) {
                        switch (mode) {
                            .save => {
                                app_state.saveStateSlot(slot);
                                app_state.show_android_save_state_dialog = false;
                            },
                            .load => {
                                app_state.loadStateSlot(slot);
                                app_state.show_android_sidepanel = false;
                                app_state.show_android_load_state_dialog = false;
                            },
                        }
                    }
                }
            }
            scroll.end();
        }
        panel.end();
    }
    float_panel.end();

    const pointer_over_dialog = clay.pointerOver(float_panel.id);
    if (is_open and !pointer_over_dialog and ui.current_window.ctx.frame.mouse_down) {
        if (ui.hasTimerExpired("android_state_dialog").unwrap_or(false)) {
            app_state.show_android_save_state_dialog = false;
            app_state.show_android_load_state_dialog = false;
        }
    }

    return pointer_over_dialog;
}

fn drawAndroidStateSlotRow(
    ui: *UI,
    slot: usize,
    slot_info: ?save_state.SlotInfo,
    mode: SaveStateMenuMode,
) bool {
    const frame_alloc = ui.main_window.ctx.frameAlloc();
    const enabled = mode == .save or slot_info != null;
    const slot_label = std.fmt.allocPrint(frame_alloc, "Slot {d}", .{slot + 1}) catch @panic("OOM");
    const status_label = if (slot_info) |info|
        std.fmt.allocPrint(frame_alloc, "{s}", .{&info.display_time}) catch @panic("OOM")
    else if (mode == .save)
        "Empty slot"
    else
        "No state";

    const row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fixed(48) },
        .padding = .{ .left = 10, .right = 10, .top = 6, .bottom = 6 },
        .gap = 8,
        .bg_color = if (enabled) theme.bg_panel else theme.bg_panel.darken(0.18),
        .hover_bg_color = if (enabled)
            switch (mode) {
                .save => theme.accent_green,
                .load => theme.accent_blue,
            }.withAlpha(0.28)
        else
            null,
        .corner_radius = 5,
        .border = .{
            .width = .outside(1),
            .color = (if (enabled) theme.border_dim else theme.border_dim.darken(0.18)).toClay(),
        },
        .child_alignment = .{ .x = .left, .y = .center },
    });
    {
        _ = ui.label(.{
            .text = slot_label,
            .font_size = 14,
            .line_height = 16,
            .color = if (enabled) theme.text_primary else theme.text_muted,
        });
        _ = ui.spacer(.{ .sizing = .{ .w = .grow } });
        _ = ui.label(.{
            .text = status_label,
            .font_size = 12,
            .line_height = 14,
            .color = if (slot_info != null)
                (if (mode == .load) theme.text_value else theme.text_secondary)
            else
                theme.text_muted,
        });
    }
    row.end();

    return enabled and row.clicked(ui.current_window.ctx);
}

fn drawAndroidDrawerSectionLabel(ui: *UI, text: []const u8) void {
    const row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fixed(34) },
        .padding = .{ .left = 20, .right = 20, .top = 12, .bottom = 4 },
        .child_alignment = .{ .x = .left, .y = .center },
    });
    {
        _ = ui.label(.{ .text = text, .font_size = 12, .line_height = 14, .color = theme.text_accent });
    }
    row.end();
}

fn drawAndroidDrawerAction(ui: *UI, text: []const u8, enabled: bool) *widgets.Button {
    return ui.button(.{
        .text = text,
        .font_size = 16,
        .text_color = if (enabled) theme.text_primary else theme.text_muted,
        .bg_color = theme.bg_panel,
        .hover_color = theme.bg_hover,
        .padding = .{ .left = 20, .right = 20, .top = 14, .bottom = 14 },
        .corner_radius = 0,
        .sizing = .{ .w = .grow, .h = .fit },
        .text_alignment = .left,
        .enabled = enabled,
    });
}

fn nextEmulationSpeed(speed: EmulationSpeed) EmulationSpeed {
    return switch (speed) {
        .half => .normal,
        .normal => .double,
        .double => .triple,
        .triple => .quadruple,
        .quadruple => .half,
    };
}

fn drawAndroidEditMode(ui: *UI, app_state: *AppState, safe_area_padding: clay.Padding) void {
    const orientation = android.currentScreenOrientation().?;
    const is_portrait = orientation == .portrait or orientation == .portrait_flipped;

    const root = ui.column(.{
        .sizing = .grow,
        .bg_color = theme.bg_base,
    });
    {
        const padding: clay.Padding = switch (orientation) {
            .portrait => .{ .top = safe_area_padding.top },
            .portrait_flipped => .{ .bottom = safe_area_padding.bottom },
            .landscape => .{ .left = safe_area_padding.left },
            .landscape_flipped => .{ .right = safe_area_padding.right },
            else => unreachable,
        };
        drawAndroidEditModeHeader(ui, app_state, orientation, padding);
        const canvas = ui.canvas(.{
            .pixel_format = c.SDL_PIXELFORMAT_ABGR8888,
            .pixels = createStoppedPreviewPlaceholder(ui.current_window.ctx.frameAlloc()),
            .w = NES_WIDTH,
            .h = NES_VISIBLE_HEIGHT,
            .padding = .{ .left = padding.left, .right = padding.right },
            .aspect_ratio = if (is_portrait) .@"4_3" else app_state.settings.aspect_ratio,
            .viewport_alignment = if (is_portrait) .top else .center,
            .bg_color = Color.black,
        });

        drawOnScreenGamepadEditMode(ui, app_state, canvas.id, orientation);
    }
    root.end();
}

fn drawAndroidEditModeHeader(ui: *UI, app_state: *AppState, orientation: android.ScreenOrientation, safe_area_padding: clay.Padding) void {
    const root = ui.column(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .bg_color = theme.bg_panel,
        .padding = .all(8),
    });
    _ = ui.spacer(.{ .sizing = .{ .h = .fixed(@floatFromInt(safe_area_padding.top)) } });
    {
        const row = ui.row(.{});
        {
            if (ui.button(.{
                .text = "Cancel",
                .font_size = 15,
                .text_color = theme.text_primary,
                .bg_color = theme.bg_hover,
                .hover_color = theme.border,
                .padding = .{ .left = 14, .right = 14, .top = 8, .bottom = 8 },
                .corner_radius = 5,
            }).clicked(ui.current_window.ctx)) {
                closeAndroidEditMode(ui, app_state, false);
            }
            _ = ui.spacer(.{ .sizing = .grow });

            const text = switch (orientation) {
                .portrait, .portrait_flipped => "Portrait Layout",
                .landscape, .landscape_flipped => "Landscape Layout",
                else => unreachable,
            };

            _ = ui.label(.{ .text = text, .color = .white, .font_size = 15 });

            _ = ui.spacer(.{ .sizing = .grow });

            if (ui.button(.{
                .text = "Save",
                .font_size = 15,
                .text_color = .white,
                .bg_color = theme.accent_blue,
                .hover_color = theme.accent_blue.lighten(0.12),
                .padding = .{ .left = 16, .right = 16, .top = 8, .bottom = 8 },
                .corner_radius = 5,
            }).clicked(ui.current_window.ctx)) {
                closeAndroidEditMode(ui, app_state, true);
            }
        }
        row.end();
    }
    root.end();
}

fn closeAndroidEditMode(ui: *UI, app_state: *AppState, save: bool) void {
    if (save) {
        app_state.settings.android_onscreen_controller = app_state.android_onscreen_controller;
        app_state.saveSettings();
    } else {
        app_state.android_onscreen_controller = app_state.settings.android_onscreen_controller;
    }

    ui.setWindowFullscreen(false);
    app_state.android_edit_mode = false;
    app_state.show_android_settings_ui = true;
}

fn drawAndroidSettingsUI(ui: *UI, app_state: *AppState, safe_area_padding: clay.Padding) void {
    const has_gamepad_connected = ui.getGamepadCount() > 0;
    const root = ui.column(.{
        .bg_color = theme.bg_base,
        .padding = .{
            .bottom = safe_area_padding.bottom,
            .left = safe_area_padding.left,
            .right = safe_area_padding.right,
        },
    });
    {
        const scroll = ui.scrollArea(.{});
        {
            const body = ui.column(.{
                .sizing = .{ .w = .grow, .h = .fit },
                .gap = 8,
            });
            {
                const tabs = ui.row(.{
                    .sizing = .{ .w = .grow, .h = .fit },
                    .padding = .{ .left = 12, .right = 12, .top = 10, .bottom = 10 },
                    .gap = 8,
                    .bg_color = theme.bg_panel,
                });
                {
                    drawAndroidSettingsTab(ui, app_state, .general);
                    drawAndroidSettingsTab(ui, app_state, .video);
                    drawAndroidSettingsTab(ui, app_state, .shader);
                    drawAndroidSettingsTab(ui, app_state, .controls);
                }
                tabs.end();

                switch (app_state.selected_category) {
                    .general => drawSettingsGeneralContent(ui, app_state),
                    .shader => drawSettingsShaderContent(ui, app_state),
                    .video => drawSettingsVideoContent(ui, app_state),
                    .controls => {
                        if (has_gamepad_connected) {
                            android.setScreenOrientation(.landscape);
                        }
                        drawSettingsControlsContent(ui, app_state, has_gamepad_connected);
                    },
                }
            }
            body.end();
        }
        scroll.end();

        const footer = ui.row(.{
            .sizing = .{ .w = .grow, .h = .fit },
            .bg_color = theme.bg_panel,
            .padding = .{ .left = 12, .right = 12, .top = 10, .bottom = 10 },
            .gap = 8,
            .child_alignment = .{ .x = .right, .y = .center },
        });
        {
            if (ui.button(.{
                .text = "Library",
                .font_size = 15,
                .text_color = theme.text_primary,
                .bg_color = theme.bg_hover,
                .hover_color = theme.border,
                .padding = .{ .left = 16, .right = 16, .top = 9, .bottom = 9 },
                .corner_radius = 6,
                .enabled = !app_state.isConnectedClient(),
            }).clicked(ui.main_window.ctx)) {
                app_state.show_android_settings_ui = false;
                app_state.render_home_ui = true;
            }

            _ = ui.spacer(.{ .sizing = .grow });

            if (ui.button(.{
                .text = "Save",
                .font_size = 15,
                .text_color = Color.white,
                .bg_color = theme.accent_blue,
                .hover_color = theme.accent_blue.lighten(0.12),
                .padding = .{ .left = 18, .right = 18, .top = 9, .bottom = 9 },
                .corner_radius = 6,
            }).clicked(ui.main_window.ctx)) {
                app_state.show_android_settings_ui = false;
                app_state.render_home_ui = !app_state.emulation_running;

                app_state.saveSettings();
                ui.setVSync(app_state.settings.vsync);
            }
        }
        footer.end();

        if (app_state.show_custom_file_picker) {
            drawAndroidShaderFilePicker(ui, app_state, root.id);
        }
    }
    root.end();
}

fn drawAndroidSettingsTab(ui: *UI, app_state: *AppState, category: SettingsCategory) void {
    const is_active = app_state.selected_category == category;
    if (ui.button(.{
        .text = category.displayName(),
        .sizing = .{ .w = .grow, .h = .fit },
        .font_size = 15,
        .text_color = if (is_active) Color.white else theme.text_secondary,
        .bg_color = if (is_active) theme.accent_blue else theme.bg_hover,
        .hover_color = if (is_active) theme.accent_blue.lighten(0.1) else theme.border,
        .padding = .{ .left = 12, .right = 12, .top = 9, .bottom = 9 },
        .corner_radius = 6,
    }).clicked(ui.main_window.ctx)) {
        if (app_state.selected_category == .controls and category != .controls) {
            android.setScreenOrientation(.unspecified);
        }
        app_state.selected_category = category;
    }
}

fn drawAndroidShaderFilePicker(ui: *UI, app_state: *AppState, root_id: clay.ElementId) void {
    const overlay = ui.float(.{
        .attach_to = .to_element_with_id,
        .parentId = root_id.id,
        .attach_points = .{ .parent = .left_top, .element = .left_top },
        .z_index = 90,
        .sizing = .grow,
    });
    {
        const scrim = ui.column(.{
            .sizing = .grow,
            .bg_color = Color.black.withAlpha(0.72),
            .padding = .{ .left = 14, .right = 14, .top = 18, .bottom = 18 },
            .child_alignment = .center,
        });
        {
            const max_panel_w = @min(560.0, ui.main_window.logical_width - 28.0);
            const max_panel_h = @min(620.0, ui.main_window.logical_height - 36.0);
            const panel = ui.column(.{
                .sizing = .{
                    .w = .growMinMax(.{ .max = max_panel_w }),
                    .h = .fitMinMax(.{ .max = max_panel_h }),
                },
                .bg_color = theme.bg_section,
                .border = .{ .width = .outside(1), .color = theme.border.toClay() },
                .corner_radius = 4,
                .padding = .{ .left = 12, .right = 12, .top = 12, .bottom = 12 },
                .gap = 10,
                .child_alignment = .{ .x = .left, .y = .top },
            });
            {
                drawAndroidShaderFilePickerHeader(ui, app_state);
                drawAndroidShaderFilePickerBody(ui, app_state);
            }
            panel.end();
        }
        scrim.end();
    }
    overlay.end();
}

fn drawAndroidShaderFilePickerHeader(ui: *UI, app_state: *AppState) void {
    const row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .gap = 8,
        .child_alignment = .{ .y = .center },
    });
    {
        _ = ui.label(.{
            .text = "Load Shader",
            .font_size = 18,
            .color = theme.text_primary,
        });
        _ = ui.spacer(.{ .sizing = .grow });
        if (ui.button(.{
            .text = "Close",
            .font_size = 15,
            .text_color = theme.text_primary,
            .bg_color = theme.bg_hover,
            .hover_color = theme.border,
            .padding = .{ .left = 10, .right = 10, .top = 6, .bottom = 6 },
            .corner_radius = 3,
        }).clicked(ui.main_window.ctx)) {
            app_state.closeShaderFilePicker();
        }
    }
    row.end();
}

fn drawAndroidShaderFilePickerBody(ui: *UI, app_state: *AppState) void {
    const frame_alloc = ui.current_window.ctx.frameAlloc();

    if (app_state.shader_file_picker_error) |msg| {
        const text = frame_alloc.dupe(u8, msg) catch @panic("OOM");
        _ = ui.label(.{
            .text = text,
            .font_size = 14,
            .color = theme.accent_red,
        });
        return;
    }

    const entries = app_state.shaderFilePickerEntries();
    const current_dir = app_state.shader_file_picker_current_dir;
    _ = ui.label(.{
        .text = if (current_dir.len == 0) "/" else frame_alloc.dupe(u8, current_dir) catch
            @panic("OOM"),
        .font_size = 13,
        .color = theme.text_secondary,
    });

    const scroll = ui.scrollArea(.{
        .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .max = 520 }) },
        .gap = 5,
        .vertical = true,
        .padding = .{ .right = 4 },
    });
    blk: {
        if (app_state.shaderFilePickerCanGoUp()) {
            if (ui.button(.{
                .text = "../",
                .font_size = 14,
                .text_color = theme.text_primary,
                .bg_color = theme.bg_panel,
                .hover_color = theme.bg_hover,
                .padding = .{ .left = 10, .right = 10, .top = 8, .bottom = 8 },
                .corner_radius = 2,
                .sizing = .{ .w = .grow, .h = .fit },
                .text_alignment = .left,
            }).clicked(ui.main_window.ctx)) {
                app_state.shaderFilePickerGoUp();
                break :blk;
            }
        }

        if (entries.len == 0) {
            _ = ui.label(.{
                .text = "No folders or .slangp files found",
                .font_size = 14,
                .color = theme.text_secondary,
            });
        } else {
            for (entries, 0..) |entry, idx| {
                const is_dir = entry.kind == .directory;
                if (ui.button(.{
                    .text = frame_alloc.dupe(u8, entry.label) catch @panic("OOM"),
                    .font_size = 14,
                    .text_color = if (is_dir) theme.text_accent else theme.text_primary,
                    .bg_color = theme.bg_panel,
                    .hover_color = theme.bg_hover,
                    .padding = .{ .left = 10, .right = 10, .top = 8, .bottom = 8 },
                    .corner_radius = 2,
                    .sizing = .{ .w = .grow, .h = .fit },
                    .text_alignment = .left,
                }).clicked(ui.main_window.ctx)) {
                    if (is_dir) {
                        app_state.openShaderFilePickerEntry(idx);
                    } else {
                        app_state.selectShaderFilePickerEntry(idx);
                    }
                    break :blk;
                }
            }
        }
    }
    scroll.end();
}

const TouchControl = enum {
    dpad,
    start,
    select,
    b,
    a,
};

const TouchControlLayout = struct {
    attach_points: clay.FloatingAttachPoints,
    initial_size: clay.Dimensions,
    min_size: clay.Dimensions,
    max_size: clay.Dimensions,
};

const DraggableResizable = struct {
    ui: *UI,
    draggable: *widgets.Draggable,
    resizable: *widgets.ResizablePanel,
    box: *widgets.Container,

    const Params = struct {
        drag_id: []const u8,
        resize_id: []const u8,
        initial_size: clay.Dimensions,
        current_size: clay.Dimensions,
        min_size: clay.Dimensions,
        max_size: clay.Dimensions,
        parentId: u32 = 0,
        z_index: i16 = 0,
        attach_points: clay.FloatingAttachPoints = .{ .element = .left_top, .parent = .left_top },
        attach_to: clay.FloatingAttachToElement = .to_root,
        offset: clay.Vector2 = .{ .x = 0, .y = 0 },
        bounds: ?clay.BoundingBox = null,
    };
    const Self = @This();

    fn start(ui: *UI, params: Params) Self {
        const draggable = ui.draggable(.{
            .id = params.drag_id,
            .parentId = params.parentId,
            .z_index = params.z_index,
            .attach_points = params.attach_points,
            .attach_to = params.attach_to,
            .offset = params.offset,
            .bounds = params.bounds,
        });
        const resizable = ui.resizablePanel(.{
            .id = params.resize_id,
            .initial_size = params.initial_size,
            .sizing = .{ .w = .fixed(params.current_size.w), .h = .fixed(params.current_size.h) },
            .max_size = params.max_size,
            .min_size = params.min_size,
            .preserve_aspect_ratio = true,
        });
        const box = ui.column(.{
            .sizing = .grow,
            .bg_color = theme.accent_blue.withAlpha(0.08),
            .border = .{ .width = .outside(1), .color = theme.accent_blue.withAlpha(0.82).toClay() },
            .corner_radius = 10,
            .child_alignment = .center,
        });

        return .{ .ui = ui, .draggable = draggable, .resizable = resizable, .box = box };
    }

    fn scale(self: *const Self) f32 {
        return self.resizable.scale();
    }

    fn offset(self: *const Self) clay.Vector2 {
        return self.draggable.offset();
    }

    fn end(self: *const Self) void {
        drawResizeHandles(self.ui, self.box.id);
        self.box.end();
        self.resizable.end();
        self.draggable.end();
    }
};

fn drawOnScreenGamepad(ui: *UI, app_state: *AppState, parent_id: clay.ElementId, screen_orientation: android.ScreenOrientation) void {
    const controller = app_state.settings.android_onscreen_controller.forOrientation(screen_orientation);
    drawOnScreenTouchControl(ui, parent_id, screen_orientation, .dpad, controller.dpad, true);
    drawOnScreenTouchControl(ui, parent_id, screen_orientation, .start, controller.start_btn, true);
    drawOnScreenTouchControl(ui, parent_id, screen_orientation, .select, controller.select_btn, true);
    drawOnScreenTouchControl(ui, parent_id, screen_orientation, .b, controller.action_btn_B, true);
    drawOnScreenTouchControl(ui, parent_id, screen_orientation, .a, controller.action_btn_A, true);
}

fn drawOnScreenGamepadEditMode(ui: *UI, app_state: *AppState, parent_id: clay.ElementId, screen_orientation: android.ScreenOrientation) void {
    drawEditableTouchControl(ui, app_state, parent_id, screen_orientation, .dpad);
    drawEditableTouchControl(ui, app_state, parent_id, screen_orientation, .start);
    drawEditableTouchControl(ui, app_state, parent_id, screen_orientation, .select);
    drawEditableTouchControl(ui, app_state, parent_id, screen_orientation, .b);
    drawEditableTouchControl(ui, app_state, parent_id, screen_orientation, .a);
}

fn drawOnScreenTouchControl(
    ui: *UI,
    parent_id: clay.ElementId,
    screen_orientation: android.ScreenOrientation,
    control: TouchControl,
    pos: AppState.OnScreenController.Pos,
    interactive: bool,
) void {
    const layout = touchControlLayout(control);
    const float = ui.float(.{
        .attach_to = .to_element_with_id,
        .parentId = parent_id.id,
        .z_index = 1,
        .attach_points = layout.attach_points,
        .offset = addVec(touchControlBaseOffset(control, screen_orientation), pos.offset),
    });
    {
        drawTouchControlVisual(ui, control, pos.scale, interactive);
    }
    float.end();
}

fn drawEditableTouchControl(
    ui: *UI,
    app_state: *AppState,
    parent_id: clay.ElementId,
    screen_orientation: android.ScreenOrientation,
    control: TouchControl,
) void {
    const layout = touchControlLayout(control);
    const ids = touchControlWidgetIds(control, screen_orientation);
    const controller = app_state.android_onscreen_controller.forOrientation(screen_orientation);
    const pos = touchControlPos(controller, control);
    const base_offset = touchControlBaseOffset(control, screen_orientation);
    const current_size = clampSize(scaleSize(layout.initial_size, pos.scale), layout.min_size, layout.max_size);
    const parent_data = clay.getElementData(parent_id);
    std.debug.assert(parent_data.found);
    const drag = DraggableResizable.start(ui, .{
        .drag_id = ids.drag_id,
        .resize_id = ids.resize_id,
        .parentId = parent_id.id,
        .z_index = 4,
        .attach_to = .to_element_with_id,
        .attach_points = layout.attach_points,
        .offset = addVec(base_offset, pos.offset),
        // Clay can report the previous frame's canvas bounds during orientation changes.
        // Skip clamping for that transient frame so stale landscape/portrait dimensions
        // do not overwrite the saved control offsets.
        .bounds = if (validTouchControlBounds(parent_data.bounding_box, screen_orientation))
            parent_data.bounding_box
        else
            null,
        .initial_size = layout.initial_size,
        .current_size = current_size,
        .min_size = layout.min_size,
        .max_size = layout.max_size,
    });
    {
        pos.scale = drag.scale();
        pos.offset = subVec(drag.offset(), base_offset);
        drawTouchControlVisual(ui, control, pos.scale, false);
    }
    drag.end();
}

fn touchControlLayout(control: TouchControl) TouchControlLayout {
    return switch (control) {
        .dpad => .{
            .attach_points = .{ .parent = .left_bottom, .element = .left_bottom },
            .initial_size = .{ .w = 162, .h = 162 },
            .min_size = .{ .w = 114, .h = 114 },
            .max_size = .{ .w = 308, .h = 308 },
        },
        .start => .{
            .attach_points = .{ .parent = .center_bottom, .element = .center_bottom },
            .initial_size = .{ .w = 78, .h = 38 },
            .min_size = .{ .w = 59, .h = 29 },
            .max_size = .{ .w = 148, .h = 72 },
        },
        .select => .{
            .attach_points = .{ .parent = .center_bottom, .element = .center_bottom },
            .initial_size = .{ .w = 86, .h = 38 },
            .min_size = .{ .w = 65, .h = 29 },
            .max_size = .{ .w = 163, .h = 72 },
        },
        .b => .{
            .attach_points = .{ .parent = .right_bottom, .element = .right_bottom },
            .initial_size = .{ .w = 62, .h = 62 },
            .min_size = .{ .w = 44, .h = 44 },
            .max_size = .{ .w = 118, .h = 118 },
        },
        .a => .{
            .attach_points = .{ .parent = .right_bottom, .element = .right_bottom },
            .initial_size = .{ .w = 62, .h = 62 },
            .min_size = .{ .w = 44, .h = 44 },
            .max_size = .{ .w = 118, .h = 118 },
        },
    };
}

fn touchControlWidgetIds(control: TouchControl, screen_orientation: android.ScreenOrientation) struct {
    drag_id: []const u8,
    resize_id: []const u8,
} {
    const is_portrait = screen_orientation == .portrait or screen_orientation == .portrait_flipped;
    return switch (control) {
        .dpad => if (is_portrait)
            .{ .drag_id = "android_touch_portrait_dpad_drag", .resize_id = "android_touch_portrait_dpad_resize" }
        else
            .{ .drag_id = "android_touch_landscape_dpad_drag", .resize_id = "android_touch_landscape_dpad_resize" },
        .start => if (is_portrait)
            .{ .drag_id = "android_touch_portrait_start_drag", .resize_id = "android_touch_portrait_start_resize" }
        else
            .{ .drag_id = "android_touch_landscape_start_drag", .resize_id = "android_touch_landscape_start_resize" },
        .select => if (is_portrait)
            .{ .drag_id = "android_touch_portrait_select_drag", .resize_id = "android_touch_portrait_select_resize" }
        else
            .{ .drag_id = "android_touch_landscape_select_drag", .resize_id = "android_touch_landscape_select_resize" },
        .b => if (is_portrait)
            .{ .drag_id = "android_touch_portrait_b_drag", .resize_id = "android_touch_portrait_b_resize" }
        else
            .{ .drag_id = "android_touch_landscape_b_drag", .resize_id = "android_touch_landscape_b_resize" },
        .a => if (is_portrait)
            .{ .drag_id = "android_touch_portrait_a_drag", .resize_id = "android_touch_portrait_a_resize" }
        else
            .{ .drag_id = "android_touch_landscape_a_drag", .resize_id = "android_touch_landscape_a_resize" },
    };
}

fn touchControlBaseOffset(control: TouchControl, screen_orientation: android.ScreenOrientation) clay.Vector2 {
    const bottom_offset: f32 = -22;
    const is_portrait = screen_orientation == .portrait or screen_orientation == .portrait_flipped;
    const center_button_y = if (is_portrait) bottom_offset - 182 else bottom_offset;

    return switch (control) {
        .dpad => .{ .x = 18, .y = bottom_offset - 6 },
        .start => .{ .x = -48, .y = center_button_y },
        .select => .{ .x = 44, .y = center_button_y },
        .b => .{ .x = -96, .y = bottom_offset - 4 },
        .a => .{ .x = -18, .y = bottom_offset - 4 },
    };
}

fn validTouchControlBounds(bounds: clay.BoundingBox, screen_orientation: android.ScreenOrientation) bool {
    if (bounds.width <= 0 or bounds.height <= 0) return false;

    return switch (screen_orientation) {
        .portrait, .portrait_flipped => bounds.height >= bounds.width,
        .landscape, .landscape_flipped => bounds.width >= bounds.height,
        .unknown, .unspecified => true,
    };
}

fn touchControlPos(
    controller: *AppState.OnScreenController.Layout,
    control: TouchControl,
) *AppState.OnScreenController.Pos {
    return switch (control) {
        .dpad => &controller.dpad,
        .start => &controller.start_btn,
        .select => &controller.select_btn,
        .b => &controller.action_btn_B,
        .a => &controller.action_btn_A,
    };
}

fn drawTouchControlVisual(ui: *UI, control: TouchControl, resize_scale: f32, interactive: bool) void {
    switch (control) {
        .dpad => drawAndroidOnScreenDpad(ui, resize_scale, interactive),
        .start => drawAndroidOnScreenStartBtn(ui, resize_scale, interactive),
        .select => drawAndroidOnScreenSelectBtn(ui, resize_scale, interactive),
        .b => {
            const button = controllerButton(
                ui,
                .{ .text = "B", .font_size = scaledFontSize(18, resize_scale) },
                scaleSizing(.{ .w = 62, .h = 62 }, resize_scale),
            );
            if (interactive and button.clickedOrHold(ui.main_window.ctx)) ui.pressOnScreenControllerButton(.b);
        },
        .a => {
            const button = controllerButton(
                ui,
                .{ .text = "A", .font_size = scaledFontSize(18, resize_scale) },
                scaleSizing(.{ .w = 62, .h = 62 }, resize_scale),
            );
            if (interactive and button.clickedOrHold(ui.main_window.ctx)) ui.pressOnScreenControllerButton(.a);
        },
    }
}

fn drawResizeHandles(ui: *UI, parent_id: clay.ElementId) void {
    drawResizeHandle(ui, parent_id, .left_top, .{ .w = 10, .h = 10 }, .{ .x = -1, .y = -1 });
    drawResizeHandle(ui, parent_id, .right_top, .{ .w = 10, .h = 10 }, .{ .x = 1, .y = -1 });
    drawResizeHandle(ui, parent_id, .left_bottom, .{ .w = 10, .h = 10 }, .{ .x = -1, .y = 1 });
    drawResizeHandle(ui, parent_id, .right_bottom, .{ .w = 10, .h = 10 }, .{ .x = 1, .y = 1 });
    drawResizeHandle(ui, parent_id, .center_top, .{ .w = 28, .h = 4 }, .{ .x = 0, .y = -1 });
    drawResizeHandle(ui, parent_id, .center_bottom, .{ .w = 28, .h = 4 }, .{ .x = 0, .y = 1 });
    drawResizeHandle(ui, parent_id, .left_center, .{ .w = 4, .h = 28 }, .{ .x = -1, .y = 0 });
    drawResizeHandle(ui, parent_id, .right_center, .{ .w = 4, .h = 28 }, .{ .x = 1, .y = 0 });
}

fn drawResizeHandle(
    ui: *UI,
    parent_id: clay.ElementId,
    attach_point: clay.FloatingAttachPointType,
    size: clay.Dimensions,
    offset: clay.Vector2,
) void {
    const handle = ui.float(.{
        .attach_to = .to_element_with_id,
        .parentId = parent_id.id,
        .z_index = 5,
        .attach_points = .{ .parent = attach_point, .element = attach_point },
        .offset = offset,
        .pointer_capture_mode = .passthrough,
        .sizing = .{ .w = .fixed(size.w), .h = .fixed(size.h) },
    });
    {
        const body = ui.column(.{
            .sizing = .grow,
            .bg_color = theme.accent_blue.withAlpha(0.92),
            .border = .{ .width = .outside(1), .color = Color.white.withAlpha(0.5).toClay() },
            .corner_radius = 3,
        });
        body.end();
    }
    handle.end();
}

fn addVec(a: clay.Vector2, b: clay.Vector2) clay.Vector2 {
    return .{ .x = a.x + b.x, .y = a.y + b.y };
}

fn subVec(a: clay.Vector2, b: clay.Vector2) clay.Vector2 {
    return .{ .x = a.x - b.x, .y = a.y - b.y };
}

fn scaleSize(size: clay.Dimensions, scale: f32) clay.Dimensions {
    return .{ .w = size.w * scale, .h = size.h * scale };
}

fn clampSize(size: clay.Dimensions, min_size: clay.Dimensions, max_size: clay.Dimensions) clay.Dimensions {
    return .{
        .w = std.math.clamp(size.w, min_size.w, max_size.w),
        .h = std.math.clamp(size.h, min_size.h, max_size.h),
    };
}

fn scaleSizing(size: clay.Dimensions, scale: f32) clay.Sizing {
    const scaled = scaleSize(size, scale);
    return .{ .w = .fixed(scaled.w), .h = .fixed(scaled.h) };
}

fn scaledFontSize(base: u16, scale: f32) u16 {
    return @intFromFloat(@max(1.0, @as(f32, @floatFromInt(base)) * scale));
}

fn drawAndroidOnScreenDpad(ui: *UI, resize_scale: f32, interactive: bool) void {
    const col = ui.column(.{
        .child_alignment = .center,
        .sizing = .{ .w = .fixed(162 * resize_scale), .h = .fixed(162 * resize_scale) },
    });
    {
        const up_button = controllerButton(
            ui,
            .{ .icon = .{ .icon = .dpad_up, .size = 32 * resize_scale } },
            .{ .w = .fixed(54 * resize_scale), .h = .fixed(54 * resize_scale) },
        );
        if (interactive and up_button.clickedOrHold(ui.main_window.ctx)) ui.pressOnScreenControllerButton(.up);

        const row = ui.row(.{ .gap = @intFromFloat(54 * resize_scale) });
        {
            const left_button = controllerButton(
                ui,
                .{ .icon = .{ .icon = .dpad_left, .size = 32 * resize_scale } },
                .{ .w = .fixed(54 * resize_scale), .h = .fixed(54 * resize_scale) },
            );
            if (interactive and left_button.clickedOrHold(ui.main_window.ctx)) ui.pressOnScreenControllerButton(.left);

            const right_button = controllerButton(
                ui,
                .{ .icon = .{ .icon = .dpad_right, .size = 32 * resize_scale } },
                .{ .w = .fixed(54 * resize_scale), .h = .fixed(54 * resize_scale) },
            );
            if (interactive and right_button.clickedOrHold(ui.main_window.ctx)) ui.pressOnScreenControllerButton(.right);
        }
        row.end();

        const down_button = controllerButton(
            ui,
            .{ .icon = .{ .icon = .dpad_down, .size = 32 * resize_scale } },
            .{ .w = .fixed(54 * resize_scale), .h = .fixed(54 * resize_scale) },
        );
        if (interactive and down_button.clickedOrHold(ui.main_window.ctx)) ui.pressOnScreenControllerButton(.down);
    }
    col.end();
}

fn drawAndroidOnScreenStartBtn(ui: *UI, resize_scale: f32, interactive: bool) void {
    const start_button = ui.button(.{
        .text = "START",
        .font_size = scaledFontSize(13, resize_scale),
        .text_color = theme.text_primary,
        .sizing = scaleSizing(.{ .w = 78, .h = 38 }, resize_scale),
        .bg_color = Color.black.withAlpha(0.58),
        .hover_color = theme.bg_hover.withAlpha(0.82),
        .padding = .all(0),
        .corner_radius = 10,
        .border = .{ .color = Color.white.withAlpha(0.28).toClay(), .width = .outside(1) },
    });
    if (interactive and start_button.clickedOrHold(ui.main_window.ctx)) ui.pressOnScreenControllerButton(.start);
}

fn drawAndroidOnScreenSelectBtn(ui: *UI, resize_scale: f32, interactive: bool) void {
    const select_button = ui.button(.{
        .text = "SELECT",
        .font_size = scaledFontSize(13, resize_scale),
        .text_color = theme.text_primary,
        .sizing = scaleSizing(.{ .w = 86, .h = 38 }, resize_scale),
        .bg_color = Color.black.withAlpha(0.58),
        .hover_color = theme.bg_hover.withAlpha(0.82),
        .padding = .all(0),
        .corner_radius = 10,
        .border = .{ .color = Color.white.withAlpha(0.28).toClay(), .width = .outside(1) },
    });
    if (interactive and select_button.clickedOrHold(ui.main_window.ctx)) ui.pressOnScreenControllerButton(.select);
}

fn controllerButton(
    ui: *UI,
    params: struct {
        text: ?[]const u8 = null,
        icon: ?struct { icon: UI.Icon, size: f32 = 32 } = null,
        font_size: u16 = 18,
    },
    sizing: clay.Sizing,
) *widgets.Button {
    return ui.button(.{
        .text = params.text orelse "",
        .icon = if (params.icon) |icon| .{
            .icon = ui.icons.get(icon.icon),
            .size = icon.size,
        } else null,
        .font_size = params.font_size,
        .text_color = theme.text_primary,
        .sizing = sizing,
        .bg_color = Color.black.withAlpha(0.52),
        .hover_color = theme.accent_blue.withAlpha(0.72),
        .border = .{ .color = Color.white.withAlpha(0.28).toClay(), .width = .outside(1) },
        .corner_radius = 10,
        .padding = .all(0),
    });
}

const SaveStateMenuMode = enum { save, load };

fn drawStateSlotItems(ui: *UI, app_state: *AppState, mode: SaveStateMenuMode) void {
    for (0..save_state.SLOT_COUNT) |slot| {
        const slot_info = app_state.saveStateInfo(slot);
        const label = if (slot_info) |info|
            std.fmt.allocPrint(
                ui.main_window.ctx.frameAlloc(),
                "Slot {d} - {s}",
                .{ slot + 1, &info.display_time },
            ) catch @panic("OOM")
        else
            std.fmt.allocPrint(
                ui.main_window.ctx.frameAlloc(),
                "Slot {d}",
                .{slot + 1},
            ) catch @panic("OOM");

        if (ui.menuItem(.{
            .label = label,
            .enabled = app_state.emulation_running,
            .bg_color = theme.bg_section,
            .hover_color = theme.accent_blue,
            .text_color = theme.text_secondary,
        }).clicked(ui.main_window.ctx)) {
            switch (mode) {
                .save => app_state.saveStateSlot(slot),
                .load => app_state.loadStateSlot(slot),
            }
        }
    }
}

const FilePickerCallbackData = struct { ui: *UI, app_state: *AppState };
fn openRomDialog(ui: *UI, app_state: *AppState) void {
    const alloc = app_state.alloc;
    const default_location = std.process.currentPathAlloc(app_state.io, alloc) catch
        @panic("OOM");
    defer alloc.free(default_location);

    const default_location_z = alloc.dupeZ(u8, default_location) catch @panic("OOM");
    defer alloc.free(default_location_z);

    const data = ui.main_window.ctx.persistent_arena.allocator()
        .create(FilePickerCallbackData) catch @panic("OOM");
    data.* = .{ .app_state = app_state, .ui = ui };

    c.SDL_ShowOpenFileDialog(
        file_picker_callback,
        clay.anytypeToAnyopaquePtr(data),
        ui.main_window.ptr,
        &dialog_filter_list,
        dialog_filter_list.len,
        default_location_z.ptr,
        false,
    );
}

fn drawHomeUI(ui: *UI, app_state: *AppState, safe_area_padding: clay.Padding) void {
    const entries = app_state.history.entries.items;

    const root = ui.column(.{
        .sizing = .grow,
        .bg_color = theme.bg_base,
        .child_alignment = .{ .x = .center, .y = .top },
        .padding = if (builtin.abi.isAndroid())
            .{
                .bottom = safe_area_padding.bottom,
                .left = safe_area_padding.left,
                .right = safe_area_padding.right,
            }
        else
            .{},
    });
    {
        const float = ui.float(.{
            .z_index = 5,
            .attach_to = .to_element_with_id,
            .parentId = root.id.id,
            .sizing = .grow,
        });
        {
            if (entries.len == 0) { // Empty state home
                if (builtin.abi.isAndroid()) {
                    _ = ui.spacer(.{ .sizing = .grow });
                    _ = ui.label(.{ .text = "No ROMs added", .font_size = 16, .color = theme.text_secondary });
                    _ = ui.spacer(.{ .sizing = .grow });
                } else {
                    _ = ui.spacer(.{ .sizing = .grow });
                    if (ui.button(.{
                        .text = "Open ROM",
                        .font_size = 15,
                        .bg_color = theme.accent_blue,
                        .text_color = Color.white,
                        .padding = .{ .left = 28, .right = 28, .top = 10, .bottom = 10 },
                        .corner_radius = 6,
                    }).clicked(ui.main_window.ctx)) openRomDialog(ui, app_state);
                    _ = ui.spacer(.{ .sizing = .{ .w = .fixed(0), .h = .fixed(14) } });
                    _ = ui.label(.{ .text = "or use System > Open", .font_size = 13, .color = theme.text_secondary });
                    _ = ui.spacer(.{ .sizing = .grow });
                }
            } else {
                const card_title_lines = maxGameCardTitleLines(ui, entries);
                const scroll = ui.scrollArea(.{ .sizing = .grow, .vertical = true });
                {
                    const grid = ui.grid(.{
                        .id = "cards_grid",
                        .gap = 16,
                        .sizing = .grow,
                        .child_alignment = .{ .x = .left, .y = .top },
                        .padding = .{ .left = 24, .right = 24, .top = 20, .bottom = 24 },
                        .item_transition = .{
                            .handler = clay.easeOut,
                            .duration = 0.22,
                            .properties = clay.TransitionProperty.position,
                            .interaction_handling = .allow_interactions_while_transitioning_position,
                        },
                    });
                    for (entries) |*entry| {
                        const slot = grid.item();
                        drawGameCard(ui, app_state, entry, card_title_lines);
                        slot.end();
                    }
                    grid.end();
                }
                scroll.end();
            }
        }
        float.end();

        if (app_state.settings.show_home_screen_snow_effect) {
            const shader = ui.shaderMode(.{ .id = "snow" });
            _ = ui.canvas(.{
                .pixels = &.{ 0, 0, 0, 255 },
                .w = 1,
                .h = 1,
                .pixel_format = c.SDL_PIXELFORMAT_ABGR8888,
            });
            shader.end();
        }
    }
    root.end();
}

const CARD_W: f32 = if (builtin.abi.isAndroid()) 150 else 200;
const CARD_THUMB_H: f32 = CARD_W * 224 / 256;
const CARD_CONTENT_W: f32 = CARD_W - 20;
const CARD_TITLE_FONT_SIZE: u16 = 14;
const CARD_TITLE_LINE_H: u16 = 16;
const CARD_TITLE_MAX_LINES: u16 = 4;
const CARD_PLAY_TIME_LINE_H: u16 = 14;
const CARD_CORNER_RADIUS: f32 = 6;
const PLACEHOLDER_THUMBNAIL_PIXEL = [_]u8{ 0, 0, 0, 255 };

fn cardTitleHeight(title_lines: u16) f32 {
    return @floatFromInt(CARD_TITLE_LINE_H * title_lines);
}

fn cardInfoHeight(title_lines: u16) f32 {
    return 8 + cardTitleHeight(title_lines) + 4 + CARD_PLAY_TIME_LINE_H + 8;
}

fn cardHeight(title_lines: u16) f32 {
    return CARD_THUMB_H + cardInfoHeight(title_lines);
}

fn drawGameCard(ui: *UI, app_state: *AppState, entry: *const game_history.GameEntry, title_lines: u16) void {
    const ctx = ui.main_window.ctx;
    const info_h = cardInfoHeight(title_lines);

    const card = ui.column(.{
        .sizing = .{ .w = .fixed(CARD_W), .h = .fixed(cardHeight(title_lines)) },
        .bg_color = theme.bg_section,
        .hover_bg_color = theme.bg_hover,
        .corner_radius = CARD_CORNER_RADIUS,
        .border = .{ .width = .outside(1), .color = theme.border_dim.toClay() },
        .child_alignment = .{ .x = .left, .y = .top },
        .gap = 0,
    });
    {
        if (entry.thumbnail) |thumb| {
            _ = ui.canvas(.{
                .pixel_format = c.SDL_PIXELFORMAT_ABGR8888,
                .pixels = thumb,
                .w = game_history.THUMBNAIL_WIDTH,
                .h = game_history.THUMBNAIL_HEIGHT,
                .sizing = .{ .w = .fixed(CARD_W), .h = .fixed(CARD_THUMB_H) },
                .corner_radius = .{ .top_left = CARD_CORNER_RADIUS, .top_right = CARD_CORNER_RADIUS },
            });
        } else {
            _ = ui.canvas(.{
                .pixel_format = c.SDL_PIXELFORMAT_ABGR8888,
                .pixels = PLACEHOLDER_THUMBNAIL_PIXEL[0..],
                .w = 1,
                .h = 1,
                .sizing = .{ .w = .fixed(CARD_W), .h = .fixed(CARD_THUMB_H) },
                .bg_color = Color.rgb(10, 12, 16),
                .corner_radius = .{ .top_left = CARD_CORNER_RADIUS, .top_right = CARD_CORNER_RADIUS },
            });
        }

        const info = ui.column(.{
            .sizing = .{ .w = .fixed(CARD_W), .h = .fixed(info_h) },
            .padding = .{ .left = 10, .right = 10, .top = 8, .bottom = 8 },
            .gap = 4,
            .child_alignment = .{ .x = .left, .y = .top },
        });
        {
            _ = ui.label(.{
                .text = entry.name,
                .font_size = CARD_TITLE_FONT_SIZE,
                .line_height = CARD_TITLE_LINE_H,
                .color = theme.text_primary,
            });
            _ = ui.spacer(.{ .sizing = .grow });
            _ = ui.label(.{
                .text = utils.formatPlayTime(entry.play_time_secs, ctx.frameAlloc()),
                .font_size = 13,
                .line_height = CARD_PLAY_TIME_LINE_H,
                .color = theme.text_secondary,
            });
        }
        info.end();
    }
    card.end();

    if (card.clicked(ui.main_window.ctx)) {
        app_state.loadRom(entry.rom_path) catch |err| std.debug.panic("Failed to load selected ROM: {any}\n", .{err});

        if (builtin.abi.isAndroid()) {
            ui.setWindowFullscreen(true);
        }
    }
}

fn maxGameCardTitleLines(ui: *UI, entries: []const game_history.GameEntry) u16 {
    const space_w = ui.measureTextWidth(" ", CARD_TITLE_FONT_SIZE);
    var max_lines: u16 = 1;
    for (entries) |entry| {
        max_lines = @max(max_lines, calculateGameCardTitleTotalLines(ui, entry.name, space_w));
    }
    return @min(max_lines, CARD_TITLE_MAX_LINES);
}

fn calculateGameCardTitleTotalLines(ui: *UI, title: []const u8, space_w: f32) u16 {
    var lines: u16 = 1;
    var line_w: f32 = 0;

    var it = std.mem.splitScalar(u8, title, ' ');
    while (it.next()) |word| {
        const word_w = ui.measureTextWidth(word, CARD_TITLE_FONT_SIZE);
        line_w += word_w + space_w;

        if (line_w > CARD_CONTENT_W) {
            lines += 1;
            line_w = word_w;
        }
    }

    return lines;
}

const nav_active_bg = Color.rgb(28, 110, 90);
const nav_active_text = Color.rgb(210, 240, 232);
const nav_hover_bg = Color.rgb(35, 42, 52);

fn drawSettingsWindowUI(ui: *UI, user_data: ?*anyopaque) void {
    const app_state: *AppState = @ptrCast(@alignCast(user_data));

    const root = ui.row(.{
        .sizing = .grow,
        .bg_color = theme.bg_base,
        .child_alignment = .{ .x = .left, .y = .top },
    });
    {
        drawSettingsSidebar(ui, app_state);
        _ = ui.separator(.{ .direction = .vertical, .color = theme.border_dim });

        const body = ui.column(.{
            .sizing = .{ .w = .grow, .h = .grow },
            .child_alignment = .{ .x = .left, .y = .top },
        });
        {
            drawSettingsContent(ui, app_state);
            drawSettingsFooter(ui, app_state);
        }
        body.end();
    }
    root.end();
}

fn drawSettingsFooter(ui: *UI, app_state: *AppState) void {
    const has_changes = app_state.hasSettingsChanges();

    const footer = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .bg_color = theme.bg_panel,
        .padding = .{ .left = 12, .right = 12, .top = 10, .bottom = 10 },
        .gap = 8,
        .child_alignment = .{ .x = .right, .y = .center },
    });
    {
        _ = ui.spacer(.{ .sizing = .grow });

        if (ui.button(.{
            .text = "Cancel",
            .font_size = 15,
            .text_color = theme.text_primary,
            .bg_color = theme.bg_hover,
            .hover_color = theme.border,
            .padding = .{ .left = 14, .right = 14, .top = 7, .bottom = 7 },
            .corner_radius = 3,
        }).clicked(ui.current_window.ctx)) {
            app_state.restoreSavedSettings();
            ui.closeCurrentWindow();
        }

        if (ui.button(.{
            .text = "Save",
            .font_size = 15,
            .enabled = has_changes,
            .text_color = if (has_changes) Color.white else theme.text_secondary,
            .bg_color = if (has_changes) theme.accent_blue else theme.bg_hover,
            .hover_color = if (has_changes) theme.accent_blue.lighten(0.12) else theme.bg_hover,
            .padding = .{ .left = 16, .right = 16, .top = 7, .bottom = 7 },
            .corner_radius = 3,
        }).clicked(ui.current_window.ctx)) {
            app_state.saveSettings();
            ui.closeCurrentWindow(); // close settings window
            ui.setVSync(app_state.settings.vsync);
        }
    }
    footer.end();
}

fn drawSettingsSidebar(ui: *UI, app_state: *AppState) void {
    const sidebar = ui.column(.{
        .sizing = .{ .w = .fixed(130), .h = .grow },
        .bg_color = theme.bg_base,
        .padding = .{ .top = 6, .bottom = 6 },
        .gap = 0,
        .child_alignment = .{ .x = .left, .y = .top },
    });
    {
        inline for (@typeInfo(SettingsCategory).@"enum".fields) |category| {
            drawSidebarItem(ui, app_state, @field(SettingsCategory, category.name));
        }
    }
    sidebar.end();
}

fn drawSidebarItem(ui: *UI, app_state: *AppState, category: SettingsCategory) void {
    const is_active = app_state.selected_category == category;
    if (ui.button(.{
        .text = category.displayName(),
        .font_size = 19,
        .text_color = if (is_active) nav_active_text else theme.text_secondary,
        .bg_color = if (is_active) nav_active_bg else theme.bg_base,
        .hover_color = if (is_active) nav_active_bg else nav_hover_bg,
        .padding = .{ .left = 14, .right = 14, .top = 9, .bottom = 9 },
        .corner_radius = 0,
        .sizing = .{ .w = .grow, .h = .fit },
        .text_alignment = .left,
    }).clicked(ui.current_window.ctx)) {
        app_state.selected_category = category;
    }
}

fn drawSettingsContent(ui: *UI, app_state: *AppState) void {
    const scroll = ui.scrollArea(.{});
    {
        const content = ui.column(.{
            .sizing = .grow,
            .bg_color = theme.bg_panel,
            .child_alignment = .{ .x = .left, .y = .top },
            .padding = .all(5),
            .gap = 5,
        });

        {
            switch (app_state.selected_category) {
                .general => drawSettingsGeneralContent(ui, app_state),
                .video => drawSettingsVideoContent(ui, app_state),
                .shader => drawSettingsShaderContent(ui, app_state),
                .controls => drawSettingsControlsContent(ui, app_state, true),
            }
        }

        content.end();
    }
    scroll.end();
}

fn drawContentSectionHeader(ui: *UI, title: []const u8) void {
    _ = ui.label(.{
        .text = title,
        .font_size = 14,
        .color = theme.accent_green,
    });
}

fn drawContentSection(ui: *UI, params: struct { padding: ?clay.Padding = null, sizing: ?clay.Sizing = null }) *widgets.Container {
    return ui.column(.{
        .sizing = if (params.sizing) |sizing| sizing else .{ .w = .grow, .h = .fit },
        .bg_color = theme.bg_section,
        .padding = if (params.padding) |padding| padding else .{ .left = 14, .right = 14, .top = 12, .bottom = 12 },
        .gap = 12,
        .border = .{ .width = .outside(1), .color = theme.border_dim.toClay() },
        .corner_radius = 3,
        .child_alignment = .{ .x = .left, .y = .top },
    });
}

fn drawSettingsVideoContent(ui: *UI, app_state: *AppState) void {
    drawContentSectionHeader(ui, "General");
    const general_section = drawContentSection(ui, .{});
    {
        const row = ui.row(.{
            .sizing = .{ .w = .grow, .h = .fit },
        });
        {
            _ = ui.label(.{
                .text = "VSync",
                .font_size = theme.LABEL_FONT,
                .color = theme.text_primary,
            });
            _ = ui.spacer(.{ .sizing = .grow });
            app_state.settings.vsync = ui.toggle(.{ .value = app_state.settings.vsync, .size = 22 }).value();
        }
        row.end();
    }
    general_section.end();

    drawContentSectionHeader(ui, "Display");
    const display_section = drawContentSection(ui, .{});
    {
        drawAspectRatioRow(ui, app_state);
    }
    display_section.end();
}

fn drawSettingsGeneralContent(ui: *UI, app_state: *AppState) void {
    drawContentSectionHeader(ui, "General");
    const section = drawContentSection(ui, .{});
    {
        if (!builtin.abi.isAndroid()) {
            const row = ui.row(.{
                .sizing = .{ .w = .grow, .h = .fit },
            });
            {
                _ = ui.label(.{
                    .text = "Hide mouse on inactivity",
                    .font_size = theme.LABEL_FONT,
                    .color = theme.text_primary,
                });
                _ = ui.spacer(.{ .sizing = .grow });
                app_state.settings.hide_mouse_on_inactivity = ui
                    .toggle(.{ .value = app_state.settings.hide_mouse_on_inactivity, .size = 22 })
                    .value();
            }
            row.end();
        }

        drawEmulationSpeedRow(ui, app_state);

        const row2 = ui.row(.{
            .sizing = .{ .w = .grow, .h = .fit },
        });
        {
            _ = ui.label(.{
                .text = "Show snow effect on the home screen",
                .font_size = theme.LABEL_FONT,
                .color = theme.text_primary,
            });
            _ = ui.spacer(.{ .sizing = .grow });
            app_state.settings.show_home_screen_snow_effect = ui
                .toggle(.{ .value = app_state.settings.show_home_screen_snow_effect, .size = 22 })
                .value();
        }
        row2.end();
    }
    section.end();
}

fn drawSettingsControlsContent(ui: *UI, app_state: *AppState, draw_gamepad_section: bool) void {
    if (builtin.abi.isAndroid()) drawTouchControlsSettingSection(ui, app_state);

    if (draw_gamepad_section) {
        drawContentSectionHeader(ui, if (builtin.abi.isAndroid()) "Gamepad" else "Controls");
        const controller_keymap_section = drawContentSection(ui, .{});
        {
            updateControllerBindingCapture(ui, app_state);
            updateGamepadBindingCapture(ui, app_state);
            updateGeneralBindingCapture(ui, app_state);
            drawControllerPlayerSelector(ui, app_state);
            if (!builtin.abi.isAndroid()) drawInputDeviceSelector(ui, app_state);
            drawControllerBindingOverlay(ui, app_state);

            if (app_state.selectedTmpInputDevice().* == .gamepad) {
                drawContentSectionHeader(ui, "Analog Stick");
                const gamepad_section = drawContentSection(ui, .{});
                drawGamepadDeadzoneRow(ui, app_state);
                gamepad_section.end();
            }
        }
        controller_keymap_section.end();
    }

    if (!builtin.abi.isAndroid()) {
        drawContentSectionHeader(ui, "General");
        const general_keymap_section = drawContentSection(ui, .{
            .padding = .{ .left = 14, .right = 5, .top = 12, .bottom = 12 },
        });
        {
            const scroll = ui.scrollArea(.{
                .sizing = .{ .h = .fixed(200), .w = .grow },
                .gap = 12,
            });
            {
                inline for (@typeInfo(GeneralAction).@"enum".fields) |action_field| {
                    drawGeneralBindingRow(ui, app_state, @field(GeneralAction, action_field.name));
                }
            }
            scroll.end();
        }
        general_keymap_section.end();
    }
}

fn drawTouchControlsSettingSection(ui: *UI, app_state: *AppState) void {
    drawContentSectionHeader(ui, "Touch Controls");
    const general_section = drawContentSection(ui, .{});
    {
        const row = ui.row(.{
            .sizing = .{ .w = .grow, .h = .fit },
            .child_alignment = .{ .y = .center },
        });
        {
            _ = ui.label(.{
                .text = "Hide touch controls when gamepad is connected",
                .font_size = theme.LABEL_FONT,
                .color = theme.text_primary,
            });
            _ = ui.spacer(.{ .sizing = .grow });
            app_state.settings.hide_android_onscreen_controller = ui
                .toggle(.{ .value = app_state.settings.hide_android_onscreen_controller, .size = 22 })
                .value();
        }
        row.end();

        const edit_row = ui.row(.{
            .sizing = .{ .w = .grow, .h = .fit },
            .child_alignment = .{ .y = .center },
        });
        {
            _ = ui.label(.{
                .text = "Touch Controls Layout",
                .font_size = theme.LABEL_FONT,
                .color = theme.text_primary,
            });
            _ = ui.spacer(.{ .sizing = .grow });
            if (ui.button(.{
                .text = "Edit",
                .font_size = 15,
                .text_color = Color.white,
                .bg_color = theme.accent_blue,
                .hover_color = theme.accent_blue.lighten(0.12),
                .padding = .{ .left = 18, .right = 18, .top = 9, .bottom = 9 },
                .corner_radius = 6,
            }).clicked(ui.current_window.ctx)) {
                app_state.android_onscreen_controller = app_state.settings.android_onscreen_controller;
                ui.setWindowFullscreen(true);
                app_state.android_edit_mode = true;
                app_state.show_android_settings_ui = false;
            }
        }
        edit_row.end();
    }
    general_section.end();
}

fn drawGamepadDeadzoneRow(ui: *UI, app_state: *AppState) void {
    const alloc = ui.current_window.ctx.frameAlloc();
    const current: f32 = @floatFromInt(app_state.settings.gamepad_deadzone);

    const row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .{ .y = .center },
        .gap = 8,
    });
    {
        _ = ui.label(.{
            .text = "Deadzone",
            .font_size = theme.LABEL_FONT,
            .color = theme.text_primary,
        });
        _ = ui.spacer(.{ .sizing = .grow });
        _ = ui.label(.{
            .text = std.fmt.allocPrint(alloc, "{d}%", .{app_state.settings.gamepad_deadzone}) catch "?",
            .font_size = 14,
            .color = theme.text_value,
        });
    }
    row.end();

    const drag = ui.slider(.{
        .value = current,
        .min = 0.0,
        .max = 100.0,
        .step = 1.0,
        .height = 4,
        .fill_color = theme.accent_green,
        .track_color = theme.bg_hover,
        .thumb_color = theme.text_primary,
        .corner_radius = 2,
    });
    const new_val: u8 = @intFromFloat(drag.value());
    if (app_state.settings.gamepad_deadzone != new_val) {
        app_state.settings.gamepad_deadzone = new_val;
    }
}

fn drawInputDeviceSelector(ui: *UI, app_state: *AppState) void {
    if (app_state.input_devices.items.len == 1 and app_state.input_devices.items[0] == .keyboard) return;

    const selected_input_device = app_state.selectedTmpInputDevice();
    const row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .{ .y = .center },
        .gap = 8,
    });
    {
        _ = ui.label(.{
            .text = "Input Device",
            .font_size = theme.LABEL_FONT,
            .color = theme.text_primary,
        });

        _ = ui.spacer(.{ .sizing = .grow });

        const input_devices = ui.combobox(InputDevice, .{
            .id = switch (app_state.settings.selected_player) {
                .one => "player1",
                .two => "player2",
            },
            .filtered_options = app_state.input_devices.items,
            .selected = selected_input_device.*,
            .sizing = .{ .w = .fixed(240), .h = .fit },
            .bg_color = theme.bg_hover,
            .bg_color_on_hover = theme.bg_hover,
            .border = .{ .width = .outside(1), .color = theme.border_dim.toClay() },
            .border_color_on_open = theme.border_open,
            .border_color_on_hover = theme.border,
            .text_color = theme.text_primary,
            .float_panel = .{
                .bg_color = theme.bg_section,
                .border_color = theme.border_open,
            },
            .item = .{
                .bg_color_on_hover = theme.bg_hover,
                .bg_color = .{ .r = 0, .g = 0, .b = 0, .a = 0 },
                .text_color = theme.text_secondary,
                .text_color_on_hover = theme.text_accent,
            },
        });

        const new_selected_input_device = input_devices.selected();
        if (!selected_input_device.eql(new_selected_input_device)) {
            selected_input_device.* = new_selected_input_device;
            app_state.settings.capture_binding = null;
            app_state.settings.capture_gamepad_binding = null;
        }
    }
    row.end();
}

fn drawControllerPlayerSelector(ui: *UI, app_state: *AppState) void {
    const row = ui.row(.{
        .sizing = .{ .w = .fit, .h = .fit },
        .gap = 6,
        .child_alignment = .{ .y = .center },
    });
    {
        inline for (@typeInfo(Player).@"enum".fields) |player_field| {
            const player = @field(Player, player_field.name);
            const is_selected = app_state.settings.selected_player == player;
            if (ui.button(.{
                .text = player.displayName(),
                .font_size = 14,
                .text_color = if (is_selected) nav_active_text else theme.text_secondary,
                .bg_color = if (is_selected) nav_active_bg else theme.bg_hover,
                .hover_color = if (is_selected) nav_active_bg else nav_hover_bg,
                .padding = .{ .left = 10, .right = 10, .top = 6, .bottom = 6 },
                .corner_radius = 3,
            }).clicked(ui.current_window.ctx)) {
                app_state.settings.selected_player = player;
                app_state.settings.capture_binding = null;
                app_state.settings.capture_general_binding = null;
                app_state.settings.capture_gamepad_binding = null;
            }
        }
    }
    row.end();
}

fn updateControllerBindingCapture(ui: *UI, app_state: *AppState) void {
    const target = app_state.settings.capture_binding orelse return;
    const key = ui.getPressedKey() orelse return;
    if (key == .UNKNOWN) return;

    const player_bindings = app_state.settings.controller_bindings.forPlayer(target.player);
    if (player_bindings.get(target.action) != key) {
        player_bindings.set(target.action, key);
    }
    app_state.settings.capture_binding = null;
}

fn updateGamepadBindingCapture(ui: *UI, app_state: *AppState) void {
    const target = app_state.settings.capture_gamepad_binding orelse return;
    const pressed = ui.getPressedGamepadButton() orelse return;

    const player_bindings = app_state.settings.gamepad_bindings.forPlayer(target.player);
    if (player_bindings.get(target.action) != pressed.btn) {
        player_bindings.set(target.action, pressed.btn);
    }
    app_state.settings.capture_gamepad_binding = null;
}

fn updateGeneralBindingCapture(ui: *UI, app_state: *AppState) void {
    const action = app_state.settings.capture_general_binding orelse return;
    const key = ui.getPressedKey() orelse return;
    if (key == .UNKNOWN) return;

    if (app_state.settings.general_bindings.get(action) != key) {
        app_state.settings.general_bindings.set(action, key);
    }
    app_state.settings.capture_general_binding = null;
}

fn drawControllerBindingOverlay(ui: *UI, app_state: *AppState) void {
    const img_w = app_state.controller_img.w();
    const img_h = app_state.controller_img.h();
    const display_w: f32 = @min(500.0, @as(f32, @floatFromInt(img_w)));
    const display_h = display_w * @as(f32, @floatFromInt(img_h)) / @as(f32, @floatFromInt(img_w));

    const root = ui.column(.{ .child_alignment = .center });
    {
        _ = ui.canvas(.{
            .sizing = .{ .w = .fixed(display_w), .h = .fixed(display_h) },
            .pixel_format = app_state.controller_img.format(),
            .pixels = app_state.controller_img.pixels(),
            .w = img_w,
            .h = img_h,
        });

        const player = app_state.settings.selected_player;
        inline for (@typeInfo(ControllerAction).@"enum".fields) |action_field| {
            const action = @field(ControllerAction, action_field.name);
            drawControllerBindingField(ui, app_state, root.id, player, action);
        }
    }
    root.end();
}

fn drawControllerBindingField(
    ui: *UI,
    app_state: *AppState,
    parent_id: clay.ElementId,
    player: Player,
    action: ControllerAction,
) void {
    const uses_gamepad = if (builtin.abi.isAndroid()) true else app_state.selectedTmpInputDevice().* == .gamepad;

    const position = controllerBindingPosition(action);
    const target = ControllerBindingTarget{ .player = player, .action = action };

    const is_capturing_kb = !uses_gamepad and isControllerBindingTarget(app_state.settings.capture_binding, target);
    const is_capturing_gp = uses_gamepad and isControllerBindingTarget(app_state.settings.capture_gamepad_binding, target);
    const is_capturing = is_capturing_kb or is_capturing_gp;

    const label: []const u8 = if (is_capturing_gp)
        "Press btn"
    else if (is_capturing_kb)
        "Press key"
    else if (uses_gamepad)
        app_state.settings.gamepad_bindings.forPlayer(player).get(action).displayName()
    else
        app_state.settings.controller_bindings.forPlayer(player).get(action).keyName();

    const float = ui.float(.{
        .attach_to = .to_element_with_id,
        .parentId = parent_id.id,
        .attach_points = .{ .element = .center_center, .parent = .left_top },
        .offset = .{ .x = position.x, .y = position.y },
        .z_index = 20,
        .clip_to = .to_attached_parent,
    });
    {
        const btn = ui.button(.{
            .sizing = .{ .w = .fitMinMax(.{ .min = 50, .max = 65 }), .h = .fit },
            .padding = .all(4),
            .bg_color = if (is_capturing) theme.bg_active.withAlpha(0.85) else theme.bg_panel,
            .corner_radius = 0,
            .border = .{ .color = (if (is_capturing) theme.border_selected else theme.border).toClay(), .width = .outside(1) },
            .text = label,
            .font_size = 14,
            .tooltip = if (action == .select)
                .{ .text = "Select", .text_size = 16 }
            else if (action == .start)
                .{ .text = "Start", .text_size = 16 }
            else
                null,
        });
        if (btn.clicked(ui.current_window.ctx)) {
            if (uses_gamepad) {
                app_state.settings.capture_gamepad_binding = target;
                app_state.settings.capture_binding = null;
            } else {
                app_state.settings.capture_binding = target;
                app_state.settings.capture_gamepad_binding = null;
            }
            app_state.settings.capture_general_binding = null;
        }
    }
    float.end();
}

fn isControllerBindingTarget(current: ?ControllerBindingTarget, target: ControllerBindingTarget) bool {
    const active = current orelse return false;
    return active.player == target.player and active.action == target.action;
}

fn controllerBindingPosition(action: ControllerAction) clay.Vector2 {
    if (builtin.abi.isAndroid()) {
        return switch (action) {
            .up => .{ .x = 220, .y = 45 },
            .down => .{ .x = 220, .y = 195 },
            .left => .{ .x = 140, .y = 120 },
            .right => .{ .x = 310, .y = 120 },
            .select => .{ .x = 335, .y = 180 },
            .start => .{ .x = 400, .y = 180 },
            .b => .{ .x = 485, .y = 210 },
            .a => .{ .x = 560, .y = 210 },
        };
    } else {
        return switch (action) {
            .up => .{ .x = 95, .y = 45 },
            .down => .{ .x = 95, .y = 195 },
            .left => .{ .x = 10, .y = 120 },
            .right => .{ .x = 180, .y = 120 },
            .select => .{ .x = 205, .y = 180 },
            .start => .{ .x = 275, .y = 180 },
            .b => .{ .x = 360, .y = 210 },
            .a => .{ .x = 430, .y = 210 },
        };
    }
}

fn drawGeneralBindingRow(ui: *UI, app_state: *AppState, action: GeneralAction) void {
    const is_capturing = isGeneralBindingTarget(app_state.settings.capture_general_binding, action);
    const key = app_state.settings.general_bindings.get(action);
    const label = if (is_capturing) "Press key" else key.keyName();

    const row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .{ .y = .center },
        .gap = 10,
    });
    {
        _ = ui.label(.{
            .text = action.displayName(),
            .font_size = theme.LABEL_FONT,
            .color = theme.text_primary,
        });

        _ = ui.spacer(.{ .sizing = .grow });

        const btn = ui.button(.{
            .sizing = .{ .w = .fitMinMax(.{ .min = 95, .max = 130 }), .h = .fit },
            .padding = .{ .left = 10, .right = 10, .top = 6, .bottom = 6 },
            .bg_color = if (is_capturing) theme.bg_active else theme.bg_panel,
            .hover_color = if (is_capturing) theme.bg_active else theme.bg_hover,
            .text_color = if (is_capturing) theme.text_accent else theme.text_primary,
            .corner_radius = 0,
            .border = .{ .color = (if (is_capturing) theme.border_selected else theme.border).toClay(), .width = .outside(1) },
            .text = label,
            .font_size = 16,
        });
        if (btn.clicked(ui.current_window.ctx)) {
            app_state.settings.capture_general_binding = action;
            app_state.settings.capture_binding = null;
        }
    }
    row.end();
}

fn isGeneralBindingTarget(current: ?GeneralAction, action: GeneralAction) bool {
    const active = current orelse return false;
    return active == action;
}

fn drawEmulationSpeedRow(ui: *UI, app_state: *AppState) void {
    const row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .{ .y = .center },
        .gap = 8,
    });
    {
        _ = ui.label(.{
            .text = "Emulation Speed",
            .font_size = theme.LABEL_FONT,
            .color = theme.text_primary,
        });

        _ = ui.spacer(.{ .sizing = .grow });

        if (app_state.isConnectedClient()) {
            _ = ui.label(.{
                .text = app_state.settings.emulation_speed.label(),
                .font_size = theme.LABEL_FONT,
                .color = theme.text_value,
            });
            row.end();
            return;
        }

        const speed_opts = ui.combobox(EmulationSpeed, .{
            .selected = app_state.settings.emulation_speed,
            .bg_color = theme.bg_hover,
            .bg_color_on_hover = theme.bg_hover,
            .border = .{ .width = .outside(1), .color = theme.border_dim.toClay() },
            .border_color_on_open = theme.border_open,
            .border_color_on_hover = theme.border,
            .text_color = theme.text_primary,
            .float_panel = .{
                .bg_color = theme.bg_section,
                .border_color = theme.border_open,
            },
            .item = .{
                .bg_color_on_hover = theme.bg_hover,
                .bg_color = .{ .r = 0, .g = 0, .b = 0, .a = 0 },
                .text_color = theme.text_secondary,
                .text_color_on_hover = theme.text_accent,
            },
        });

        const selected_speed = speed_opts.selected();
        if (app_state.settings.emulation_speed != selected_speed) {
            app_state.setEmulationSpeed(selected_speed);
        }
    }
    row.end();
}

fn drawAspectRatioRow(ui: *UI, app_state: *AppState) void {
    const row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .{ .y = .center },
        .gap = 8,
    });
    {
        _ = ui.label(.{
            .text = "Aspect Ratio",
            .font_size = theme.LABEL_FONT,
            .color = theme.text_primary,
        });

        _ = ui.spacer(.{ .sizing = .grow });

        const aspect_ratio_opts = ui.combobox(viewport.AspectRatio, .{
            .id = "aspect_ratio_combo",
            .selected = app_state.settings.aspect_ratio,
            .bg_color = theme.bg_hover,
            .bg_color_on_hover = theme.bg_hover,
            .border = .{ .width = .outside(1), .color = theme.border_dim.toClay() },
            .border_color_on_open = theme.border_open,
            .border_color_on_hover = theme.border,
            .text_color = theme.text_primary,
            .float_panel = .{
                .bg_color = theme.bg_section,
                .border_color = theme.border_open,
            },
            .item = .{
                .bg_color_on_hover = theme.bg_hover,
                .bg_color = .{ .r = 0, .g = 0, .b = 0, .a = 0 },
                .text_color = theme.text_secondary,
                .text_color_on_hover = theme.text_accent,
            },
        });

        const selected_aspect_ratio = aspect_ratio_opts.selected();
        if (app_state.settings.aspect_ratio != selected_aspect_ratio) {
            app_state.settings.aspect_ratio = selected_aspect_ratio;
        }
    }
    row.end();
}

fn drawSettingsShaderContent(ui: *UI, app_state: *AppState) void {
    if (builtin.abi.isAndroid()) {
        drawContentSectionHeader(ui, "Shader Library");
        {
            const section = drawContentSection(ui, .{});
            drawAndroidShaderDownload(ui, app_state);
            section.end();
        }
    }

    drawContentSectionHeader(ui, "Shader");
    {
        const section = drawContentSection(ui, .{});
        drawShaderPresetRow(ui, app_state);
        if (app_state.shader_loading) {
            const progress = ui.getShaderProgress("main");
            const alloc = ui.current_window.ctx.frameAlloc();
            const progress_text = if (progress.total > 0)
                std.fmt.allocPrint(
                    alloc,
                    "Compiling... {d}/{d} passes",
                    .{ progress.completed, progress.total },
                ) catch "Compiling..."
            else
                "Compiling...";
            _ = ui.label(.{
                .text = progress_text,
                .font_size = 13,
                .color = theme.accent_purple,
            });
        } else if (app_state.shader_error) |err_msg| {
            _ = ui.label(.{
                .text = err_msg,
                .font_size = 13,
                .color = theme.accent_red,
            });
        }

        const param_infos = ui.getShaderParamInfos("main");
        if (!app_state.shader_loading and param_infos.len > 0) {
            drawShaderParamsSection(ui, app_state, "Parameters", param_infos, .main);
        }
        section.end();
    }

    drawContentSectionHeader(ui, "Border Shader");
    {
        const section = drawContentSection(ui, .{});
        drawBorderShaderPresetRow(ui, app_state);
        if (app_state.border_shader_loading) {
            _ = ui.label(.{
                .text = "Compiling...",
                .font_size = 13,
                .color = theme.accent_purple,
            });
        } else if (app_state.border_shader_error) |err_msg| {
            _ = ui.label(.{
                .text = err_msg,
                .font_size = 13,
                .color = theme.accent_red,
            });
        }

        const border_param_infos = ui.getShaderParamInfos("border");
        if (!app_state.border_shader_loading and border_param_infos.len > 0) {
            drawShaderParamsSection(ui, app_state, "Border Parameters", border_param_infos, .border);
        }

        section.end();
    }
}

fn drawAndroidShaderDownload(ui: *UI, app_state: *AppState) void {
    var status = app_state.shaderDownloadStatus();
    const installed = app_state.shaderDownloadInstalled();
    const can_download = !status.active and !installed;

    const row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .{ .y = .center },
        .gap = 8,
    });
    {
        _ = ui.label(.{
            .text = "slang-shaders",
            .font_size = theme.LABEL_FONT,
            .color = theme.text_primary,
        });
        _ = ui.spacer(.{ .sizing = .grow });

        const button_text: []const u8 = if (installed) "Installed" else if (status.active) "Downloading" else "Download";
        if (ui.button(.{
            .text = button_text,
            .font_size = 15,
            .enabled = can_download,
            .text_color = if (can_download) theme.text_primary else theme.text_secondary,
            .bg_color = if (can_download) theme.bg_hover else theme.bg_panel,
            .hover_color = if (can_download) theme.border else theme.bg_panel,
            .padding = .{ .left = 10, .right = 10, .top = 5, .bottom = 5 },
        }).clicked(ui.current_window.ctx)) {
            app_state.startShaderDownload() catch |err| {
                std.log.err("failed to start shader download: {s}", .{@errorName(err)});
                if (app_state.shader_download_error) |old| app_state.alloc.free(old);
                app_state.shader_download_error = std.fmt.allocPrint(
                    app_state.alloc,
                    "Download failed: {s}",
                    .{@errorName(err)},
                ) catch null;
            };
            status = app_state.shaderDownloadStatus();
        }
    }
    row.end();

    switch (status.state) {
        .downloading => drawShaderDownloadProgress(ui, status.bytes, status.total_bytes),
        .extracting => {
            _ = ui.label(.{
                .text = "Extracting...",
                .font_size = 13,
                .color = theme.accent_purple,
            });
        },
        .failed => if (status.error_message) |msg| {
            _ = ui.label(.{
                .text = msg,
                .font_size = 13,
                .color = theme.accent_red,
            });
        },
        else => {},
    }
}

fn drawShaderDownloadProgress(ui: *UI, bytes: u64, total_bytes: u64) void {
    const alloc = ui.current_window.ctx.frameAlloc();
    const known_total = total_bytes != shader_download.unknown_total and total_bytes > 0;

    const progress_text = if (known_total)
        std.fmt.allocPrint(
            alloc,
            "Downloading... {s} / {s}",
            .{ utils.formatByteCount(alloc, bytes), utils.formatByteCount(alloc, total_bytes) },
        ) catch "Downloading..."
    else
        std.fmt.allocPrint(
            alloc,
            "Downloading... {s}",
            .{utils.formatByteCount(alloc, bytes)},
        ) catch "Downloading...";

    _ = ui.label(.{
        .text = progress_text,
        .font_size = 13,
        .color = theme.accent_purple,
    });
}

fn drawShaderParamsSection(
    ui: *UI,
    app_state: *AppState,
    title: []const u8,
    param_infos: []const pipeline.ParamInfo,
    target: ParamTarget,
) void {
    drawContentSectionHeader(ui, title);

    const section = drawContentSection(ui, .{
        .sizing = .{ .w = .growMinMax(.{ .max = 500 }), .h = .fit },
    });
    {
        const scroll = ui.scrollArea(.{
            .gap = 12,
            .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .max = 350 }) },
        });
        {
            for (param_infos) |info| {
                drawParamRow(ui, app_state, info, target);
            }
        }
        scroll.end();
    }
    section.end();
}

const PREVIEW_MAX_W: f32 = 320;
const PREVIEW_MAX_H: f32 = 270;
const PREVIEW_FRAME_PADDING: f32 = 2;
const PREVIEW_LABEL_HEIGHT: f32 = 22;
const PreviewColor = struct {
    r: u8,
    g: u8,
    b: u8,
};

fn createStoppedPreviewPlaceholder(alloc: std.mem.Allocator) []const u8 {
    const pixels = alloc.alloc(u8, NES_VISIBLE_PIXEL_BYTES) catch @panic("OOM");

    var y: u32 = 0;
    while (y < NES_VISIBLE_HEIGHT) : (y += 1) {
        var x: u32 = 0;
        while (x < NES_WIDTH) : (x += 1) {
            const sky: PreviewColor = if (y < 120)
                .{ .r = 78, .g = 151, .b = 198 }
            else
                .{ .r = 64, .g = 130, .b = 176 };
            setPreviewPixel(pixels, x, y, sky);
        }
    }

    fillPreviewRect(pixels, 18, 28, 28, 8, .{ .r = 220, .g = 232, .b = 228 });
    fillPreviewRect(pixels, 26, 20, 18, 8, .{ .r = 220, .g = 232, .b = 228 });
    fillPreviewRect(pixels, 154, 38, 36, 8, .{ .r = 220, .g = 232, .b = 228 });
    fillPreviewRect(pixels, 166, 30, 18, 8, .{ .r = 220, .g = 232, .b = 228 });

    fillPreviewRect(pixels, 0, 134, 96, 38, .{ .r = 48, .g = 132, .b = 88 });
    fillPreviewRect(pixels, 96, 148, 86, 24, .{ .r = 42, .g = 112, .b = 78 });
    fillPreviewRect(pixels, 182, 128, 74, 44, .{ .r = 54, .g = 144, .b = 92 });

    drawPreviewBlock(pixels, 112, 86, .{ .r = 194, .g = 126, .b = 50 });
    drawPreviewBlock(pixels, 128, 86, .{ .r = 194, .g = 126, .b = 50 });
    drawPreviewBlock(pixels, 144, 86, .{ .r = 194, .g = 126, .b = 50 });
    drawPreviewBlock(pixels, 184, 62, .{ .r = 224, .g = 181, .b = 64 });
    fillPreviewRect(pixels, 188, 66, 4, 4, .{ .r = 86, .g = 61, .b = 38 });

    fillPreviewRect(pixels, 34, 120, 44, 10, .{ .r = 90, .g = 74, .b = 55 });
    fillPreviewRect(pixels, 34, 116, 44, 4, .{ .r = 116, .g = 161, .b = 82 });
    fillPreviewRect(pixels, 172, 106, 44, 10, .{ .r = 90, .g = 74, .b = 55 });
    fillPreviewRect(pixels, 172, 102, 44, 4, .{ .r = 116, .g = 161, .b = 82 });

    fillPreviewRect(pixels, 0, 172, NES_WIDTH, 52, .{ .r = 118, .g = 74, .b = 44 });
    fillPreviewRect(pixels, 0, 172, NES_WIDTH, 8, .{ .r = 72, .g = 161, .b = 76 });
    y = 184;
    while (y < NES_VISIBLE_HEIGHT) : (y += 16) {
        var x: u32 = 0;
        while (x < NES_WIDTH) : (x += 16) {
            fillPreviewRect(pixels, x, y, 14, 2, .{ .r = 92, .g = 55, .b = 34 });
        }
    }

    fillPreviewRect(pixels, 80, 140, 12, 18, .{ .r = 213, .g = 76, .b = 54 });
    fillPreviewRect(pixels, 82, 130, 10, 10, .{ .r = 240, .g = 184, .b = 112 });
    fillPreviewRect(pixels, 78, 142, 4, 8, .{ .r = 240, .g = 184, .b = 112 });
    fillPreviewRect(pixels, 92, 142, 4, 8, .{ .r = 240, .g = 184, .b = 112 });
    fillPreviewRect(pixels, 80, 158, 5, 8, .{ .r = 48, .g = 64, .b = 118 });
    fillPreviewRect(pixels, 88, 158, 5, 8, .{ .r = 48, .g = 64, .b = 118 });
    fillPreviewRect(pixels, 84, 134, 2, 2, .{ .r = 38, .g = 32, .b = 28 });

    fillPreviewRect(pixels, 148, 156, 12, 8, .{ .r = 92, .g = 66, .b = 45 });
    fillPreviewRect(pixels, 150, 150, 8, 6, .{ .r = 132, .g = 92, .b = 54 });
    fillPreviewRect(pixels, 142, 164, 24, 4, .{ .r = 72, .g = 45, .b = 31 });

    fillPreviewRect(pixels, 10, 8, 28, 4, .{ .r = 255, .g = 232, .b = 128 });
    fillPreviewRect(pixels, 42, 8, 6, 4, .{ .r = 255, .g = 232, .b = 128 });
    fillPreviewRect(pixels, 212, 8, 34, 4, .{ .r = 255, .g = 232, .b = 128 });

    return pixels;
}

fn drawPreviewBlock(pixels: []u8, x: u32, y: u32, color: PreviewColor) void {
    fillPreviewRect(pixels, x, y, 16, 16, color);
    fillPreviewRect(pixels, x, y, 16, 2, .{ .r = 240, .g = 170, .b = 84 });
    fillPreviewRect(pixels, x, y + 14, 16, 2, .{ .r = 112, .g = 70, .b = 38 });
    fillPreviewRect(pixels, x + 14, y, 2, 16, .{ .r = 112, .g = 70, .b = 38 });
}

fn fillPreviewRect(pixels: []u8, x: u32, y: u32, w: u32, h: u32, color: PreviewColor) void {
    const x_end = @min(x + w, NES_WIDTH);
    const y_end = @min(y + h, NES_VISIBLE_HEIGHT);
    var py = y;
    while (py < y_end) : (py += 1) {
        var px = x;
        while (px < x_end) : (px += 1) {
            setPreviewPixel(pixels, px, py, color);
        }
    }
}

fn setPreviewPixel(pixels: []u8, x: u32, y: u32, color: PreviewColor) void {
    const idx: usize = (@as(usize, y) * NES_WIDTH + x) * 4;
    pixels[idx + 0] = color.r;
    pixels[idx + 1] = color.g;
    pixels[idx + 2] = color.b;
    pixels[idx + 3] = 255;
}

fn drawShaderPreview(ui: *UI, pixels: []const u8, px_w: u32, px_h: u32, aspect_ratio: viewport.AspectRatio) void {
    const frame_aspect = @as(f32, @floatFromInt(16)) / 9;
    const width_limited_height = PREVIEW_MAX_W / frame_aspect;
    const preview_w: f32 = if (width_limited_height <= PREVIEW_MAX_H) PREVIEW_MAX_W else PREVIEW_MAX_H * frame_aspect;
    const preview_h: f32 = if (width_limited_height <= PREVIEW_MAX_H) width_limited_height else PREVIEW_MAX_H;

    const center = ui.row(.{ .sizing = .{ .w = .grow, .h = .fit }, .child_alignment = .{ .x = .center } });
    {
        const wrapper = ui.column(.{
            .sizing = .{
                .w = .fixed(preview_w + PREVIEW_FRAME_PADDING),
                .h = .fixed(preview_h + PREVIEW_FRAME_PADDING + PREVIEW_LABEL_HEIGHT),
            },
            .bg_color = theme.border_dim,
            .padding = .all(1),
            .child_alignment = .center,
        });
        {
            _ = ui.label(.{ .text = "Preview", .color = theme.accent_green });
            _ = ui.canvas(.{
                .sizing = .grow,
                .pixel_format = c.SDL_PIXELFORMAT_ABGR8888,
                .pixels = pixels,
                .w = px_w,
                .h = px_h,
                .aspect_ratio = aspect_ratio,
            });
        }
        wrapper.end();
    }
    center.end();
}

fn drawShaderPresetRow(ui: *UI, app_state: *AppState) void {
    const can_clear_shader = app_state.settings.shader_preset_path != null or app_state.shader_loading;

    const header_row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .{ .y = .center },
        .gap = 8,
    });
    {
        _ = ui.label(.{
            .text = "Shader",
            .font_size = theme.LABEL_FONT,
            .color = theme.text_primary,
        });

        _ = ui.spacer(.{ .sizing = .grow });

        if (ui.button(.{
            .text = "Load",
            .font_size = 15,
            .text_color = theme.text_primary,
            .bg_color = theme.bg_hover,
            .hover_color = theme.border,
            .padding = .{ .left = 10, .right = 10, .top = 5, .bottom = 5 },
        }).clicked(ui.current_window.ctx)) {
            if (builtin.abi.isAndroid()) {
                app_state.openShaderFilePicker(.main);
            } else {
                const alloc = app_state.alloc;
                const default_location = std.process.currentPathAlloc(app_state.io, alloc) catch @panic("OOM");
                defer alloc.free(default_location);

                const default_location_z = alloc.dupeZ(u8, default_location) catch @panic("OOM");
                defer alloc.free(default_location_z);

                c.SDL_ShowOpenFileDialog(
                    shader_dialog_callback,
                    clay.anytypeToAnyopaquePtr(app_state),
                    ui.main_window.ptr,
                    &shader_filter_list,
                    shader_filter_list.len,
                    default_location_z.ptr,
                    false,
                );
            }
        }

        if (ui.button(.{
            .text = "Clear",
            .font_size = 15,
            .enabled = can_clear_shader,
            .text_color = if (can_clear_shader) theme.text_primary else theme.text_secondary,
            .bg_color = if (can_clear_shader) theme.bg_hover else theme.bg_panel,
            .hover_color = if (can_clear_shader) theme.border else theme.bg_panel,
            .padding = .{ .left = 10, .right = 10, .top = 5, .bottom = 5 },
        }).clicked(ui.current_window.ctx)) {
            if (app_state.settings.shader_preset_path) |path| {
                app_state.alloc.free(path);
                app_state.settings.shader_preset_path = null;
            }
            settings.clearShaderParamSettings(app_state.alloc, &app_state.settings.shader_params);
            app_state.should_load_shader = false;
            app_state.should_clear_shader = true;
        }
    }
    header_row.end();

    // Active preset filename.
    const active_path = ui.getShaderPresetPath("main");
    const filename = if (active_path) |p| blk: {
        const slash = std.mem.lastIndexOfScalar(u8, p, '/') orelse
            std.mem.lastIndexOfScalar(u8, p, '\\') orelse 0;
        break :blk if (slash > 0) p[slash + 1 ..] else p;
    } else "None";

    _ = ui.label(.{
        .text = filename,
        .font_size = 14,
        .color = theme.text_secondary,
    });

    if (app_state.emulation_running) {
        const shader = ui.shaderMode(.{ .id = "main" });
        drawShaderPreview(
            ui,
            app_state.framePixels(OVERSCAN_PIXEL_OFFSET, NES_VISIBLE_PIXEL_BYTES),
            NES_WIDTH,
            NES_VISIBLE_HEIGHT,
            app_state.settings.aspect_ratio,
        );
        shader.end();
    }
}

fn getParamValue(ui: *UI, target: ParamTarget, name: []const u8) f32 {
    return switch (target) {
        .main => ui.getShaderParam("main", name),
        .border => ui.getShaderParam("border", name),
    };
}

fn setParamValue(ui: *UI, target: ParamTarget, name: []const u8, value: f32) void {
    switch (target) {
        .main => ui.setShaderParam("main", name, value),
        .border => ui.setShaderParam("border", name, value),
    }
}

fn drawParamRow(ui: *UI, app_state: *AppState, info: pipeline.ParamInfo, target: ParamTarget) void {
    const alloc = ui.current_window.ctx.frameAlloc();

    // Section title: min == max == 0 (and step == 0 or null).
    // Shader authors use these entries to label groups of related params.
    const is_section = info.min == 0.0 and info.max == 0.0 and
        (info.step == null or info.step.? == 0.0);
    if (is_section) {
        const row = ui.row(.{
            .sizing = .{ .w = .grow, .h = .fit },
            .child_alignment = .{ .y = .center },
            .gap = 6,
        });
        {
            _ = ui.label(.{
                .text = info.display_name,
                .font_size = 12,
                .color = theme.accent_purple,
            });
            const sep = ui.row(.{ .sizing = .{ .w = .grow, .h = .fixed(1) } });
            sep.end();
            _ = ui.spacer(.{ .sizing = .{ .w = .fixed(0), .h = .fixed(0) } });
        }
        row.end();
        return;
    }

    // Toggle: range [0, 1] with step 1 — treat as boolean on/off switch.
    const is_toggle = info.min == 0.0 and info.max == 1.0 and
        info.step != null and info.step.? == 1.0;

    const current = getParamValue(ui, target, info.name);

    const row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .{ .y = .center },
        .gap = 8,
    });
    {
        _ = ui.label(.{
            .text = info.display_name,
            .font_size = 14,
            .color = theme.text_secondary,
        });
        _ = ui.spacer(.{ .sizing = .grow });

        if (is_toggle) {
            const tog = ui.toggle(.{
                .value = current != 0.0,
                .on_color = theme.accent_purple,
                .off_color = theme.text_muted,
                .thumb_color = theme.text_primary,
                .size = 18,
            });
            const new_val: f32 = if (tog.value()) 1.0 else 0.0;
            if (new_val != current) {
                setParamValue(ui, target, info.name, new_val);
                app_state.setShaderParamSetting(target, info.name, new_val);
            }
        } else {
            // Format value label: derive precision from step size.
            const decimals: usize = if (info.step) |s| (if (s >= 1.0) 0 else if (s >= 0.1) 1 else 2) else 2;
            const value_text = switch (decimals) {
                0 => std.fmt.allocPrint(alloc, "{d:.0}", .{current}) catch "?",
                1 => std.fmt.allocPrint(alloc, "{d:.1}", .{current}) catch "?",
                else => std.fmt.allocPrint(alloc, "{d:.2}", .{current}) catch "?",
            };
            _ = ui.label(.{
                .text = value_text,
                .font_size = 14,
                .color = theme.text_value,
            });
        }
    }
    row.end();

    if (!is_toggle) {
        // Draggable track bar below the label row.
        const drag = ui.slider(.{
            .value = current,
            .min = info.min,
            .max = info.max,
            .step = info.step,
            .height = 4,
            .fill_color = theme.accent_purple,
            .track_color = theme.bg_section,
            .thumb_color = theme.text_primary,
            .corner_radius = 2,
        });
        const new_val = drag.value();
        if (new_val != current) {
            setParamValue(ui, target, info.name, new_val);
            app_state.setShaderParamSetting(target, info.name, new_val);
        }
    }
}

const shader_filter_list: [2]c.SDL_DialogFileFilter = [_]c.SDL_DialogFileFilter{
    .{ .name = "RetroArch Shader Presets", .pattern = "slangp" },
    .{ .name = "All files", .pattern = "*" },
};

fn shader_dialog_callback(userdata: ?*anyopaque, filelist: [*c]const [*c]const u8, _: c_int) callconv(.c) void {
    const app_state = clay.anyopaquePtrToType(*AppState, userdata);

    if (filelist == null) {
        std.log.err("An error ocurred while selecting shader file: {s}\n", .{c.SDL_GetError()});
        return;
    }
    if (filelist.* == null) {
        return;
    }

    const filepath = std.mem.span(filelist.*);
    std.log.debug("User selected shader: {s}", .{filepath});

    if (app_state.settings.shader_preset_path) |old_path| {
        app_state.alloc.free(old_path);
    }
    app_state.settings.shader_preset_path = app_state.alloc.dupe(u8, filepath) catch @panic("Failed to allocate!");
    settings.clearShaderParamSettings(app_state.alloc, &app_state.settings.shader_params);
    app_state.should_load_shader = true;
    app_state.should_clear_shader = false;
}

fn drawBorderShaderPresetRow(ui: *UI, app_state: *AppState) void {
    const header_row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .{ .y = .center },
        .gap = 8,
    });
    {
        _ = ui.label(.{
            .text = "Border Shader",
            .font_size = theme.LABEL_FONT,
            .color = theme.text_primary,
        });

        _ = ui.spacer(.{ .sizing = .grow });

        const border_shader_opts = ui.combobox(BorderShaderOpts, .{
            .id = "border_shader_combo",
            .selected = app_state.settings.border_shader,
            .bg_color = theme.bg_hover,
            .bg_color_on_hover = theme.bg_hover,
            .border = .{ .width = .outside(1), .color = theme.border_dim.toClay() },
            .border_color_on_open = theme.border_open,
            .border_color_on_hover = theme.border,
            .text_color = theme.text_primary,
            .float_panel = .{
                .bg_color = theme.bg_section,
                .border_color = theme.border_open,
            },
            .item = .{
                .bg_color_on_hover = theme.bg_hover,
                .bg_color = .{ .r = 0, .g = 0, .b = 0, .a = 0 },
                .text_color = theme.text_secondary,
                .text_color_on_hover = theme.text_accent,
            },
        });

        const selected = border_shader_opts.selected();
        if (app_state.settings.border_shader != selected) {
            app_state.settings.border_shader = selected;
            settings.clearShaderParamSettings(app_state.alloc, &app_state.settings.border_shader_params);
            app_state.should_load_border_shader = selected != .none;
            app_state.should_clear_border_shader = selected == .none;
        }
    }
    header_row.end();

    const show_border_preview = app_state.settings.border_shader != .none or
        app_state.border_shader_loading;
    if (show_border_preview) {
        const preview_pixels: []const u8 = if (app_state.emulation_running)
            app_state.framePixels(OVERSCAN_PIXEL_OFFSET, NES_VISIBLE_PIXEL_BYTES)
        else
            createStoppedPreviewPlaceholder(ui.current_window.ctx.frameAlloc());

        const shader = ui.shaderMode(.{
            .id = "border",
            .target = .canvas,
            .composition = .original_on_top,
        });
        drawShaderPreview(
            ui,
            preview_pixels,
            NES_WIDTH,
            NES_VISIBLE_HEIGHT,
            .@"4_3",
        );
        shader.end();
    }
}

fn file_picker_callback(userdata: ?*anyopaque, filelist: [*c]const [*c]const u8, _: c_int) callconv(.c) void {
    if (filelist == null) {
        std.log.err("An error ocurred while selecting the file: {s}\n", .{c.SDL_GetError()});
        return;
    }
    if (filelist.* == null) { // A pointer to NULL, the user either didn't choose any file or canceled the dialog.
        return;
    }
    const data = clay.anyopaquePtrToType(*FilePickerCallbackData, userdata);
    const ui = data.ui;
    const app_state = data.app_state;
    defer ui.main_window.ctx.persistent_arena.allocator().destroy(data);

    const filepath = std.mem.span(filelist.*);
    std.log.debug("User selected file: {s}", .{filepath});
    app_state.loadRom(filepath) catch |err| std.debug.panic("Failed to load selected ROM: {any}\n", .{err});

    if (builtin.abi.isAndroid()) ui.setWindowFullscreen(true);
}
