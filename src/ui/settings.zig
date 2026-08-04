const std = @import("std");

const bindings = @import("bindings.zig");
const viewport = @import("core/viewport.zig");
const AppState = @import("state.zig").AppState;
pub const BorderShaderOpts = @import("../shaders/builtin.zig").BorderShader;

const EmulatorSettings = AppState.EmulatorSettings;
const OnScreenController = AppState.OnScreenController;
const OnScreenControllerLayout = OnScreenController.Layout;

const SETTINGS_FILENAME = "config.json";

pub const EmulationSpeed = enum {
    half,
    normal,
    double,
    triple,
    quadruple,

    pub fn label(self: @This()) []const u8 {
        return switch (self) {
            .half => "0.5x",
            .normal => "1x",
            .double => "2x",
            .triple => "3x",
            .quadruple => "4x",
        };
    }

    pub fn multiplier(self: @This()) f32 {
        return switch (self) {
            .half => 0.5,
            .normal => 1.0,
            .double => 2.0,
            .triple => 3.0,
            .quadruple => 4.0,
        };
    }
};

pub const ParamTarget = enum {
    main,
    border,
};

pub const ShaderParamSetting = struct {
    name: []const u8,
    value: f32,
};

pub const SettingsConfig = struct {
    version: u32 = 1,
    aspect_ratio: viewport.AspectRatio = .@"4_3",
    shader_preset_path: ?[]const u8 = null,
    shader_params: []const ShaderParamSetting = &.{},
    border_shader: BorderShaderOpts = .none,
    border_shader_params: []const ShaderParamSetting = &.{},
    hide_mouse_on_inactivity: bool = false,
    vsync: bool = true,
    emulation_speed: EmulationSpeed = .normal,
    controller_bindings: bindings.ControllerKeyBindings = .{},
    general_bindings: bindings.GeneralKeyBindings = .{},
    gamepad_bindings: bindings.GamepadKeyBindings = .{},
    gamepad_deadzone: u8 = 25,
    show_home_screen_snow_effect: bool = true,
    hide_android_onscreen_controller: bool = false,
    android_onscreen_controller: OnScreenControllerConfig = .{},
};

const OnScreenControllerConfig = struct {
    portrait: ?OnScreenControllerLayout = null,
    landscape: ?OnScreenControllerLayout = null,

    fn fromRuntime(controller: OnScreenController) @This() {
        return .{
            .portrait = controller.portrait,
            .landscape = controller.landscape,
        };
    }

    fn toRuntime(self: @This()) OnScreenController {
        return .{
            .portrait = if (self.portrait) |portrait| portrait else .{},
            .landscape = if (self.landscape) |landscape| landscape else .{},
        };
    }
};

pub fn load(alloc: std.mem.Allocator, io: std.Io, config_dir: ?[]const u8, settings: *EmulatorSettings) !void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    const dir = config_dir orelse return;
    const config_path = try std.fs.path.join(arena_alloc, &.{ dir, SETTINGS_FILENAME });

    const file = std.Io.Dir.openFileAbsolute(io, config_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return,
        else => return err,
    };
    defer file.close(io);

    var file_reader = file.reader(io, &.{});
    const json_bytes = try file_reader.interface.allocRemaining(arena_alloc, .limited(256 * 1024));
    const parsed = try std.json.parseFromSlice(SettingsConfig, arena_alloc, json_bytes, .{
        .ignore_unknown_fields = true,
    });
    defer parsed.deinit();

    try applyConfig(alloc, settings, parsed.value);
}

pub fn save(alloc: std.mem.Allocator, io: std.Io, config_dir: ?[]const u8, settings: EmulatorSettings) !void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    const dir = config_dir orelse return;
    try std.Io.Dir.cwd().createDirPath(io, dir);

    const config_path = try std.fs.path.join(arena_alloc, &.{ dir, SETTINGS_FILENAME });
    const json_bytes = try std.json.Stringify.valueAlloc(arena_alloc, configFromSettings(settings), .{
        .whitespace = .indent_2,
        .emit_null_optional_fields = false,
    });

    const file = try std.Io.Dir.createFileAbsolute(io, config_path, .{});
    defer file.close(io);
    try file.writeStreamingAll(io, json_bytes);
}

fn applyConfig(alloc: std.mem.Allocator, settings: *EmulatorSettings, config: SettingsConfig) !void {
    inline for (std.meta.fields(SettingsConfig)) |field| {
        if (@hasField(EmulatorSettings, field.name)) {
            try applyConfigField(alloc, &@field(settings, field.name), @field(config, field.name));
        }
    }

    clearParamsWithoutPreset(alloc, settings);
}

fn configFromSettings(settings: EmulatorSettings) SettingsConfig {
    var config: SettingsConfig = .{};
    inline for (std.meta.fields(SettingsConfig)) |field| {
        if (@hasField(EmulatorSettings, field.name)) {
            @field(config, field.name) = configFieldValue(field.type, @field(settings, field.name));
        }
    }
    return config;
}

fn configFieldValue(comptime T: type, value: anytype) T {
    if (T == []const ShaderParamSetting and @TypeOf(value) == std.ArrayList(ShaderParamSetting)) {
        return value.items;
    }
    if (T == OnScreenControllerConfig and @TypeOf(value) == OnScreenController) {
        return OnScreenControllerConfig.fromRuntime(value);
    }

    return value;
}

fn applyConfigField(alloc: std.mem.Allocator, dest: anytype, source: anytype) !void {
    const Dest = @typeInfo(@TypeOf(dest)).pointer.child;
    const Source = @TypeOf(source);

    if (Dest == ?[]u8 and Source == ?[]const u8) {
        try replaceOptionalOwnedString(alloc, dest, source);
    } else if (Dest == std.ArrayList(ShaderParamSetting) and Source == []const ShaderParamSetting) {
        try replaceShaderParamSettings(alloc, dest, source);
    } else if (Dest == OnScreenController and Source == OnScreenControllerConfig) {
        dest.* = source.toRuntime();
    } else {
        dest.* = source;
    }
}

fn clearParamsWithoutPreset(alloc: std.mem.Allocator, settings: *EmulatorSettings) void {
    if (settings.shader_preset_path == null) {
        clearShaderParamSettings(alloc, &settings.shader_params);
    }
    if (settings.border_shader == .none) {
        clearShaderParamSettings(alloc, &settings.border_shader_params);
    }
}

pub fn setShaderParamSetting(
    alloc: std.mem.Allocator,
    param_settings: *std.ArrayList(ShaderParamSetting),
    name: []const u8,
    value: f32,
) !void {
    for (param_settings.items) |*item| {
        if (std.mem.eql(u8, item.name, name)) {
            item.value = value;
            return;
        }
    }

    const owned_name = try alloc.dupe(u8, name);
    errdefer alloc.free(owned_name);

    try param_settings.append(alloc, .{
        .name = owned_name,
        .value = value,
    });
}

fn replaceOptionalOwnedString(alloc: std.mem.Allocator, dest: *?[]u8, source: ?[]const u8) !void {
    if (dest.*) |old| {
        alloc.free(old);
        dest.* = null;
    }

    if (source) |value| {
        dest.* = try alloc.dupe(u8, value);
    }
}

pub fn clearShaderParamSettings(alloc: std.mem.Allocator, settings: *std.ArrayList(ShaderParamSetting)) void {
    for (settings.items) |item| {
        alloc.free(item.name);
    }
    settings.clearRetainingCapacity();
}

fn replaceShaderParamSettings(
    alloc: std.mem.Allocator,
    dest: *std.ArrayList(ShaderParamSetting),
    source: []const ShaderParamSetting,
) !void {
    clearShaderParamSettings(alloc, dest);
    errdefer clearShaderParamSettings(alloc, dest);

    for (source) |item| {
        const owned_name = try alloc.dupe(u8, item.name);
        errdefer alloc.free(owned_name);

        try dest.append(alloc, .{
            .name = owned_name,
            .value = item.value,
        });
    }
}
