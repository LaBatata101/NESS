const std = @import("std");
const builtin = @import("builtin");
const raster = @import("font_raster");

const c = @import("../../root.zig").c;
const clay = @import("clay.zig");
pub const widgets = @import("widgets.zig");
const Color = @import("color.zig").Color;
const sdlError = @import("../../utils/sdl.zig").sdlError;
const vulkan = @import("../../root.zig").vulkan;
const FPSManager = @import("../../render.zig").FPSManager;
const shaders = @import("shaders.zig");
const utils = @import("viewport.zig");
const Optional = @import("../../utils/types.zig").Optional;
const android = @import("../../utils/android.zig");
const pipeline = @import("../../shaders/pipeline.zig");
const GamepadButton = @import("../bindings.zig").GamepadButton;
const ControllerAction = @import("../bindings.zig").ControllerAction;
const ControllerButton = @import("../../controller.zig").ControllerButton;
const env = @import("../../env.zig");

const PIXELOID_FONT = @embedFile("pixeloid_font");
const APP_ICON = @embedFile("app_icon");

fn handleClayError(error_data: clay.ErrorData) callconv(.c) void {
    std.log.err("Clay Error: {s}\n", .{error_data.error_text.chars[0..@intCast(error_data.error_text.length)]});
}

const FontUserData = struct {
    font_cache: *FontCache,
    scale: *const f32,
    measure_cache: *std.AutoHashMap(u64, clay.Dimensions),

    const MEASURE_CACHE_MAX_ENTRIES = 4096;

    fn effectiveScale(self: *const @This()) f32 {
        return self.scale.*;
    }

    fn measureCacheKey(self: *const @This(), text: []const u8, font_size: u16) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(text);
        std.hash.autoHash(&hasher, font_size);
        std.hash.autoHash(&hasher, @as(u32, @bitCast(self.effectiveScale())));
        return hasher.final();
    }

    fn measure(self: *const @This(), text: []const u8, font_size: u16) clay.Dimensions {
        const key = self.measureCacheKey(text, font_size);
        if (self.measure_cache.get(key)) |dims| return dims;

        self.font_cache.setLogicalSize(font_size, self.effectiveScale()) catch @panic("Failed to resize UI font");
        const measured = self.font_cache.face.measureString(text) catch @panic("Failed to measure UI text");

        const scale = self.effectiveScale();
        const dims: clay.Dimensions = .{
            .w = @floatCast(measured.width / scale),
            .h = @floatCast(measured.height / scale),
        };

        if (self.measure_cache.count() >= MEASURE_CACHE_MAX_ENTRIES) {
            self.measure_cache.clearRetainingCapacity();
        }

        self.measure_cache.put(key, dims) catch @panic("OOM");
        return dims;
    }
};

const GlyphKey = struct {
    size_bits: u32,
    glyph_index: u32,
};

const LayoutGlyph = struct {
    glyph: raster.Glyph,
    pen_x: f32,
    baseline_y: f32,
};

const TextLayout = struct {
    glyphs: []LayoutGlyph,
};

const FontCache = struct {
    allocator: std.mem.Allocator,
    library: raster.Library,
    face: raster.Face,
    atlas: raster.Atlas,
    glyphs: std.AutoHashMap(GlyphKey, raster.Glyph),
    layouts: std.AutoHashMap(u64, TextLayout),
    texture: ?*c.SDL_GPUTexture = null,
    texture_size: u32 = 0,
    current_size_bits: u32,
    uploaded_generation: usize = 0,

    const Self = @This();
    const INITIAL_ATLAS_SIZE: u32 = 1024;
    const MAX_LAYOUTS: usize = 4096;
    const COMMON_FONT_SIZES = [_]u16{ 12, 13, 14, 15, 16, 18, 19, 20, 25, 42 };

    fn init(allocator: std.mem.Allocator, io: std.Io, device: ?*c.SDL_GPUDevice, initial_scale: f32) !*Self {
        const self = try allocator.create(Self);
        errdefer allocator.destroy(self);

        var library = try raster.Library.init(allocator, io);
        errdefer library.deinit();
        var face = try raster.Face.initMemory(library, PIXELOID_FONT, 0, .{
            .size = desiredSize(16, initial_scale),
        });
        errdefer face.deinit();
        var atlas = try raster.Atlas.init(allocator, INITIAL_ATLAS_SIZE, .grayscale);
        errdefer atlas.deinit(allocator);

        self.* = .{
            .allocator = allocator,
            .library = library,
            .face = face,
            .atlas = atlas,
            .glyphs = .init(allocator),
            .layouts = .init(allocator),
            .current_size_bits = @bitCast(desiredSize(16, initial_scale).points),
        };
        errdefer self.glyphs.deinit();
        errdefer self.layouts.deinit();
        errdefer if (self.texture) |texture| c.SDL_ReleaseGPUTexture(device, texture);

        try self.prewarm(initial_scale);
        try self.recreateAtlasTextureIfChanged(device);
        return self;
    }

    fn deinit(self: *Self, device: ?*c.SDL_GPUDevice) void {
        if (self.texture) |texture| c.SDL_ReleaseGPUTexture(device, texture);
        self.clearLayouts();
        self.layouts.deinit();
        self.glyphs.deinit();
        self.atlas.deinit(self.allocator);
        self.face.deinit();
        self.library.deinit();
        self.allocator.destroy(self);
    }

    fn desiredSize(font_size: u16, scale: f32) raster.DesiredSize {
        return .{
            .points = @as(f32, @floatFromInt(font_size)) * scale,
            .xdpi = 72,
            .ydpi = 72,
        };
    }

    fn setLogicalSize(self: *Self, font_size: u16, scale: f32) !void {
        const size = desiredSize(font_size, scale);
        const size_bits: u32 = @bitCast(size.points);
        if (size_bits == self.current_size_bits) return;
        try self.face.setSize(size);
        self.current_size_bits = size_bits;
    }

    fn prewarm(self: *Self, scale: f32) !void {
        for (COMMON_FONT_SIZES) |font_size| {
            var codepoint: u32 = 0x20;
            while (codepoint <= 0x7e) : (codepoint += 1) {
                _ = try self.cacheGlyph(font_size, scale, codepoint);
            }
        }
    }

    fn layoutKey(font_size: u16, scale: f32, text: []const u8) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(text);
        std.hash.autoHash(&hasher, @as(u32, @bitCast(desiredSize(font_size, scale).points)));
        return hasher.final();
    }

    fn getLayout(self: *const Self, font_size: u16, scale: f32, text: []const u8) ?*const TextLayout {
        return self.layouts.getPtr(layoutKey(font_size, scale, text));
    }

    fn cacheTextLayout(self: *Self, font_size: u16, scale: f32, text: []const u8) !*const TextLayout {
        const key = layoutKey(font_size, scale, text);
        if (self.layouts.getPtr(key)) |layout| return layout;

        if (self.layouts.count() >= MAX_LAYOUTS) self.clearLayouts();
        try self.setLogicalSize(font_size, scale);

        const metrics = self.face.metrics();
        const view = try std.unicode.Utf8View.init(text);
        var glyphs: std.ArrayList(LayoutGlyph) = .empty;
        defer glyphs.deinit(self.allocator);
        try glyphs.ensureTotalCapacity(self.allocator, text.len);

        var pen_x: f32 = 0;
        var baseline_y: f32 = @floatCast(metrics.ascent);
        var iterator = view.iterator();
        while (iterator.nextCodepoint()) |codepoint| {
            switch (codepoint) {
                '\n' => {
                    pen_x = 0;
                    baseline_y += @floatCast(metrics.lineHeight());
                    continue;
                },
                '\r' => continue,
                else => {},
            }
            const glyph = try self.cacheGlyph(font_size, scale, codepoint);
            try glyphs.append(self.allocator, .{ .glyph = glyph, .pen_x = pen_x, .baseline_y = baseline_y });
            pen_x += @floatCast(glyph.advance_x);
        }

        try self.layouts.put(key, .{ .glyphs = try glyphs.toOwnedSlice(self.allocator) });
        return self.layouts.getPtr(key).?;
    }

    fn clearLayouts(self: *Self) void {
        var iterator = self.layouts.valueIterator();
        while (iterator.next()) |layout| self.allocator.free(layout.glyphs);
        self.layouts.clearRetainingCapacity();
    }

    fn cacheGlyph(self: *Self, font_size: u16, scale: f32, codepoint: u32) !raster.Glyph {
        try self.setLogicalSize(font_size, scale);
        const glyph_index = self.face.glyphIndex(codepoint) orelse 0;
        const key: GlyphKey = .{ .size_bits = self.current_size_bits, .glyph_index = glyph_index };
        if (self.glyphs.get(key)) |glyph| return glyph;

        const glyph = while (true) {
            break self.face.renderGlyph(self.allocator, &self.atlas, glyph_index) catch |err| switch (err) {
                error.AtlasFull => {
                    try self.atlas.grow(self.allocator, self.atlas.size * 2);
                    continue;
                },
                else => return err,
            };
        };
        try self.glyphs.put(key, glyph);
        return glyph;
    }

    fn recreateAtlasTextureIfChanged(self: *Self, device: ?*c.SDL_GPUDevice) !void {
        const generation = self.atlas.modified.load(.monotonic);
        if (self.texture != null and self.texture_size == self.atlas.size and self.uploaded_generation == generation) return;

        if (self.texture_size != self.atlas.size) {
            if (self.texture) |texture| c.SDL_ReleaseGPUTexture(device, texture);
            self.texture = sdlError(c.SDL_CreateGPUTexture(device, &.{
                .type = c.SDL_GPU_TEXTURETYPE_2D,
                .format = c.SDL_GPU_TEXTUREFORMAT_A8_UNORM,
                .width = self.atlas.size,
                .height = self.atlas.size,
                .layer_count_or_depth = 1,
                .num_levels = 1,
                .usage = c.SDL_GPU_TEXTUREUSAGE_SAMPLER,
            }));
            self.texture_size = self.atlas.size;
        }

        const transfer_buffer = sdlError(c.SDL_CreateGPUTransferBuffer(device, &.{
            .usage = c.SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,
            .size = @intCast(self.atlas.data.len),
        }));
        defer c.SDL_ReleaseGPUTransferBuffer(device, transfer_buffer);
        const map: [*]u8 = @ptrCast(@alignCast(sdlError(c.SDL_MapGPUTransferBuffer(device, transfer_buffer, false))));
        @memcpy(map[0..self.atlas.data.len], self.atlas.data);
        c.SDL_UnmapGPUTransferBuffer(device, transfer_buffer);

        const cmd = sdlError(c.SDL_AcquireGPUCommandBuffer(device));
        const copy_pass = sdlError(c.SDL_BeginGPUCopyPass(cmd));
        c.SDL_UploadToGPUTexture(
            copy_pass,
            &.{ .transfer_buffer = transfer_buffer, .pixels_per_row = self.atlas.size, .rows_per_layer = self.atlas.size },
            &.{ .texture = self.texture, .w = self.atlas.size, .h = self.atlas.size, .d = 1 },
            false,
        );
        c.SDL_EndGPUCopyPass(copy_pass);
        sdlError(c.SDL_SubmitGPUCommandBuffer(cmd));
        self.uploaded_generation = generation;
    }
};

fn measureText(text: []const u8, config: *clay.TextElementConfig, user_data: *const FontUserData) clay.Dimensions {
    return user_data.measure(text, config.font_size);
}

pub const CursorIcon = enum {
    default,
    text,
    wait,
    crosshair,
    progress,
    nwse_resize,
    nesw_resize,
    ew_resize,
    ns_resize,
    move,
    not_allowed,
    pointer,
    nw_resize,
    n_resize,
    ne_resize,
    e_resize,
    se_resize,
    s_resize,
    sw_resize,
    w_resize,
};

var requested_cursor: CursorIcon = .default;
var current_cursor: CursorIcon = .default;
var system_cursors: [@typeInfo(CursorIcon).@"enum".fields.len]?*c.SDL_Cursor = .{null} ** @typeInfo(CursorIcon).@"enum".fields.len;

pub fn setMouseCursorText(value: bool) void {
    if (value) setMouseCursor(.text);
}

pub fn setMouseCursorMove(value: bool) void {
    if (value) setMouseCursor(.move);
}

fn resetMouseCursor() void {
    requested_cursor = .default;
}

fn applyMouseCursor() void {
    if (requested_cursor == current_cursor) return;

    if (requested_cursor == .default) {
        sdlError(c.SDL_SetCursor(c.SDL_GetDefaultCursor()));
    } else {
        const cursor_id = @intFromEnum(requested_cursor);
        if (system_cursors[cursor_id] == null) {
            system_cursors[cursor_id] = sdlError(c.SDL_CreateSystemCursor(cursor_id));
        }
        sdlError(c.SDL_SetCursor(system_cursors[cursor_id]));
    }

    current_cursor = requested_cursor;
}

pub fn setMouseCursor(cursor_id: CursorIcon) void {
    requested_cursor = cursor_id;
}

fn setWindowIcon(window: ?*c.SDL_Window) void {
    const io = c.SDL_IOFromConstMem(APP_ICON, APP_ICON.len);
    const surface = c.SDL_LoadPNG_IO(io, true);
    defer c.SDL_DestroySurface(surface);

    sdlError(c.SDL_SetWindowIcon(window, surface));
}

pub const ShaderMode = struct {
    id: []const u8,
    target: Target = .viewport,
    composition: Composition = .shader_only,

    pub const Target = enum {
        /// Render into the content viewport, excluding surrounding canvas space.
        viewport,
        /// Render into the full canvas area, including surrounding canvas space.
        canvas,
    };
    pub const Composition = enum {
        /// Draw only the shader pipeline output for this canvas.
        shader_only,
        /// Draw the shader pipeline output first, then redraw the original canvas pixels over it.
        original_on_top,
    };
};

pub const ShaderModeScope = struct {
    ctx: *UIContext,

    pub fn start(ctx: *UIContext, params: ShaderMode) @This() {
        ctx.pushShaderMode(params);
        return .{ .ctx = ctx };
    }

    pub fn end(self: *const @This()) void {
        self.ctx.popShaderMode();
    }
};

const CanvasCacheItem = struct {
    texture: ?*c.SDL_GPUTexture,
    width: u32,
    height: u32,
};

const MAX_ACTIVE_TOUCHES = 5;
const FingerState = struct {
    down: bool = false,
    x: f32 = 0,
    y: f32 = 0,
};

pub const UIContext = struct {
    /// The time between the current and previous frame.
    dt: f32,
    clay_ctx: *clay.Context,

    input: InputContext,

    // Frame-ephemeral data (reset each frame)
    frame: FrameState,
    parent_stack: std.ArrayList(clay.ElementId),
    shader_mode_stack: std.ArrayList(ShaderMode),

    // Persistent widget state (survives across frames)
    widgets: WidgetStateMap,

    text_measure_cache: std.AutoHashMap(u64, clay.Dimensions),
    canvas_cache: std.AutoHashMap(u32, CanvasCacheItem),

    timers: std.AutoHashMap(u64, struct { start: u64, ms: u64 }),

    mouse_x: f32 = 0,
    mouse_y: f32 = 0,
    prev_mouse_x: f32 = 0,
    prev_mouse_y: f32 = 0,
    fingers: [MAX_ACTIVE_TOUCHES]FingerState,

    frame_arena: std.heap.ArenaAllocator,
    persistent_arena: std.heap.ArenaAllocator,
    clay_memory: []u8,

    const Self = @This();
    const TAP_MAX_DURATION_MS = 450;

    pub fn init(allocator: std.mem.Allocator, layout_dimensions: clay.Dimensions, error_handler: clay.ErrorHandler) !*Self {
        var ctx = try allocator.create(UIContext);

        const clay_mem_size = clay.minMemorySize();
        const clay_memory = try allocator.alloc(u8, clay_mem_size);
        const arena = clay.createArenaWithCapacityAndMemory(clay_memory);
        ctx.clay_ctx = clay.initialize(arena, layout_dimensions, error_handler);
        ctx.clay_memory = clay_memory;

        ctx.frame_arena = std.heap.ArenaAllocator.init(allocator);
        ctx.persistent_arena = std.heap.ArenaAllocator.init(allocator);

        const persistent_arena_alloc = ctx.persistent_arena.allocator();

        ctx.timers = .init(persistent_arena_alloc);
        ctx.parent_stack = .empty;
        ctx.frame = .{
            .text_input = .empty,
            .scroll = .{},
        };
        ctx.shader_mode_stack = .empty;

        ctx.dt = 0;
        ctx.widgets = WidgetStateMap.init(persistent_arena_alloc);
        ctx.text_measure_cache = std.AutoHashMap(u64, clay.Dimensions).init(persistent_arena_alloc);
        ctx.canvas_cache = std.AutoHashMap(u32, CanvasCacheItem).init(persistent_arena_alloc);

        ctx.input = InputContext.init();
        ctx.fingers = [_]FingerState{.{}} ** MAX_ACTIVE_TOUCHES;

        return ctx;
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator, device: ?*c.SDL_GPUDevice) void {
        self.text_measure_cache.deinit();

        var canvas_iter = self.canvas_cache.iterator();
        while (canvas_iter.next()) |item| {
            c.SDL_ReleaseGPUTexture(device, item.value_ptr.texture);
        }
        self.canvas_cache.deinit();

        self.frame_arena.deinit();
        self.persistent_arena.deinit();
        allocator.free(self.clay_memory);
        allocator.destroy(self);
    }

    fn releaseCachedTextures(self: *Self, device: ?*c.SDL_GPUDevice) void {
        self.text_measure_cache.clearRetainingCapacity();

        var canvas_iter = self.canvas_cache.iterator();
        while (canvas_iter.next()) |item| {
            c.SDL_ReleaseGPUTexture(device, item.value_ptr.texture);
        }
        self.canvas_cache.clearRetainingCapacity();
    }

    fn freeFrameArena(self: *Self) void {
        _ = self.frame_arena.reset(.free_all);
        self.frame.text_input = .empty;
        self.parent_stack = .empty;
        self.shader_mode_stack = .empty;
    }

    pub fn update(self: *Self) void {
        self.input.update(self.dt);

        const scroll_smoothing: f32 = 5.0; // Higher = faster decay (try 5.0-15.0)
        const scroll_threshold: f32 = 0.1; // Stop animating below this velocity
        // Any intentional mouse movement cancels leftover scroll velocity so it
        // doesn't bleed into a neighbouring (e.g. outer) scroll container.
        const md = self.frame.mouse_delta;
        if (md.x * md.x + md.y * md.y > 4.0) {
            self.frame.scroll.velocity_x = 0;
            self.frame.scroll.velocity_y = 0;
        }
        // Apply velocity to scroll deltas for smooth animation
        if (@abs(self.frame.scroll.velocity_x) > scroll_threshold or @abs(self.frame.scroll.velocity_y) > scroll_threshold) {
            self.frame.scroll.delta_x = self.frame.scroll.velocity_x * self.dt * 60.0;
            self.frame.scroll.delta_y = self.frame.scroll.velocity_y * self.dt * 60.0;

            const decay = @exp(-scroll_smoothing * self.dt);
            self.frame.scroll.velocity_x *= decay;
            self.frame.scroll.velocity_y *= decay;
        } else {
            // Stop scrolling when velocity is negligible
            self.frame.scroll.velocity_x = 0;
            self.frame.scroll.velocity_y = 0;
            self.frame.scroll.delta_x = 0;
            self.frame.scroll.delta_y = 0;
        }

        clay.setCurrentContext(self.clay_ctx);
        clay.setPointerState(
            .{ .x = self.mouse_x, .y = self.mouse_y },
            self.frame.mouse_down,
        );
        clay.updateScrollContainers(builtin.abi.isAndroid(), .{
            .x = self.frame.scroll.delta_x,
            .y = self.frame.scroll.delta_y,
        }, self.dt);
    }

    pub fn reset(self: *Self) void {
        _ = self.frame_arena.reset(.retain_capacity);

        self.input.advanceFrame();

        self.parent_stack = .empty;
        self.shader_mode_stack = .empty;
        self.frame.text_input = .empty;

        self.frame.mouse_pressed = false;
        self.frame.mouse_released = false;
        self.frame.mouse_delta = .{ .x = 0, .y = 0 };

        self.frame.menu_item_clicked = false;
        self.frame.hot_id = null;
    }

    pub fn frameAlloc(self: *Self) std.mem.Allocator {
        return self.frame_arena.allocator();
    }

    pub fn pushParent(self: *Self, id: clay.ElementId) void {
        self.parent_stack.append(self.frameAlloc(), id) catch
            @panic("Failed to allocate memory");
    }

    /// Pops the active parent (call this when the container closes/ends)
    pub fn popParent(self: *Self) void {
        _ = self.parent_stack.pop();
    }

    pub fn pushShaderMode(self: *Self, params: ShaderMode) void {
        self.shader_mode_stack.append(self.frameAlloc(), .{
            .id = self.frameAlloc().dupe(u8, params.id) catch @panic("OOM"),
            .target = params.target,
            .composition = params.composition,
        }) catch @panic("OOM");
    }

    pub fn popShaderMode(self: *Self) void {
        _ = self.shader_mode_stack.pop();
    }

    // TODO: find a better way of getting parents other than the immediate parent.
    /// Returns the ID of the immediate parent, or null if root
    pub fn getParent(self: *const Self) ?clay.ElementId {
        if (self.parent_stack.items.len == 0) return null;
        return self.parent_stack.items[self.parent_stack.items.len - 1];
    }

    /// Returns the ID of the grand parent, or null if root
    pub fn getGrandParent(self: *const Self) ?clay.ElementId {
        if (self.parent_stack.items.len < 2) return null;
        return self.parent_stack.items[self.parent_stack.items.len - 2];
    }

    pub fn getParentState(self: *const Self) ?*WidgetState {
        const parent_id = self.getParent() orelse return null;
        return self.getWidgetStateById(parent_id);
    }

    pub fn getWidgetStateById(self: *const Self, id: clay.ElementId) ?*WidgetState {
        return self.widgets.getPtr(id.id);
    }

    pub fn getOrCreateWidgetState(self: *Self, id: clay.ElementId, default: WidgetState) *WidgetState {
        const result = self.widgets.getOrPut(id.id) catch @panic("Failed to allocate!");
        if (!result.found_existing) {
            result.value_ptr.* = default;
        }
        return result.value_ptr;
    }

    pub fn setHotId(self: *Self) void {
        clay.onHover(*Self, self, struct {
            fn callback(element_id: clay.ElementId, _: clay.PointerData, ctx: *Self) void {
                ctx.frame.hot_id = element_id.id;
            }
        }.callback);
    }

    fn timerKey(name: []const u8) u64 {
        return std.hash.Wyhash.hash(0, name);
    }

    pub fn tickTimerId(self: *Self, id: u64, ms: u64) bool {
        const now = c.SDL_GetTicks();
        const result = self.timers.getOrPut(id) catch @panic("OOM");

        if (!result.found_existing) {
            result.value_ptr.* = .{ .start = now, .ms = ms };
            return false;
        }

        result.value_ptr.ms = ms;
        if (now - result.value_ptr.start < ms) return false;

        result.value_ptr.start = now;
        return true;
    }

    pub fn tickTimer(self: *Self, name: []const u8, ms: u64) bool {
        return self.tickTimerId(timerKey(name), ms);
    }

    pub fn setTimer(self: *Self, name: []const u8, ms: u64) void {
        self.timers.put(timerKey(name), .{ .start = c.SDL_GetTicks(), .ms = ms }) catch @panic("OOM");
    }

    pub fn removeTimer(self: *Self, name: []const u8) void {
        self.removeTimerId(timerKey(name));
    }

    pub fn removeTimerId(self: *Self, id: u64) void {
        _ = self.timers.remove(id);
    }

    pub fn hasTimerExpired(self: *Self, name: []const u8) Optional(bool) {
        const key = timerKey(name);
        const value = self.timers.get(key) orelse return .none;

        return .{ .value = c.SDL_GetTicks() - value.start >= value.ms };
    }

    fn allocWidget(self: *Self, T: type, value: T) *T {
        const w = self.frameAlloc().create(T) catch
            std.debug.panic("Failed to allocate widget: {s}", .{@typeName(T)});
        w.* = value;
        return w;
    }

    fn updateMousePos(self: *Self, motion: c.SDL_MouseMotionEvent, scale_x: f32, scale_y: f32) void {
        self.updatePointerPosition(
            motion.x * scale_x,
            motion.y * scale_y,
            motion.xrel * scale_x,
            motion.yrel * scale_y,
        );
    }

    fn updatePointerPosition(self: *Self, x: f32, y: f32, dx: f32, dy: f32) void {
        self.prev_mouse_x = self.mouse_x;
        self.prev_mouse_y = self.mouse_y;
        self.mouse_x = x;
        self.mouse_y = y;

        self.frame.mouse_delta.x += dx;
        self.frame.mouse_delta.y += dy;
    }

    fn beginPointerDown(self: *Self) void {
        self.frame.mouse_pressed = true;
        self.frame.mouse_down = true;
        self.frame.pointer_down_time_ms = c.SDL_GetTicks();
    }

    pub fn pointerReleased(self: *const Self) bool {
        if (!self.frame.mouse_released) return false;
        if (self.frame.pointer_down_time_ms == 0) return false;
        return c.SDL_GetTicks() - self.frame.pointer_down_time_ms <= TAP_MAX_DURATION_MS;
    }

    fn setFingerDown(self: *Self, finger_id: c.SDL_FingerID, value: bool) void {
        if (finger_id > MAX_ACTIVE_TOUCHES) {
            std.log.warn("FingerID: {} greater than supported MAX_ACTIVE_TOUCHES", .{finger_id});
            return;
        }

        // SDL finger ID starts at 1
        self.fingers[finger_id - 1].down = value;
    }

    fn setFingerPos(self: *Self, finger_id: c.SDL_FingerID, x: f32, y: f32) void {
        if (finger_id > MAX_ACTIVE_TOUCHES) {
            std.log.warn("FingerID: {} greater than supported MAX_ACTIVE_TOUCHES", .{finger_id});
            return;
        }

        self.fingers[finger_id - 1].x = x;
        self.fingers[finger_id - 1].y = y;
    }

    fn hasFingerDown(self: *const Self) bool {
        for (self.fingers) |finger| {
            if (finger.down) return true;
        }
        return false;
    }

    pub fn activeFingerOverElement(self: *const Self, id: clay.ElementId) bool {
        const data = clay.getElementData(id);
        if (!data.found) return false;

        for (self.fingers) |finger| {
            if (finger.down and pointInBox(.{ .x = finger.x, .y = finger.y }, data.bounding_box)) {
                return true;
            }
        }
        return false;
    }

    fn pointInBox(point: clay.Vector2, box: clay.BoundingBox) bool {
        return point.x >= box.x and point.x <= box.x + box.width and point.y >= box.y and point.y <= box.y + box.height;
    }

    fn mouseMotion(self: *const Self) bool {
        return self.mouse_x != self.prev_mouse_x and self.mouse_y != self.prev_mouse_y;
    }
};

pub const FrameState = struct {
    text_input: std.ArrayList(u8),
    /// True only on the frame the button was clicked
    mouse_pressed: bool = false,
    /// True only on the frame the button was released
    mouse_released: bool = false,
    /// True as long as the button is held
    mouse_down: bool = false,
    mouse_delta: clay.Vector2 = .{ .x = 0, .y = 0 },
    pointer_down_time_ms: u64 = 0,

    scroll: struct {
        delta_x: f32 = 0,
        delta_y: f32 = 0,
        velocity_x: f32 = 0,
        velocity_y: f32 = 0,
    },

    menu_item_clicked: bool = false,

    hot_id: ?u32 = null, // Widget under cursor
    active_id: ?u32 = null, // Widget being interacted with
    focused_id: ?clay.ElementId = null, // Widget with keyboard focus
};

pub const WidgetStateMap = std.AutoHashMap(u32, WidgetState);

pub const WidgetState = union(enum) {
    text_input: TextInputState,
    scroll: ScrollState,
    dropdown_menu: DropdownMenuState,
    submenu: SubMenuState,
    combobox: ComboboxState,
    tooltip: TooltipState,
    grid: GridState,
    slider: SliderState,
    draggable_panel: DraggablePanelState,
    resizable_panel: ResizablePanelState,

    pub const TextInputState = struct {
        buffer: std.ArrayList(u8),
        cursor_pos: usize,
        show_android_paste_toolbox: bool = false,
        initial_value_appended: bool = false,
    };
    pub const ScrollState = struct {
        offset: clay.Vector2,
        velocity: clay.Vector2,
        vertical_scrollbar_visible: bool = false,
        horizontal_scrollbar_visible: bool = false,
    };
    pub const DropdownMenuState = struct {
        is_open: bool,
    };
    pub const SubMenuState = struct {
        is_open: bool,
    };
    pub const ComboboxState = struct {
        is_open: bool,
        selected_key: u32,
    };
    pub const TooltipState = struct {
        visible: bool = false,
    };
    pub const GridState = struct {
        items_per_row: usize,
    };
    pub const SliderState = struct {
        dragging: bool = false,
    };
    pub const DraggablePanelState = struct {
        offset: clay.Vector2,
        dragging: bool = false,
    };
    pub const ResizablePanelState = struct {
        size: ?clay.Dimensions = null,
        initial_size: ?clay.Dimensions = null,
        resizing: bool = false,
        edges: struct {
            left: bool = false,
            right: bool = false,
            top: bool = false,
            bottom: bool = false,
        } = .{},
    };
};

const DrawCall = struct {
    texture: ?*c.SDL_GPUTexture,
    index_offset: u32,
    index_count: u32,
    clip_rect: ?c.SDL_Rect,
    kind: DrawKind,
};

const DrawKind = enum { ui, text };

const TextureUpload = struct {
    texture: ?*c.SDL_GPUTexture,
    pixels: []const u8,
    width: u32,
    height: u32,
    pixel_format: c.SDL_PixelFormat,

    fn byteSize(self: TextureUpload) u32 {
        const bytes_per_pixel: u32 = @intCast(c.SDL_GetPixelFormatDetails(self.pixel_format).*.bytes_per_pixel);
        return self.width * self.height * bytes_per_pixel;
    }
};

pub const Key = enum(u16) {
    UNKNOWN = 0,

    A = 4,
    B = 5,
    C = 6,
    D = 7,
    E = 8,
    F = 9,
    G = 10,
    H = 11,
    I = 12,
    J = 13,
    K = 14,
    L = 15,
    M = 16,
    N = 17,
    O = 18,
    P = 19,
    Q = 20,
    R = 21,
    S = 22,
    T = 23,
    U = 24,
    V = 25,
    W = 26,
    X = 27,
    Y = 28,
    Z = 29,

    N_1 = 30,
    N_2 = 31,
    N_3 = 32,
    N_4 = 33,
    N_5 = 34,
    N_6 = 35,
    N_7 = 36,
    N_8 = 37,
    N_9 = 38,
    N_0 = 39,

    RETURN = 40,
    ESCAPE = 41,
    BACKSPACE = 42,
    TAB = 43,
    SPACE = 44,

    MINUS = 45,
    EQUALS = 46,
    LEFTBRACKET = 47,
    RIGHTBRACKET = 48,
    BACKSLASH = 49,
    NONUSHASH = 50,
    SEMICOLON = 51,
    APOSTROPHE = 52,
    GRAVE = 53,
    COMMA = 54,
    PERIOD = 55,
    SLASH = 56,

    CAPSLOCK = 57,

    F1 = 58,
    F2 = 59,
    F3 = 60,
    F4 = 61,
    F5 = 62,
    F6 = 63,
    F7 = 64,
    F8 = 65,
    F9 = 66,
    F10 = 67,
    F11 = 68,
    F12 = 69,

    PRINTSCREEN = 70,
    SCROLLLOCK = 71,
    PAUSE = 72,
    INSERT = 73,
    HOME = 74,
    PAGEUP = 75,
    DELETE = 76,
    END = 77,
    PAGEDOWN = 78,
    RIGHT = 79,
    LEFT = 80,
    DOWN = 81,
    UP = 82,

    NUMLOCKCLEAR = 83,
    KP_DIVIDE = 84,
    KP_MULTIPLY = 85,
    KP_MINUS = 86,
    KP_PLUS = 87,
    KP_ENTER = 88,
    KP_1 = 89,
    KP_2 = 90,
    KP_3 = 91,
    KP_4 = 92,
    KP_5 = 93,
    KP_6 = 94,
    KP_7 = 95,
    KP_8 = 96,
    KP_9 = 97,
    KP_0 = 98,
    KP_PERIOD = 99,

    NONUSBACKSLASH = 100,
    APPLICATION = 101,
    POWER = 102,
    KP_EQUALS = 103,
    F13 = 104,
    F14 = 105,
    F15 = 106,
    F16 = 107,
    F17 = 108,
    F18 = 109,
    F19 = 110,
    F20 = 111,
    F21 = 112,
    F22 = 113,
    F23 = 114,
    F24 = 115,
    EXECUTE = 116,
    HELP = 117,
    MENU = 118,
    SELECT = 119,
    STOP = 120,
    AGAIN = 121,
    UNDO = 122,
    CUT = 123,
    COPY = 124,
    PASTE = 125,
    FIND = 126,
    MUTE = 127,
    VOLUMEUP = 128,
    VOLUMEDOWN = 129,
    KP_COMMA = 133,
    KP_EQUALSAS400 = 134,

    INTERNATIONAL1 = 135,
    INTERNATIONAL2 = 136,
    INTERNATIONAL3 = 137,
    INTERNATIONAL4 = 138,
    INTERNATIONAL5 = 139,
    INTERNATIONAL6 = 140,
    INTERNATIONAL7 = 141,
    INTERNATIONAL8 = 142,
    INTERNATIONAL9 = 143,
    LANG1 = 144,
    LANG2 = 145,
    LANG3 = 146,
    LANG4 = 147,
    LANG5 = 148,
    LANG6 = 149,
    LANG7 = 150,
    LANG8 = 151,
    LANG9 = 152,

    ALTERASE = 153,
    SYSREQ = 154,
    CANCEL = 155,
    CLEAR = 156,
    PRIOR = 157,
    RETURN2 = 158,
    SEPARATOR = 159,
    OUT = 160,
    OPER = 161,
    CLEARAGAIN = 162,
    CRSEL = 163,
    EXSEL = 164,

    KP_00 = 176,
    KP_000 = 177,
    THOUSANDSSEPARATOR = 178,
    DECIMALSEPARATOR = 179,
    CURRENCYUNIT = 180,
    CURRENCYSUBUNIT = 181,
    KP_LEFTPAREN = 182,
    KP_RIGHTPAREN = 183,
    KP_LEFTBRACE = 184,
    KP_RIGHTBRACE = 185,
    KP_TAB = 186,
    KP_BACKSPACE = 187,
    KP_A = 188,
    KP_B = 189,
    KP_C = 190,
    KP_D = 191,
    KP_E = 192,
    KP_F = 193,
    KP_XOR = 194,
    KP_POWER = 195,
    KP_PERCENT = 196,
    KP_LESS = 197,
    KP_GREATER = 198,
    KP_AMPERSAND = 199,
    KP_DBLAMPERSAND = 200,
    KP_VERTICALBAR = 201,
    KP_DBLVERTICALBAR = 202,
    KP_COLON = 203,
    KP_HASH = 204,
    KP_SPACE = 205,
    KP_AT = 206,
    KP_EXCLAM = 207,
    KP_MEMSTORE = 208,
    KP_MEMRECALL = 209,
    KP_MEMCLEAR = 210,
    KP_MEMADD = 211,
    KP_MEMSUBTRACT = 212,
    KP_MEMMULTIPLY = 213,
    KP_MEMDIVIDE = 214,
    KP_PLUSMINUS = 215,
    KP_CLEAR = 216,
    KP_CLEARENTRY = 217,
    KP_BINARY = 218,
    KP_OCTAL = 219,
    KP_DECIMAL = 220,
    KP_HEXADECIMAL = 221,

    LCTRL = 224,
    LSHIFT = 225,
    LALT = 226,
    LGUI = 227,
    RCTRL = 228,
    RSHIFT = 229,
    RALT = 230,
    RGUI = 231,

    MODE = 257,

    SLEEP = 258,
    WAKE = 259,

    CHANNEL_INCREMENT = 260,
    CHANNEL_DECREMENT = 261,

    MEDIA_PLAY = 262,
    MEDIA_PAUSE = 263,
    MEDIA_RECORD = 264,
    MEDIA_FAST_FORWARD = 265,
    MEDIA_REWIND = 266,
    MEDIA_NEXT_TRACK = 267,
    MEDIA_PREVIOUS_TRACK = 268,
    MEDIA_STOP = 269,
    MEDIA_EJECT = 270,
    MEDIA_PLAY_PAUSE = 271,
    MEDIA_SELECT = 272,

    AC_NEW = 273,
    AC_OPEN = 274,
    AC_CLOSE = 275,
    AC_EXIT = 276,
    AC_SAVE = 277,
    AC_PRINT = 278,
    AC_PROPERTIES = 279,

    AC_SEARCH = 280,
    AC_HOME = 281,
    AC_BACK = 282,
    AC_FORWARD = 283,
    AC_STOP = 284,
    AC_REFRESH = 285,
    AC_BOOKMARKS = 286,

    SOFTLEFT = 287,
    SOFTRIGHT = 288,
    CALL = 289,
    ENDCALL = 290,

    RESERVED = 400,

    _,

    pub fn count() u16 {
        return 512;
    }

    pub fn keyName(self: @This()) []const u8 {
        return switch (self) {
            .RETURN => "Enter",
            .ESCAPE => "Esc",
            .BACKSPACE => "Back",
            .SPACE => "Space",
            .LEFT => "Left",
            .RIGHT => "Right",
            .UP => "Up",
            .DOWN => "Down",
            .N_0 => "0",
            .N_1 => "1",
            .N_2 => "2",
            .N_3 => "3",
            .N_4 => "4",
            .N_5 => "5",
            .N_6 => "6",
            .N_7 => "7",
            .N_8 => "8",
            .N_9 => "9",
            else => @tagName(self),
        };
    }
};

const KeyData = struct {
    event: KeyEventType,
    press_count_this_frame: u16,
    duration: f32,
    duration_prev: f32,

    const default_state: KeyData = .{
        .event = .none,
        .press_count_this_frame = 0,
        .duration = -1.0,
        .duration_prev = -1.0,
    };
};

const KeyEventType = enum {
    pressed,
    down,
    released,
    none,
};

const GAMEPAD_BUTTON_COUNT = 15;

const GamepadButtonData = struct {
    event: KeyEventType,
    duration: f32,
    duration_prev: f32,

    const default_state: GamepadButtonData = .{
        .event = .none,
        .duration = -1.0,
        .duration_prev = -1.0,
    };
};

const GamepadState = struct {
    handle: ?*c.SDL_Gamepad,
    name: []const u8,
    buttons: [GAMEPAD_BUTTON_COUNT]GamepadButtonData,

    fn init(handle: ?*c.SDL_Gamepad) GamepadState {
        const name = c.SDL_GetGamepadName(handle);
        return .{
            .handle = handle,
            .name = if (name == null) "Gamepad" else std.mem.span(name),
            .buttons = [_]GamepadButtonData{GamepadButtonData.default_state} ** GAMEPAD_BUTTON_COUNT,
        };
    }

    fn addButtonEvent(self: *GamepadState, btn_idx: u8, event: KeyEventType) void {
        if (btn_idx < GAMEPAD_BUTTON_COUNT) {
            const btn = &self.buttons[btn_idx];
            switch (event) {
                .down => if (btn.event != .pressed and btn.event != .down) {
                    btn.event = .pressed;
                },
                .released => if (btn.event == .pressed or btn.event == .down) {
                    btn.event = .released;
                },
                .pressed, .none => btn.event = event,
            }
        }
    }

    fn advanceFrame(self: *GamepadState) void {
        for (&self.buttons) |*btn| {
            btn.event = switch (btn.event) {
                .pressed => .down,
                .released => .none,
                else => btn.event,
            };
        }
    }

    fn update(self: *GamepadState, dt: f32) void {
        for (&self.buttons) |*btn| {
            btn.duration_prev = btn.duration;
            switch (btn.event) {
                .pressed, .down => {
                    if (btn.duration < 0.0) {
                        btn.duration = 0.0;
                    } else {
                        btn.duration += dt;
                    }
                },
                .released, .none => btn.duration = -1.0,
            }
        }
    }
};

pub const InputContext = struct {
    keys: [Key.count()]KeyData,

    key_repeat_delay: f32 = 0.250, // 250ms
    key_repeat_rate: f32 = 0.050, // 50ms

    const KeyEvent = struct {
        event: KeyEventType,
        scancode: c.SDL_Scancode,
    };

    pub fn init() InputContext {
        var ctx = InputContext{
            .keys = undefined,
        };
        @memset(&ctx.keys, KeyData.default_state);
        return ctx;
    }

    pub fn addKeyEvent(self: *InputContext, event: InputContext.KeyEvent) void {
        const idx = event.scancode;
        if (idx >= 0 and idx < self.keys.len) {
            const key = &self.keys[idx];
            switch (event.event) {
                .down => if (key.event != .pressed and key.event != .down) {
                    key.press_count_this_frame +|= 1;
                    key.event = .pressed;
                },
                .released => if (key.event == .pressed or key.event == .down) {
                    key.event = .released;
                },
                .pressed, .none => key.event = event.event,
            }
        }
    }

    pub fn advanceFrame(self: *InputContext) void {
        for (&self.keys) |*key| {
            key.press_count_this_frame = 0;
            key.event = switch (key.event) {
                .pressed => .down,
                .released => .none,
                else => key.event,
            };
        }
    }

    pub fn update(self: *InputContext, dt: f32) void {
        for (&self.keys) |*key| {
            key.duration_prev = key.duration;

            switch (key.event) {
                .pressed, .down => {
                    if (key.duration < 0.0) {
                        key.duration = 0.0;
                    } else {
                        key.duration += dt;
                    }
                },
                .released, .none => key.duration = -1.0,
            }
        }
    }

    pub fn isKeyPressed(self: *const InputContext, key: Key, repeat: bool) bool {
        const key_data = self.keys[@intFromEnum(key)];

        if (key_data.press_count_this_frame > 0) return true;

        if (key_data.event == .released) return false;

        if (key_data.event == .pressed) return true;

        const t = key_data.duration;

        if (repeat and t > self.key_repeat_delay) {
            const t_prev = key_data.duration_prev;
            const delay = self.key_repeat_delay;
            const rate = self.key_repeat_rate;

            const count_prev = if (t_prev > delay) @divFloor((t_prev - delay), rate) else -1.0;
            const count_curr = if (t > delay) @divFloor((t - delay), rate) else -1.0;

            if (count_curr > count_prev) {
                return true;
            }
        }

        return false;
    }

    pub fn keyPressCount(self: *const InputContext, key: Key) u16 {
        return self.keys[@intFromEnum(key)].press_count_this_frame;
    }

    pub fn getPressedKey(self: *const InputContext) ?Key {
        for (self.keys, 0..) |key, i| { // TODO: find a way where we don't have to iterate all the keys
            if (key.press_count_this_frame > 0) {
                return @enumFromInt(i);
            }
        }
        return null;
    }

    pub fn getReleasedKey(self: *const InputContext) ?Key {
        for (self.keys, 0..) |key, i| { // TODO: find a way where we don't have to iterate all the keys
            if (key.event == .released) {
                return @enumFromInt(i);
            }
        }
        return null;
    }

    pub fn isKeyDown(self: *const InputContext, key: Key) bool {
        return switch (self.keys[@intFromEnum(key)].event) {
            .pressed, .down => true,
            .released, .none => false,
        };
    }
};

const UIVertex = extern struct {
    position: c.SDL_FPoint,
    color: c.SDL_FColor,
    tex_coord: c.SDL_FPoint,
    rect: c.SDL_FRect,
    corner_radius: c.SDL_FColor,
    overlay_color: c.SDL_FColor,

    const empty_rect = c.SDL_FRect{ .x = 0, .y = 0, .w = 0, .h = 0 };
    const no_radius = c.SDL_FColor{ .r = 0, .g = 0, .b = 0, .a = 0 };

    fn init(position: c.SDL_FPoint, color: c.SDL_FColor, tex_coord: c.SDL_FPoint, overlay_color: c.SDL_FColor) UIVertex {
        return .{
            .position = position,
            .color = color,
            .tex_coord = tex_coord,
            .rect = empty_rect,
            .corner_radius = no_radius,
            .overlay_color = overlay_color,
        };
    }

    fn rounded(
        position: c.SDL_FPoint,
        color: c.SDL_FColor,
        tex_coord: c.SDL_FPoint,
        rect: c.SDL_FRect,
        corner_radius: clay.CornerRadius,
        overlay_color: c.SDL_FColor,
    ) UIVertex {
        return .{
            .position = position,
            .color = color,
            .tex_coord = tex_coord,
            .rect = rect,
            .corner_radius = .{
                .r = corner_radius.top_left,
                .g = corner_radius.top_right,
                .b = corner_radius.bottom_right,
                .a = corner_radius.bottom_left,
            },
            .overlay_color = overlay_color,
        };
    }
};

const TextVertex = extern struct {
    position: c.SDL_FPoint,
    color: c.SDL_FColor,
    tex_coord: c.SDL_FPoint,

    fn init(position: c.SDL_FPoint, color: c.SDL_FColor, tex_coord: c.SDL_FPoint) TextVertex {
        return .{ .position = position, .color = color, .tex_coord = tex_coord };
    }
};

pub const Renderer = struct {
    allocator: std.mem.Allocator,
    device: ?*c.SDL_GPUDevice,
    pipeline: ?*c.SDL_GPUGraphicsPipeline,
    text_pipeline: ?*c.SDL_GPUGraphicsPipeline,
    sampler: ?*c.SDL_GPUSampler,

    vertices: std.ArrayList(UIVertex),
    indices: std.ArrayList(u32),
    text_vertices: std.ArrayList(TextVertex),
    text_indices: std.ArrayList(u32),
    draw_calls: std.ArrayList(DrawCall),
    clip_stack: std.ArrayList(?c.SDL_Rect),
    overlay_stack: std.ArrayList(c.SDL_FColor),
    textures: std.ArrayList(TextureUpload),

    vertex_buffer: ?*c.SDL_GPUBuffer = null,
    index_buffer: ?*c.SDL_GPUBuffer = null,
    text_vertex_buffer: ?*c.SDL_GPUBuffer = null,
    text_index_buffer: ?*c.SDL_GPUBuffer = null,
    vertex_buffer_capacity: u32 = 0,
    index_buffer_capacity: u32 = 0,
    text_vertex_buffer_capacity: u32 = 0,
    text_index_buffer_capacity: u32 = 0,
    frame_transfer_buffer: ?*c.SDL_GPUTransferBuffer = null,
    frame_transfer_buffer_capacity: u32 = 0,

    current_texture: ?*c.SDL_GPUTexture = null,
    current_kind: DrawKind = .ui,
    current_clip: ?c.SDL_Rect = null,
    current_overlay_color: c.SDL_FColor = .{ .r = 0, .g = 0, .b = 0, .a = 0 },

    // Default 1x1 white texture for solid color rendering
    white_texture: ?*c.SDL_GPUTexture,

    const Self = @This();
    const ROUNDED_RECT_AA_PAD: f32 = 1.0;

    fn createSDLShaderFromSpirv(
        device: ?*c.SDL_GPUDevice,
        spirv: []const u8,
        stage: c.SDL_GPUShaderStage,
        num_samplers: u32,
        num_uniform_buffers: u32,
    ) !*c.SDL_GPUShader {
        return c.SDL_CreateGPUShader(device, &.{
            .code_size = spirv.len,
            .code = @ptrCast(spirv),
            .entrypoint = "main",
            .format = c.SDL_GPU_SHADERFORMAT_SPIRV,
            .stage = stage,
            .num_samplers = num_samplers,
            .num_uniform_buffers = num_uniform_buffers,
        }) orelse error.ShaderCreateFailed;
    }

    fn init(allocator: std.mem.Allocator, io: std.Io, device: ?*c.SDL_GPUDevice, window: ?*c.SDL_Window, vk_version: c_uint) !*Self {
        var self = try allocator.create(Self);
        self.allocator = allocator;
        self.device = device;
        self.vertices = .empty;
        self.indices = .empty;

        sdlError(c.SDL_ClaimWindowForGPUDevice(device, window));

        var shader_compiler = try shaders.ShaderCompiler.init(allocator, io);
        defer shader_compiler.deinit();

        try shader_compiler.addShader(shaders.VERT, .Vertex);
        try shader_compiler.addShader(shaders.FRAG, .Fragment);

        const result = try shader_compiler.compile(vk_version);
        defer {
            for (result) |spirv| {
                allocator.free(spirv.bytes);
            }
            allocator.free(result);
        }

        var vert_spirv: []const u8 = undefined;
        var frag_spirv: []const u8 = undefined;
        for (result) |spirv| {
            switch (spirv.stage) {
                .Vertex => vert_spirv = spirv.bytes,
                .Fragment => frag_spirv = spirv.bytes,
            }
        }

        const vert = try createSDLShaderFromSpirv(device, vert_spirv, c.SDL_GPU_SHADERSTAGE_VERTEX, 0, 1);
        const frag = try createSDLShaderFromSpirv(device, frag_spirv, c.SDL_GPU_SHADERSTAGE_FRAGMENT, 1, 0);
        defer {
            c.SDL_ReleaseGPUShader(device, vert);
            c.SDL_ReleaseGPUShader(device, frag);
        }

        var text_shader_compiler = try shaders.ShaderCompiler.init(allocator, io);
        defer text_shader_compiler.deinit();
        try text_shader_compiler.addShader(shaders.TEXT_VERT, .Vertex);
        try text_shader_compiler.addShader(shaders.TEXT_FRAG, .Fragment);
        const text_result = try text_shader_compiler.compile(vk_version);
        defer {
            for (text_result) |spirv| allocator.free(spirv.bytes);
            allocator.free(text_result);
        }
        var text_vert_spirv: []const u8 = undefined;
        var text_frag_spirv: []const u8 = undefined;
        for (text_result) |spirv| switch (spirv.stage) {
            .Vertex => text_vert_spirv = spirv.bytes,
            .Fragment => text_frag_spirv = spirv.bytes,
        };
        const text_vert = try createSDLShaderFromSpirv(device, text_vert_spirv, c.SDL_GPU_SHADERSTAGE_VERTEX, 0, 1);
        const text_frag = try createSDLShaderFromSpirv(device, text_frag_spirv, c.SDL_GPU_SHADERSTAGE_FRAGMENT, 1, 0);
        defer {
            c.SDL_ReleaseGPUShader(device, text_vert);
            c.SDL_ReleaseGPUShader(device, text_frag);
        }

        const color_desc = c.SDL_GPUColorTargetDescription{
            .format = c.SDL_GetGPUSwapchainTextureFormat(device, window),
            .blend_state = .{
                .src_color_blendfactor = c.SDL_GPU_BLENDFACTOR_SRC_ALPHA,
                .dst_color_blendfactor = c.SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
                .color_blend_op = c.SDL_GPU_BLENDOP_ADD,
                .src_alpha_blendfactor = c.SDL_GPU_BLENDFACTOR_SRC_ALPHA,
                .dst_alpha_blendfactor = c.SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
                .alpha_blend_op = c.SDL_GPU_BLENDOP_ADD,
                .enable_blend = true,
            },
        };

        const pixels = [_]u8{ 255, 255, 255, 255 };
        const transfer_buffer = sdlError(c.SDL_CreateGPUTransferBuffer(
            device,
            &.{ .usage = c.SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD, .size = 4 },
        ));
        const map: [*]u8 = @ptrCast(@alignCast(sdlError(c.SDL_MapGPUTransferBuffer(device, transfer_buffer, false))));
        @memcpy(map[0..4], &pixels);
        c.SDL_UnmapGPUTransferBuffer(device, transfer_buffer);

        var vertex_attrs = [_]c.SDL_GPUVertexAttribute{
            .{ .location = 0, .buffer_slot = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, .offset = @offsetOf(UIVertex, "position") },
            .{ .location = 1, .buffer_slot = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4, .offset = @offsetOf(UIVertex, "color") },
            .{ .location = 2, .buffer_slot = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, .offset = @offsetOf(UIVertex, "tex_coord") },
            .{ .location = 3, .buffer_slot = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4, .offset = @offsetOf(UIVertex, "rect") },
            .{ .location = 4, .buffer_slot = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4, .offset = @offsetOf(UIVertex, "corner_radius") },
            .{ .location = 5, .buffer_slot = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4, .offset = @offsetOf(UIVertex, "overlay_color") },
        };
        const vertex_bindings = [_]c.SDL_GPUVertexBufferDescription{.{
            .slot = 0,
            .pitch = @sizeOf(UIVertex),
            .input_rate = c.SDL_GPU_VERTEXINPUTRATE_VERTEX,
            .instance_step_rate = 0,
        }};
        var text_vertex_attrs = [_]c.SDL_GPUVertexAttribute{
            .{ .location = 0, .buffer_slot = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, .offset = @offsetOf(TextVertex, "position") },
            .{ .location = 1, .buffer_slot = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4, .offset = @offsetOf(TextVertex, "color") },
            .{ .location = 2, .buffer_slot = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, .offset = @offsetOf(TextVertex, "tex_coord") },
        };
        const text_vertex_bindings = [_]c.SDL_GPUVertexBufferDescription{.{
            .slot = 0,
            .pitch = @sizeOf(TextVertex),
            .input_rate = c.SDL_GPU_VERTEXINPUTRATE_VERTEX,
            .instance_step_rate = 0,
        }};
        const white_texture = sdlError(c.SDL_CreateGPUTexture(device, &.{
            .type = c.SDL_GPU_TEXTURETYPE_2D,
            .format = c.SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
            .width = 1,
            .height = 1,
            .layer_count_or_depth = 1,
            .num_levels = 1,
            .usage = c.SDL_GPU_TEXTUREUSAGE_SAMPLER,
        }));

        self.* = .{
            .allocator = allocator,
            .device = device,
            .vertices = .empty,
            .indices = .empty,
            .text_vertices = .empty,
            .text_indices = .empty,
            .draw_calls = .empty,
            .clip_stack = .empty,
            .overlay_stack = .empty,
            .textures = .empty,
            .current_texture = white_texture,
            .current_overlay_color = .{ .r = 0, .g = 0, .b = 0, .a = 0 },
            .pipeline = sdlError(c.SDL_CreateGPUGraphicsPipeline(device, &.{
                .vertex_shader = vert,
                .fragment_shader = frag,
                .vertex_input_state = .{
                    .vertex_buffer_descriptions = &vertex_bindings,
                    .num_vertex_buffers = 1,
                    .vertex_attributes = &vertex_attrs,
                    .num_vertex_attributes = vertex_attrs.len,
                },
                .primitive_type = c.SDL_GPU_PRIMITIVETYPE_TRIANGLELIST,
                .target_info = .{ .color_target_descriptions = &color_desc, .num_color_targets = 1 },
            })),
            .text_pipeline = sdlError(c.SDL_CreateGPUGraphicsPipeline(device, &.{
                .vertex_shader = text_vert,
                .fragment_shader = text_frag,
                .vertex_input_state = .{
                    .vertex_buffer_descriptions = &text_vertex_bindings,
                    .num_vertex_buffers = 1,
                    .vertex_attributes = &text_vertex_attrs,
                    .num_vertex_attributes = text_vertex_attrs.len,
                },
                .primitive_type = c.SDL_GPU_PRIMITIVETYPE_TRIANGLELIST,
                .target_info = .{ .color_target_descriptions = &color_desc, .num_color_targets = 1 },
            })),
            .sampler = sdlError(c.SDL_CreateGPUSampler(device, &.{
                .min_filter = c.SDL_GPU_FILTER_NEAREST,
                .mag_filter = c.SDL_GPU_FILTER_NEAREST,
                .mipmap_mode = c.SDL_GPU_SAMPLERMIPMAPMODE_NEAREST,
                .address_mode_u = c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
                .address_mode_v = c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
            })),
            .white_texture = white_texture,
            .vertex_buffer = sdlError(c.SDL_CreateGPUBuffer(
                device,
                &.{ .usage = c.SDL_GPU_BUFFERUSAGE_VERTEX, .size = 4 },
            )),
            .index_buffer = sdlError(c.SDL_CreateGPUBuffer(
                device,
                &.{ .usage = c.SDL_GPU_BUFFERUSAGE_INDEX, .size = 4 },
            )),
            .text_vertex_buffer = sdlError(c.SDL_CreateGPUBuffer(
                device,
                &.{ .usage = c.SDL_GPU_BUFFERUSAGE_VERTEX, .size = 4 },
            )),
            .text_index_buffer = sdlError(c.SDL_CreateGPUBuffer(
                device,
                &.{ .usage = c.SDL_GPU_BUFFERUSAGE_INDEX, .size = 4 },
            )),
        };

        const cmd_buf = sdlError(c.SDL_AcquireGPUCommandBuffer(device));
        const copy_pass = sdlError(c.SDL_BeginGPUCopyPass(cmd_buf));
        var src = c.SDL_GPUTextureTransferInfo{ .transfer_buffer = transfer_buffer };
        var dst = c.SDL_GPUTextureRegion{ .texture = self.white_texture, .w = 1, .h = 1, .d = 1 };
        c.SDL_UploadToGPUTexture(copy_pass, &src, &dst, false);
        c.SDL_EndGPUCopyPass(copy_pass);
        sdlError(c.SDL_SubmitGPUCommandBuffer(cmd_buf));
        c.SDL_ReleaseGPUTransferBuffer(device, transfer_buffer);

        return self;
    }

    fn deinit(self: *Self) void {
        c.SDL_ReleaseGPUBuffer(self.device, self.vertex_buffer);
        c.SDL_ReleaseGPUBuffer(self.device, self.index_buffer);
        c.SDL_ReleaseGPUBuffer(self.device, self.text_vertex_buffer);
        c.SDL_ReleaseGPUBuffer(self.device, self.text_index_buffer);
        c.SDL_ReleaseGPUTransferBuffer(self.device, self.frame_transfer_buffer);
        c.SDL_ReleaseGPUTexture(self.device, self.white_texture);
        c.SDL_ReleaseGPUSampler(self.device, self.sampler);
        c.SDL_ReleaseGPUGraphicsPipeline(self.device, self.pipeline);
        c.SDL_ReleaseGPUGraphicsPipeline(self.device, self.text_pipeline);
        self.vertices.deinit(self.allocator);
        self.indices.deinit(self.allocator);
        self.text_vertices.deinit(self.allocator);
        self.text_indices.deinit(self.allocator);
        self.draw_calls.deinit(self.allocator);
        self.clip_stack.deinit(self.allocator);
        self.overlay_stack.deinit(self.allocator);
        self.textures.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    fn reset(self: *Self) void {
        self.vertices.clearRetainingCapacity();
        self.indices.clearRetainingCapacity();
        self.text_vertices.clearRetainingCapacity();
        self.text_indices.clearRetainingCapacity();
        self.draw_calls.clearRetainingCapacity();
        self.clip_stack.clearRetainingCapacity();
        self.overlay_stack.clearRetainingCapacity();
        self.textures.clearRetainingCapacity();
        self.current_texture = self.white_texture;
        self.current_kind = .ui;
        self.current_clip = null;
        self.current_overlay_color = .{ .r = 0, .g = 0, .b = 0, .a = 0 };

        self.draw_calls.append(self.allocator, .{
            .texture = self.white_texture,
            .index_offset = 0,
            .index_count = 0,
            .clip_rect = null,
            .kind = .ui,
        }) catch @panic("Failed to allocate");
    }

    fn releaseMemory(self: *Self) void {
        self.vertices.clearAndFree(self.allocator);
        self.indices.clearAndFree(self.allocator);
        self.text_vertices.clearAndFree(self.allocator);
        self.text_indices.clearAndFree(self.allocator);
        self.draw_calls.clearAndFree(self.allocator);
        self.clip_stack.clearAndFree(self.allocator);
        self.overlay_stack.clearAndFree(self.allocator);
        self.textures.clearAndFree(self.allocator);
        c.SDL_ReleaseGPUTransferBuffer(self.device, self.frame_transfer_buffer);
        self.frame_transfer_buffer = null;
        self.frame_transfer_buffer_capacity = 0;
        // TODO: free `current_texture`??
    }

    fn pushTexture(self: *Self, upload: TextureUpload) void {
        self.textures.append(self.allocator, upload) catch @panic("Failed to allocate");
    }

    fn createFrameTransferBuffer(self: *Self, size: u32) ?*c.SDL_GPUTransferBuffer {
        if (size > self.frame_transfer_buffer_capacity) {
            if (self.frame_transfer_buffer) |buffer| c.SDL_ReleaseGPUTransferBuffer(self.device, buffer);
            self.frame_transfer_buffer = sdlError(c.SDL_CreateGPUTransferBuffer(
                self.device,
                &.{ .usage = c.SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD, .size = size },
            ));
            self.frame_transfer_buffer_capacity = size;
        }
        return self.frame_transfer_buffer;
    }

    fn flush(self: *Self) void {
        const last = &self.draw_calls.items[self.draw_calls.items.len - 1];

        const clip_changed = blk: {
            if (last.clip_rect == null and self.current_clip == null) break :blk false;
            if (last.clip_rect != null and self.current_clip != null) {
                const a = last.clip_rect.?;
                const b = self.current_clip.?;
                break :blk (a.x != b.x or a.y != b.y or a.w != b.w or a.h != b.h);
            }
            break :blk true;
        };

        if (last.texture != self.current_texture or last.kind != self.current_kind or clip_changed) {
            if (last.index_count == 0) {
                last.texture = self.current_texture;
                last.clip_rect = self.current_clip;
                last.kind = self.current_kind;
                last.index_offset = self.currentIndexCount();
            } else {
                self.draw_calls.append(self.allocator, .{
                    .texture = self.current_texture,
                    .index_offset = self.currentIndexCount(),
                    .index_count = 0,
                    .clip_rect = self.current_clip,
                    .kind = self.current_kind,
                }) catch @panic("Failed to allocate");
            }
        }
    }

    fn currentIndexCount(self: *const Self) u32 {
        return @intCast(switch (self.current_kind) {
            .ui => self.indices.items.len,
            .text => self.text_indices.items.len,
        });
    }

    fn forceDrawCallBreak(self: *Self) usize {
        const split_index = self.draw_calls.items.len;
        self.draw_calls.append(self.allocator, .{
            .texture = self.current_texture,
            .index_offset = self.currentIndexCount(),
            .index_count = 0,
            .clip_rect = self.current_clip,
            .kind = self.current_kind,
        }) catch @panic("Failed to allocate");
        return split_index;
    }

    fn setTexture(self: *Self, texture: ?*c.SDL_GPUTexture) void {
        self.current_kind = .ui;
        self.current_texture = texture orelse self.white_texture;
        self.flush();
    }

    fn setTextTexture(self: *Self, texture: ?*c.SDL_GPUTexture) void {
        self.current_kind = .text;
        self.current_texture = texture;
        self.flush();
    }

    fn setClipRect(self: *Self, rect: ?c.SDL_Rect) void {
        self.current_clip = rect;
        self.flush();
    }

    fn pushOverlayColor(self: *Self, color: c.SDL_FColor) void {
        self.overlay_stack.append(self.allocator, self.current_overlay_color) catch @panic("OOM");
        self.current_overlay_color = color;
    }

    fn popOverlayColor(self: *Self) void {
        self.current_overlay_color = self.overlay_stack.pop().?;
    }

    fn reserveGeometry(self: *Self, vertex_count: usize, index_count: usize) void {
        self.vertices.ensureUnusedCapacity(self.allocator, vertex_count) catch @panic("Failed to allocate");
        self.indices.ensureUnusedCapacity(self.allocator, index_count) catch @panic("Failed to allocate");
    }

    fn reserveTextGeometry(self: *Self, vertex_count: usize, index_count: usize) void {
        self.text_vertices.ensureUnusedCapacity(self.allocator, vertex_count) catch @panic("Failed to allocate");
        self.text_indices.ensureUnusedCapacity(self.allocator, index_count) catch @panic("Failed to allocate");
    }

    fn pushClipRect(self: *Self, rect: c.SDL_Rect) void {
        self.clip_stack.append(self.allocator, self.current_clip) catch @panic("Failed to allocate");
        self.setClipRect(if (self.current_clip) |current| intersectClipRects(current, rect) else rect);
    }

    fn popClipRect(self: *Self) void {
        if (self.clip_stack.pop()) |rect| {
            self.setClipRect(rect);
        } else {
            self.setClipRect(null);
        }
    }

    fn intersectClipRects(a: c.SDL_Rect, b: c.SDL_Rect) c.SDL_Rect {
        const left = @max(a.x, b.x);
        const top = @max(a.y, b.y);
        const right = @min(a.x + a.w, b.x + b.w);
        const bottom = @min(a.y + a.h, b.y + b.h);

        return .{
            .x = left,
            .y = top,
            .w = @max(0, right - left),
            .h = @max(0, bottom - top),
        };
    }

    fn pushRect(self: *Self, rect: c.SDL_FRect, color: c.SDL_FColor, uv: ?c.SDL_FRect) void {
        self.pushTexturedRect(rect, color, uv);
    }

    fn pushTextRect(self: *Self, rect: c.SDL_FRect, color: c.SDL_FColor, uv: c.SDL_FRect) void {
        const base_idx: u32 = @intCast(self.text_vertices.items.len);
        self.reserveTextGeometry(4, 6);
        self.text_vertices.appendSliceAssumeCapacity(&.{
            TextVertex.init(.{ .x = rect.x, .y = rect.y }, color, .{ .x = uv.x, .y = uv.y }),
            TextVertex.init(.{ .x = rect.x + rect.w, .y = rect.y }, color, .{ .x = uv.x + uv.w, .y = uv.y }),
            TextVertex.init(.{ .x = rect.x + rect.w, .y = rect.y + rect.h }, color, .{ .x = uv.x + uv.w, .y = uv.y + uv.h }),
            TextVertex.init(.{ .x = rect.x, .y = rect.y + rect.h }, color, .{ .x = uv.x, .y = uv.y + uv.h }),
        });
        self.text_indices.appendSliceAssumeCapacity(&.{ base_idx, base_idx + 1, base_idx + 2, base_idx, base_idx + 2, base_idx + 3 });
        self.draw_calls.items[self.draw_calls.items.len - 1].index_count += 6;
    }

    fn pushTexturedRect(self: *Self, rect: c.SDL_FRect, color: c.SDL_FColor, uv: ?c.SDL_FRect) void {
        const x = rect.x;
        const y = rect.y;
        const w = rect.w;
        const h = rect.h;
        const base_idx: u32 = @intCast(self.vertices.items.len);
        const uvs = uv orelse c.SDL_FRect{ .x = 0, .y = 0, .w = 1, .h = 1 };
        self.reserveGeometry(4, 6);

        // 4 vertices (Top-Left, Top-Right, Bottom-Right, Bottom-Left)
        self.vertices.appendSliceAssumeCapacity(&[_]UIVertex{
            UIVertex.init(.{ .x = x, .y = y }, color, .{ .x = uvs.x, .y = uvs.y }, self.current_overlay_color),
            UIVertex.init(.{ .x = x + w, .y = y }, color, .{ .x = uvs.x + uvs.w, .y = uvs.y }, self.current_overlay_color),
            UIVertex.init(.{ .x = x + w, .y = y + h }, color, .{ .x = uvs.x + uvs.w, .y = uvs.y + uvs.h }, self.current_overlay_color),
            UIVertex.init(.{ .x = x, .y = y + h }, color, .{ .x = uvs.x, .y = uvs.y + uvs.h }, self.current_overlay_color),
        });

        // 6 indices (2 triangles)
        self.indices.appendSliceAssumeCapacity(&[_]u32{
            base_idx, base_idx + 1, base_idx + 2, // Triangle 1
            base_idx, base_idx + 2, base_idx + 3, // Triangle 2
        });

        self.draw_calls.items[self.draw_calls.items.len - 1].index_count += 6;
    }

    fn hasCornerRadius(corner_radius: clay.CornerRadius) bool {
        return corner_radius.top_left > 0.0 or
            corner_radius.top_right > 0.0 or
            corner_radius.bottom_left > 0.0 or
            corner_radius.bottom_right > 0.0;
    }

    fn texCoordForPoint(rect: c.SDL_FRect, uv: c.SDL_FRect, px: f32, py: f32) c.SDL_FPoint {
        const u = uv.x + uv.w * ((px - rect.x) / rect.w);
        const v = uv.y + uv.h * ((py - rect.y) / rect.h);
        return .{ .x = u, .y = v };
    }

    fn pushRoundedTexturedRect(self: *Self, rect: c.SDL_FRect, color: c.SDL_FColor, corner_radius: clay.CornerRadius, uv_: ?c.SDL_FRect) void {
        if (!hasCornerRadius(corner_radius)) {
            self.pushRect(rect, color, uv_);
            return;
        }

        const uv = uv_ orelse c.SDL_FRect{ .x = 0, .y = 0, .w = 1, .h = 1 };
        var radius = corner_radius;
        const max_radius = @min(rect.w, rect.h) / 2.0;
        radius.top_left = @min(@max(radius.top_left, 0.0), max_radius);
        radius.top_right = @min(@max(radius.top_right, 0.0), max_radius);
        radius.bottom_left = @min(@max(radius.bottom_left, 0.0), max_radius);
        radius.bottom_right = @min(@max(radius.bottom_right, 0.0), max_radius);

        const ScalePair = struct {
            fn apply(a: *f32, b: *f32, max_sum: f32) void {
                const sum = a.* + b.*;
                if (sum > max_sum and sum > 0.0) {
                    const scale = max_sum / sum;
                    a.* *= scale;
                    b.* *= scale;
                }
            }
        };
        ScalePair.apply(&radius.top_left, &radius.top_right, rect.w);
        ScalePair.apply(&radius.bottom_left, &radius.bottom_right, rect.w);
        ScalePair.apply(&radius.top_left, &radius.bottom_left, rect.h);
        ScalePair.apply(&radius.top_right, &radius.bottom_right, rect.h);

        const pad = @min(ROUNDED_RECT_AA_PAD, @min(rect.w, rect.h) * 0.5);
        const x = rect.x - pad;
        const y = rect.y - pad;
        const w = rect.w + pad * 2.0;
        const h = rect.h + pad * 2.0;
        const base_idx: u32 = @intCast(self.vertices.items.len);
        self.reserveGeometry(4, 6);

        self.vertices.appendSliceAssumeCapacity(&[_]UIVertex{
            UIVertex.rounded(.{ .x = x, .y = y }, color, texCoordForPoint(rect, uv, x, y), rect, radius, self.current_overlay_color),
            UIVertex.rounded(.{ .x = x + w, .y = y }, color, texCoordForPoint(rect, uv, x + w, y), rect, radius, self.current_overlay_color),
            UIVertex.rounded(.{ .x = x + w, .y = y + h }, color, texCoordForPoint(rect, uv, x + w, y + h), rect, radius, self.current_overlay_color),
            UIVertex.rounded(.{ .x = x, .y = y + h }, color, texCoordForPoint(rect, uv, x, y + h), rect, radius, self.current_overlay_color),
        });

        self.indices.appendSliceAssumeCapacity(&[_]u32{
            base_idx, base_idx + 1, base_idx + 2,
            base_idx, base_idx + 2, base_idx + 3,
        });

        self.draw_calls.items[self.draw_calls.items.len - 1].index_count += 6;
    }

    fn pushRoundedBorder(self: *Self, box: clay.BoundingBox, border: clay.BorderRenderData) void {
        const w = border.width;
        // If no visible border, return early
        if (w.left == 0 and w.right == 0 and w.top == 0 and w.bottom == 0) return;

        const col = c.SDL_FColor{
            .r = border.color[0] / 255.0,
            .g = border.color[1] / 255.0,
            .b = border.color[2] / 255.0,
            .a = border.color[3] / 255.0,
        };

        //  Define Outer and Inner Rects
        const outer = c.SDL_FRect{ .x = box.x, .y = box.y, .w = box.width, .h = box.height };
        const inner = c.SDL_FRect{
            .x = box.x + @as(f32, @floatFromInt(w.left)),
            .y = box.y + @as(f32, @floatFromInt(w.top)),
            .w = box.width - @as(f32, @floatFromInt(w.left + w.right)),
            .h = box.height - @as(f32, @floatFromInt(w.top + w.bottom)),
        };

        const radius_outer: f32 = @max(border.corner_radius.top_left, border.corner_radius.bottom_right);
        const border_avg = (@as(f32, @floatFromInt(w.left + w.top)) / 2.0);
        var radius_inner = radius_outer - border_avg;
        if (radius_inner < 0) radius_inner = 0;

        // Center Points for corners
        const centers_out = [_]clay.Vector2{
            .{ .x = outer.x + radius_outer, .y = outer.y + radius_outer }, // Top-Left
            .{ .x = (outer.x + outer.w) - radius_outer, .y = outer.y + radius_outer }, // Top-Right
            .{ .x = (outer.x + outer.w) - radius_outer, .y = (outer.y + outer.h) - radius_outer }, // Bot-Right
            .{ .x = outer.x + radius_outer, .y = (outer.y + outer.h) - radius_outer }, // Bot-Left
        };

        const centers_in = [_]clay.Vector2{
            .{ .x = inner.x + radius_inner, .y = inner.y + radius_inner },
            .{ .x = (inner.x + inner.w) - radius_inner, .y = inner.y + radius_inner },
            .{ .x = (inner.x + inner.w) - radius_inner, .y = (inner.y + inner.h) - radius_inner },
            .{ .x = inner.x + radius_inner, .y = (inner.y + inner.h) - radius_inner },
        };

        const segments: usize = 8;
        const step_length: f32 = 90.0 / @as(f32, @floatFromInt(segments));
        const deg2rad = std.math.pi / 180.0;
        const angles = [_]f32{ 180.0, 270.0, 0.0, 90.0 };
        self.reserveGeometry(4 * (segments + 1) * 2, ((4 * (segments + 1)) - 1) * 6 + 6);

        // Save initial vertex index to close the loop later
        const start_v_count: u32 = @intCast(self.vertices.items.len);
        var current_v_count = start_v_count;

        // Generate geometry loop
        for (0..4) |k| {
            var angle = angles[k];
            const c_out = centers_out[k];
            const c_in = centers_in[k];

            for (0..segments + 1) |_| {
                const rad = angle * deg2rad;
                const cos_a = @cos(rad);
                const sin_a = @sin(rad);

                // Push Outer Vertex
                self.vertices.appendAssumeCapacity(UIVertex.init(
                    .{ .x = c_out.x + cos_a * radius_outer, .y = c_out.y + sin_a * radius_outer },
                    col,
                    .{ .x = 0, .y = 0 },
                    self.current_overlay_color,
                ));

                // Push Inner Vertex
                self.vertices.appendAssumeCapacity(UIVertex.init(
                    .{ .x = c_in.x + cos_a * radius_inner, .y = c_in.y + sin_a * radius_inner },
                    col,
                    .{ .x = 0, .y = 0 },
                    self.current_overlay_color,
                ));

                const idx_out = current_v_count;
                const idx_in = current_v_count + 1;

                // Add indices to connect to previous pair (triangle strip logic)
                // We skip the very first pair of the first corner because there is no previous pair yet
                if (current_v_count > start_v_count) {
                    self.indices.appendSliceAssumeCapacity(&[_]u32{
                        idx_out - 2, idx_out, idx_out - 1, // Tri 1
                        idx_out - 1, idx_out, idx_in, // Tri 2
                    });
                    self.draw_calls.items[self.draw_calls.items.len - 1].index_count += 6;
                }

                current_v_count += 2;
                angle += step_length;
            }
        }

        // Close the loop (Connect end of Bot-Left to start of Top-Left)
        const last_out = current_v_count - 2;
        const last_in = current_v_count - 1;
        const first_out = start_v_count;
        const first_in = start_v_count + 1;

        self.indices.appendSliceAssumeCapacity(&[_]u32{ last_out, first_out, last_in, last_in, first_out, first_in });
        self.draw_calls.items[self.draw_calls.items.len - 1].index_count += 6;
    }

    fn pushRoundedRect(self: *Self, rect: c.SDL_FRect, color: c.SDL_FColor, corner_radius: clay.CornerRadius) void {
        self.pushRoundedTexturedRect(rect, color, corner_radius, null);
    }

    fn pushTriangle(self: *Self, p1: clay.Vector2, p2: clay.Vector2, p3: clay.Vector2, color: c.SDL_FColor) void {
        const base_idx: u32 = @intCast(self.vertices.items.len);
        self.reserveGeometry(3, 3);
        self.vertices.appendSliceAssumeCapacity(&[_]UIVertex{
            UIVertex.init(.{ .x = p1.x, .y = p1.y }, color, .{ .x = 0, .y = 0 }, self.current_overlay_color),
            UIVertex.init(.{ .x = p2.x, .y = p2.y }, color, .{ .x = 0, .y = 0 }, self.current_overlay_color),
            UIVertex.init(.{ .x = p3.x, .y = p3.y }, color, .{ .x = 0, .y = 0 }, self.current_overlay_color),
        });
        self.indices.appendSliceAssumeCapacity(&[_]u32{
            base_idx, base_idx + 1, base_idx + 2,
        });
        self.draw_calls.items[self.draw_calls.items.len - 1].index_count += 3;
    }

    fn resizeGPUBuffers(self: *Self, vertex_count: u32, index_count: u32, text_vertex_count: u32, text_index_count: u32) void {
        const vertex_size_needed = vertex_count * @sizeOf(UIVertex);
        const index_size_needed = index_count * @sizeOf(u32);
        const text_vertex_size_needed = text_vertex_count * @sizeOf(TextVertex);
        const text_index_size_needed = text_index_count * @sizeOf(u32);

        if (vertex_size_needed > self.vertex_buffer_capacity) {
            if (self.vertex_buffer) |b| c.SDL_ReleaseGPUBuffer(self.device, b);
            self.vertex_buffer = c.SDL_CreateGPUBuffer(
                self.device,
                &.{ .usage = c.SDL_GPU_BUFFERUSAGE_VERTEX, .size = vertex_size_needed },
            );
            self.vertex_buffer_capacity = vertex_size_needed;
        }

        if (index_size_needed > self.index_buffer_capacity) {
            if (self.index_buffer) |b| c.SDL_ReleaseGPUBuffer(self.device, b);
            self.index_buffer = c.SDL_CreateGPUBuffer(
                self.device,
                &.{ .usage = c.SDL_GPU_BUFFERUSAGE_INDEX, .size = index_size_needed },
            );
            self.index_buffer_capacity = index_size_needed;
        }

        if (text_vertex_size_needed > self.text_vertex_buffer_capacity) {
            if (self.text_vertex_buffer) |b| c.SDL_ReleaseGPUBuffer(self.device, b);
            self.text_vertex_buffer = c.SDL_CreateGPUBuffer(
                self.device,
                &.{ .usage = c.SDL_GPU_BUFFERUSAGE_VERTEX, .size = text_vertex_size_needed },
            );
            self.text_vertex_buffer_capacity = text_vertex_size_needed;
        }

        if (text_index_size_needed > self.text_index_buffer_capacity) {
            if (self.text_index_buffer) |b| c.SDL_ReleaseGPUBuffer(self.device, b);
            self.text_index_buffer = c.SDL_CreateGPUBuffer(
                self.device,
                &.{ .usage = c.SDL_GPU_BUFFERUSAGE_INDEX, .size = text_index_size_needed },
            );
            self.text_index_buffer_capacity = text_index_size_needed;
        }
    }
};

pub const Window = struct {
    ctx: *UIContext,
    ptr: ?*c.SDL_Window,
    renderer: *Renderer,
    /// Logical UI width in Clay/render units, derived from `pixel_width / display_scale`.
    logical_width: f32,
    /// Logical UI height in Clay/render units, derived from `pixel_height / display_scale`.
    logical_height: f32,
    /// Drawable framebuffer width in physical pixels.
    pixel_width: i32,
    /// Drawable framebuffer height in physical pixels.
    pixel_height: i32,
    /// SDL window width in window/screen coordinates, used by input events.
    window_width: i32,
    /// SDL window height in window/screen coordinates, used by input events.
    window_height: i32,
    /// Scale factor between logical UI units and physical drawable pixels.
    display_scale: f32,
    safe_area: c.SDL_Rect,
    title: []const u8,
    font_user_data: FontUserData,

    fn fullWindowArea(width: i32, height: i32) c.SDL_Rect {
        return .{ .x = 0, .y = 0, .w = width, .h = height };
    }

    pub fn setWindowSize(self: *Window, width: i32, height: i32) void {
        sdlError(c.SDL_SetWindowSize(self.ptr, width, height));
        self.updateWindowSize();
    }

    fn updateWindowSize(self: *Window) void {
        self.display_scale = currentDisplayScale(self.ptr);

        sdlError(c.SDL_GetWindowSize(self.ptr, &self.window_width, &self.window_height));
        sdlError(c.SDL_GetWindowSizeInPixels(self.ptr, &self.pixel_width, &self.pixel_height));
        sdlError(c.SDL_GetWindowSafeArea(self.ptr, &self.safe_area));

        self.logical_width = @as(f32, @floatFromInt(self.pixel_width)) / self.display_scale;
        self.logical_height = @as(f32, @floatFromInt(self.pixel_height)) / self.display_scale;
    }

    fn currentDisplayScale(window: ?*c.SDL_Window) f32 {
        return c.SDL_GetWindowDisplayScale(window);
    }

    fn logicalFromWindowX(self: *const Window, value: f32) f32 {
        const pixel_density = @as(f32, @floatFromInt(self.pixel_width)) / @as(f32, @floatFromInt(self.window_width));
        return value * pixel_density / self.display_scale;
    }

    fn logicalFromWindowY(self: *const Window, value: f32) f32 {
        const pixel_density = @as(f32, @floatFromInt(self.pixel_height)) / @as(f32, @floatFromInt(self.window_height));
        return value * pixel_density / self.display_scale;
    }

    fn mouseScaleX(self: *const Window) f32 {
        return self.logicalFromWindowX(1.0);
    }

    fn mouseScaleY(self: *const Window) f32 {
        return self.logicalFromWindowY(1.0);
    }

    pub fn safeAreaPadding(self: *const Window) clay.Padding {
        if (builtin.abi.isAndroid()) {
            const orientation = android.currentScreenOrientation() orelse .unknown;
            const xy_inset: u16 = @intFromFloat(
                self.logicalFromWindowX(@floatFromInt(self.window_width - (self.safe_area.x + self.safe_area.w))),
            );
            return .{
                .left = if (orientation == .landscape) xy_inset else 0,
                .right = if (orientation == .landscape_flipped) xy_inset else 0,
                .top = @intFromFloat(self.logicalFromWindowY(@floatFromInt(self.safe_area.y))),
                .bottom = @intFromFloat(
                    self.logicalFromWindowY(@floatFromInt(self.window_height - (self.safe_area.y + self.safe_area.h))),
                ),
            };
        }

        return .{};
    }

    fn logicalRectToPixel(self: *const Window, rect: c.SDL_Rect) c.SDL_Rect {
        return self.logicalBoundsToPixel(
            @floatFromInt(rect.x),
            @floatFromInt(rect.y),
            @floatFromInt(rect.w),
            @floatFromInt(rect.h),
        );
    }

    fn logicalViewportToPixel(self: *const Window, viewport: pipeline.Viewport) pipeline.Viewport {
        const rect = self.logicalBoundsToPixel(
            @floatFromInt(viewport.x),
            @floatFromInt(viewport.y),
            @floatFromInt(viewport.w),
            @floatFromInt(viewport.h),
        );
        return .{
            .x = rect.x,
            .y = rect.y,
            .w = @intCast(rect.w),
            .h = @intCast(rect.h),
        };
    }

    fn logicalBoundsToPixel(self: *const Window, x: f32, y: f32, w: f32, h: f32) c.SDL_Rect {
        const scale = self.display_scale;
        const pixel_x = @floor(x * scale);
        const pixel_y = @floor(y * scale);
        const pixel_right = @ceil((x + w) * scale);
        const pixel_bottom = @ceil((y + h) * scale);
        return .{
            .x = @intFromFloat(pixel_x),
            .y = @intFromFloat(pixel_y),
            .w = @intFromFloat(pixel_right - pixel_x),
            .h = @intFromFloat(pixel_bottom - pixel_y),
        };
    }

    fn logicalTouchOffset(self: *const Window, x: f32, y: f32) clay.Vector2 {
        return .{
            .x = x * @as(f32, @floatFromInt(self.window_width)) * self.mouseScaleX(),
            .y = y * @as(f32, @floatFromInt(self.window_height)) * self.mouseScaleY(),
        };
    }

    fn deinit(self: *@This(), alloc: std.mem.Allocator, device: ?*c.SDL_GPUDevice) void {
        self.renderer.deinit();
        c.SDL_ReleaseWindowFromGPUDevice(device, self.ptr);
        c.SDL_DestroyWindow(self.ptr);
        self.ctx.deinit(alloc, device);
        alloc.destroy(self);
    }

    fn update(self: *Window) void {
        self.ctx.update();
        clay.setLayoutDimensions(.{ .w = self.logical_width, .h = self.logical_height });
    }

    pub fn id(self: *const Window) c.SDL_WindowID {
        return c.SDL_GetWindowID(self.ptr);
    }
};

const SecondaryWindow = struct {
    inner: *Window,
    // Used to correctly scale the "Settings" window UI elements.
    original_width: i32,
    /// Opaque pointer forwarded to draw_fn as user data.
    user_data: ?*anyopaque = null,
    draw_fn: ?*const fn (*UI, user_data: ?*anyopaque) void = null,
    on_close: ?*const fn (*UI, user_data: ?*anyopaque) void = null,

    fn deinit(self: *const @This(), alloc: std.mem.Allocator, device: ?*c.SDL_GPUDevice) void {
        self.inner.deinit(alloc, device);
    }
};

fn loadIconTexture(device: ?*c.SDL_GPUDevice, png_data: []const u8) ?*c.SDL_GPUTexture {
    const io = c.SDL_IOFromConstMem(png_data.ptr, png_data.len);
    const surface = c.SDL_LoadPNG_IO(io, true);
    defer c.SDL_DestroySurface(surface);

    const pixel_format = c.SDL_PIXELFORMAT_ABGR8888;
    const rgba = sdlError(c.SDL_ConvertSurface(surface, pixel_format));
    defer c.SDL_DestroySurface(rgba);

    const w: u32 = @intCast(rgba.*.w);
    const h: u32 = @intCast(rgba.*.h);
    const bytes_per_pixel: u32 = @intCast(c.SDL_GetPixelFormatDetails(pixel_format).*.bytes_per_pixel);
    const size = w * h * bytes_per_pixel;

    const texture = sdlError(c.SDL_CreateGPUTexture(device, &.{
        .type = c.SDL_GPU_TEXTURETYPE_2D,
        .format = c.SDL_GetGPUTextureFormatFromPixelFormat(pixel_format),
        .width = w,
        .height = h,
        .layer_count_or_depth = 1,
        .num_levels = 1,
        .usage = c.SDL_GPU_TEXTUREUSAGE_SAMPLER,
    }));

    const tb = c.SDL_CreateGPUTransferBuffer(device, &.{
        .usage = c.SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,
        .size = size,
    });
    const map: [*]u8 = @ptrCast(@alignCast(c.SDL_MapGPUTransferBuffer(device, tb, false)));
    @memcpy(map[0..size], @as([*]const u8, @ptrCast(@alignCast(rgba.*.pixels)))[0..size]);
    c.SDL_UnmapGPUTransferBuffer(device, tb);

    const cmd = c.SDL_AcquireGPUCommandBuffer(device);
    const copy_pass = c.SDL_BeginGPUCopyPass(cmd);
    c.SDL_UploadToGPUTexture(
        copy_pass,
        &.{ .transfer_buffer = tb },
        &.{ .texture = texture, .w = w, .h = h, .d = 1 },
        false,
    );
    c.SDL_EndGPUCopyPass(copy_pass);
    _ = c.SDL_SubmitGPUCommandBuffer(cmd);
    c.SDL_ReleaseGPUTransferBuffer(device, tb);

    return texture;
}

pub const UI = struct {
    allocator: std.mem.Allocator,
    io: std.Io,
    /// The main windows created by the program.
    main_window: *Window,
    /// Secondary windows created by the program, e.g. dialogs.
    secondary_windows: std.ArrayList(SecondaryWindow),
    /// The current window in focus.
    current_window: *Window,
    gpu_device: ?*c.SDL_GPUDevice,
    font_cache: *FontCache,

    quit: bool = false,
    pending_close_window: ?*Window = null,
    fps_manager: FPSManager,
    is_suspended: bool = false,
    android_back_requested: bool = false,

    vk_version: c_uint,
    shaders_pipeline: std.StringHashMap(*pipeline.ShaderPipeline),

    gamepads: std.ArrayList(GamepadState) = .empty,
    on_screen_controller: ControllerButton = .{},

    icons: std.EnumArray(Icon, ?*c.SDL_GPUTexture) = .initUndefined(),

    pub const Icon = enum {
        play,
        stop,
        skip_next,
        fast_forward,
        menu,
        keyboard,
        controller,
        dpad_up,
        dpad_down,
        dpad_left,
        dpad_right,
        copy,

        fn data(self: @This()) []const u8 {
            return switch (self) {
                .play => @embedFile("play_icon"),
                .stop => @embedFile("stop_icon"),
                .skip_next => @embedFile("skip_next_icon"),
                .fast_forward => @embedFile("fast_forward_icon"),
                .menu => @embedFile("menu_icon"),
                .keyboard => @embedFile("keyboard_icon"),
                .controller => @embedFile("controller_icon"),
                .dpad_up => @embedFile("dpad_up_icon"),
                .dpad_down => @embedFile("dpad_down_icon"),
                .dpad_left => @embedFile("dpad_left_icon"),
                .dpad_right => @embedFile("dpad_right_icon"),
                .copy => @embedFile("copy_icon"),
            };
        }
    };

    pub const PressedGamepadButton = struct { gamepad_idx: usize, btn: GamepadButton };

    const Self = @This();
    const CLAY_ERROR_HANDLER = clay.ErrorHandler{ .error_handler_function = handleClayError };

    pub fn init(allocator: std.mem.Allocator, io: std.Io, title: []const u8, width: i32, height: i32) !*Self {
        sdlError(c.SDL_SetAppMetadata("NESkwik", "1.0.0", "com.labatata.neskwik"));
        sdlError(c.SDL_Init(c.SDL_INIT_VIDEO | c.SDL_INIT_AUDIO | c.SDL_INIT_GAMEPAD));

        if (c.glslang_initialize_process() != 1) {
            return error.GLSlangFailedToInitialize;
        }

        const vk_version = if (builtin.abi.isAndroid())
            c.VK_MAKE_VERSION(1, 0, 0)
        else
            vulkan.detect_vulkan_version();
        std.log.debug("Detected Vulkan version: {}.{}.{}", .{
            c.VK_VERSION_MAJOR(vk_version),
            c.VK_VERSION_MINOR(vk_version),
            c.VK_VERSION_PATCH(vk_version),
        });

        sdlError(c.SDL_SetHint(c.SDL_HINT_QUIT_ON_LAST_WINDOW_CLOSE, "0"));

        const props = c.SDL_CreateProperties();
        defer c.SDL_DestroyProperties(props);

        var vkOptions: c.SDL_GPUVulkanOptions = .{
            .vulkan_api_version = c.VK_MAKE_API_VERSION(
                c.VK_API_VERSION_VARIANT(vk_version),
                c.VK_VERSION_MAJOR(vk_version),
                c.VK_VERSION_MINOR(vk_version),
                c.VK_VERSION_PATCH(vk_version),
            ),
        };
        sdlError(c.SDL_SetPointerProperty(
            props,
            c.SDL_PROP_GPU_DEVICE_CREATE_VULKAN_OPTIONS_POINTER,
            @ptrCast(&vkOptions),
        ));

        sdlError(c.SDL_SetBooleanProperty(props, c.SDL_PROP_GPU_DEVICE_CREATE_SHADERS_SPIRV_BOOLEAN, true));
        if (builtin.abi.isAndroid()) {
            sdlError(c.SDL_SetBooleanProperty(props, c.SDL_PROP_GPU_DEVICE_CREATE_FEATURE_CLIP_DISTANCE_BOOLEAN, false));
        }

        sdlError(c.SDL_SetBooleanProperty(
            props,
            c.SDL_PROP_GPU_DEVICE_CREATE_DEBUGMODE_BOOLEAN,
            builtin.mode == .Debug,
        ));

        const gpu_device = sdlError(c.SDL_CreateGPUDeviceWithProperties(props));

        const ui_ctx = try UIContext.init(
            allocator,
            .{ .w = @floatFromInt(width), .h = @floatFromInt(height) },
            CLAY_ERROR_HANDLER,
        );

        if (env.get("UI_DEBUG")) |value| {
            clay.setDebugModeEnabled(std.mem.eql(u8, value, "1"));
        }

        const main_window = try allocator.create(Window);
        const win_ptr = sdlError(c.SDL_CreateWindow(
            title.ptr,
            width,
            height,
            c.SDL_WINDOW_RESIZABLE | c.SDL_WINDOW_HIGH_PIXEL_DENSITY,
        ));
        const font_cache = try FontCache.init(allocator, io, gpu_device, Window.currentDisplayScale(win_ptr));
        const display_scale = Window.currentDisplayScale(win_ptr);
        main_window.* = .{
            .ptr = win_ptr,
            .logical_width = @as(f32, @floatFromInt(width)) / display_scale,
            .logical_height = @as(f32, @floatFromInt(height)) / display_scale,
            .pixel_width = @intCast(width),
            .pixel_height = @intCast(height),
            .window_width = width,
            .window_height = height,
            .display_scale = display_scale,
            .safe_area = Window.fullWindowArea(width, height),
            .title = title,
            .ctx = ui_ctx,
            .renderer = try Renderer.init(allocator, io, gpu_device, win_ptr, vk_version),
            .font_user_data = .{
                .font_cache = font_cache,
                .scale = &main_window.display_scale,
                .measure_cache = &ui_ctx.text_measure_cache,
            },
        };

        clay.setMeasureTextFunction(*const FontUserData, &main_window.font_user_data, measureText);

        if (!builtin.abi.isAndroid()) {
            sdlError(c.SDL_SetWindowMinimumSize(main_window.ptr, 300, 480));
            setWindowIcon(main_window.ptr);
        }

        const gui = try allocator.create(UI);
        gui.* = .{
            .allocator = allocator,
            .io = io,
            .main_window = main_window,
            .secondary_windows = .empty,
            .current_window = main_window,
            .gpu_device = gpu_device,
            .font_cache = font_cache,
            .fps_manager = FPSManager.init(),
            .vk_version = vk_version,
            .shaders_pipeline = .init(allocator),
            .icons = blk: {
                var arr = std.EnumArray(Icon, ?*c.SDL_GPUTexture).initUndefined();
                inline for (std.meta.tags(Icon)) |tag| {
                    arr.set(tag, loadIconTexture(gpu_device, tag.data()));
                }
                break :blk arr;
            },
        };
        // Open any gamepads already connected at startup.
        var gp_count: c_int = 0;
        const gp_ids = c.SDL_GetGamepads(&gp_count);
        if (gp_ids != null) {
            var i: c_int = 0;
            while (i < gp_count) : (i += 1) {
                gui.gamepads.append(allocator, GamepadState.init(c.SDL_OpenGamepad(gp_ids[@intCast(i)]))) catch
                    @panic("OOM");
            }
            c.SDL_free(gp_ids);
        }

        return gui;
    }

    pub fn deinit(self: *Self) void {
        for (self.gamepads.items) |state| c.SDL_CloseGamepad(state.handle);
        self.gamepads.deinit(self.allocator);

        self.main_window.deinit(self.allocator, self.gpu_device);

        for (self.secondary_windows.items) |window| {
            window.deinit(self.allocator, self.gpu_device);
        }
        self.secondary_windows.deinit(self.allocator);

        var shader_iter = self.shaders_pipeline.iterator();
        while (shader_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit();
        }
        self.shaders_pipeline.deinit();

        inline for (std.meta.tags(Icon)) |tag| {
            c.SDL_ReleaseGPUTexture(self.gpu_device, self.icons.get(tag));
        }

        self.font_cache.deinit(self.gpu_device);
        c.glslang_finalize_process();
        c.SDL_DestroyGPUDevice(self.gpu_device);
        c.SDL_Quit();

        self.allocator.destroy(self);
    }

    pub fn setFramerate(self: *Self, fps: FPSManager.FramerateMode) void {
        self.fps_manager.setFramerate(fps);
    }

    pub fn shouldClose(self: *Self) bool {
        self.current_window.ctx.dt = @as(f32, @floatFromInt(self.fps_manager.delay())) / c.SDL_MS_PER_SECOND;

        // Stop rendering if the Android app goes to background
        if (builtin.abi.isAndroid() and self.is_suspended) {
            var event: c.SDL_Event = undefined;
            while (true) {
                while (c.SDL_PollEvent(&event)) {
                    self.handleEvent(&event);
                }
                if (!self.is_suspended) break;
                c.SDL_Delay(1);
            }
        }

        return self.quit;
    }

    pub fn setTimer(self: *Self, name: []const u8, ms: u64) void {
        self.current_window.ctx.setTimer(name, ms);
    }

    pub fn tickTimer(self: *Self, name: []const u8, ms: u64) bool {
        return self.current_window.ctx.tickTimer(name, ms);
    }

    pub fn hasTimerExpired(self: *Self, name: []const u8) Optional(bool) {
        return self.current_window.ctx.hasTimerExpired(name);
    }

    pub fn isHovering(_: *const Self, id: []const u8) bool {
        return clay.pointerOver(clay.ElementId.ID(id));
    }

    pub fn isWindowFullscreen(self: *const Self) bool {
        return c.SDL_GetWindowFlags(self.current_window.ptr) & c.SDL_WINDOW_FULLSCREEN != 0;
    }

    pub fn setWindowFullscreen(self: *const Self, value: bool) void {
        sdlError(c.SDL_SetWindowFullscreen(self.current_window.ptr, value));
    }

    pub fn setVSync(self: *const Self, enabled: bool) void {
        const present_mode: c.SDL_GPUPresentMode = if (enabled)
            c.SDL_GPU_PRESENTMODE_VSYNC
        else if (builtin.abi.isAndroid())
            c.SDL_GPU_PRESENTMODE_MAILBOX
        else
            c.SDL_GPU_PRESENTMODE_IMMEDIATE;

        sdlError(c.SDL_SetGPUSwapchainParameters(
            self.gpu_device,
            self.main_window.ptr,
            c.SDL_GPU_SWAPCHAINCOMPOSITION_SDR,
            present_mode,
        ));
    }

    pub fn getShaderParamInfos(self: *const Self, name: []const u8) []const pipeline.ParamInfo {
        const shader_pipe = self.shaders_pipeline.get(name) orelse return &.{};
        return shader_pipe.getParamInfos();
    }

    pub fn getShaderParam(self: *const Self, name: []const u8, param: []const u8) f32 {
        const shader_pipe = self.shaders_pipeline.get(name) orelse return 0;
        return shader_pipe.getParam(param);
    }

    pub fn setShaderParam(self: *Self, name: []const u8, param: []const u8, value: f32) void {
        const shader_pipe = self.shaders_pipeline.get(name) orelse return;
        shader_pipe.setParam(param, value);
    }

    /// Start an asynchronous load of a `.slangp` shader preset.
    /// Poll progress and completion each frame via `pollShaderLoad`.
    pub fn loadShaderPreset(self: *Self, name: []const u8, path: []const u8) !void {
        const shader_pipeline = self.shaders_pipeline.get(name) orelse blk: {
            const new_pipeline = try pipeline.ShaderPipeline.init(self.allocator, self.io, self.gpu_device, self.vk_version);
            errdefer new_pipeline.deinit();

            const owned_name = try self.allocator.dupe(u8, name);
            errdefer self.allocator.free(owned_name);

            try self.shaders_pipeline.put(owned_name, new_pipeline);
            break :blk new_pipeline;
        };

        const swapchain_format = c.SDL_GetGPUSwapchainTextureFormat(self.gpu_device, self.current_window.ptr);
        try shader_pipeline.loadPreset(path, swapchain_format);
    }

    /// Poll the status of an in-progress async shader load.
    pub fn pollShaderLoad(self: *Self, name: []const u8) pipeline.ShaderPipeline.ShaderLoadPoll {
        const shader_pipe = self.shaders_pipeline.get(name) orelse return .idle;
        return shader_pipe.pollLoadResult();
    }

    /// Return the current async compile progress counters.
    pub fn getShaderProgress(self: *const Self, name: []const u8) struct { completed: u32, total: u32 } {
        const shader_pipe = self.shaders_pipeline.get(name) orelse return .{ .completed = 0, .total = 0 };
        return .{
            .completed = shader_pipe.compile_progress.load(.monotonic),
            .total = shader_pipe.compile_total,
        };
    }

    /// Clear the active shader preset and return to passthrough rendering.
    pub fn clearShaderPreset(self: *Self, name: []const u8) void {
        const shader_pipe = self.shaders_pipeline.get(name) orelse return;
        shader_pipe.unloadPreset();
    }

    /// Returns the path of the currently loaded shader preset, or null if none.
    pub fn getShaderPresetPath(self: *const Self, name: []const u8) ?[]const u8 {
        const shader_pipe = self.shaders_pipeline.get(name) orelse return null;
        return shader_pipe.getPresetPath();
    }

    pub fn createWindow(
        self: *Self,
        title: []const u8,
        width: i32,
        height: i32,
        params: struct {
            user_data: ?*anyopaque = null,
            draw_fn: ?*const fn (*UI, user_data: ?*anyopaque) void = null,
            on_close: ?*const fn (*UI, user_data: ?*anyopaque) void = null,
        },
    ) *Window {
        const previous_clay_ctx = clay.getCurrentContext();
        const debug_mode_enabled = clay.isDebugModeEnabled();

        const window = self.allocator.create(Window) catch @panic("OOM");
        const win_ptr = sdlError(c.SDL_CreateWindow(
            title.ptr,
            width,
            height,
            c.SDL_WINDOW_ALWAYS_ON_TOP | c.SDL_WINDOW_HIGH_PIXEL_DENSITY,
        ));
        const display_scale = Window.currentDisplayScale(win_ptr);
        window.* = .{
            .ptr = win_ptr,
            .logical_width = @as(f32, @floatFromInt(width)) / display_scale,
            .logical_height = @as(f32, @floatFromInt(height)) / display_scale,
            .pixel_width = @intCast(width),
            .pixel_height = @intCast(height),
            .window_width = width,
            .window_height = height,
            .display_scale = display_scale,
            .safe_area = Window.fullWindowArea(width, height),
            .title = title,
            .ctx = UIContext.init(
                self.allocator,
                .{ .w = @floatFromInt(width), .h = @floatFromInt(height) },
                CLAY_ERROR_HANDLER,
            ) catch @panic("OOM"),
            .renderer = Renderer.init(
                self.allocator,
                self.io,
                self.gpu_device,
                win_ptr,
                vulkan.detect_vulkan_version(),
            ) catch @panic("OOM"),
            .font_user_data = .{
                .font_cache = self.font_cache,
                .scale = &window.display_scale,
                .measure_cache = &window.ctx.text_measure_cache,
            },
        };
        if (display_scale > 1.0) {
            const new_width: i32 = @intFromFloat(@as(f32, @floatFromInt(window.window_width)) * display_scale);
            sdlError(c.SDL_SetWindowSize(window.ptr, new_width, window.window_height));
            window.updateWindowSize();
        }

        clay.setDebugModeEnabled(debug_mode_enabled);
        setWindowIcon(window.ptr);
        sdlError(c.SDL_SetWindowParent(window.ptr, self.main_window.ptr));
        sdlError(c.SDL_SetWindowModal(window.ptr, true));

        clay.setMeasureTextFunction(*const FontUserData, &window.font_user_data, measureText);

        self.secondary_windows.append(self.allocator, .{
            .inner = window,
            .original_width = width,
            .user_data = params.user_data,
            .draw_fn = params.draw_fn,
            .on_close = params.on_close,
        }) catch @panic("OOM");

        clay.setCurrentContext(previous_clay_ctx);
        return window;
    }

    pub fn closeCurrentWindow(self: *const Self) void {
        self.closeWindow(self.current_window.id());
    }

    pub fn closeWindow(_: *const Self, window_id: c.SDL_WindowID) void {
        var event: c.SDL_Event = std.mem.zeroes(c.SDL_Event);
        event.type = c.SDL_EVENT_WINDOW_CLOSE_REQUESTED;
        event.window.type = c.SDL_EVENT_WINDOW_CLOSE_REQUESTED;
        event.window.timestamp = c.SDL_GetTicksNS();
        event.window.windowID = window_id;

        sdlError(c.SDL_PushEvent(&event));
    }

    pub fn setClipboardText(self: *Self, value: []const u8) !void {
        const terminated = try self.allocator.dupeZ(u8, value);
        defer self.allocator.free(terminated);
        sdlError(c.SDL_SetClipboardText(terminated));
    }

    pub fn beginFrame(self: *Self) void {
        self.main_window.ctx.reset();
        resetMouseCursor();

        for (self.secondary_windows.items) |window| {
            window.inner.ctx.reset();
        }
        for (self.gamepads.items) |*state| {
            state.advanceFrame();
        }

        if (self.main_window.ctx.tickTimer("title_update", 1000)) {
            const title = std.fmt.allocPrintSentinel(
                self.main_window.ctx.frameAlloc(),
                "{s} - FPS: {}",
                .{ self.main_window.title, self.fps_manager.getFPS() },
                0,
            ) catch "0";
            sdlError(c.SDL_SetWindowTitle(self.main_window.ptr, title.ptr));
        }

        var event: c.SDL_Event = undefined;
        while (c.SDL_PollEvent(&event)) {
            self.handleEvent(&event);
        }

        self.processPendingWindowClose();

        self.main_window.update();
        self.updateGamepadStates(self.main_window.ctx.dt);
        self.on_screen_controller = .{};

        clay.beginLayout();
    }

    /// Begin a frame from an SDL live-resize event watcher. On Windows the
    /// normal event loop is blocked by the system's modal resize loop, so this
    /// must not poll SDL events.
    pub fn beginFrameNoSDLEvents(self: *Self) void {
        self.main_window.ctx.reset();
        for (self.secondary_windows.items) |window| {
            window.inner.ctx.reset();
        }

        // Resize events are still queued while the Win32 modal loop is active.
        // Query the window directly so this frame uses the current window size.
        self.main_window.updateWindowSize();
        self.main_window.update();

        clay.beginLayout();
    }

    pub fn endFrame(self: *Self) void {
        const render_commands = clay.endLayout(self.current_window.ctx.dt);
        self.syncTextInput(self.main_window);
        self.renderCommands(self.main_window, render_commands);

        // Render secondary windows
        for (self.secondary_windows.items) |window| {
            window.inner.update();

            clay.beginLayout();

            if (window.draw_fn) |draw_fn| {
                draw_fn(self, window.user_data);
            }

            const secondary_render_commands = clay.endLayout(self.current_window.ctx.dt);
            self.syncTextInput(window.inner);
            self.renderCommands(window.inner, secondary_render_commands);
        }

        clay.setCurrentContext(self.main_window.ctx.clay_ctx);
        applyMouseCursor();
    }

    fn syncTextInput(_: *const Self, window: *Window) void {
        const focused_id = window.ctx.frame.focused_id orelse return;

        const data = clay.getElementData(focused_id);
        if (!data.found) {
            window.ctx.frame.focused_id = null;
            if (c.SDL_TextInputActive(window.ptr)) sdlError(c.SDL_StopTextInput(window.ptr));
            sdlError(c.SDL_SetTextInputArea(window.ptr, null, 0));
            return;
        }

        const bb = data.bounding_box;
        const area = window.logicalBoundsToPixel(bb.x, bb.y, bb.width, bb.height);
        sdlError(c.SDL_SetTextInputArea(window.ptr, &area, area.w));
        if (!c.SDL_TextInputActive(window.ptr)) sdlError(c.SDL_StartTextInput(window.ptr));
    }

    pub fn isKeyPressed(self: *const Self, key: Key) bool {
        return self.current_window.ctx.input.isKeyPressed(key, false);
    }

    pub fn androidBackRequested(self: *Self) bool {
        if (!builtin.abi.isAndroid()) return false;
        if (self.android_back_requested) {
            self.android_back_requested = false;
            return true;
        } else {
            return false;
        }
    }

    pub fn isKeyDown(self: *const Self, key: Key) bool {
        return self.current_window.ctx.input.isKeyDown(key);
    }

    pub fn getPressedKey(self: *const Self) ?Key {
        return self.current_window.ctx.input.getPressedKey();
    }

    pub fn getReleasedKey(self: *const Self) ?Key {
        return self.current_window.ctx.input.getReleasedKey();
    }

    pub fn mouseMotion(self: *const Self) bool {
        return self.current_window.ctx.mouseMotion();
    }

    pub fn measureTextWidth(self: *const Self, text: []const u8, font_size: u16) f32 {
        return self.current_window.font_user_data.measure(text, font_size).w;
    }

    pub fn getGamepadCount(self: *const Self) usize {
        return self.gamepads.items.len;
    }

    pub fn pressOnScreenControllerButton(self: *Self, action: ControllerAction) void {
        self.on_screen_controller.insert(action.button());
    }

    pub fn onScreenControllerStatus(self: *const Self) ControllerButton {
        return self.on_screen_controller;
    }

    pub fn isGamepadButtonDown(self: *const Self, gamepad_idx: usize, btn: GamepadButton) bool {
        return switch (self.gamepads.items[gamepad_idx].buttons[@intFromEnum(btn)].event) {
            .pressed, .down => true,
            .released, .none => false,
        };
    }

    pub fn getPressedGamepadButton(self: *const Self) ?PressedGamepadButton {
        for (self.gamepads.items, 0..) |*state, i| {
            for (state.buttons, 0..) |btn, b| {
                if (btn.event == .pressed) {
                    return .{ .gamepad_idx = i, .btn = @enumFromInt(b) };
                }
            }
        }
        return null;
    }

    fn updateGamepadStates(self: *Self, dt: f32) void {
        for (self.gamepads.items) |*state| state.update(dt);
    }

    fn handleAndroidLowMemory(self: *Self) void {
        self.main_window.ctx.freeFrameArena();
        self.main_window.renderer.releaseMemory();
        for (self.secondary_windows.items) |window| {
            window.inner.ctx.freeFrameArena();
            window.inner.renderer.releaseMemory();
        }

        if (!self.is_suspended) {
            self.main_window.ctx.releaseCachedTextures(self.gpu_device);
            for (self.secondary_windows.items) |window| {
                window.inner.ctx.releaseCachedTextures(self.gpu_device);
            }
        }
    }

    pub fn getGamepadAxis(self: *const Self, gamepad_idx: usize, axis: c.SDL_GamepadAxis) i16 {
        return c.SDL_GetGamepadAxis(self.gamepads.items[gamepad_idx].handle, axis);
    }

    pub fn getConnectedGamepadName(self: *const Self, index: usize) ?[]const u8 {
        if (index >= self.gamepads.items.len) return null;
        const name = c.SDL_GetGamepadName(self.gamepads.items[index].handle);
        if (name == null) return null;
        return std.mem.span(name);
    }

    fn handleEvent(self: *Self, event: *c.SDL_Event) void {
        switch (event.type) {
            c.SDL_EVENT_QUIT, c.SDL_EVENT_TERMINATING => {
                self.quit = true;
                return;
            },
            c.SDL_EVENT_LOW_MEMORY => {
                self.handleAndroidLowMemory();
                return;
            },
            c.SDL_EVENT_WILL_ENTER_BACKGROUND => { // TODO: pause the emulation on background
                self.is_suspended = true;
                return;
            },
            c.SDL_EVENT_DID_ENTER_BACKGROUND => return,
            c.SDL_EVENT_WILL_ENTER_FOREGROUND => return,
            c.SDL_EVENT_DID_ENTER_FOREGROUND => {
                self.is_suspended = false;
                return;
            },
            c.SDL_EVENT_WINDOW_FOCUS_GAINED => {
                const focused_win = event.window.windowID;
                if (focused_win == self.main_window.id()) {
                    self.current_window = self.main_window;
                } else {
                    for (self.secondary_windows.items) |window| {
                        if (window.inner.id() == focused_win) {
                            self.current_window = window.inner;
                        }
                    }
                }

                return;
            },
            c.SDL_EVENT_GAMEPAD_ADDED => {
                const which = event.gdevice.which;
                for (self.gamepads.items) |*state| {
                    if (c.SDL_GetGamepadID(state.handle) == which) return;
                }
                self.gamepads.append(self.allocator, GamepadState.init(c.SDL_OpenGamepad(which))) catch @panic("OOM");
                self.setTimer("gamepad_connected_toast", 2500);
                return;
            },
            c.SDL_EVENT_GAMEPAD_REMOVED => {
                const removed_id = event.gdevice.which;
                for (self.gamepads.items, 0..) |*state, i| {
                    if (c.SDL_GetGamepadID(state.handle) == removed_id) {
                        c.SDL_CloseGamepad(state.handle);
                        _ = self.gamepads.swapRemove(i);
                        self.setTimer("gamepad_disconnected_toast", 2500);
                        break;
                    }
                }
                return;
            },
            c.SDL_EVENT_GAMEPAD_BUTTON_DOWN => {
                const which = event.gbutton.which;
                for (self.gamepads.items) |*state| {
                    if (c.SDL_GetGamepadID(state.handle) == which) {
                        state.addButtonEvent(event.gbutton.button, .down);
                        break;
                    }
                }
                return;
            },
            c.SDL_EVENT_GAMEPAD_BUTTON_UP => {
                const which = event.gbutton.which;
                for (self.gamepads.items) |*state| {
                    if (c.SDL_GetGamepadID(state.handle) == which) {
                        state.addButtonEvent(event.gbutton.button, .released);
                        break;
                    }
                }
                return;
            },
            else => {},
        }

        // Only process the events of the focused window
        if (event.window.windowID == self.current_window.id() or event.tfinger.windowID == self.current_window.id()) {
            self.handleWindowEvent(event);
        }
    }

    fn handleWindowEvent(self: *Self, event: *c.SDL_Event) void {
        switch (event.type) {
            c.SDL_EVENT_WINDOW_CLOSE_REQUESTED => {
                const closed_window_id = event.window.windowID;
                if (closed_window_id == self.main_window.id()) {
                    self.quit = true;
                } else {
                    // We only have one secondary window opened at most, so this is fine.
                    const secondary_window = self.secondary_windows.pop() orelse unreachable;
                    if (secondary_window.on_close) |callback| callback(self, secondary_window.user_data);
                    self.pending_close_window = secondary_window.inner;

                    if (self.current_window == secondary_window.inner) {
                        self.current_window = self.main_window;
                    }
                }
            },
            c.SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED => {
                if (event.window.windowID == self.main_window.id()) {
                    self.main_window.updateWindowSize();
                    return;
                }

                // There's only one secondary window in this program, which is the "Settings" window.
                // We need to update the width of the "Settings" window proportionally to the scale so the UI elements
                // are correctly placed.
                for (self.secondary_windows.items) |window| {
                    const display_scale = Window.currentDisplayScale(window.inner.ptr);
                    const new_width: i32 = @intFromFloat(@as(f32, @floatFromInt(window.original_width)) * display_scale);
                    sdlError(c.SDL_SetWindowSize(window.inner.ptr, new_width, window.inner.window_height));

                    window.inner.updateWindowSize();
                }
            },
            c.SDL_EVENT_WINDOW_SAFE_AREA_CHANGED => sdlError(c.SDL_GetWindowSafeArea(
                self.current_window.ptr,
                &self.current_window.safe_area,
            )),
            c.SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED, c.SDL_EVENT_WINDOW_RESIZED => self.current_window.updateWindowSize(),
            c.SDL_EVENT_MOUSE_MOTION => self.current_window.ctx.updateMousePos(
                event.motion,
                self.current_window.mouseScaleX(),
                self.current_window.mouseScaleY(),
            ),
            c.SDL_EVENT_FINGER_DOWN => {
                const pos = self.current_window.logicalTouchOffset(event.tfinger.x, event.tfinger.y);

                self.current_window.ctx.updatePointerPosition(pos.x, pos.y, 0, 0);
                self.current_window.ctx.setFingerPos(event.tfinger.fingerID, pos.x, pos.y);
                self.current_window.ctx.setFingerDown(event.tfinger.fingerID, true);
                self.current_window.ctx.beginPointerDown();
            },
            c.SDL_EVENT_FINGER_UP, c.SDL_EVENT_FINGER_CANCELED => {
                self.current_window.ctx.setFingerDown(event.tfinger.fingerID, false);

                self.current_window.ctx.frame.mouse_released = true;
                self.current_window.ctx.frame.mouse_down = self.current_window.ctx.hasFingerDown();
            },
            c.SDL_EVENT_MOUSE_BUTTON_DOWN => {
                if (event.button.button == c.SDL_BUTTON_LEFT) {
                    self.current_window.ctx.beginPointerDown();
                }
            },
            c.SDL_EVENT_MOUSE_BUTTON_UP => {
                if (event.button.button == c.SDL_BUTTON_LEFT) {
                    self.current_window.ctx.frame.mouse_released = true;
                    self.current_window.ctx.frame.mouse_down = false;
                }
            },
            c.SDL_EVENT_MOUSE_WHEEL => {
                const scroll_multiplier = 2.0;
                self.current_window.ctx.frame.scroll.velocity_x += event.wheel.x * scroll_multiplier;
                self.current_window.ctx.frame.scroll.velocity_y += event.wheel.y * scroll_multiplier;
            },
            c.SDL_EVENT_KEY_DOWN => {
                if (builtin.abi.isAndroid() and event.key.scancode == c.SDL_SCANCODE_AC_BACK) {
                    self.android_back_requested = true;
                }
                self.current_window.ctx.input.addKeyEvent(.{ .event = .down, .scancode = event.key.scancode });
            },
            c.SDL_EVENT_KEY_UP => self.current_window.ctx.input.addKeyEvent(.{ .event = .released, .scancode = event.key.scancode }),
            c.SDL_EVENT_TEXT_INPUT => {
                const text: []const u8 = std.mem.span(event.text.text);
                self.current_window.ctx.frame.text_input.appendSlice(self.current_window.ctx.frameAlloc(), text) catch
                    @panic("Fafiled to allocate memory!");
            },
            else => {},
        }
    }

    fn processPendingWindowClose(self: *Self) void {
        const window = self.pending_close_window orelse return;

        var mouse_x: f32 = 0;
        var mouse_y: f32 = 0;
        // Make sure we have no mouse events to be prossed, otherwise the program crashes when closing the window.
        if (c.SDL_GetMouseState(&mouse_x, &mouse_y) != 0) {
            return;
        }

        self.pending_close_window = null;
        window.deinit(self.allocator, self.gpu_device);
    }

    fn renderCommands(self: *Self, window: *Window, commands: []clay.RenderCommand) void {
        window.renderer.reset();
        self.font_cache.recreateAtlasTextureIfChanged(self.gpu_device) catch @panic("Failed to upload UI font atlas");

        const cmd = sdlError(c.SDL_AcquireGPUCommandBuffer(self.gpu_device));
        for (commands) |clay_cmd| {
            switch (clay_cmd.command_type) {
                clay.RenderCommandType.rectangle => self.renderRectangle(&clay_cmd, window),
                clay.RenderCommandType.text => self.renderText(&clay_cmd, window),
                clay.RenderCommandType.border => self.renderBorder(&clay_cmd, window),
                clay.RenderCommandType.image => self.renderImage(&clay_cmd, window),
                clay.RenderCommandType.scissor_start => {
                    const clip_rect = c.SDL_Rect{
                        .x = @intFromFloat(clay_cmd.bounding_box.x),
                        .y = @intFromFloat(clay_cmd.bounding_box.y),
                        .w = @intFromFloat(clay_cmd.bounding_box.width),
                        .h = @intFromFloat(clay_cmd.bounding_box.height),
                    };
                    window.renderer.pushClipRect(clip_rect);
                },
                clay.RenderCommandType.scissor_end => window.renderer.popClipRect(),
                clay.RenderCommandType.overlay_color_start => {
                    const color = clay_cmd.render_data.overlay_color.color;
                    window.renderer.pushOverlayColor(.{
                        .r = color[0] / 255.0,
                        .g = color[1] / 255.0,
                        .b = color[2] / 255.0,
                        .a = color[3] / 255.0,
                    });
                },
                clay.RenderCommandType.overlay_color_end => window.renderer.popOverlayColor(),
                clay.RenderCommandType.custom => self.renderCustom(&clay_cmd, cmd, window),
                else => {
                    std.log.warn("Render command not impl: {any}", .{clay_cmd.command_type});
                },
            }
        }

        var swapchain_tex: ?*c.SDL_GPUTexture = null;
        var win_w: u32 = 0;
        var win_h: u32 = 0;
        _ = c.SDL_WaitAndAcquireGPUSwapchainTexture(
            cmd,
            window.ptr,
            &swapchain_tex,
            &win_w,
            &win_h,
        );
        if (swapchain_tex == null) {
            sdlError(c.SDL_CancelGPUCommandBuffer(cmd));
            return;
        }

        const vertices_len: u32 = @intCast(window.renderer.vertices.items.len);
        const indices_len: u32 = @intCast(window.renderer.indices.items.len);
        const text_vertices_len: u32 = @intCast(window.renderer.text_vertices.items.len);
        const text_indices_len: u32 = @intCast(window.renderer.text_indices.items.len);
        const vertices_size = vertices_len * @sizeOf(UIVertex);
        const indices_size = indices_len * @sizeOf(u32);
        const text_vertices_size = text_vertices_len * @sizeOf(TextVertex);
        const text_indices_size = text_indices_len * @sizeOf(u32);
        window.renderer.resizeGPUBuffers(vertices_len, indices_len, text_vertices_len, text_indices_len);

        var texture_uploads_size: u32 = 0;
        for (window.renderer.textures.items) |upload| {
            texture_uploads_size += upload.byteSize();
        }
        const total_upload_size = texture_uploads_size + vertices_size + indices_size + text_vertices_size + text_indices_size;
        if (total_upload_size > 0) {
            const transfer_buffer = window.renderer.createFrameTransferBuffer(total_upload_size).?;
            const ptr: [*]u8 = @ptrCast(@alignCast(sdlError(c.SDL_MapGPUTransferBuffer(
                self.gpu_device,
                transfer_buffer,
                true,
            ))));

            var copy_offset: u32 = 0;
            for (window.renderer.textures.items) |upload| {
                const upload_size = upload.byteSize();
                @memcpy(ptr[copy_offset .. copy_offset + upload_size], upload.pixels[0..upload_size]);
                copy_offset += upload_size;
            }
            if (vertices_size > 0) {
                @memcpy(ptr[copy_offset .. copy_offset + vertices_size], std.mem.sliceAsBytes(window.renderer.vertices.items));
                copy_offset += vertices_size;
                @memcpy(ptr[copy_offset .. copy_offset + indices_size], std.mem.sliceAsBytes(window.renderer.indices.items));
                copy_offset += indices_size;
            }
            if (text_vertices_size > 0) {
                @memcpy(ptr[copy_offset .. copy_offset + text_vertices_size], std.mem.sliceAsBytes(window.renderer.text_vertices.items));
                copy_offset += text_vertices_size;
                @memcpy(ptr[copy_offset .. copy_offset + text_indices_size], std.mem.sliceAsBytes(window.renderer.text_indices.items));
            }
            c.SDL_UnmapGPUTransferBuffer(self.gpu_device, transfer_buffer);

            const copy_pass = c.SDL_BeginGPUCopyPass(cmd);

            copy_offset = 0;
            for (window.renderer.textures.items) |upload| {
                const upload_size = upload.byteSize();
                c.SDL_UploadToGPUTexture(
                    copy_pass,
                    &.{ .transfer_buffer = transfer_buffer, .offset = copy_offset },
                    &.{ .texture = upload.texture, .w = upload.width, .h = upload.height, .d = 1 },
                    false,
                );
                copy_offset += upload_size;
            }

            if (vertices_size > 0) {
                c.SDL_UploadToGPUBuffer(
                    copy_pass,
                    &.{ .transfer_buffer = transfer_buffer, .offset = copy_offset },
                    &.{ .buffer = window.renderer.vertex_buffer.?, .offset = 0, .size = vertices_size },
                    false,
                );
                copy_offset += vertices_size;

                c.SDL_UploadToGPUBuffer(
                    copy_pass,
                    &.{ .transfer_buffer = transfer_buffer, .offset = copy_offset },
                    &.{ .buffer = window.renderer.index_buffer.?, .offset = 0, .size = indices_size },
                    false,
                );
                copy_offset += indices_size;
            }

            if (text_vertices_size > 0) {
                c.SDL_UploadToGPUBuffer(
                    copy_pass,
                    &.{ .transfer_buffer = transfer_buffer, .offset = copy_offset },
                    &.{ .buffer = window.renderer.text_vertex_buffer.?, .offset = 0, .size = text_vertices_size },
                    false,
                );
                copy_offset += text_vertices_size;

                c.SDL_UploadToGPUBuffer(
                    copy_pass,
                    &.{ .transfer_buffer = transfer_buffer, .offset = copy_offset },
                    &.{ .buffer = window.renderer.text_index_buffer.?, .offset = 0, .size = text_indices_size },
                    false,
                );
            }
            c.SDL_EndGPUCopyPass(copy_pass);
        }

        const MVP = [16]f32{
            2.0 / window.logical_width, 0,                            0, 0,
            0,                          -2.0 / window.logical_height, 0, 0,
            0,                          0,                            1, 0,
            -1,                         1,                            0, 1,
        };

        self.renderDrawCalls(
            window,
            cmd,
            swapchain_tex,
            win_w,
            win_h,
            &MVP,
            c.SDL_GPU_LOADOP_CLEAR,
        );

        sdlError(c.SDL_SubmitGPUCommandBuffer(cmd));
    }

    fn renderDrawCalls(
        _: *Self,
        window: *Window,
        cmd: ?*c.SDL_GPUCommandBuffer,
        swapchain_tex: ?*c.SDL_GPUTexture,
        win_w: u32,
        win_h: u32,
        mvp: *const [16]f32,
        load_op: c.SDL_GPULoadOp,
    ) void {
        const color_target = c.SDL_GPUColorTargetInfo{
            .texture = swapchain_tex,
            .clear_color = .{ .r = 0.94, .g = 0.94, .b = 0.98, .a = 1.0 },
            .load_op = load_op,
            .store_op = c.SDL_GPU_STOREOP_STORE,
        };
        const render_pass = c.SDL_BeginGPURenderPass(cmd, &color_target, 1, null);
        c.SDL_PushGPUVertexUniformData(cmd, 0, mvp, @sizeOf(@TypeOf(mvp.*)));

        var bound_kind: ?DrawKind = null;
        for (window.renderer.draw_calls.items) |call| {
            if (call.index_count == 0) continue;
            if (bound_kind != call.kind) {
                switch (call.kind) {
                    .ui => {
                        c.SDL_BindGPUGraphicsPipeline(render_pass, window.renderer.pipeline);
                        c.SDL_BindGPUVertexBuffers(render_pass, 0, &.{ .buffer = window.renderer.vertex_buffer, .offset = 0 }, 1);
                        c.SDL_BindGPUIndexBuffer(render_pass, &.{ .buffer = window.renderer.index_buffer, .offset = 0 }, c.SDL_GPU_INDEXELEMENTSIZE_32BIT);
                    },
                    .text => {
                        c.SDL_BindGPUGraphicsPipeline(render_pass, window.renderer.text_pipeline);
                        c.SDL_BindGPUVertexBuffers(render_pass, 0, &.{ .buffer = window.renderer.text_vertex_buffer, .offset = 0 }, 1);
                        c.SDL_BindGPUIndexBuffer(render_pass, &.{ .buffer = window.renderer.text_index_buffer, .offset = 0 }, c.SDL_GPU_INDEXELEMENTSIZE_32BIT);
                    },
                }
                bound_kind = call.kind;
            }
            c.SDL_BindGPUFragmentSamplers(
                render_pass,
                0,
                &.{ .texture = call.texture, .sampler = window.renderer.sampler },
                1,
            );
            if (call.clip_rect) |rect| {
                const scissor = window.logicalRectToPixel(rect);
                c.SDL_SetGPUScissor(render_pass, &scissor);
            } else {
                c.SDL_SetGPUScissor(render_pass, &.{ .x = 0, .y = 0, .w = @intCast(win_w), .h = @intCast(win_h) });
            }
            c.SDL_DrawGPUIndexedPrimitives(render_pass, call.index_count, 1, call.index_offset, 0, 0);
        }
        c.SDL_EndGPURenderPass(render_pass);
    }

    fn uploadCanvasTexture(
        self: *Self,
        window: *Window,
        id: u32,
        pixel_format: c_uint,
        pixels: []const u8,
        w: u32,
        h: u32,
    ) ?*c.SDL_GPUTexture {
        var texture: ?*c.SDL_GPUTexture = undefined;
        var should_create = true;

        if (window.ctx.canvas_cache.getPtr(id)) |item| {
            if (item.width != w or item.height != h) {
                c.SDL_ReleaseGPUTexture(self.gpu_device, item.texture);
            } else {
                texture = item.texture;
                should_create = false;
            }
        }

        if (should_create) {
            texture = sdlError(c.SDL_CreateGPUTexture(self.gpu_device, &.{
                .type = c.SDL_GPU_TEXTURETYPE_2D,
                .format = c.SDL_GetGPUTextureFormatFromPixelFormat(pixel_format),
                .width = @intCast(w),
                .height = @intCast(h),
                .layer_count_or_depth = 1,
                .num_levels = 1,
                .usage = c.SDL_GPU_TEXTUREUSAGE_SAMPLER,
            }));

            window.ctx.canvas_cache.put(id, .{
                .texture = texture,
                .width = w,
                .height = h,
            }) catch @panic("Failed to cache canvas texture");
        }

        window.renderer.pushTexture(.{
            .texture = texture,
            .pixels = pixels,
            .width = w,
            .height = h,
            .pixel_format = pixel_format,
        });

        return texture;
    }

    fn drawCanvasBackground(
        window: *Window,
        bounds: clay.BoundingBox,
        bg_color: ?Color,
        corner_radius: clay.CornerRadius,
    ) void {
        const bg_color_sdl: c.SDL_FColor = if (bg_color) |bg|
            bg.toSDL()
        else
            .{ .r = 0.0, .g = 0.0, .b = 0.0, .a = 0.0 };

        window.renderer.setTexture(null);
        window.renderer.pushRoundedTexturedRect(.{
            .x = bounds.x,
            .y = bounds.y,
            .w = bounds.width,
            .h = bounds.height,
        }, bg_color_sdl, corner_radius, null);
    }

    fn drawTextureInViewport(
        window: *Window,
        texture: ?*c.SDL_GPUTexture,
        canvas_bounds: clay.BoundingBox,
        style: widgets.Canvas.Params,
        color: c.SDL_FColor,
    ) void {
        const viewport = utils.calculateViewport(
            canvas_bounds.x,
            canvas_bounds.y,
            canvas_bounds.width,
            canvas_bounds.height,
            style.aspect_ratio,
            style.viewport_alignment,
        );
        window.renderer.setTexture(texture);
        window.renderer.pushRoundedTexturedRect(
            .{ .x = viewport.x, .y = viewport.y, .w = viewport.w, .h = viewport.h },
            color,
            style.corner_radius,
            null,
        );
    }

    fn renderCanvasWithShader(
        self: *Self,
        window: *Window,
        cmd: *const clay.RenderCommand,
        gpu_cmd: ?*c.SDL_GPUCommandBuffer,
        canvas_: widgets.Canvas,
        texture: ?*c.SDL_GPUTexture,
        bounds: clay.BoundingBox,
    ) void {
        const vp = utils.calculateViewport(
            bounds.x,
            bounds.y,
            bounds.width,
            bounds.height,
            canvas_.params.aspect_ratio,
            canvas_.params.viewport_alignment,
        );
        const color = if (canvas_.params.fg_color) |fg_color| fg_color.toSDL() else Color.white.toSDL();

        drawCanvasBackground(window, cmd.bounding_box, canvas_.params.bg_color, canvas_.params.corner_radius);

        const swapchain_format = c.SDL_GetGPUSwapchainTextureFormat(self.gpu_device, window.ptr);
        for (canvas_.shader_modes) |shader_mode| {
            const rect: c.SDL_FRect = switch (shader_mode.target) {
                .canvas => .{ .x = bounds.x, .y = bounds.y, .w = bounds.width, .h = bounds.height },
                .viewport => .{ .x = vp.x, .y = vp.y, .w = vp.w, .h = vp.h },
            };

            const shader_pipeline = self.shaders_pipeline.get(shader_mode.id) orelse continue;
            if (!shader_pipeline.isActive()) continue;

            const output = shader_pipeline.renderFrame(
                texture,
                canvas_.params.w,
                canvas_.params.h,
                window.logicalViewportToPixel(.{
                    .w = @intFromFloat(rect.w),
                    .h = @intFromFloat(rect.h),
                    .x = @intFromFloat(rect.x),
                    .y = @intFromFloat(rect.y),
                }),
                gpu_cmd,
                @intCast(window.pixel_width),
                @intCast(window.pixel_height),
                swapchain_format,
            ) catch |err| {
                std.log.err("Shader pipeline '{s}' render failed: {any}", .{ shader_mode.id, err });
                continue;
            };

            window.renderer.setTexture(output.?.ptr);
            window.renderer.pushRoundedTexturedRect(rect, color, canvas_.params.corner_radius, null);

            if (shader_mode.composition == .original_on_top) {
                drawTextureInViewport(window, texture, bounds, canvas_.params, color);
            }
        }
    }

    fn renderCanvas(
        _: *Self,
        window: *Window,
        bounds: clay.BoundingBox,
        texture: ?*c.SDL_GPUTexture,
        style: widgets.Canvas.Params,
    ) void {
        drawCanvasBackground(window, bounds, style.bg_color, style.corner_radius);
        const color = if (style.fg_color) |fg_color| fg_color.toSDL() else Color.white.toSDL();
        const canvas_bounds = utils.canvasContentBounds(bounds, style.padding);
        drawTextureInViewport(window, texture, canvas_bounds, style, color);
        window.renderer.flush();
    }

    fn renderCustom(self: *Self, clay_cmd: *const clay.RenderCommand, gpu_cmd: ?*c.SDL_GPUCommandBuffer, window: *Window) void {
        const data = clay.anyopaquePtrToType(*widgets.CustomData, clay_cmd.render_data.custom.custom_data.?);
        switch (data.*) {
            .canvas => |canvas_| {
                const canvas_bounds = utils.canvasContentBounds(clay_cmd.bounding_box, canvas_.params.padding);
                const texture = self.uploadCanvasTexture(
                    window,
                    clay_cmd.id,
                    canvas_.params.pixel_format,
                    canvas_.params.pixels,
                    canvas_.params.w,
                    canvas_.params.h,
                );

                if (self.hasAnyShaderLoaded(canvas_.shader_modes)) {
                    self.renderCanvasWithShader(window, clay_cmd, gpu_cmd, canvas_, texture, canvas_bounds);
                } else {
                    self.renderCanvas(window, clay_cmd.bounding_box, texture, canvas_.params);
                }
            },
            .shape => |shape_data| {
                const rect = clay_cmd.bounding_box;
                const x = rect.x;
                const y = rect.y;
                const w = rect.width;
                const h = rect.height;

                // Center of the bounding box
                const cx = x + (w / 2.0);
                const cy = y + (h / 2.0);

                const rads: f32 = std.math.degreesToRadians(shape_data.rotation);
                const cos_theta = std.math.cos(rads);
                const sin_theta = std.math.sin(rads);

                const color = shape_data.color.toSDL();

                window.renderer.setTexture(null);

                // We need at least 3 vertices to draw a visible shape
                if (shape_data.vertices.len >= 3) {
                    const transform = struct {
                        fn apply(
                            v: clay.Vector2,
                            bx: f32,
                            by: f32,
                            bw: f32,
                            bh: f32,
                            bcx: f32,
                            bcy: f32,
                            cos_a: f32,
                            sin_a: f32,
                        ) clay.Vector2 {
                            // Map normalized coordinates (0.0-1.0) to actual bounding box
                            const px = bx + v.x * bw;
                            const py = by + v.y * bh;

                            // Apply rotation around the center (bcx, bcy)
                            return .{
                                .x = cos_a * (px - bcx) - sin_a * (py - bcy) + bcx,
                                .y = sin_a * (px - bcx) + cos_a * (py - bcy) + bcy,
                            };
                        }
                    }.apply;

                    // Map and rotate the anchor vertex (index 0)
                    const p0 = transform(shape_data.vertices[0], x, y, w, h, cx, cy, cos_theta, sin_theta);

                    var i: usize = 1;
                    while (i + 1 < shape_data.vertices.len) : (i += 1) {
                        const p1 = transform(shape_data.vertices[i], x, y, w, h, cx, cy, cos_theta, sin_theta);
                        const p2 = transform(shape_data.vertices[i + 1], x, y, w, h, cx, cy, cos_theta, sin_theta);

                        window.renderer.pushTriangle(p0, p1, p2, color);
                    }
                }
            },
        }
    }

    fn renderImage(_: *const Self, cmd: *const clay.RenderCommand, window: *Window) void {
        const image = cmd.render_data.image;
        const image_texture = clay.anyopaquePtrToType(*c.SDL_GPUTexture, image.image_data);
        const background_color = image.background_color;
        const color: c.SDL_FColor = if (background_color[0] == 0 and background_color[1] == 0 and background_color[2] == 0 and background_color[3] == 0)
            Color.white.toSDL()
        else
            .{
                .r = background_color[0] / 255.0,
                .g = background_color[1] / 255.0,
                .b = background_color[2] / 255.0,
                .a = background_color[3] / 255.0,
            };

        window.renderer.setTexture(image_texture);
        window.renderer.pushRoundedTexturedRect(
            .{
                .x = cmd.bounding_box.x,
                .y = cmd.bounding_box.y,
                .w = cmd.bounding_box.width,
                .h = cmd.bounding_box.height,
            },
            color,
            image.corner_radius,
            null,
        );
    }

    fn renderRectangle(_: *const Self, cmd: *const clay.RenderCommand, window: *Window) void {
        const config = cmd.render_data.rectangle;
        const color = c.SDL_FColor{
            .r = config.background_color[0] / 255.0,
            .g = config.background_color[1] / 255.0,
            .b = config.background_color[2] / 255.0,
            .a = config.background_color[3] / 255.0,
        };
        const radius = cmd.render_data.rectangle.corner_radius;
        const rect = c.SDL_FRect{
            .x = cmd.bounding_box.x,
            .y = cmd.bounding_box.y,
            .w = cmd.bounding_box.width,
            .h = cmd.bounding_box.height,
        };

        window.renderer.setTexture(null);

        if (radius.top_left > 0.0 or radius.top_right > 0.0 or radius.bottom_left > 0.0 or radius.bottom_right > 0.0) {
            window.renderer.pushRoundedRect(rect, color, radius);
        } else {
            window.renderer.pushRect(rect, color, null);
        }
    }

    fn renderBorder(_: *const Self, cmd: *const clay.RenderCommand, window: *Window) void {
        window.renderer.setTexture(null);
        window.renderer.pushRoundedBorder(cmd.bounding_box, cmd.render_data.border);
    }

    fn renderText(self: *Self, cmd: *const clay.RenderCommand, window: *Window) void {
        const text_data = cmd.render_data.text;
        const color = text_data.text_color;
        const text_slice = text_data.string_contents;
        if (text_slice.length == 0) return;

        const text = text_slice.chars[0..@intCast(text_slice.length)];
        const scale = window.font_user_data.effectiveScale();

        _ = self.font_cache.cacheTextLayout(text_data.font_size, scale, text) catch @panic("Failed to rasterize UI text");

        const layout = self.font_cache.getLayout(text_data.font_size, scale, text) orelse return;
        const atlas_size: f32 = @floatFromInt(self.font_cache.atlas.size);
        var draw_color = c.SDL_FColor{
            .r = color[0] / 255.0,
            .g = color[1] / 255.0,
            .b = color[2] / 255.0,
            .a = color[3] / 255.0,
        };
        const overlay = window.renderer.current_overlay_color;
        draw_color.r = draw_color.r * (1.0 - overlay.a) + overlay.r * overlay.a;
        draw_color.g = draw_color.g * (1.0 - overlay.a) + overlay.g * overlay.a;
        draw_color.b = draw_color.b * (1.0 - overlay.a) + overlay.b * overlay.a;

        window.renderer.setTextTexture(self.font_cache.texture);
        for (layout.glyphs) |positioned| {
            const glyph = positioned.glyph;
            if (glyph.width > 0 and glyph.height > 0) {
                window.renderer.pushTextRect(.{
                    .x = cmd.bounding_box.x + (positioned.pen_x + @as(f32, @floatFromInt(glyph.offset_x))) / scale,
                    .y = cmd.bounding_box.y + (positioned.baseline_y - @as(f32, @floatFromInt(glyph.offset_y))) / scale,
                    .w = @as(f32, @floatFromInt(glyph.width)) / scale,
                    .h = @as(f32, @floatFromInt(glyph.height)) / scale,
                }, draw_color, .{
                    .x = @as(f32, @floatFromInt(glyph.atlas_x)) / atlas_size,
                    .y = @as(f32, @floatFromInt(glyph.atlas_y)) / atlas_size,
                    .w = @as(f32, @floatFromInt(glyph.width)) / atlas_size,
                    .h = @as(f32, @floatFromInt(glyph.height)) / atlas_size,
                });
            }
        }
    }

    fn hasAnyShaderLoaded(self: *const Self, shaders_mode: []const ShaderMode) bool {
        for (shaders_mode) |shader| {
            if (self.shaders_pipeline.get(shader.id)) |shader_pipeline| {
                if (shader_pipeline.isActive()) return true;
            }
        }
        return false;
    }

    pub fn scrollArea(self: *Self, params: widgets.ScrollContainer.Params) *widgets.ScrollContainer {
        return self.current_window.ctx.allocWidget(widgets.ScrollContainer, .start(self.current_window.ctx, params));
    }

    pub fn row(self: *Self, params: widgets.Container.Params) *widgets.Container {
        var p = params;
        p.direction = .row;
        return self.current_window.ctx.allocWidget(widgets.Container, .start(p));
    }

    pub fn column(self: *Self, params: widgets.Container.Params) *widgets.Container {
        var p = params;
        p.direction = .column;
        return self.current_window.ctx.allocWidget(widgets.Container, .start(p));
    }

    pub fn grid(self: *Self, params: widgets.Grid.Params) *widgets.Grid {
        return self.current_window.ctx.allocWidget(widgets.Grid, .start(self.current_window.ctx, params));
    }

    pub fn float(self: *Self, params: widgets.Float.Params) *widgets.Float {
        return self.current_window.ctx.allocWidget(widgets.Float, .start(params));
    }

    pub fn label(self: *Self, params: widgets.Label.Params) *widgets.Label {
        return self.current_window.ctx.allocWidget(widgets.Label, .start(params));
    }

    pub fn button(self: *Self, params: widgets.Button.Params) *widgets.Button {
        return self.current_window.ctx.allocWidget(widgets.Button, .start(params, self.current_window.ctx));
    }

    pub fn spacer(self: *Self, params: widgets.Spacer.Params) *widgets.Spacer {
        return self.current_window.ctx.allocWidget(widgets.Spacer, .start(params));
    }

    pub fn canvas(self: *Self, params: widgets.Canvas.Params) *widgets.Canvas {
        return self.current_window.ctx.allocWidget(widgets.Canvas, .start(self.current_window.ctx, params));
    }

    pub fn shaderMode(self: *Self, params: ShaderMode) *ShaderModeScope {
        return self.current_window.ctx.allocWidget(ShaderModeScope, .start(self.current_window.ctx, params));
    }

    pub fn menuBar(self: *Self, params: widgets.MenuBar.Params) *widgets.MenuBar {
        return self.current_window.ctx.allocWidget(widgets.MenuBar, .start(self.current_window.ctx, params));
    }

    pub fn dropdownMenu(self: *Self, params: widgets.DropdownMenu.Params) *widgets.DropdownMenu {
        return self.current_window.ctx.allocWidget(widgets.DropdownMenu, .start(self.current_window.ctx, params));
    }

    pub fn menuItem(self: *Self, params: widgets.MenuItem.Params) *widgets.MenuItem {
        return self.current_window.ctx.allocWidget(widgets.MenuItem, .start(self.current_window.ctx, params));
    }

    pub fn textField(self: *Self, params: widgets.TextField.Params) *widgets.TextField {
        return self.current_window.ctx.allocWidget(widgets.TextField, .start(self.current_window.ctx, params));
    }

    pub fn padding(self: *Self, params: widgets.Padding.Params) *widgets.Padding {
        return self.current_window.ctx.allocWidget(widgets.Padding, .start(params));
    }

    pub fn combobox(self: *Self, comptime Option: type, params: widgets.Combobox(Option).Params) *widgets.Combobox(Option) {
        return self.current_window.ctx.allocWidget(widgets.Combobox(Option), .start(self.current_window.ctx, params));
    }

    pub fn comboboxItem(self: *Self, params: widgets.ComboboxItem.Params) *widgets.ComboboxItem {
        return self.current_window.ctx.allocWidget(widgets.ComboboxItem, .start(self.current_window.ctx, params));
    }

    pub fn shape(self: *Self, params: widgets.Shape.Params) *widgets.Shape {
        return self.current_window.ctx.allocWidget(widgets.Shape, .start(self.current_window.ctx, params));
    }

    pub fn separator(self: *Self, params: widgets.Separator.Params) *widgets.Separator {
        return self.current_window.ctx.allocWidget(widgets.Separator, .start(params));
    }

    pub fn slider(self: *Self, params: widgets.Slider.Params) *widgets.Slider {
        return self.current_window.ctx.allocWidget(widgets.Slider, .start(self.current_window.ctx, params));
    }

    pub fn toggle(self: *Self, params: widgets.Toggle.Params) *widgets.Toggle {
        return self.current_window.ctx.allocWidget(widgets.Toggle, .start(self.current_window.ctx, params));
    }

    pub fn draggablePanel(self: *Self, params: widgets.DraggablePanel.Params) *widgets.DraggablePanel {
        return self.current_window.ctx.allocWidget(widgets.DraggablePanel, .start(self.current_window.ctx, params));
    }

    pub fn resizablePanel(self: *Self, params: widgets.ResizablePanel.Params) *widgets.ResizablePanel {
        return self.current_window.ctx.allocWidget(widgets.ResizablePanel, .start(self.current_window.ctx, params));
    }

    pub fn draggable(self: *Self, params: widgets.Draggable.Params) *widgets.Draggable {
        return self.current_window.ctx.allocWidget(widgets.Draggable, .start(self.current_window.ctx, params));
    }

    pub fn iconButton(self: *Self, params: widgets.IconButton.Params) *widgets.IconButton {
        return self.current_window.ctx.allocWidget(widgets.IconButton, .start(self.current_window.ctx, params));
    }

    pub fn icon(self: *Self, params: widgets.Icon.Params) *widgets.Icon {
        return self.current_window.ctx.allocWidget(widgets.Icon, .start(params));
    }
};
