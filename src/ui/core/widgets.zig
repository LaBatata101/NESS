const std = @import("std");
const builtin = @import("builtin");
const c = @import("../../root.zig").c;
const clay = @import("clay.zig");
const Color = @import("color.zig").Color;
pub const ui = @import("ui.zig");
pub const UIContext = ui.UIContext;
const viewport = @import("viewport.zig");

pub const CustomData = union(enum) {
    canvas: Canvas,
    shape: ShapeData,
};

pub const LayoutDirection = enum {
    row,
    column,
};

pub const Container = struct {
    id: clay.ElementId,
    params: Params,

    pub const Params = struct {
        id: ?[]const u8 = null,
        direction: LayoutDirection = .column,
        padding: clay.Padding = .{},
        gap: u16 = 0,
        sizing: clay.Sizing = .grow,
        bg_color: ?Color = null,
        hover_bg_color: ?Color = null,
        corner_radius: f32 = 0,
        border: clay.BorderElementConfig = .{},
        child_alignment: clay.ChildAlignment = .{ .x = .left, .y = .center },
        transition: clay.TransitionElementConfig = .{},
    };
    const Self = @This();

    pub fn start(params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        const layout_dir: clay.LayoutDirection = if (params.direction == .row) .left_to_right else .top_to_bottom;

        const is_hovered = params.hover_bg_color != null and clay.hovered();
        const effective_bg = if (is_hovered) params.hover_bg_color else params.bg_color;
        clay.configureOpenElement(.{
            .layout = .{
                .sizing = params.sizing,
                .padding = params.padding,
                .child_gap = params.gap,
                .child_alignment = params.child_alignment,
                .direction = layout_dir,
            },
            .background_color = if (effective_bg) |color| color.toClay() else clay.Color{ 0, 0, 0, 0 },
            .corner_radius = .all(params.corner_radius),
            .border = params.border,
            .transition = params.transition,
        });

        return .{ .id = element_id, .params = params };
    }

    pub fn end(_: *const Self) void {
        clay.closeElement();
    }

    pub fn clicked(self: *const Self, ctx: *UIContext) bool {
        const is_hovered = clay.pointerOver(self.id);
        return is_hovered and ctx.pointerReleased();
    }
};

pub const Grid = struct {
    id: clay.ElementId,
    container: Container,
    params: Params,
    items_per_row: usize,
    item_count: usize = 0,
    active_row: ?Container = null,

    pub const Params = struct {
        id: []const u8,
        gap: u16 = 0,
        sizing: clay.Sizing = .grow,
        padding: clay.Padding = .{},
        bg_color: ?Color = null,
        child_alignment: clay.ChildAlignment = .{ .x = .left, .y = .top },
        item_transition: clay.TransitionElementConfig = .{},
    };

    pub const Item = struct {
        pub fn end(_: Item) void {
            clay.closeElement();
        }
    };

    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const grid_id = clay.ElementId.ID(params.id);
        const first_item_id = clay.ElementId.IDI(params.id, 0);

        const padding_w: f32 = @floatFromInt(params.padding.left + params.padding.right);
        const available: f32 = clay.getLayoutDimensions().w - padding_w;

        // Derive column count from the first item's measured width last frame.
        // Defaults to 1 column on the first frame until a measurement exists.
        const item_data = clay.getElementData(first_item_id);
        const desired_items_per_row: usize = if (item_data.found and item_data.bounding_box.width > 0) blk: {
            const gap: f32 = @floatFromInt(params.gap);
            break :blk @max(1, @as(usize, @intFromFloat(
                @floor((available + gap) / (item_data.bounding_box.width + gap)),
            )));
        } else 1;

        const state = ctx.getOrCreateWidgetState(grid_id, .{ .grid = .{
            .items_per_row = desired_items_per_row,
        } });

        // On the frame after a resize Clay clears element position data, so skip
        // updating the column count that frame
        if (!ctx.clay_ctx.rootResizedLastFrame) {
            state.grid.items_per_row = desired_items_per_row;
        }

        const container = Container.start(.{
            .id = params.id,
            .direction = .column,
            .sizing = params.sizing,
            .padding = params.padding,
            .gap = params.gap,
            .bg_color = params.bg_color,
            .child_alignment = params.child_alignment,
        });

        return .{
            .id = grid_id,
            .container = container,
            .params = params,
            .items_per_row = state.grid.items_per_row,
        };
    }

    pub fn item(self: *Self) Item {
        if (self.item_count % self.items_per_row == 0) {
            if (self.active_row) |*active| active.end();
            self.active_row = Container.start(.{
                .direction = .row,
                .sizing = .{ .w = .grow, .h = .fit },
                .gap = self.params.gap,
                .child_alignment = .{ .x = .left, .y = .top },
            });
        }

        const index = self.item_count;
        self.item_count += 1;

        clay.openElementWithId(clay.ElementId.IDI(self.params.id, @intCast(index)));
        clay.configureOpenElement(.{
            .layout = .{ .sizing = .{ .w = .fit, .h = .fit } },
            .transition = self.params.item_transition,
        });
        return .{};
    }

    pub fn end(self: *Self) void {
        if (self.active_row) |*active| {
            active.end();
            self.active_row = null;
        }
        self.container.end();
    }
};

pub const Float = struct {
    id: clay.ElementId,

    pub const Params = struct {
        id: ?[]const u8 = null,
        offset: clay.Vector2 = .{ .x = 0, .y = 0 },
        expand: clay.Dimensions = .{ .w = 0, .h = 0 },
        parentId: u32 = 0,
        z_index: i16 = 0,
        attach_points: clay.FloatingAttachPoints = .{ .element = .left_top, .parent = .left_top },
        pointer_capture_mode: clay.PointerCaptureMode = .capture,
        attach_to: clay.FloatingAttachToElement = .to_none,
        clip_to: clay.FloatingClipToElement = .to_none,
        sizing: clay.Sizing = .fit,
        transition: clay.TransitionElementConfig = .{},
        no_render: bool = false,
        layout: struct {
            direction: LayoutDirection = .column,
            child_alignment: clay.ChildAlignment = .center,
            child_gap: u16 = 0,
            padding: clay.Padding = .{},
        } = .{},
    };
    const Self = @This();

    pub fn start(params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        clay.configureOpenElement(.{
            .floating = .{
                .offset = params.offset,
                .expand = params.expand,
                .parentId = params.parentId,
                .z_index = params.z_index,
                .attach_points = params.attach_points,
                .pointer_capture_mode = params.pointer_capture_mode,
                .attach_to = params.attach_to,
                .clip_to = params.clip_to,
            },
            .layout = .{
                .sizing = params.sizing,
                .child_alignment = params.layout.child_alignment,
                .child_gap = params.layout.child_gap,
                .padding = params.layout.padding,
                .direction = if (params.layout.direction == .row) .left_to_right else .top_to_bottom,
            },
            .transition = params.transition,
            .no_render = params.no_render,
        });

        return .{ .id = element_id };
    }

    pub fn end(_: *const Self) void {
        clay.closeElement();
    }
};

pub const DraggablePanel = struct {
    id: clay.ElementId,

    pub const Params = struct {
        id: ?[]const u8 = null,
        expand: clay.Dimensions = .{ .w = 0, .h = 0 },
        parentId: u32 = 0,
        z_index: i16 = 0,
        attach_points: clay.FloatingAttachPoints = .{ .element = .center_center, .parent = .center_center },
        pointer_capture_mode: clay.PointerCaptureMode = .capture,
        attach_to: clay.FloatingAttachToElement = .to_root,
        clip_to: clay.FloatingClipToElement = .to_none,
        sizing: clay.Sizing = .fit,
        bg_color: ?Color = null,
        border: clay.BorderElementConfig = .{},
        corner_radius: f32 = 0,
        grip_color: Color = Color.rgb(80, 90, 110),
    };
    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const eid = clay.ElementId.ID(id);
            clay.openElementWithId(eid);
            break :b eid;
        } else clay.openElement();

        const state = ctx.getOrCreateWidgetState(element_id, .{ .draggable_panel = .{
            .offset = .{ .x = 0, .y = 0 },
        } });

        clay.configureOpenElement(.{
            .floating = .{
                .offset = state.draggable_panel.offset,
                .expand = params.expand,
                .parentId = params.parentId,
                .z_index = params.z_index,
                .attach_points = params.attach_points,
                .pointer_capture_mode = params.pointer_capture_mode,
                .attach_to = params.attach_to,
                .clip_to = params.clip_to,
            },
            .layout = .{
                .sizing = params.sizing,
                .direction = .left_to_right,
                .child_alignment = .center,
            },
            .background_color = if (params.bg_color) |col| col.toClay() else .{ 0, 0, 0, 0 },
            .corner_radius = .all(params.corner_radius),
            .border = params.border,
        });

        // Render the internal drag handle (grip strip + separator)
        const dot_params = Shape.Params{
            .vertices = &Shape.SQUARE,
            .sizing = .{ .w = .fixed(3), .h = .fixed(3) },
            .color = params.grip_color,
        };
        const grip = DragHandle.start(ctx, element_id, .{
            .sizing = .{ .w = .grow, .h = .fit },
            .padding = .{ .left = 10, .right = 10, .top = 6, .bottom = 5 },
            .child_alignment = .center,
            .gap = 4,
        });
        {
            const col1 = Container.start(.{ .sizing = .fit, .gap = 3 });
            _ = Shape.start(ctx, dot_params);
            _ = Shape.start(ctx, dot_params);
            _ = Shape.start(ctx, dot_params);
            col1.end();
            const col2 = Container.start(.{ .sizing = .fit, .gap = 3 });
            _ = Shape.start(ctx, dot_params);
            _ = Shape.start(ctx, dot_params);
            _ = Shape.start(ctx, dot_params);
            col2.end();
        }
        grip.end();
        const has_border = params.border.width.left > 0 or
            params.border.width.right > 0 or
            params.border.width.top > 0 or
            params.border.width.bottom > 0;
        _ = Separator.start(.{
            .color = if (has_border) .{
                .r = @intFromFloat(params.border.color[0]),
                .g = @intFromFloat(params.border.color[1]),
                .b = @intFromFloat(params.border.color[2]),
                .a = @intFromFloat(params.border.color[3]),
            } else Color.rgb(35, 40, 50),
            .direction = .vertical,
            .thickness = 2,
        });

        return .{ .id = element_id };
    }

    pub fn end(_: *const Self) void {
        clay.closeElement();
    }
};

const DragHandle = struct {
    const Params = struct {
        sizing: clay.Sizing = .fit,
        padding: clay.Padding = .{},
        gap: u16 = 0,
        child_alignment: clay.ChildAlignment = .{ .x = .left, .y = .center },
    };
    const Self = @This();

    fn start(ctx: *UIContext, panel_id: clay.ElementId, params: Params) Self {
        const element_id = clay.openElement();

        const panel_state = ctx.getWidgetStateById(panel_id).?;

        const is_hovered = clay.pointerOver(element_id);
        if (is_hovered and ctx.frame.mouse_pressed) panel_state.draggable_panel.dragging = true;
        if (ctx.frame.mouse_released) panel_state.draggable_panel.dragging = false;

        const is_dragging = panel_state.draggable_panel.dragging and ctx.frame.mouse_down;
        if (is_dragging) {
            const dx = ctx.frame.mouse_delta.x;
            const dy = ctx.frame.mouse_delta.y;
            const data = clay.getElementData(panel_id);
            if (data.found) {
                const bb = data.bounding_box;
                const dimensions = clay.getLayoutDimensions();
                panel_state.draggable_panel.offset.x += std.math.clamp(dx, -bb.x, dimensions.w - bb.x - bb.width);
                panel_state.draggable_panel.offset.y += std.math.clamp(dy, -bb.y, dimensions.h - bb.y - bb.height);
            } else {
                panel_state.draggable_panel.offset.x += dx;
                panel_state.draggable_panel.offset.y += dy;
            }
        }

        ui.setMouseCursorMove(is_hovered or is_dragging);

        clay.configureOpenElement(.{
            .layout = .{
                .sizing = params.sizing,
                .padding = params.padding,
                .child_gap = params.gap,
                .direction = .left_to_right,
                .child_alignment = params.child_alignment,
            },
        });

        return .{};
    }

    fn end(_: *const Self) void {
        clay.closeElement();
    }
};

pub const Draggable = struct {
    offset_dt: clay.Vector2,

    pub const Params = struct {
        id: ?[]const u8 = null,
        parentId: u32 = 0,
        z_index: i16 = 0,
        attach_points: clay.FloatingAttachPoints = .{ .element = .left_top, .parent = .left_top },
        attach_to: clay.FloatingAttachToElement = .to_root,
        pointer_capture_mode: clay.PointerCaptureMode = .capture,
        clip_to: clay.FloatingClipToElement = .to_none,
        offset: clay.Vector2 = .{ .x = 0, .y = 0 },
        bounds: ?clay.BoundingBox = null,
    };
    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        const state = ctx.getOrCreateWidgetState(element_id, .{ .draggable_panel = .{
            .offset = params.offset,
        } });

        const is_hovered = clay.pointerOver(element_id);
        if (is_hovered and ctx.frame.mouse_pressed) state.draggable_panel.dragging = true;
        if (ctx.frame.mouse_released) state.draggable_panel.dragging = false;

        const is_dragging = state.draggable_panel.dragging and ctx.frame.mouse_down;
        if (is_dragging) {
            const dx = ctx.frame.mouse_delta.x;
            const dy = ctx.frame.mouse_delta.y;
            const bb = clay.getElementData(element_id).bounding_box;
            const dimensions = clay.getLayoutDimensions();
            state.draggable_panel.offset.x += std.math.clamp(dx, -bb.x, dimensions.w - bb.x - bb.width);
            state.draggable_panel.offset.y += std.math.clamp(dy, -bb.y, dimensions.h - bb.y - bb.height);
        }

        if (params.bounds) |bounds| {
            state.draggable_panel.offset = clampOffsetToBoundingBox(
                state.draggable_panel.offset,
                params.attach_points,
                element_id,
                bounds,
            );
        }

        ui.setMouseCursorMove(is_hovered or is_dragging);

        clay.configureOpenElement(.{
            .layout = .{ .sizing = .fit },
            .floating = .{
                .offset = state.draggable_panel.offset,
                .attach_points = params.attach_points,
                .attach_to = params.attach_to,
                .parentId = params.parentId,
                .z_index = params.z_index,
                .pointer_capture_mode = params.pointer_capture_mode,
                .clip_to = params.clip_to,
            },
        });

        return .{ .offset_dt = state.draggable_panel.offset };
    }

    pub fn offset(self: *const Self) clay.Vector2 {
        return self.offset_dt;
    }

    pub fn end(_: *const Self) void {
        clay.closeElement();
    }

    fn clampOffsetToBoundingBox(
        value: clay.Vector2,
        attach_points: clay.FloatingAttachPoints,
        element_id: clay.ElementId,
        bounds: clay.BoundingBox,
    ) clay.Vector2 {
        const data = clay.getElementData(element_id);
        std.debug.assert(data.found);

        const bounds_size = clay.Dimensions{ .w = bounds.width, .h = bounds.height };
        const element_size = clay.Dimensions{ .w = data.bounding_box.width, .h = data.bounding_box.height };

        const parent_anchor = attachPointOffset(attach_points.parent, bounds_size);
        const element_anchor = attachPointOffset(attach_points.element, element_size);
        const origin_delta = .{
            .x = element_anchor.x - parent_anchor.x,
            .y = element_anchor.y - parent_anchor.y,
        };

        const min = origin_delta;
        const max = clay.Vector2{
            .x = bounds_size.w - element_size.w + origin_delta.x,
            .y = bounds_size.h - element_size.h + origin_delta.y,
        };

        return .{
            .x = std.math.clamp(value.x, min.x, max.x),
            .y = std.math.clamp(value.y, min.y, max.y),
        };
    }

    fn attachPointOffset(point: clay.FloatingAttachPointType, size: clay.Dimensions) clay.Vector2 {
        return .{
            .x = switch (point) {
                .left_top, .left_center, .left_bottom => 0,
                .center_top, .center_center, .center_bottom => size.w * 0.5,
                .right_top, .right_center, .right_bottom => size.w,
            },
            .y = switch (point) {
                .left_top, .center_top, .right_top => 0,
                .left_center, .center_center, .right_center => size.h * 0.5,
                .left_bottom, .center_bottom, .right_bottom => size.h,
            },
        };
    }
};

pub const ResizablePanel = struct {
    id: clay.ElementId,
    scale_value: f32,

    pub const Params = struct {
        id: ?[]const u8 = null,
        initial_size: ?clay.Dimensions = null,
        min_size: ?clay.Dimensions = null,
        max_size: ?clay.Dimensions = null,
        sizing: clay.Sizing = .fit,
        clip: bool = false,
        edge_size: f32 = 12,
        preserve_aspect_ratio: bool = false,
    };
    const Self = @This();

    const ResizeEdges = struct {
        left: bool = false,
        right: bool = false,
        top: bool = false,
        bottom: bool = false,

        fn any(self: @This()) bool {
            return self.left or self.right or self.top or self.bottom;
        }
    };

    pub fn start(ctx: *UIContext, params: Params) Self {
        std.debug.assert(params.sizing.w.type != .grow and params.sizing.h.type != .grow);

        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        const initial_size: ?clay.Dimensions = if (params.initial_size) |size| blk: {
            break :blk clampSize(size, params.min_size, params.max_size);
        } else if (params.sizing.h.type == .fixed and params.sizing.w.type == .fixed) blk: {
            break :blk clampSize(
                .{ .w = params.sizing.w.size.minmax.min, .h = params.sizing.h.size.minmax.min },
                params.min_size,
                params.max_size,
            );
        } else null;

        const current_size = if (params.sizing.w.type == .fixed and params.sizing.h.type == .fixed)
            clampSize(
                .{ .w = params.sizing.w.size.minmax.min, .h = params.sizing.h.size.minmax.min },
                params.min_size,
                params.max_size,
            )
        else
            initial_size;

        const state = ctx.getOrCreateWidgetState(element_id, .{ .resizable_panel = .{
            .size = current_size,
            .initial_size = initial_size,
        } });

        if (state.resizable_panel.initial_size == null) {
            const data = clay.getElementData(element_id);
            if (data.found and data.bounding_box.width > 0 and data.bounding_box.height > 0) {
                state.resizable_panel.initial_size = .{ .w = data.bounding_box.width, .h = data.bounding_box.height };
            }
        }

        const hovered_edges = detectHoveredEdges(ctx, element_id, params.edge_size);
        if (hovered_edges.any() and ctx.frame.mouse_pressed) {
            if (state.resizable_panel.size == null) {
                const measured_size = clampSize(measuredSize(element_id), params.min_size, params.max_size);
                state.resizable_panel.size = measured_size;
                state.resizable_panel.initial_size = measured_size;
            }
            state.resizable_panel.resizing = true;
            state.resizable_panel.edges = .{
                .left = hovered_edges.left,
                .right = hovered_edges.right,
                .top = hovered_edges.top,
                .bottom = hovered_edges.bottom,
            };
        }
        if (ctx.frame.mouse_released) {
            state.resizable_panel.resizing = false;
            state.resizable_panel.edges = .{};
        }

        if (state.resizable_panel.resizing and ctx.frame.mouse_down) {
            applyResize(state, ctx.frame.mouse_delta, params.min_size, params.max_size, params.preserve_aspect_ratio);
        }

        const cursor_edges: ResizeEdges = if (state.resizable_panel.resizing) .{
            .left = state.resizable_panel.edges.left,
            .right = state.resizable_panel.edges.right,
            .top = state.resizable_panel.edges.top,
            .bottom = state.resizable_panel.edges.bottom,
        } else hovered_edges;
        if (cursor_edges.any()) {
            ui.setMouseCursor(cursorIcon(cursor_edges));
        }

        clay.configureOpenElement(.{
            .layout = .{
                .sizing = if (state.resizable_panel.size) |size|
                    .{ .w = .fixed(size.w), .h = .fixed(size.h) }
                else
                    params.sizing,
            },
            .clip = .{ .horizontal = params.clip, .vertical = params.clip },
        });

        return .{
            .id = element_id,
            .scale_value = scaleRatio(state.resizable_panel.initial_size, state.resizable_panel.size),
        };
    }

    pub fn scale(self: *const Self) f32 {
        return self.scale_value;
    }

    pub fn end(_: *const Self) void {
        clay.closeElement();
    }

    fn detectHoveredEdges(ctx: *UIContext, element_id: clay.ElementId, edge_size: f32) ResizeEdges {
        const data = clay.getElementData(element_id);
        std.debug.assert(data.found);

        const bb = data.bounding_box;
        const x = ctx.mouse_x;
        const y = ctx.mouse_y;

        return .{
            .left = @abs(x - bb.x) <= edge_size and y >= bb.y - edge_size and y <= bb.y + bb.height + edge_size,
            .right = @abs(x - (bb.x + bb.width)) <= edge_size and y >= bb.y - edge_size and y <= bb.y + bb.height + edge_size,
            .top = @abs(y - bb.y) <= edge_size and x >= bb.x - edge_size and x <= bb.x + bb.width + edge_size,
            .bottom = @abs(y - (bb.y + bb.height)) <= edge_size and x >= bb.x - edge_size and x <= bb.x + bb.width + edge_size,
        };
    }

    fn cursorIcon(edges: ResizeEdges) ui.CursorIcon {
        if ((edges.left and edges.top) or (edges.right and edges.bottom)) {
            return .nwse_resize;
        }
        if ((edges.right and edges.top) or (edges.left and edges.bottom)) {
            return .nesw_resize;
        }
        if (edges.left or edges.right) {
            return .ew_resize;
        }
        if (edges.top or edges.bottom) {
            return .ns_resize;
        }
        return .default;
    }

    fn measuredSize(element_id: clay.ElementId) clay.Dimensions {
        const data = clay.getElementData(element_id);
        std.debug.assert(data.found);
        return .{ .w = data.bounding_box.width, .h = data.bounding_box.height };
    }

    fn applyResize(
        state: *ui.WidgetState,
        delta: clay.Vector2,
        min_size: ?clay.Dimensions,
        max_size: ?clay.Dimensions,
        preserve_aspect_ratio: bool,
    ) void {
        const edges = state.resizable_panel.edges;
        const current_size = state.resizable_panel.size orelse return;
        var next_size = current_size;

        if (edges.right) {
            const old_w = next_size.w;
            next_size.w = clampSizeValue(
                old_w + delta.x,
                if (min_size) |size| size.w else null,
                if (max_size) |size| size.w else null,
            );
        }
        if (edges.bottom) {
            const old_h = next_size.h;
            next_size.h = clampSizeValue(
                old_h + delta.y,
                if (min_size) |size| size.h else null,
                if (max_size) |size| size.h else null,
            );
        }
        if (edges.left) {
            const old_w = next_size.w;
            next_size.w = clampSizeValue(
                old_w - delta.x,
                if (min_size) |size| size.w else null,
                if (max_size) |size| size.w else null,
            );
        }
        if (edges.top) {
            const old_h = next_size.h;
            next_size.h = clampSizeValue(
                old_h - delta.y,
                if (min_size) |size| size.h else null,
                if (max_size) |size| size.h else null,
            );
        }

        if (preserve_aspect_ratio) {
            if (state.resizable_panel.initial_size) |initial_size| {
                if (initial_size.w > 0 and initial_size.h > 0) {
                    const w_scale = next_size.w / initial_size.w;
                    const h_scale = next_size.h / initial_size.h;
                    const target_scale = if ((edges.left or edges.right) and !(edges.top or edges.bottom))
                        w_scale
                    else if ((edges.top or edges.bottom) and !(edges.left or edges.right))
                        h_scale
                    else
                        @max(w_scale, h_scale);
                    next_size = clampSize(.{
                        .w = initial_size.w * target_scale,
                        .h = initial_size.h * target_scale,
                    }, min_size, max_size);
                }
            }
        }

        state.resizable_panel.size = next_size;
    }

    fn clampSize(size: clay.Dimensions, min_size: ?clay.Dimensions, max_size: ?clay.Dimensions) clay.Dimensions {
        return .{
            .w = clampSizeValue(size.w, if (min_size) |min| min.w else null, if (max_size) |max| max.w else null),
            .h = clampSizeValue(size.h, if (min_size) |min| min.h else null, if (max_size) |max| max.h else null),
        };
    }

    fn clampSizeValue(value: f32, min: ?f32, max: ?f32) f32 {
        const lower = min orelse 1.0;
        const upper = max orelse std.math.floatMax(f32);
        return std.math.clamp(value, lower, upper);
    }

    fn scaleRatio(initial_size: ?clay.Dimensions, current_size: ?clay.Dimensions) f32 {
        const base = initial_size orelse return 1.0;
        const current = current_size orelse return 1.0;
        return @min(current.w / base.w, current.h / base.h);
    }
};

pub const Label = struct {
    params: Params,

    pub const Params = struct {
        text: []const u8,
        color: Color = Color.black,
        font_size: u16 = 16,
        line_height: ?u16 = null,
        wrap_mode: clay.TextElementConfigWrapMode = .words,
        alignment: clay.TextAlignment = .left,
    };
    const Self = @This();

    pub fn start(params: Params) Self {
        const line_height = params.line_height orelse @as(u16, @intFromFloat(@as(f32, @floatFromInt(params.font_size)) * 1.2));

        const text_config = clay.TextElementConfig{
            .color = params.color.toClay(),
            .font_size = params.font_size,
            .font_id = 0,
            .letter_spacing = 0,
            .line_height = line_height,
            .wrap_mode = params.wrap_mode,
            .alignment = params.alignment,
        };
        clay.textDynamic(params.text, text_config);
        return .{ .params = params };
    }
};

pub const Button = struct {
    id: clay.ElementId,
    params: Params,

    pub const Params = struct {
        id: ?[]const u8 = null,
        text: []const u8,
        icon: ?struct {
            icon: ?*c.SDL_GPUTexture,
            size: f32 = 32,
            overlay_color: Color = Color.transparent,
            gap: u16 = 0,
        } = null,
        enabled: bool = true,
        bg_color: Color = Color.blue,
        hover_color: ?Color = null,
        text_color: Color = Color.white,
        font_size: u16 = 16,
        padding: clay.Padding = .all(8),
        corner_radius: f32 = 8,
        border: clay.BorderElementConfig = .{},
        sizing: clay.Sizing = .fit,
        text_alignment: clay.TextAlignment = .center,
        tooltip: ?Tooltip = null,
    };
    const Tooltip = struct {
        text: []const u8,
        text_size: u16 = 14,
        color: Color = Color.white,
        wrap_mode: clay.TextElementConfigWrapMode = .words,
    };
    const Self = @This();

    pub fn start(params: Params, ctx: *UIContext) Self {
        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        const is_hovered = params.enabled and clay.hovered();
        const hover_col = params.hover_color orelse params.bg_color.lighten(0.15);
        const button_color = if (is_hovered) hover_col else params.bg_color;

        clay.configureOpenElement(.{
            .layout = .{
                .sizing = params.sizing,
                .padding = params.padding,
                .child_alignment = if (params.text_alignment == .left) .{ .x = .left, .y = .center } else .center,
                .child_gap = if (params.icon) |icon| icon.gap else 0,
            },
            .background_color = button_color.toClay(),
            .corner_radius = .all(params.corner_radius),
            .border = params.border,
        });

        if (params.icon) |icon| {
            _ = Icon.start(.{ .icon = icon.icon, .size = icon.size, .overlay_color = icon.overlay_color });
        }
        _ = Label.start(.{
            .text = params.text,
            .alignment = params.text_alignment,
            .font_size = params.font_size,
            .color = params.text_color,
        });
        clay.closeElement();

        if (params.tooltip) |tooltip| {
            const state = ctx.getOrCreateWidgetState(element_id, .{ .tooltip = .{} });
            if (is_hovered) {
                // Draw tooltip if 500ms has passed
                if (!state.tooltip.visible and ctx.tickTimerId(element_id.id, 500)) {
                    state.tooltip.visible = true;
                }

                if (state.tooltip.visible) Button.drawTooltip(ctx, element_id, tooltip);
            } else {
                ctx.removeTimerId(element_id.id);
                state.tooltip.visible = false;
            }
        }

        return .{ .id = element_id, .params = params };
    }

    pub fn clicked(self: *const Self, ctx: *UIContext) bool {
        if (!self.params.enabled) return false;
        const is_hovered = clay.pointerOver(self.id);
        return is_hovered and ctx.pointerReleased();
    }

    pub fn clickedOrHold(self: *const Self, ctx: *UIContext) bool {
        return self.clicked(ctx) or (ctx.activeFingerOverElement(self.id) and ctx.frame.mouse_down);
    }

    fn drawTooltip(ctx: *UIContext, element_id: clay.ElementId, params: Self.Tooltip) void {
        const tooltip_id = clay.ElementId.localIDI("tooltip", element_id.id);

        const button_data = clay.getElementData(element_id);
        const layout_dims = ctx.clay_ctx.layoutDimensions;

        var attach_parent: clay.FloatingAttachPointType = .left_bottom;
        var attach_element: clay.FloatingAttachPointType = .left_top;
        var offset_y: f32 = 4;

        if (button_data.found) {
            const is_right_half = button_data.bounding_box.x > (layout_dims.w / 2.0);
            const is_bottom_half = button_data.bounding_box.y > (layout_dims.h / 2.0);

            if (is_right_half and is_bottom_half) {
                attach_parent = .right_top;
                attach_element = .right_bottom;
                offset_y = -4; // Reverses the offset to avoid overlapping the button.
            } else if (is_right_half) {
                attach_parent = .right_bottom;
                attach_element = .right_top;
            } else if (is_bottom_half) {
                attach_parent = .left_top;
                attach_element = .left_bottom;
                offset_y = -4;
            }
        }

        const max_tooltip_width = @min(250.0, layout_dims.w - 32.0);

        clay.openElementWithId(tooltip_id);
        clay.configureOpenElement(.{
            .layout = .{
                .sizing = .{
                    .w = clay.SizingAxis.fitMinMax(.{ .max = max_tooltip_width }),
                    .h = .fit,
                },
                .padding = .{ .left = 8, .right = 8, .top = 5, .bottom = 5 },
                .child_alignment = .center,
            },
            .background_color = Color.rgb(30, 30, 30).toClay(),
            .corner_radius = .all(8),
            .border = .{
                .color = Color.rgb(80, 80, 80).toClay(),
                .width = .outside(1),
            },
            .floating = .{
                .attach_to = .to_element_with_id,
                .parentId = element_id.id,
                .attach_points = .{ .element = attach_element, .parent = attach_parent },
                .offset = .{ .x = 0, .y = 4 },
                .z_index = 200,
                .pointer_capture_mode = .passthrough,
            },
        });

        _ = Label.start(.{
            .text = params.text,
            .font_size = params.text_size,
            .color = params.color,
            .wrap_mode = params.wrap_mode,
        });

        clay.closeElement();
    }
};

pub const Icon = struct {
    id: clay.ElementId,

    pub const Params = struct {
        id: ?[]const u8 = null,
        icon: ?*c.SDL_GPUTexture,
        size: f32 = 32,
        overlay_color: Color = Color.transparent,
    };
    const Self = @This();

    pub fn start(params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        clay.configureOpenElement(.{
            .layout = .{ .sizing = .{ .w = .fixed(params.size), .h = .fixed(params.size) } },
            .overlay_color = params.overlay_color.toClay(),
            .image = .{ .image_data = params.icon },
        });

        clay.closeElement();

        return .{ .id = element_id };
    }
};

pub const IconButton = struct {
    id: clay.ElementId,
    enabled: bool,

    pub const Params = struct {
        id: ?[]const u8 = null,
        icon: ?*c.SDL_GPUTexture,
        size: u16 = 32,
        enabled: bool = true,
        bg_color: Color = Color.transparent,
        hover_color: ?Color = null,
        overlay_color: Color = Color.transparent,
        corner_radius: f32 = 4,
        padding: clay.Padding = .all(4),
        tooltip: ?Button.Tooltip = null,
    };
    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const eid = clay.ElementId.ID(id);
            clay.openElementWithId(eid);
            break :b eid;
        } else clay.openElement();

        const is_hovered = params.enabled and clay.hovered();
        const hover_col = params.hover_color orelse params.bg_color.lighten(0.2);
        const bg = if (is_hovered) hover_col else params.bg_color;

        clay.configureOpenElement(.{
            .layout = .{
                .sizing = .fit,
                .padding = params.padding,
                .child_alignment = .center,
            },
            .background_color = bg.toClay(),
            .corner_radius = .all(params.corner_radius),
        });

        {
            const icon_size: f32 = @floatFromInt(params.size);
            _ = clay.openElement();
            clay.configureOpenElement(.{
                .layout = .{ .sizing = .{ .w = .fixed(icon_size), .h = .fixed(icon_size) } },
                .overlay_color = params.overlay_color.toClay(),
                .image = .{ .image_data = params.icon },
            });
            clay.closeElement();
        }

        if (params.tooltip) |tooltip| {
            const state = ctx.getOrCreateWidgetState(element_id, .{ .tooltip = .{} });
            if (is_hovered) {
                if (!state.tooltip.visible and ctx.tickTimerId(element_id.id, 500)) {
                    state.tooltip.visible = true;
                }

                if (state.tooltip.visible) Button.drawTooltip(ctx, element_id, tooltip);
            } else {
                ctx.removeTimerId(element_id.id);
                state.tooltip.visible = false;
            }
        }

        clay.closeElement();

        return .{ .id = element_id, .enabled = params.enabled };
    }

    pub fn clicked(self: *const Self, ctx: *UIContext) bool {
        if (!self.enabled) return false;
        return clay.pointerOver(self.id) and ctx.frame.mouse_pressed;
    }
};

pub const TextField = struct {
    id: clay.ElementId,
    params: Params,
    ctx: *UIContext,

    pub const Params = struct {
        id: ?[]const u8 = null,
        placeholder: []const u8 = "",
        width: clay.SizingAxis = .grow,
        padding_val: clay.Padding = .all(8),
        corner_radius: f32 = 8,
        value: []const u8 = "",
        read_only: bool = false,
    };
    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        const state = ctx.getOrCreateWidgetState(element_id, .{ .text_input = .{
            .buffer = .empty,
            .cursor_pos = 0,
        } });

        if (!state.text_input.initial_value_appended) {
            state.text_input.buffer.appendSlice(ctx.persistent_arena.allocator(), params.value) catch
                @panic("OOM");
            state.text_input.initial_value_appended = true;
        }

        const is_hovered = clay.hovered();
        const is_focused = if (ctx.frame.focused_id) |fid| fid.id == element_id.id else false;

        ui.setMouseCursorText(is_hovered);

        var placeholder = params.placeholder;
        if (is_hovered and ctx.frame.mouse_pressed and !params.read_only) {
            ctx.frame.focused_id = element_id;
            placeholder = "";
        }

        if (is_focused and !params.read_only) {
            if (ctx.frame.text_input.items.len > 0) {
                state.text_input.show_android_paste_toolbox = false;
                state.text_input.buffer.appendSlice(ctx.persistent_arena.allocator(), ctx.frame.text_input.items) catch
                    @panic("OOM");
            }

            var backspace_count = ctx.input.keyPressCount(.BACKSPACE);
            if (backspace_count == 0 and ctx.input.isKeyPressed(.BACKSPACE, true)) {
                backspace_count = 1;
            }
            while (backspace_count > 0 and state.text_input.buffer.items.len > 0) : (backspace_count -= 1) {
                _ = state.text_input.buffer.pop();
            }

            const ctrl = ctx.input.isKeyDown(.LCTRL) or ctx.input.isKeyDown(.RCTRL);
            if (ctrl and ctx.input.isKeyPressed(.V, false)) {
                appendClipboardText(ctx, &state.text_input.buffer);
            }
        }

        clay.configureOpenElement(.{
            .layout = .{
                .sizing = .{ .w = params.width, .h = .fit },
                .padding = params.padding_val,
                .child_alignment = .{ .y = .center },
            },
            .background_color = Color.white.toClay(),
            .corner_radius = .all(params.corner_radius),
            .border = .{
                .color = if (is_focused) Color.blue.toClay() else Color.lightGray.toClay(),
                .width = .outside(if (is_focused) 2 else 1),
            },
        });

        ctx.setHotId();

        if (!is_focused and state.text_input.buffer.items.len == 0) {
            _ = Label.start(.{ .text = placeholder, .color = Color.gray });
        } else {
            _ = Label.start(.{
                .text = state.text_input.buffer.items,
                .color = Color.black,
                .wrap_mode = .none,
            });
        }

        // Start end text cursor
        const show_cursor = c.SDL_GetTicks() % c.SDL_MS_PER_SECOND < 500; // blink cursor every 500ms
        // When the text field is not focused, end the cursor transparent to avoid layout size change when focusing.
        const cursor_color = if (is_focused and show_cursor) Color.black else Color.black.withAlpha(0);

        const text_cursor = clay.openElement();
        clay.configureOpenElement(.{
            .layout = .{
                .sizing = .{ .w = .fixed(2), .h = .fixed(20) },
                .padding = .{ .left = 1 },
            },
            .background_color = cursor_color.toClay(),
        });
        clay.closeElement(); // End end text cursor

        clay.closeElement();

        var paste_btn_hovered = false;
        if (builtin.abi.isAndroid()) {
            if (!params.read_only and is_focused and ctx.activeFingerOverElement(element_id) and ctx.frame.mouse_down) {
                if (ctx.tickTimer("android_paste_toolbox", 500)) {
                    ctx.removeTimer("android_paste_toolbox");
                    state.text_input.show_android_paste_toolbox = true;
                }
            } else {
                ctx.removeTimer("android_paste_toolbox");
            }

            if (state.text_input.show_android_paste_toolbox) {
                const container = Float.start(.{
                    .attach_to = .to_element_with_id,
                    .parentId = text_cursor.id,
                    .attach_points = .{ .element = .left_top, .parent = .left_top },
                    .offset = .{ .x = 2, .y = -30 },
                    .z_index = std.math.maxInt(i16),
                });
                {
                    const paste_button = Button.start(.{
                        .text = "Paste",
                        .bg_color = Color.blue,
                        .text_color = Color.white,
                        .font_size = 13,
                        .padding = .{ .left = 8, .right = 8, .top = 8, .bottom = 8 },
                        .corner_radius = 4,
                    }, ctx);
                    paste_btn_hovered = clay.pointerOver(paste_button.id);
                    if (paste_button.clicked(ctx)) {
                        appendClipboardText(ctx, &state.text_input.buffer);
                        state.text_input.show_android_paste_toolbox = false;
                    }
                }
                container.end();
            }
        }

        if (ctx.frame.mouse_pressed and is_focused and !is_hovered and !paste_btn_hovered) {
            ctx.frame.focused_id = null;
            state.text_input.show_android_paste_toolbox = false;
        }

        return .{ .id = element_id, .params = params, .ctx = ctx };
    }

    fn appendClipboardText(ctx: *UIContext, buffer: *std.ArrayList(u8)) void {
        const clipboard_text = c.SDL_GetClipboardText();
        defer c.SDL_free(clipboard_text);

        const text = std.mem.span(clipboard_text);
        if (text.len == 0) {
            std.log.debug("Failed to paste clipboard text on text field widget: {s}", .{c.SDL_GetError()});
        }

        buffer.appendSlice(ctx.persistent_arena.allocator(), text) catch @panic("OOM");
    }

    pub fn value(self: *const Self) []const u8 {
        return self.ctx.getWidgetStateById(self.id).?.text_input.buffer.items;
    }
};

pub const ScrollContainer = struct {
    id: clay.ElementId,
    ctx: *UIContext,
    params: Params,

    pub const Params = struct {
        id: ?[]const u8 = null,
        sizing: clay.Sizing = .grow,
        vertical: bool = true,
        horizontal: bool = false,
        padding: clay.Padding = .{},
        gap: u16 = 0,
        show_scrollbar: bool = true,
        scrollbar_color: Color = Color.rgb(120, 130, 150),
    };
    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        ctx.pushParent(element_id);
        const state = ctx.getOrCreateWidgetState(element_id, .{ .scroll = .{
            .offset = .{ .x = 0, .y = 0 },
            .velocity = .{ .x = 0, .y = 0 },
        } });

        var padding = params.padding;
        if (params.show_scrollbar and params.vertical and state.scroll.vertical_scrollbar_visible) {
            padding.right = 10;
        }
        if (params.show_scrollbar and params.horizontal and state.scroll.horizontal_scrollbar_visible) {
            padding.bottom = 10;
        }

        // Reset for this frame; set to true below if we actually render each scrollbar.
        state.scroll.vertical_scrollbar_visible = false;
        state.scroll.horizontal_scrollbar_visible = false;

        clay.configureOpenElement(.{
            .layout = .{
                .sizing = params.sizing,
                .padding = padding,
                .child_gap = params.gap,
                .direction = .top_to_bottom,
            },
            .clip = .{
                .vertical = params.vertical,
                .horizontal = params.horizontal,
                .child_offset = state.scroll.offset,
            },
        });

        if (params.show_scrollbar and (params.vertical or params.horizontal)) {
            const scroll_data = clay.getScrollContainerData(element_id);
            if (scroll_data.found) {
                state.scroll.offset = scroll_data.scroll_position.*;

                if (params.vertical and scroll_data.content_dimensions.h > scroll_data.scroll_container_dimensions.h) {
                    state.scroll.vertical_scrollbar_visible = true;
                    renderVerticalScrollbar(ctx, element_id, scroll_data, params.scrollbar_color);
                }

                if (params.horizontal and scroll_data.content_dimensions.w > scroll_data.scroll_container_dimensions.w) {
                    state.scroll.horizontal_scrollbar_visible = true;
                    renderHorizontalScrollbar(ctx, element_id, scroll_data, params.scrollbar_color);
                }
            }
        }

        return .{ .id = element_id, .ctx = ctx, .params = params };
    }

    fn renderVerticalScrollbar(
        ctx: *UIContext,
        element_id: clay.ElementId,
        scroll_data: clay.ScrollContainerData,
        scrollbar_color: Color,
    ) void {
        const viewport_h = scroll_data.scroll_container_dimensions.h;
        const content_h = scroll_data.content_dimensions.h;
        var scroll_bar_height = (viewport_h / content_h) * viewport_h;
        if (scroll_bar_height < 20.0) scroll_bar_height = @min(20.0, viewport_h);

        const max_scroll_y = content_h - viewport_h;
        const current_scroll_y = @abs(scroll_data.scroll_position.y);
        const scroll_ratio = if (max_scroll_y > 0) current_scroll_y / max_scroll_y else 0;
        const scroll_track_space = viewport_h - scroll_bar_height;
        const scroll_bar_y = scroll_ratio * scroll_track_space;

        const scrollbar_id = clay.ElementId.localIDI("scrollbar-y", element_id.id);
        const is_hovered = clay.pointerOver(scrollbar_id);

        if (is_hovered and ctx.frame.mouse_pressed) {
            ctx.frame.active_id = scrollbar_id.id;
        }
        const is_dragging = (ctx.frame.active_id == scrollbar_id.id);
        if (is_dragging) {
            if (ctx.frame.mouse_down) {
                if (scroll_track_space > 0) {
                    const move_ratio = max_scroll_y / scroll_track_space;
                    scroll_data.scroll_position.y -= ctx.frame.mouse_delta.y * move_ratio;
                }
            } else {
                ctx.frame.active_id = null;
            }
        }

        const scrollbar_data = ctx.frameAlloc().create(clay.ElementId) catch @panic("failed to allocate");
        scrollbar_data.* = scrollbar_id;
        clay.openElementWithId(scrollbar_id);
        clay.configureOpenElement(.{
            .layout = .{ .sizing = .{ .w = .fixed(6), .h = .fixed(scroll_bar_height) } },
            .floating = .{
                .attach_to = .to_element_with_id,
                .parentId = element_id.id,
                .attach_points = .{ .element = .right_top, .parent = .right_top },
                .offset = .{ .x = -1, .y = scroll_bar_y },
                .z_index = std.math.maxInt(i16),
                .pointer_capture_mode = .passthrough,
                .clip_to = .to_attached_parent,
            },
            .background_color = if (is_dragging or is_hovered)
                scrollbar_color.toClay()
            else
                scrollbar_color.withAlpha(0.55).toClay(),
            .corner_radius = .all(3),
        });
        clay.closeElement();
    }

    fn renderHorizontalScrollbar(
        ctx: *UIContext,
        element_id: clay.ElementId,
        scroll_data: clay.ScrollContainerData,
        scrollbar_color: Color,
    ) void {
        const viewport_w = scroll_data.scroll_container_dimensions.w;
        const content_w = scroll_data.content_dimensions.w;
        var scroll_bar_width = (viewport_w / content_w) * viewport_w;
        if (scroll_bar_width < 20.0) scroll_bar_width = @min(20.0, viewport_w);

        const max_scroll_x = content_w - viewport_w;
        const current_scroll_x = @abs(scroll_data.scroll_position.x);
        const scroll_ratio = if (max_scroll_x > 0) current_scroll_x / max_scroll_x else 0;
        const scroll_track_space = viewport_w - scroll_bar_width;
        const scroll_bar_x = scroll_ratio * scroll_track_space;

        const scrollbar_id = clay.ElementId.localIDI("scrollbar-x", element_id.id);
        const is_hovered = clay.pointerOver(scrollbar_id);

        if (is_hovered and ctx.frame.mouse_pressed) {
            ctx.frame.active_id = scrollbar_id.id;
        }
        const is_dragging = (ctx.frame.active_id == scrollbar_id.id);
        if (is_dragging) {
            if (ctx.frame.mouse_down) {
                if (scroll_track_space > 0) {
                    const move_ratio = max_scroll_x / scroll_track_space;
                    scroll_data.scroll_position.x -= ctx.frame.mouse_delta.x * move_ratio;
                }
            } else {
                ctx.frame.active_id = null;
            }
        }

        const scrollbar_data = ctx.frameAlloc().create(clay.ElementId) catch @panic("failed to allocate");
        scrollbar_data.* = scrollbar_id;
        clay.openElementWithId(scrollbar_id);
        clay.configureOpenElement(.{
            .layout = .{ .sizing = .{ .w = .fixed(scroll_bar_width), .h = .fixed(6) } },
            .floating = .{
                .attach_to = .to_element_with_id,
                .parentId = element_id.id,
                .attach_points = .{ .element = .left_bottom, .parent = .left_bottom },
                .offset = .{ .x = scroll_bar_x, .y = -1 },
                .z_index = std.math.maxInt(i16),
                .pointer_capture_mode = .passthrough,
                .clip_to = .to_attached_parent,
            },
            .background_color = if (is_dragging or is_hovered)
                scrollbar_color.toClay()
            else
                scrollbar_color.withAlpha(0.55).toClay(),
            .corner_radius = .all(3),
        });
        clay.closeElement();
    }

    pub fn end(self: *const Self) void {
        self.ctx.popParent();
        clay.closeElement();
    }
};

pub const Spacer = struct {
    params: Params,

    pub const Params = struct {
        sizing: clay.Sizing,
    };
    const Self = @This();

    pub fn start(params: Params) @This() {
        _ = clay.openElement();
        clay.configureOpenElement(.{ .layout = .{ .sizing = params.sizing } });
        clay.closeElement();
        return .{ .params = params };
    }
};

pub const Canvas = struct {
    id: clay.ElementId,
    params: Params,
    shader_modes: []const ui.ShaderMode,

    pub const Params = struct {
        id: ?[]const u8 = null,
        sizing: clay.Sizing = .grow,
        pixel_format: c_uint,
        pixels: []const u8,
        w: u32 = 0,
        h: u32 = 0,
        fg_color: ?Color = null,
        bg_color: ?Color = null,
        corner_radius: clay.CornerRadius = .{},
        padding: clay.Padding = .{},
        aspect_ratio: viewport.AspectRatio = .none,
        viewport_alignment: viewport.ViewportAlignment = .center,
        overlay_color: Color = Color.transparent,
    };
    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        const alloc = ctx.frameAlloc();
        const shader_modes = alloc.dupe(ui.ShaderMode, ctx.shader_mode_stack.items) catch @panic("Alloc failed");
        const custom_data = alloc.create(CustomData) catch @panic("Alloc failed");
        custom_data.* = .{ .canvas = .{
            .id = element_id,
            .params = params,
            .shader_modes = shader_modes,
        } };

        clay.configureOpenElement(.{
            .layout = .{ .sizing = params.sizing },
            .overlay_color = params.overlay_color.toClay(),
            .corner_radius = params.corner_radius,
            .custom = .{ .custom_data = clay.anytypeToAnyopaquePtr(custom_data) },
        });
        clay.closeElement();

        return .{ .id = element_id, .params = params, .shader_modes = shader_modes };
    }
};

pub const MenuBar = struct {
    ctx: *UIContext,
    container: Container,
    params: Params,

    pub const Params = struct {
        bg_color: Color = Color.white,
        border_color: Color = Color.lightGray,
    };
    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const container = Container.start(.{
            .direction = .row,
            .sizing = .{ .w = .grow, .h = .fit },
            .gap = 4,
            .border = .{ .width = .outside(1), .color = params.border_color.toClay() },
            .bg_color = params.bg_color,
            .child_alignment = .{ .y = .center },
        });
        ctx.pushParent(container.id);
        return .{ .ctx = ctx, .container = container, .params = params };
    }

    pub fn end(self: *const Self) void {
        self.ctx.popParent();
        self.container.end();
    }
};

// Shared enter/exit state for floating panels (dropdowns, comboboxes).
// Offsets the bounding box 6px downward so panels slide up when entering and slide
// down when exiting, giving a natural open/close feel regardless of direction.
fn floatingPanelEdgeState(state: clay.TransitionData, _: clay.TransitionProperty) callconv(.c) clay.TransitionData {
    var s = state;
    s.bounding_box.y += 6;
    return s;
}

const floating_panel_transition = clay.TransitionElementConfig{
    .handler = clay.easeOut,
    .duration = 0.05,
    .properties = clay.TransitionProperty.position,
    .enter = .{
        .set_initial_state = floatingPanelEdgeState,
        .trigger = .trigger_on_first_parent_frame,
    },
    .exit = .{
        .set_final_state = floatingPanelEdgeState,
        .sibling_ordering = .natural_order,
    },
};

pub const DropdownMenu = struct {
    id: clay.ElementId,
    params: Params,
    ctx: *UIContext,

    pub const Params = struct {
        id: ?[]const u8 = null,
        label: []const u8,
        width: f32 = 200,
        bg_color: Color = Color.white,
        hover_color: Color = Color.lightGray,
        text_color: Color = Color.black,
        list_bg_color: Color = Color.white,
        list_border_color: Color = Color.gray,
    };
    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else b: {
            const element_id = clay.ElementId.localIDI(params.label, ctx.getParent().?.id);
            clay.openElementWithId(element_id);
            break :b element_id;
        };

        ctx.pushParent(element_id);
        const state = ctx.getOrCreateWidgetState(element_id, .{ .dropdown_menu = .{ .is_open = false } });

        const is_hovered = clay.hovered();
        const bg = if (state.dropdown_menu.is_open or is_hovered) params.hover_color else params.bg_color;

        const menu_list_id = clay.ElementId.localIDI("dropdown_list", element_id.id);
        const is_list_hovered = if (state.dropdown_menu.is_open) clay.pointerOver(menu_list_id) else false;

        clay.configureOpenElement(.{
            .layout = .{ .padding = .all(8) },
            .background_color = bg.toClay(),
            .corner_radius = .all(4),
        });

        _ = Label.start(.{ .text = params.label, .font_size = 14, .color = params.text_color });
        clay.closeElement();

        if (is_hovered and ctx.frame.mouse_pressed) {
            ctx.frame.active_id = element_id.id;
            state.dropdown_menu.is_open = true;
        }
        // if the menu is open and a mouse click happened outside of it
        else if (state.dropdown_menu.is_open and ctx.frame.mouse_pressed) {
            if (!is_list_hovered) { // if the mouse click didn't happened in the menu item list
                state.dropdown_menu.is_open = false;
                if (ctx.frame.active_id == element_id.id) {
                    ctx.frame.active_id = null;
                }
            }
        }

        clay.openElementWithId(menu_list_id);
        clay.configureOpenElement(.{
            .layout = .{
                .sizing = .{ .w = .fixed(params.width), .h = .fit },
                .direction = .top_to_bottom,
                .padding = .all(4),
                .child_gap = 2,
            },
            .background_color = params.list_bg_color.toClay(),
            .corner_radius = .all(4),
            .border = if (state.dropdown_menu.is_open)
                .{ .width = .outside(1), .color = params.list_border_color.toClay() }
            else
                .{},
            .floating = .{
                .attach_to = .to_element_with_id,
                .parentId = element_id.id,
                .attach_points = .{ .element = .left_top, .parent = .left_bottom },
                .z_index = std.math.maxInt(i16),
            },
            .transition = floating_panel_transition,
            .no_render = !state.dropdown_menu.is_open,
        });
        return .{ .id = element_id, .params = params, .ctx = ctx };
    }

    pub fn end(self: *const Self) void {
        self.ctx.popParent();
        const state = self.ctx.getWidgetStateById(self.id).?;
        clay.closeElement();
        if (state.dropdown_menu.is_open and self.ctx.frame.menu_item_clicked) {
            state.dropdown_menu.is_open = false;
            self.ctx.frame.active_id = null;
        }
    }
};

pub const MenuItem = struct {
    id: clay.ElementId,
    ctx: *UIContext,
    params: Params,

    pub const Params = struct {
        id: ?[]const u8 = null,
        label: []const u8,
        shortcut: ?[]const u8 = null,
        padding: clay.Padding = .{ .left = 8, .right = 8, .top = 6, .bottom = 6 },
        enabled: bool = true,
        has_submenu: bool = false,
        bg_color: Color = Color.white,
        hover_color: Color = Color.blue,
        text_color: Color = Color.black,
    };
    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        const is_hovered = params.enabled and clay.hovered();
        clay.configureOpenElement(.{
            .layout = .{
                .sizing = .{ .w = .grow, .h = .fit },
                .padding = params.padding,
                .child_alignment = .{ .y = .center },
                .direction = .left_to_right,
            },
            .background_color = if (is_hovered) params.hover_color.toClay() else params.bg_color.toClay(),
            .corner_radius = .all(4),
        });

        const text_col = if (!params.enabled) Color.gray else if (is_hovered) Color.white else params.text_color;

        _ = Label.start(.{ .text = params.label, .font_size = 14, .color = text_col });

        if (params.shortcut != null or params.has_submenu) {
            _ = Spacer.start(.{ .sizing = .grow });
        }

        if (params.shortcut) |s| {
            _ = Label.start(.{ .text = s, .font_size = 14, .color = if (is_hovered) Color.lightGray else Color.gray });
        }

        if (params.has_submenu) {
            const arrow_color = if (!params.enabled) Color.gray else if (is_hovered) Color.white else params.text_color;
            _ = Shape.start(ctx, .{
                .vertices = &Shape.RIGHT_TRIANGLE,
                .sizing = .{ .w = .fixed(8), .h = .fixed(10) },
                .color = arrow_color,
            });
        }

        if (!params.has_submenu) {
            clay.closeElement();
        }

        if (is_hovered and ctx.frame.mouse_released) {
            ctx.frame.menu_item_clicked = true;
        }

        return .{ .id = id, .params = params, .ctx = ctx };
    }

    pub fn submenu(self: *const Self, params: Submenu.Params) Submenu {
        std.debug.assert(self.params.has_submenu);
        return Submenu.start(self.ctx, self, params);
    }

    /// This is a no-op if the menu item does not have a submenu
    pub fn end(self: *const Self) void {
        if (!self.params.has_submenu) return;

        clay.closeElement();
    }

    pub fn clicked(self: *const Self, ctx: *UIContext) bool {
        if (!self.params.enabled) return false;
        const is_hovered = clay.pointerOver(self.id);
        return is_hovered and ctx.frame.mouse_released;
    }
};

const Submenu = struct {
    id: clay.ElementId,
    ctx: *UIContext,

    pub const Params = struct {
        id: ?[]const u8 = null,
        width: f32 = 200,
        bg_color: Color = Color.white,
        border_color: Color = Color.gray,
        padding: clay.Padding = .all(4),
        gap: u16 = 2,
    };
    const Self = @This();

    fn start(ctx: *UIContext, parent: *const MenuItem, params: Params) Self {
        const id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        ctx.pushParent(id);
        const state = ctx.getOrCreateWidgetState(id, .{ .submenu = .{ .is_open = false } });

        if (parent.params.enabled and clay.pointerOver(parent.id)) {
            state.submenu.is_open = true;
        } else if ((state.submenu.is_open and !clay.pointerOver(id)) or (clay.pointerOver(id) and ctx.frame.mouse_released)) {
            state.submenu.is_open = false;
        }

        clay.configureOpenElement(.{
            .layout = .{
                .sizing = .{ .w = .fixed(params.width), .h = .fit },
                .direction = .top_to_bottom,
                .padding = params.padding,
                .child_gap = params.gap,
            },
            .background_color = params.bg_color.toClay(),
            .corner_radius = .all(4),
            .border = if (state.submenu.is_open) .{ .width = .outside(1), .color = params.border_color.toClay() } else .{},
            .floating = .{
                .attach_to = .to_element_with_id,
                .parentId = parent.id.id,
                .attach_points = .{ .element = .left_top, .parent = .right_top },
                .z_index = std.math.maxInt(i16),
            },
            .transition = floating_panel_transition,
            .no_render = !state.submenu.is_open,
        });
        return .{ .id = id, .ctx = ctx };
    }

    pub fn end(self: *const Self) void {
        self.ctx.popParent();
        clay.closeElement();
    }
};

pub const Padding = struct {
    params: Params,

    pub const Params = struct {
        pad: clay.Padding = .{},
        sizing: clay.Sizing = .{ .h = .fit, .w = .grow },
        direction: clay.LayoutDirection = .top_to_bottom,
    };
    const Self = @This();

    pub fn start(params: Params) Self {
        _ = clay.openElement();
        clay.configureOpenElement(.{
            .layout = .{
                .direction = params.direction,
                .padding = params.pad,
                .sizing = params.sizing,
            },
        });
        return .{ .params = params };
    }

    pub fn end(_: *const Self) void {
        clay.closeElement();
    }
};

pub fn Combobox(comptime Option: type) type {
    comptime {
        const info = @typeInfo(Option);
        if (info != .@"enum" and info != .@"union") {
            @compileError("Combobox options_type must be an enum or a tagged union");
        }
        if (!std.meta.hasFn(Option, "label")) {
            @compileError("Combobox options_type must define `pub fn label(self: @This()) []const u8`");
        }
    }

    return struct {
        id: clay.ElementId,
        ctx: *UIContext,
        params: Params,
        options: []const Option,

        pub const Params = struct {
            id: ?[]const u8 = null,
            filtered_options: ?[]const Option = null,
            selected: ?Option = null,
            border: clay.BorderElementConfig = .{},
            border_color_on_open: Color = .black,
            border_color_on_hover: Color = .black,
            bg_color: Color = .white,
            bg_color_on_hover: Color = .white,
            child_alignment: clay.ChildAlignment = .{ .y = .center },
            sizing: clay.Sizing = .{ .h = .fit, .w = .fixed(120) },
            text_color: Color = .black,
            float_panel: struct {
                bg_color: Color = .white,
                border_color: Color = .gray,
                max_height: f32 = 200.0,
            } = .{},
            item: struct {
                bg_color: Color = .white,
                bg_color_on_hover: Color = .blue,
                text_color: Color = .black,
                text_color_on_hover: Color = .white,
            } = .{},
        };
        const Self = @This();

        fn optionKey(option: Option) u32 {
            return @intFromEnum(option);
        }

        fn findOptionByKey(options: []const Option, key: u32) Option {
            for (options) |option| {
                if (optionKey(option) == key) return option;
            }
            return options[0];
        }

        pub fn start(ctx: *UIContext, params: Params) Self {
            const element_id = if (params.id) |id| b: {
                const element_id = clay.ElementId.ID(id);
                clay.openElementWithId(element_id);
                break :b element_id;
            } else clay.openElement();

            const options = if (params.filtered_options) |options|
                options
            else blk: {
                const info = @typeInfo(Option);
                const total_fields = switch (info) {
                    .@"enum" => |meta| meta.fields.len,
                    .@"union" => @panic("Combobox tagged-union options need to use filtered_options"),
                    else => unreachable,
                };

                const buffer = ctx.frameAlloc().alloc(Option, total_fields) catch @panic("OOM");
                switch (info) {
                    .@"enum" => |meta| inline for (meta.fields, 0..) |option, i| {
                        buffer[i] = @field(Option, option.name);
                    },
                    else => unreachable,
                }

                break :blk buffer;
            };

            ctx.pushParent(element_id);
            const state = ctx.getOrCreateWidgetState(element_id, .{ .combobox = .{
                .is_open = false,
                .selected_key = optionKey(params.selected orelse options[0]),
            } });

            const menu_list_id = clay.ElementId.localIDI("combobox_list", element_id.id);

            const is_hovered = clay.hovered();
            const is_list_hovered = if (state.combobox.is_open)
                clay.pointerOver(menu_list_id)
            else
                false;

            {
                const trigger_bg = if (is_hovered or state.combobox.is_open)
                    params.bg_color_on_hover
                else
                    params.bg_color;
                const trigger_border = if (state.combobox.is_open)
                    params.border_color_on_open.toClay()
                else if (is_hovered)
                    params.border_color_on_hover.toClay()
                else
                    params.border.color;
                clay.configureOpenElement(.{
                    .layout = .{
                        .padding = .{ .left = 10, .right = 8, .top = 6, .bottom = 6 },
                        .sizing = params.sizing,
                        .direction = .left_to_right,
                        .child_alignment = params.child_alignment,
                        .child_gap = 4,
                    },
                    .background_color = trigger_bg.toClay(),
                    .corner_radius = .all(4),
                    .border = .{ .width = params.border.width, .color = trigger_border },
                });

                _ = Label.start(.{
                    .text = findOptionByKey(options, state.combobox.selected_key).label(),
                    .font_size = 14,
                    .color = params.text_color,
                });
                _ = Spacer.start(.{ .sizing = .grow });

                _ = Shape.start(ctx, .{
                    .sizing = .{ .w = .fixed(10), .h = .fixed(10) },
                    .vertices = &[_]clay.Vector2{
                        .{ .x = 0.0, .y = 0.75 },
                        .{ .x = 1.0, .y = 0.75 },
                        .{ .x = 0.5, .y = 0.25 },
                    },
                    .rotation = if (state.combobox.is_open) 0 else 180,
                    .color = Color.darkGray,
                });

                clay.closeElement();
            }

            if (is_hovered and ctx.frame.mouse_pressed) {
                ctx.frame.active_id = element_id.id;
                state.combobox.is_open = !state.combobox.is_open;
            } else if (state.combobox.is_open and ctx.frame.mouse_pressed) {
                if (!is_list_hovered) {
                    state.combobox.is_open = false;
                    if (ctx.frame.active_id == element_id.id) {
                        ctx.frame.active_id = null;
                    }
                }
            }

            if (state.combobox.is_open) {
                const combobox_data = clay.getElementData(element_id);
                const layout_dims = ctx.clay_ctx.layoutDimensions;

                const max_height_cap: f32 = params.float_panel.max_height;
                const gap: f32 = 3.0;
                const margin: f32 = 4.0;

                var max_menu_height: f32 = max_height_cap;
                var attach_parent: clay.FloatingAttachPointType = .left_bottom;
                var attach_element: clay.FloatingAttachPointType = .left_top;
                var offset_y: f32 = gap;

                if (combobox_data.found) {
                    const bb = combobox_data.bounding_box;
                    const is_right_half = bb.x > (layout_dims.w / 2.0);
                    const space_below = layout_dims.h - (bb.y + bb.height) - gap - margin;
                    const space_above = bb.y - gap - margin;

                    const open_above = space_above > space_below and space_below < max_height_cap;

                    if (!open_above) {
                        max_menu_height = @min(space_below, max_height_cap);
                        attach_parent = if (is_right_half) .right_bottom else .left_bottom;
                        attach_element = if (is_right_half) .right_top else .left_top;
                        offset_y = gap;
                    } else {
                        max_menu_height = @min(space_above, max_height_cap);
                        attach_parent = if (is_right_half) .right_top else .left_top;
                        attach_element = if (is_right_half) .right_bottom else .left_bottom;
                        offset_y = -gap;
                    }
                }

                const panel_padding_v: f32 = 4.0;
                const scroll_max_h = max_menu_height - panel_padding_v * 2;

                clay.openElementWithId(menu_list_id);
                clay.configureOpenElement(.{
                    .layout = .{
                        .sizing = .{
                            .w = .fixed(combobox_data.bounding_box.width),
                            .h = .fitMinMax(.{ .min = 0, .max = max_menu_height }),
                        },
                        .direction = .top_to_bottom,
                        .padding = .all(@intFromFloat(panel_padding_v)),
                        .child_gap = 2,
                    },
                    .background_color = params.float_panel.bg_color.toClay(),
                    .corner_radius = .all(4),
                    .border = .{ .width = .outside(1), .color = params.float_panel.border_color.toClay() },
                    .floating = .{
                        .attach_to = .to_element_with_id,
                        .parentId = element_id.id,
                        .attach_points = .{ .element = attach_element, .parent = attach_parent },
                        .offset = .{ .x = 0, .y = offset_y },
                        .z_index = 1,
                    },
                    .transition = floating_panel_transition,
                });

                const scroll = ScrollContainer.start(ctx, .{
                    .sizing = .{ .w = .grow, .h = .fitMinMax(.{ .min = 0, .max = scroll_max_h }) },
                    .gap = 2,
                });
                for (options) |option| {
                    _ = ComboboxItem.start(ctx, .{
                        .key = optionKey(option),
                        .label = option.label(),
                        .bg_color = params.item.bg_color,
                        .bg_color_on_hover = params.item.bg_color_on_hover,
                        .text_color = params.item.text_color,
                        .text_color_on_hover = params.item.text_color_on_hover,
                    });
                }
                scroll.end();

                clay.closeElement();
            }

            return .{ .id = element_id, .ctx = ctx, .params = params, .options = options };
        }

        pub fn end(self: *const Self) void {
            self.ctx.popParent();
            clay.closeElement();
        }

        pub fn selected(self: *const Self) Option {
            return findOptionByKey(self.options, self.ctx.getWidgetStateById(self.id).?.combobox.selected_key);
        }
    };
}

const ComboboxItem = struct {
    pub const Params = struct {
        key: u32,
        label: []const u8,
        padding: clay.Padding = .{ .left = 10, .right = 10, .top = 5, .bottom = 5 },
        bg_color: Color = Color.white,
        bg_color_on_hover: Color = Color.blue,
        text_color: Color = Color.black,
        text_color_on_hover: Color = Color.white,
    };
    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        _ = clay.openElement();
        const is_hovered = clay.hovered();
        const bg = if (is_hovered)
            params.bg_color_on_hover
        else
            params.bg_color;
        const text_col = if (is_hovered)
            params.text_color_on_hover
        else
            params.text_color;

        clay.configureOpenElement(.{
            .layout = .{
                .sizing = .{ .w = .grow, .h = .fit },
                .padding = params.padding,
                .child_alignment = .{ .y = .center },
                .direction = .left_to_right,
                .child_gap = 6,
            },
            .background_color = bg.toClay(),
            .corner_radius = .all(3),
        });

        _ = Label.start(.{ .text = params.label, .font_size = 14, .color = text_col });

        clay.closeElement();

        if (is_hovered and ctx.frame.mouse_pressed) {
            const combobox_id = ctx.getGrandParent().?;
            const parent = ctx.getWidgetStateById(combobox_id).?;
            parent.combobox.selected_key = params.key;
            parent.combobox.is_open = false;
        }
        return .{};
    }
};

pub const Slider = struct {
    id: clay.ElementId,
    ctx: *UIContext,
    updated_value: f32,

    pub const Params = struct {
        id: ?[]const u8 = null,
        value: f32,
        min: f32,
        max: f32,
        step: ?f32 = null,
        /// Height of the track bar in pixels.
        height: u16 = 4,
        /// Diameter of the circular thumb; also controls the overall widget height.
        thumb_size: u16 = 14,
        fill_color: Color,
        track_color: Color,
        thumb_color: Color,
        corner_radius: f32 = 2,
    };

    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        const state = ctx.getOrCreateWidgetState(element_id, .{ .slider = .{
            .dragging = false,
        } });
        const is_hovered = clay.pointerOver(element_id);

        if (is_hovered and ctx.frame.mouse_pressed) state.slider.dragging = true;
        if (ctx.frame.mouse_released) state.slider.dragging = false;

        const elem = clay.getElementData(element_id);
        const track_w = if (elem.found and elem.bounding_box.width > 1) elem.bounding_box.width else 200.0;
        const track_x = if (elem.found) elem.bounding_box.x else 0.0;

        // Compute value from the mouse's absolute X position within the track.
        var raw_value = params.value;
        var new_value = params.value;
        if (state.slider.dragging and ctx.frame.mouse_down) {
            const range = params.max - params.min;
            raw_value = std.math.clamp(
                params.min + (ctx.mouse_x - track_x) / track_w * range,
                params.min,
                params.max,
            );
            new_value = raw_value;
            if (params.step) |step| if (step > 0) {
                new_value = @round((new_value - params.min) / step) * step + params.min;
                new_value = std.math.clamp(new_value, params.min, params.max);
            };
        }

        const visual_value = std.math.clamp(if (state.slider.dragging and ctx.frame.mouse_down) raw_value else params.value, params.min, params.max);

        // Full-width track with a proportional left fill and a thumb at the boundary.
        const frac = if (params.max > params.min)
            std.math.clamp((visual_value - params.min) / (params.max - params.min), 0.0, 1.0)
        else
            0.0;

        const thumb_d: f32 = @floatFromInt(params.thumb_size);
        // Fill ends half a thumb-diameter before the thumb centre so the thumb sits exactly at the value boundary.
        const fill_w = std.math.clamp(track_w * frac - thumb_d / 2.0, 0.0, track_w - thumb_d);

        const track_h: f32 = @floatFromInt(params.height);
        const is_dragging = state.slider.dragging and ctx.frame.mouse_down;
        const slider_transition = clay.TransitionElementConfig{
            .handler = clay.easeOut,
            .duration = if (is_dragging) 0 else 0.14,
            .properties = .{ .width = true },
        };
        const slider_thumb_transition = clay.TransitionElementConfig{
            .handler = clay.easeOut,
            .duration = if (is_dragging) 0 else 0.14,
            .properties = clay.TransitionProperty.position,
        };

        // Outer container
        clay.configureOpenElement(.{
            .layout = .{
                .sizing = .{ .w = .grow, .h = .fixed(thumb_d) },
                .direction = .left_to_right,
                .child_alignment = .{ .y = .center },
            },
        });

        {
            // Left fill (up to the thumb).
            clay.openElementWithId(clay.ElementId.localID("slider-fill"));
            clay.configureOpenElement(.{
                .layout = .{ .sizing = .{ .w = .fixed(fill_w), .h = .fixed(track_h) } },
                .background_color = params.fill_color.toClay(),
                .corner_radius = .all(params.corner_radius),
                .transition = slider_transition,
            });
            clay.closeElement();

            // Circular thumb
            clay.openElementWithId(clay.ElementId.localID("slider-thumb"));
            clay.configureOpenElement(.{
                .layout = .{ .sizing = .{ .w = .fixed(thumb_d), .h = .fixed(thumb_d) } },
                .background_color = params.thumb_color.toClay(),
                .corner_radius = .all(thumb_d / 2.0),
                .transition = slider_thumb_transition,
            });
            clay.closeElement();

            // Right remainder track.
            clay.openElementWithId(clay.ElementId.localID("slider-track"));
            clay.configureOpenElement(.{
                .layout = .{ .sizing = .{ .w = .grow, .h = .fixed(track_h) } },
                .background_color = params.track_color.toClay(),
                .corner_radius = .all(params.corner_radius),
            });
            clay.closeElement();
        }

        clay.closeElement();
        return .{ .id = element_id, .ctx = ctx, .updated_value = new_value };
    }

    /// Returns the value after applying any drag delta this frame.
    pub fn value(self: *const Self) f32 {
        return self.updated_value;
    }
};

/// An on/off toggle switch rendered as a pill with a sliding circle thumb.
/// Click to flip.  Call `.value()` to read the (potentially updated) state.
pub const Toggle = struct {
    id: clay.ElementId,
    updated_value: bool,

    pub const Params = struct {
        id: ?[]const u8 = null,
        value: bool,
        on_color: Color = .blue,
        off_color: Color = .gray,
        thumb_color: Color = .white,
        /// Height of the pill (width = height × 2).
        size: u16 = 20,
    };

    const Self = @This();

    pub fn start(ctx: *UIContext, params: Params) Self {
        const element_id = if (params.id) |id| b: {
            const element_id = clay.ElementId.ID(id);
            clay.openElementWithId(element_id);
            break :b element_id;
        } else clay.openElement();

        const is_hovered = clay.pointerOver(element_id);
        var new_value = params.value;
        if (is_hovered and ctx.frame.mouse_pressed) new_value = !params.value;

        const h: f32 = @floatFromInt(params.size);
        const w: f32 = h * 2.0;
        const pad: u16 = @intFromFloat(@round(h * 0.15));
        const circle_d: f32 = h - @as(f32, @floatFromInt(pad)) * 2.0;
        const travel = @max(0.0, w - @as(f32, @floatFromInt(pad * 2)) - circle_d);
        const leading_space = if (new_value) travel else 0.0;
        const trailing_space = travel - leading_space;
        const toggle_bg_transition = clay.TransitionElementConfig{
            .handler = clay.easeOut,
            .duration = 0.12,
            .properties = .{ .background_color = true },
        };
        const toggle_space_transition = clay.TransitionElementConfig{
            .handler = clay.easeOut,
            .duration = 0.12,
            .properties = .{ .width = true },
        };
        const toggle_thumb_transition = clay.TransitionElementConfig{
            .handler = clay.easeOut,
            .duration = 0.12,
            .properties = clay.TransitionProperty.position,
        };

        clay.configureOpenElement(.{
            .layout = .{
                .sizing = .{ .w = .fixed(w), .h = .fixed(h) },
                .direction = .left_to_right,
                .child_alignment = .{ .y = .center },
                .padding = .all(pad),
            },
            .background_color = (if (new_value) params.on_color else params.off_color).toClay(),
            .corner_radius = .all(h / 2.0),
            .transition = toggle_bg_transition,
        });

        clay.openElementWithId(clay.ElementId.localID("toggle-leading-space"));
        clay.configureOpenElement(.{
            .layout = .{ .sizing = .{ .w = .fixed(leading_space), .h = .fixed(0) } },
            .transition = toggle_space_transition,
        });
        clay.closeElement();

        clay.openElementWithId(clay.ElementId.localID("toggle-thumb"));
        clay.configureOpenElement(.{
            .layout = .{ .sizing = .{ .w = .fixed(circle_d), .h = .fixed(circle_d) } },
            .background_color = params.thumb_color.toClay(),
            .corner_radius = .all(circle_d / 2.0),
            .transition = toggle_thumb_transition,
        });
        clay.closeElement();

        clay.openElementWithId(clay.ElementId.localID("toggle-trailing-space"));
        clay.configureOpenElement(.{
            .layout = .{ .sizing = .{ .w = .fixed(trailing_space), .h = .fixed(0) } },
            .transition = toggle_space_transition,
        });
        clay.closeElement();

        clay.closeElement();
        return .{ .id = element_id, .updated_value = new_value };
    }

    /// Returns the (potentially flipped) value after this frame's click.
    pub fn value(self: *const Self) bool {
        return self.updated_value;
    }
};

pub const ShapeData = struct {
    /// Must have values between 0.0-1.0
    vertices: []const clay.Vector2,
    rotation: f32,
    color: Color,
};

pub const Shape = struct {
    params: Params,

    pub const Params = struct {
        sizing: clay.Sizing = .{ .w = .grow, .h = .grow },
        vertices: []const clay.Vector2,
        rotation: f32 = 0,
        color: Color = Color.black,
    };
    const Self = @This();

    pub const TRIANGLE = [_]clay.Vector2{
        .{ .x = 0.0, .y = 0.75 },
        .{ .x = 1.0, .y = 0.75 },
        .{ .x = 0.5, .y = 0.25 },
    };
    pub const RIGHT_TRIANGLE = [_]clay.Vector2{
        .{ .x = 0.25, .y = 0.0 },
        .{ .x = 0.25, .y = 1.0 },
        .{ .x = 0.75, .y = 0.5 },
    };
    pub const SQUARE = [_]clay.Vector2{
        .{ .x = 0.0, .y = 0.0 },
        .{ .x = 1.0, .y = 0.0 },
        .{ .x = 1.0, .y = 1.0 },
        .{ .x = 0.0, .y = 1.0 },
    };

    pub fn start(ctx: *UIContext, params: Params) Self {
        _ = clay.openElement();

        const custom_data = ctx.frameAlloc().create(CustomData) catch @panic("Alloc failed");
        custom_data.* = .{ .shape = .{
            .vertices = params.vertices,
            .rotation = params.rotation,
            .color = params.color,
        } };

        clay.configureOpenElement(.{
            .layout = .{ .sizing = params.sizing },
            .custom = .{ .custom_data = clay.anytypeToAnyopaquePtr(custom_data) },
        });
        clay.closeElement();

        return .{ .params = params };
    }
};

pub const Separator = struct {
    params: Params,

    pub const Direction = enum {
        horizontal,
        vertical,
    };
    pub const Params = struct {
        direction: Direction = .horizontal,
        thickness: f32 = 1.0,
        color: Color = Color.black,
    };
    const Self = @This();

    pub fn start(params: Params) Self {
        _ = clay.openElement();
        const w: clay.SizingAxis = if (params.direction == .horizontal) .grow else .fixed(params.thickness);
        const h: clay.SizingAxis = if (params.direction == .horizontal) .fixed(params.thickness) else .grow;

        clay.configureOpenElement(.{
            .layout = .{
                .sizing = .{ .w = w, .h = h },
            },
            .background_color = params.color.toClay(),
        });
        clay.closeElement();
        return .{ .params = params };
    }
};
