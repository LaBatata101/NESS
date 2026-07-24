const std = @import("std");

const c = @import("../root.zig").c;
const UI = @import("core/ui.zig").UI;
const opcodes = @import("../opcodes.zig");
const OpCode = opcodes.OpCode;
const theme = @import("common.zig").theme;
const AppState = @import("state.zig").AppState;
const Shape = @import("core/widgets.zig").Shape;
const Color = @import("core/color.zig").Color;
const OVERSCAN_TOP = @import("../root.zig").OVERSCAN_TOP;
const NES_VISIBLE_HEIGHT = @import("../root.zig").NES_VISIBLE_HEIGHT;
const OVERSCAN_PIXEL_OFFSET = OVERSCAN_TOP * NES_WIDTH * 4;
const NES_VISIBLE_PIXEL_BYTES = NES_WIDTH * NES_VISIBLE_HEIGHT * 4;
const NES_WIDTH = @import("../root.zig").NES_WIDTH;
const SYSTEM_PALETTE = @import("../render.zig").SYSTEM_PALETTE;

pub fn drawUI(ui: *UI, app_state: *AppState) void {
    const snapshot = app_state.debugSnapshot();
    const root = ui.row(.{ .bg_color = theme.bg_base, .sizing = .grow, .gap = 2 });
    {
        const col = ui.column(.{ .child_alignment = .{ .y = .top } });
        {
            drawGameScreen(ui, app_state);
            drawPatternAndPalettePanel(ui, &snapshot);
        }
        col.end();
        drawDebugSidebar(ui, &snapshot);
    }
    root.end();

    const panel = ui.draggablePanel(.{
        .attach_to = .to_element_with_id,
        .attach_points = .{ .element = .center_top, .parent = .center_top },
        .parentId = root.id.id,
        .bg_color = theme.bg_section,
        .border = .{ .width = .outside(1), .color = theme.border.toClay() },
        .corner_radius = 8,
        .grip_color = theme.text_muted,
    });
    {
        const buttons = ui.row(.{ .sizing = .fit, .padding = .all(5), .gap = 3 });
        {
            if (ui.iconButton(.{
                .icon = ui.icons.get(.skip_next),
                .tooltip = .{
                    .text = std.fmt.allocPrint(
                        ui.current_window.ctx.frameAlloc(),
                        "Run tick ({s})",
                        .{app_state.generalBinding(.run_tick).keyName()},
                    ) catch @panic("OOM"),
                },
                .bg_color = Color.transparent,
                .hover_color = theme.bg_hover,
            }).clicked(ui.current_window.ctx)) {
                app_state.runSystemTick();
            }
            if (ui.iconButton(.{
                .icon = ui.icons.get(.fast_forward),
                .tooltip = .{
                    .text = std.fmt.allocPrint(
                        ui.current_window.ctx.frameAlloc(),
                        "Run frame ({s})",
                        .{app_state.generalBinding(.run_frame).keyName()},
                    ) catch @panic("OOM"),
                },
                .bg_color = Color.transparent,
                .hover_color = theme.bg_hover,
            }).clicked(ui.current_window.ctx)) {
                app_state.runSystemFrame();
            }
            if (ui.iconButton(.{
                .icon = if (app_state.step_mode) ui.icons.get(.play) else ui.icons.get(.stop),
                .tooltip = .{
                    .text = if (app_state.step_mode) "Run emulation" else "Stop emulation",
                },
                .bg_color = Color.transparent,
                .hover_color = theme.bg_hover,
            }).clicked(ui.current_window.ctx)) {
                app_state.toggleStepMode();
            }
        }
        buttons.end();
    }
    panel.end();
}

fn drawGameScreen(ui: *UI, app_state: *AppState) void {
    const wrapper = ui.column(.{ .sizing = .{ .w = .grow, .h = .percent(0.677) } });
    _ = ui.canvas(.{
        .pixel_format = c.SDL_PIXELFORMAT_ABGR8888,
        .pixels = app_state.framePixels(OVERSCAN_PIXEL_OFFSET, NES_VISIBLE_PIXEL_BYTES),
        .w = NES_WIDTH,
        .h = NES_VISIBLE_HEIGHT,
    });
    wrapper.end();
}

fn drawDebugSidebar(ui: *UI, snapshot: *const AppState.DebugSnapshot) void {
    const sidebar = ui.column(.{
        .sizing = .grow,
        .bg_color = theme.bg_panel,
        .gap = theme.PANEL_GAP,
        .child_alignment = .{ .y = .top },
    });
    {
        drawDisassemblyPanel(ui, snapshot);
        drawRegistersPanel(ui, snapshot);
        drawPPUPanel(ui, snapshot);
    }
    sidebar.end();
}

fn drawSectionHeader(ui: *UI, title: []const u8, accent: Color) void {
    const header = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .bg_color = theme.bg_section,
        .padding = theme.HEADER_PAD,
        .child_alignment = .{ .y = .center },
        .gap = 8,
        .border = .{ .width = .outside(1), .color = theme.border_dim.toClay() },
    });
    {
        // Coloured left accent bar
        _ = ui.shape(.{
            .vertices = &Shape.SQUARE,
            .sizing = .{ .w = .fixed(3), .h = .fixed(14) },
            .color = accent,
        });

        _ = ui.label(.{
            .text = title,
            .font_size = theme.LABEL_FONT,
            .color = theme.text_secondary,
        });
    }
    header.end();
}

fn drawDisassemblyPanel(ui: *UI, snapshot: *const AppState.DebugSnapshot) void {
    drawSectionHeader(ui, "DISASSEMBLY", theme.accent_amber);

    const body = ui.column(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .bg_color = theme.bg_panel,
        .padding = .{ .top = 2, .bottom = 4 },
        .gap = 1,
    });
    {
        var op_size: u16 = 0;
        for (0..12) |i| {
            const is_current = (i == 0); // first entry is always the current PC

            const pc = snapshot.pc +% op_size;
            const code = snapshot.memPeek(pc);
            const opcode = opcodes.OP_CODES[code];

            const buf = ui.current_window.ctx.frameAlloc().alloc(u8, 23) catch @panic("alloc");
            const addr_buf = ui.current_window.ctx.frameAlloc().alloc(u8, 27) catch @panic("alloc");

            const addr_str = std.fmt.allocPrint(
                ui.current_window.ctx.frameAlloc(),
                "${X:04}",
                .{pc},
            ) catch @panic("fmt");
            const op_str = formatOpcode(
                snapshot,
                code,
                pc,
                buf,
            ) catch @panic("fmt");
            const mode_str = formatAddressingMode(
                snapshot,
                opcode,
                pc +% 1,
                addr_buf,
            ) catch @panic("fmt");

            op_size +%= opcode.size();

            drawDisassemblyRow(ui, addr_str, op_str, mode_str, is_current);
        }
    }
    body.end();
}

fn drawDisassemblyRow(ui: *UI, addr: []const u8, mnemonic: []const u8, operand: []const u8, is_current: bool) void {
    const bg = if (is_current) theme.bg_active else Color.rgb(0, 0, 0).withAlpha(0);
    const row = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .bg_color = bg,
        .padding = .{ .left = 10, .right = 10, .top = 3, .bottom = 3 },
        .child_alignment = .{ .y = .center },
    });
    {
        // PC arrow indicator (only for the current line)
        if (is_current) {
            _ = ui.shape(.{
                .vertices = &Shape.TRIANGLE,
                .sizing = .{ .w = .fixed(10), .h = .fixed(10) },
                .rotation = 90,
                .color = theme.accent_amber,
            });
            _ = ui.spacer(.{ .sizing = .{ .w = .fixed(5) } });
        } else {
            _ = ui.spacer(.{ .sizing = .{ .w = .fixed(12) } });
        }

        _ = ui.label(.{
            .text = addr,
            .font_size = theme.CODE_FONT,
            .color = if (is_current) theme.text_warn else theme.text_accent,
        });

        _ = ui.spacer(.{ .sizing = .{ .w = .fixed(10) } });

        _ = ui.label(.{
            .text = mnemonic,
            .font_size = theme.CODE_FONT,
            .color = if (is_current) theme.text_primary else theme.text_primary.withAlpha(0.71),
        });

        if (operand.len > 0) {
            _ = ui.spacer(.{ .sizing = .{ .w = .fixed(6) } });
            _ = ui.label(.{
                .text = operand,
                .font_size = theme.CODE_FONT,
                .color = if (is_current) theme.text_value else theme.text_secondary,
            });
        }
    }
    row.end();
}

fn formatOpcode(snapshot: *const AppState.DebugSnapshot, code: u8, pc: u16, buffer: []u8) ![]u8 {
    const opcode = opcodes.OP_CODES[code];
    const mnemonic = switch (code) {
        // zig fmt: off
        0x1A, 0x3A, 0x5A, 0x7A, 0xDA, 0xFA, 0x0C, 0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC, 0x04, 0x14, 0x34, 0x44, 0x54,
        0x64, 0x74, 0x80, 0x82, 0x89, 0xC2, 0xD4, 0xE2, 0xF4, 0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92,
        0xB2, 0xD2, 0xF2 => "NOP",
        0xE7, 0xF7, 0xEF, 0xFF, 0xFB, 0xE3, 0xF3 => "ISB",
        // zig fmt: on
        else => @tagName(opcode),
    };

    const instruction_size = opcode.size();
    var inst_bytes: [9]u8 = [_]u8{' '} ** 9;
    var pos: usize = 0;
    for (0..instruction_size) |i| {
        _ = try std.fmt.bufPrint(
            inst_bytes[pos..],
            "{X:02} ",
            .{snapshot.memPeek(pc + @as(u16, @intCast(i)))},
        );
        pos += 3;
    }

    pos = 0;
    var str: []u8 = undefined;
    if (opcode.is_unofficial()) {
        const p = try std.fmt.bufPrint(buffer[0..], "{s: <9}", .{inst_bytes});
        str = try std.fmt.bufPrint(buffer[p.len..], "*{s}", .{mnemonic});
        pos += p.len + str.len;
    } else {
        const p = try std.fmt.bufPrint(buffer[0..], "{s: <10}", .{inst_bytes});
        str = try std.fmt.bufPrint(buffer[p.len..], "{s}", .{mnemonic});
        pos += p.len + str.len;
    }
    return buffer[0..pos];
}

fn formatAddressingMode(snapshot: *const AppState.DebugSnapshot, opcode: OpCode, arg_addr: u16, buffer: []u8) ![]u8 {
    switch (opcode.addressing_mode()) {
        .Immediate => {
            return try std.fmt.bufPrint(buffer, " #${X:02}", .{snapshot.memPeek(arg_addr)});
        },
        .Relative => {
            const offset = @as(i8, @bitCast(snapshot.memPeek(arg_addr)));
            return try std.fmt.bufPrint(
                buffer,
                " ${X:04}",
                .{arg_addr +% 1 +% @as(u16, @bitCast(@as(i16, offset)))},
            );
        },
        .Absolute => {
            const ptr_addr = snapshot.memPeekU16(arg_addr);
            switch (opcode) {
                .JMP, .JSR => return try std.fmt.bufPrint(buffer, " ${X:04}", .{ptr_addr}),
                else => return try std.fmt.bufPrint(buffer, " ${X:04} = {X:02}", .{
                    ptr_addr,
                    snapshot.memPeek(ptr_addr),
                }),
            }
        },
        .AbsoluteX => {
            const base = snapshot.memPeekU16(arg_addr);
            const ptr_addr = base +% snapshot.register_x;
            return try std.fmt.bufPrint(
                buffer,
                " ${X:04},X @ {X:04} = {X:02}",
                .{ base, ptr_addr, snapshot.memPeek(ptr_addr) },
            );
        },
        .AbsoluteY => {
            const base = snapshot.memPeekU16(arg_addr);
            const ptr_addr = base +% snapshot.register_y;

            return try std.fmt.bufPrint(
                buffer,
                " ${X:04},Y @ {X:04} = {X:02}",
                .{ base, ptr_addr, snapshot.memPeek(ptr_addr) },
            );
        },
        .ZeroPage => {
            const ptr_addr = snapshot.memPeek(arg_addr);
            return try std.fmt.bufPrint(
                buffer,
                " ${X:02} = {X:02}",
                .{ ptr_addr, snapshot.memPeek(ptr_addr) },
            );
        },
        .ZeroPageX => {
            const base = snapshot.memPeek(arg_addr);
            const ptr_addr = base +% snapshot.register_x;
            return try std.fmt.bufPrint(
                buffer,
                " ${X:02},X @ {X:02} = {X:02}",
                .{ base, ptr_addr, snapshot.memPeek(ptr_addr) },
            );
        },
        .ZeroPageY => {
            const base = snapshot.memPeek(arg_addr);
            const ptr_addr = base +% snapshot.register_y;
            return try std.fmt.bufPrint(
                buffer,
                " ${X:02},Y @ {X:02} = {X:02}",
                .{ base, ptr_addr, snapshot.memPeek(ptr_addr) },
            );
        },
        .Indirect => {
            const ptr_addr = snapshot.memPeekU16(arg_addr);
            const jmp_addr = blk: {
                if (ptr_addr & 0xFF == 0xFF) {
                    const lo = snapshot.memPeek(ptr_addr);
                    const hi = snapshot.memPeek(ptr_addr & 0xFF00);
                    break :blk @as(u16, hi) << 8 | lo;
                } else {
                    break :blk snapshot.memPeekU16(ptr_addr);
                }
            };
            return try std.fmt.bufPrint(
                buffer,
                " (${X:04}) = {X:04}",
                .{ ptr_addr, jmp_addr },
            );
        },
        .IndirectX => {
            const base = snapshot.memPeek(arg_addr);
            const ptr = base +% snapshot.register_x;
            const lo = snapshot.memPeek(ptr);
            const hi = snapshot.memPeek(ptr +% 1);
            const ptr_addr = @as(u16, hi) << 8 | lo;
            return try std.fmt.bufPrint(
                buffer,
                " (${X:02},X) @ {X:02} = {X:04} = {X:02}",
                .{ base, ptr, ptr_addr, snapshot.memPeek(ptr_addr) },
            );
        },
        .IndirectY => {
            const base = snapshot.memPeek(arg_addr);
            const lo = snapshot.memPeek(base);
            const hi = snapshot.memPeek(base +% 1);
            const base_ptr_addr = @as(u16, hi) << 8 | lo;
            const ptr_addr = base_ptr_addr +% snapshot.register_y;
            return try std.fmt.bufPrint(
                buffer,
                " (${X:02}),Y = {X:04} @ {X:04} = {X:02}",
                .{ base, base_ptr_addr, ptr_addr, snapshot.memPeek(ptr_addr) },
            );
        },
        .Implicit => {
            switch (opcode) {
                .ASL, .LSR, .ROL, .ROR => return try std.fmt.bufPrint(buffer, " A", .{}),
                else => return "",
            }
        },
    }
}

fn drawRegistersPanel(ui: *UI, snapshot: *const AppState.DebugSnapshot) void {
    drawSectionHeader(ui, "CPU REGISTERS", theme.accent_blue);

    const body = ui.column(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .bg_color = theme.bg_panel,
        .padding = theme.SECTION_PAD,
        .gap = 6,
    });
    {
        const reg_row = ui.row(.{ .sizing = .{ .w = .grow, .h = .fit }, .gap = 8 });
        {
            drawRegisterBadge(
                ui,
                "A",
                std.fmt.allocPrint(ui.current_window.ctx.frameAlloc(), "${X:02}", .{snapshot.register_a}) catch @panic("fmt"),
                theme.accent_blue,
            );
            drawRegisterBadge(
                ui,
                "X",
                std.fmt.allocPrint(ui.current_window.ctx.frameAlloc(), "${X:02}", .{snapshot.register_x}) catch @panic("fmt"),
                theme.accent_purple,
            );
            drawRegisterBadge(
                ui,
                "Y",
                std.fmt.allocPrint(ui.current_window.ctx.frameAlloc(), "${X:02}", .{snapshot.register_y}) catch @panic("fmt"),
                theme.accent_green,
            );
            drawRegisterBadge(
                ui,
                "PC",
                std.fmt.allocPrint(ui.current_window.ctx.frameAlloc(), "${X:04}", .{snapshot.pc}) catch @panic("fmt"),
                theme.accent_amber,
            );
            drawRegisterBadge(
                ui,
                "SP",
                std.fmt.allocPrint(ui.current_window.ctx.frameAlloc(), "${X:02}", .{snapshot.sp}) catch @panic("fmt"),
                theme.accent_red,
            );
        }
        reg_row.end();

        const flags_row = ui.row(.{
            .sizing = .{ .w = .grow, .h = .fit },
            .gap = 4,
            .child_alignment = .{ .y = .center },
        });
        {
            // _ = ui.label(.{ .text = "FLAGS:", .font_size = LABEL_FONT, .color = theme.text_muted });
            _ = ui.label(.{ .text = "FLAGS:", .font_size = theme.LABEL_FONT, .color = theme.text_secondary });
            _ = ui.spacer(.{ .sizing = .{ .w = .fixed(6) } });
            drawFlagBadge(ui, "N", snapshot.status.negative_flag);
            drawFlagBadge(ui, "V", snapshot.status.overflow_flag);
            drawFlagBadge(ui, "B", snapshot.status.break_command);
            drawFlagBadge(ui, "-", snapshot.status.break2); // break2 placeholder
            drawFlagBadge(ui, "I", snapshot.status.interrupt_disable);
            drawFlagBadge(ui, "Z", snapshot.status.zero_flag);
            drawFlagBadge(ui, "C", snapshot.status.carry_flag);
        }
        flags_row.end();
    }
    body.end();
}

fn drawRegisterBadge(ui: *UI, label: []const u8, value: []const u8, accent: Color) void {
    const badge = ui.row(.{
        .sizing = .fit,
        .bg_color = theme.bg_section,
        .border = .{ .width = .outside(1), .color = theme.border.toClay() },
        .corner_radius = 4,
        .padding = .{ .left = 0, .right = 8, .top = 4, .bottom = 4 },
        .child_alignment = .{ .y = .center },
        .gap = 0,
    });
    {
        _ = ui.shape(.{
            .vertices = &Shape.SQUARE,
            .sizing = .{ .w = .fixed(3), .h = .fixed(22) },
            .color = accent,
        });
        _ = ui.spacer(.{ .sizing = .{ .w = .fixed(7) } });

        // _ = ui.label(.{ .text = label, .font_size = LABEL_FONT, .color = theme.text_muted });
        _ = ui.label(.{ .text = label, .font_size = theme.LABEL_FONT, .color = theme.text_secondary });
        _ = ui.spacer(.{ .sizing = .{ .w = .fixed(5) } });
        _ = ui.label(.{ .text = value, .font_size = theme.VALUE_FONT, .color = theme.text_value });
    }
    badge.end();
}

fn drawFlagBadge(ui: *UI, name: []const u8, active: bool) void {
    const bg = if (active) theme.accent_blue.withAlpha(0.24) else theme.bg_section;
    const txt = if (active) theme.text_primary else theme.text_muted;
    const bdr = if (active) theme.accent_blue.withAlpha(0.47) else theme.border_dim;

    const pill = ui.row(.{
        .sizing = .{ .w = .fixed(26), .h = .fixed(22) },
        .bg_color = bg,
        .border = .{ .width = .outside(1), .color = bdr.toClay() },
        .corner_radius = 3,
        .child_alignment = .center,
    });
    {
        _ = ui.label(.{ .text = name, .font_size = theme.LABEL_FONT, .color = txt });
    }
    pill.end();
}

fn drawPPUPanel(ui: *UI, snapshot: *const AppState.DebugSnapshot) void {
    drawSectionHeader(ui, "PPU STATE", theme.accent_purple);

    const body = ui.row(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .bg_color = theme.bg_panel,
        .padding = theme.SECTION_PAD,
        .gap = 6,
        .child_alignment = .{ .y = .center },
    });
    {
        drawStatBadge(ui, "SCANLINE", std.fmt.allocPrint(
            ui.current_window.ctx.frameAlloc(),
            "{}",
            .{snapshot.scanline},
        ) catch @panic("fmt"), theme.accent_purple);

        drawStatBadge(ui, "CYCLE", std.fmt.allocPrint(
            ui.current_window.ctx.frameAlloc(),
            "{}",
            .{snapshot.cycle},
        ) catch @panic("fmt"), theme.accent_blue);

        drawStatBadge(ui, "GLOBAL", std.fmt.allocPrint(
            ui.current_window.ctx.frameAlloc(),
            "{}",
            .{snapshot.global_cycle},
        ) catch @panic("fmt"), theme.accent_green);
    }
    body.end();
}

fn drawStatBadge(ui: *UI, key: []const u8, value: []const u8, accent: Color) void {
    const cell = ui.column(.{
        .sizing = .fit,
        .bg_color = theme.bg_section,
        .border = .{ .width = .outside(1), .color = theme.border.toClay() },
        .corner_radius = 4,
        .padding = .{ .left = 10, .right = 10, .top = 5, .bottom = 5 },
        .child_alignment = .{ .x = .left, .y = .top },
        .gap = 2,
    });
    {
        _ = ui.label(.{ .text = key, .font_size = theme.LABEL_FONT, .color = accent });
        _ = ui.label(.{ .text = value, .font_size = 16, .color = theme.text_primary });
    }
    cell.end();
}

fn drawPatternAndPalettePanel(ui: *UI, snapshot: *const AppState.DebugSnapshot) void {
    const row = ui.row(.{
        .sizing = .grow,
        .bg_color = theme.bg_panel,
        .gap = 10,
    });
    {
        drawPatternTablesPanel(ui, snapshot);
        drawPalettePanel(ui, snapshot);
    }
    row.end();
}

fn drawPatternTablesPanel(ui: *UI, snapshot: *const AppState.DebugSnapshot) void {
    const col = ui.column(.{
        .sizing = .{ .w = .fit, .h = .grow },
        .bg_color = theme.bg_panel,
        .gap = theme.PANEL_GAP,
    });
    {
        drawSectionHeader(ui, "PATTERN TABLES", theme.accent_green);

        const body = ui.row(.{
            .sizing = .fit,
            .bg_color = theme.bg_panel,
            .padding = .{ .left = 12, .right = 12, .top = 10, .bottom = 10 },
            .gap = 10,
            .child_alignment = .{ .y = .top },
        });
        {
            const pt1_wrapper = ui.column(.{
                .sizing = .fit,
                .bg_color = theme.bg_section,
                .border = .{ .width = .outside(1), .color = theme.border.toClay() },
                .corner_radius = 3,
                .padding = .{ .left = 4, .right = 4, .top = 4, .bottom = 2 },
                .gap = 4,
                .child_alignment = .{ .x = .center },
            });
            {
                _ = ui.canvas(.{
                    .sizing = .{ .w = .fixed(128), .h = .fixed(128) },
                    .pixel_format = c.SDL_PIXELFORMAT_ABGR8888,
                    .w = 128,
                    .h = 128,
                    .pixels = &snapshot.pattern_table_0.data,
                });
                // _ = ui.label(.{ .text = "CHR $0000", .font_size = LABEL_FONT, .color = theme.text_muted });
                _ = ui.label(.{ .text = "CHR $0000", .font_size = theme.LABEL_FONT, .color = theme.text_secondary });
            }
            pt1_wrapper.end();

            const pt2_wrapper = ui.column(.{
                .sizing = .fit,
                .bg_color = theme.bg_section,
                .border = .{ .width = .outside(1), .color = theme.border.toClay() },
                .corner_radius = 3,
                .padding = .{ .left = 4, .right = 4, .top = 4, .bottom = 2 },
                .gap = 4,
                .child_alignment = .{ .x = .center },
            });
            {
                _ = ui.canvas(.{
                    .sizing = .{ .w = .fixed(128), .h = .fixed(128) },
                    .pixel_format = c.SDL_PIXELFORMAT_ABGR8888,
                    .w = 128,
                    .h = 128,
                    .pixels = &snapshot.pattern_table_1.data,
                });
                // _ = ui.label(.{ .text = "CHR $1000", .font_size = LABEL_FONT, .color = theme.text_muted });
                _ = ui.label(.{ .text = "CHR $1000", .font_size = theme.LABEL_FONT, .color = theme.text_secondary });
            }
            pt2_wrapper.end();
        }
        body.end();
    }
    col.end();
}

fn drawPalettePanel(ui: *UI, snapshot: *const AppState.DebugSnapshot) void {
    const col = ui.column(.{
        .sizing = .grow,
        .bg_color = theme.bg_panel,
        .gap = theme.PANEL_GAP,
    });
    {
        drawSectionHeader(ui, "PALETTE RAM", theme.accent_red);

        const body = ui.row(.{
            .sizing = .{ .w = .grow, .h = .fit },
            .bg_color = theme.bg_panel,
            .padding = .{ .left = 12, .right = 12, .top = 10, .bottom = 10 },
            .gap = 10,
        });
        {
            drawPaletteGroup(ui, snapshot.palette_ram, "BACKGROUND", false);
            drawPaletteGroup(ui, snapshot.palette_ram, "SPRITE", true);
        }
        body.end();
    }
    col.end();
}

fn drawPaletteGroup(
    ui: *UI,
    palette_ram: [32]u8,
    label: []const u8,
    is_sprite: bool,
) void {
    const grp = ui.column(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .bg_color = theme.bg_section,
        .border = .{ .width = .outside(1), .color = theme.border.toClay() },
        .corner_radius = 4,
        .padding = .{ .left = 8, .right = 8, .top = 6, .bottom = 8 },
        .gap = 6,
    });
    {
        _ = ui.label(.{ .text = label, .font_size = theme.LABEL_FONT, .color = theme.text_secondary });

        // 4 sub-palettes, each 4 colours, separated by a subtle gap
        const palettes_row = ui.column(.{
            .sizing = .{ .w = .grow, .h = .fit },
            .gap = 6,
            .child_alignment = .{ .y = .top },
        });
        {
            for (0..4) |palette_num| {
                const sub = ui.row(.{
                    .sizing = .fit,
                    .bg_color = theme.bg_panel,
                    .border = .{ .width = .outside(1), .color = theme.border_dim.toClay() },
                    .corner_radius = 2,
                    .padding = .{ .left = 2, .right = 2, .top = 2, .bottom = 2 },
                    .gap = theme.SWATCH_GAP,
                });
                {
                    // Sub-palette index label
                    _ = ui.label(.{
                        .text = std.fmt.allocPrint(
                            ui.current_window.ctx.frameAlloc(),
                            "{d}",
                            .{palette_num},
                        ) catch @panic("fmt"),
                        .font_size = theme.LABEL_FONT,
                        // .color = theme.text_muted,
                        .color = theme.text_secondary,
                    });

                    // 4 colour swatches stacked vertically per sub-palette
                    for (0..4) |color_num| {
                        const i = (palette_num * 4) + color_num;
                        const color = resolveColor(palette_ram, i, is_sprite);

                        _ = ui.shape(.{
                            .vertices = &Shape.SQUARE,
                            .sizing = .{ .w = .fixed(theme.SWATCH_SIZE), .h = .fixed(theme.SWATCH_SIZE) },
                            .color = color,
                        });
                    }
                }
                sub.end();
            }
        }
        palettes_row.end();
    }
    grp.end();
}

/// Resolves a palette colour from palette RAM, handling sprite mirror addresses.
inline fn resolveColor(palette_ram: [32]u8, i: usize, is_sprite: bool) Color {
    if (!is_sprite) {
        const idx = palette_ram[i];
        return SYSTEM_PALETTE[idx & 0x3F];
    } else {
        var addr: u8 = 0x10 + @as(u8, @truncate(i));
        addr = switch (addr) {
            0x10, 0x14, 0x18, 0x1C => addr - 0x10,
            else => addr,
        };
        const idx = palette_ram[addr];
        return SYSTEM_PALETTE[idx & 0x3F];
    }
}
