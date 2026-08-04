const std = @import("std");

const CPU = @import("cpu.zig").CPU;
const opcodes = @import("opcodes.zig");
const OpCode = opcodes.OpCode;

pub fn trace(writer: *std.Io.Writer, cpu: *CPU) !void {
    try writer.print("{s}\n", .{try format_trace(cpu)});
}

pub fn format_opcode(cpu: *const CPU, code: u8, pc: u16, buffer: []u8) ![]u8 {
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
            .{cpu.bus.mem_peek(pc + @as(u16, @intCast(i)))},
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

fn format_trace(cpu: *CPU) ![73]u8 {
    var pos: usize = 0;
    var trace_str: [56]u8 = undefined;
    var buffer: [73]u8 = undefined;

    const code = cpu.bus.mem_peek(cpu.pc);
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

    // Prints the PC
    var printed_buf = try std.fmt.bufPrint(&trace_str, "{X:04}  ", .{cpu.pc});
    pos += printed_buf.len;

    var inst_bytes: [9]u8 = undefined;
    var inst_bytes_pos: usize = 0;
    for (0..instruction_size) |i| {
        printed_buf = try std.fmt.bufPrint(
            inst_bytes[inst_bytes_pos..],
            "{X:02} ",
            .{cpu.bus.mem_peek(cpu.pc + @as(u16, @intCast(i)))},
        );
        inst_bytes_pos += printed_buf.len;
    }

    if (opcode.is_unofficial()) {
        printed_buf = try std.fmt.bufPrint(trace_str[pos..], "{s: <9}", .{inst_bytes[0..inst_bytes_pos]});
        pos += printed_buf.len;
        printed_buf = try std.fmt.bufPrint(trace_str[pos..], "*{s}", .{mnemonic});
    } else {
        printed_buf = try std.fmt.bufPrint(trace_str[pos..], "{s: <10}", .{inst_bytes[0..inst_bytes_pos]});
        pos += printed_buf.len;
        printed_buf = try std.fmt.bufPrint(trace_str[pos..], "{s}", .{mnemonic});
    }
    pos += printed_buf.len;

    const arg_addr = cpu.pc + 1;
    var addr_mode_buffer: [27]u8 = undefined;
    const addr_mode_str = try format_addressing_mode(cpu, opcode, arg_addr, &addr_mode_buffer);
    @memmove(trace_str[pos .. pos + addr_mode_str.len], addr_mode_str);
    pos += addr_mode_str.len;

    _ = try std.fmt.bufPrint(
        &buffer,
        "{s:<47} A:{X:02} X:{X:02} Y:{X:02} P:{X:02} SP:{X:02}",
        .{ trace_str[0..pos], cpu.register_a, cpu.register_x, cpu.register_y, @as(u8, @bitCast(cpu.status)), cpu.sp },
    );
    return buffer;
}

pub fn format_addressing_mode(cpu: *const CPU, opcode: OpCode, arg_addr: u16, buffer: []u8) ![]u8 {
    switch (opcode.addressing_mode()) {
        .Immediate => {
            return try std.fmt.bufPrint(buffer, " #${X:02}", .{cpu.bus.mem_peek(arg_addr)});
        },
        .Relative => {
            const offset = @as(i8, @bitCast(cpu.bus.mem_peek(arg_addr)));
            return try std.fmt.bufPrint(
                buffer,
                " ${X:04}",
                .{arg_addr +% 1 +% @as(u16, @bitCast(@as(i16, offset)))},
            );
        },
        .Absolute => {
            const ptr_addr = cpu.bus.mem_peek_u16(arg_addr);
            switch (opcode) {
                .JMP, .JSR => return try std.fmt.bufPrint(buffer, " ${X:04}", .{ptr_addr}),
                else => return try std.fmt.bufPrint(buffer, " ${X:04} = {X:02}", .{
                    ptr_addr,
                    cpu.bus.mem_peek(ptr_addr),
                }),
            }
        },
        .AbsoluteX => {
            const base = cpu.bus.mem_peek_u16(arg_addr);
            const ptr_addr = base +% cpu.register_x;
            return try std.fmt.bufPrint(
                buffer,
                " ${X:04},X @ {X:04} = {X:02}",
                .{ base, ptr_addr, cpu.bus.mem_peek(ptr_addr) },
            );
        },
        .AbsoluteY => {
            const base = cpu.bus.mem_peek_u16(arg_addr);
            const ptr_addr = base +% cpu.register_y;

            return try std.fmt.bufPrint(
                buffer,
                " ${X:04},Y @ {X:04} = {X:02}",
                .{ base, ptr_addr, cpu.bus.mem_peek(ptr_addr) },
            );
        },
        .ZeroPage => {
            const ptr_addr = cpu.bus.mem_peek(arg_addr);
            return try std.fmt.bufPrint(
                buffer,
                " ${X:02} = {X:02}",
                .{ ptr_addr, cpu.bus.mem_peek(ptr_addr) },
            );
        },
        .ZeroPageX => {
            const base = cpu.bus.mem_peek(arg_addr);
            const ptr_addr = base +% cpu.register_x;
            return try std.fmt.bufPrint(
                buffer,
                " ${X:02},X @ {X:02} = {X:02}",
                .{ base, ptr_addr, cpu.bus.mem_peek(ptr_addr) },
            );
        },
        .ZeroPageY => {
            const base = cpu.bus.mem_peek(arg_addr);
            const ptr_addr = base +% cpu.register_y;
            return try std.fmt.bufPrint(
                buffer,
                " ${X:02},Y @ {X:02} = {X:02}",
                .{ base, ptr_addr, cpu.bus.mem_peek(ptr_addr) },
            );
        },
        .Indirect => {
            const ptr_addr = cpu.bus.mem_peek_u16(arg_addr);
            const jmp_addr = blk: {
                if (ptr_addr & 0xFF == 0xFF) {
                    const lo = cpu.bus.mem_peek(ptr_addr);
                    const hi = cpu.bus.mem_peek(ptr_addr & 0xFF00);
                    break :blk @as(u16, hi) << 8 | lo;
                } else {
                    break :blk cpu.bus.mem_peek_u16(ptr_addr);
                }
            };
            return try std.fmt.bufPrint(
                buffer,
                " (${X:04}) = {X:04}",
                .{ ptr_addr, jmp_addr },
            );
        },
        .IndirectX => {
            const base = cpu.bus.mem_peek(arg_addr);
            const ptr = base +% cpu.register_x;
            const lo = cpu.bus.mem_peek(ptr);
            const hi = cpu.bus.mem_peek(ptr +% 1);
            const ptr_addr = @as(u16, hi) << 8 | lo;
            return try std.fmt.bufPrint(
                buffer,
                " (${X:02},X) @ {X:02} = {X:04} = {X:02}",
                .{ base, ptr, ptr_addr, cpu.bus.mem_peek(ptr_addr) },
            );
        },
        .IndirectY => {
            const base = cpu.bus.mem_peek(arg_addr);
            const lo = cpu.bus.mem_peek(base);
            const hi = cpu.bus.mem_peek(base +% 1);
            const base_ptr_addr = @as(u16, hi) << 8 | lo;
            const ptr_addr = base_ptr_addr +% cpu.register_y;
            return try std.fmt.bufPrint(
                buffer,
                " (${X:02}),Y = {X:04} @ {X:04} = {X:02}",
                .{ base, base_ptr_addr, ptr_addr, cpu.bus.mem_peek(ptr_addr) },
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

test "format trace" {
    const rom = @import("rom.zig");
    const Bus = @import("bus.zig").Bus;

    const alloc = std.testing.allocator;

    //                                 LDX         DEX   DEY
    const instructions = [_]u8{ 0xA2, 0x01, 0xCA, 0x88 };
    var test_rom = rom.TestRom.init(alloc, &instructions);
    defer test_rom.deinit();

    var bus = Bus.init(&test_rom.rom, undefined, undefined);
    var cpu = CPU.init(&bus);
    cpu.register_a = 1;
    cpu.register_x = 2;
    cpu.register_y = 3;

    try std.testing.expect(std.mem.eql(
        u8,
        "8000  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
        &try format_trace(&cpu),
    ));
    _ = cpu.tick();

    try std.testing.expect(std.mem.eql(
        u8,
        "8002  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
        &try format_trace(&cpu),
    ));
    _ = cpu.tick();

    try std.testing.expect(std.mem.eql(
        u8,
        "8003  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
        &try format_trace(&cpu),
    ));
}

test "format mem access" {
    const rom = @import("rom.zig");
    const Bus = @import("bus.zig").Bus;

    const alloc = std.testing.allocator;

    const instructions = [_]u8{ 0x11, 0x33 };
    var test_rom = rom.TestRom.init(alloc, &instructions);
    defer test_rom.deinit();

    var bus = Bus.init(&test_rom.rom, undefined, undefined);

    //data
    bus.mem_write(0x33, 0x00);
    bus.mem_write(0x34, 0x04);

    //target cell
    bus.mem_write(0x400, 0xAA);

    var cpu = CPU.init(&bus);
    cpu.register_y = 0;

    try std.testing.expect(std.mem.eql(
        u8,
        "8000  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
        &try format_trace(&cpu),
    ));
    _ = cpu.tick();
}
