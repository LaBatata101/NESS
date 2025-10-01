const std = @import("std");

const CPU = @import("cpu.zig").CPU;
const opcodes = @import("opcodes.zig");
const OpCode = opcodes.OpCode;

pub fn trace(cpu: CPU) []const u8 {
    const allocator = std.heap.page_allocator;

    var string: std.ArrayList(u8) = .empty;
    defer string.deinit(allocator);

    var str_writer = string.writer(allocator);

    const code = cpu.mem_read(cpu.pc);
    const opcode = opcodes.get_opcode(code);

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
    str_writer.print("{X:04}  ", .{cpu.pc}) catch @panic("ERROR formating string");

    var bytes_str: std.ArrayList(u8) = .empty;
    defer bytes_str.deinit(allocator);

    var bytes_writer = bytes_str.writer(allocator);

    for (0..instruction_size) |i| {
        bytes_writer.print("{X:02} ", .{cpu.mem_read(cpu.pc + @as(u16, @intCast(i)))}) catch @panic("ERROR formating string");
    }

    if (opcode.is_unofficial()) {
        str_writer.print("{s: <9}", .{bytes_str.items}) catch @panic("ERROR formating string");
        str_writer.print("*{s}", .{mnemonic}) catch @panic("ERROR formating string");
    } else {
        str_writer.print("{s: <10}", .{bytes_str.items}) catch @panic("ERROR formating string");
        str_writer.print("{s}", .{mnemonic}) catch @panic("ERROR formating string");
    }

    const arg_addr = cpu.pc + 1;
    switch (opcode.addressing_mode()) {
        .Immediate => {
            str_writer.print(" #${X:02}", .{cpu.mem_read(arg_addr)}) catch @panic("ERROR formating string");
        },
        .Relative => {
            const offset = @as(i8, @bitCast(cpu.mem_read(arg_addr)));

            str_writer.print(
                " ${X:04}",
                .{arg_addr +% 1 +% @as(u16, @bitCast(@as(i16, offset)))},
            ) catch @panic("ERROR formating string");
        },
        .Absolute => {
            const ptr_addr = cpu.mem_read_u16(arg_addr);
            switch (opcode) {
                .JMP, .JSR => str_writer.print(" ${X:04}", .{ptr_addr}) catch @panic("ERROR formating string"),
                else => str_writer.print(" ${X:04} = {X:02}", .{
                    ptr_addr,
                    cpu.mem_read(ptr_addr),
                }) catch @panic("ERROR formating string"),
            }
        },
        .AbsoluteX => {
            const base = cpu.mem_read_u16(arg_addr);
            const ptr_addr = base +% cpu.register_x;

            str_writer.print(
                " ${X:04},X @ {X:04} = {X:02}",
                .{ base, ptr_addr, cpu.mem_read(ptr_addr) },
            ) catch @panic("ERROR formating string");
        },
        .AbsoluteY => {
            const base = cpu.mem_read_u16(arg_addr);
            const ptr_addr = base +% cpu.register_y;

            str_writer.print(
                " ${X:04},Y @ {X:04} = {X:02}",
                .{ base, ptr_addr, cpu.mem_read(ptr_addr) },
            ) catch @panic("ERROR formating string");
        },
        .ZeroPage => {
            const ptr_addr = cpu.mem_read(arg_addr);
            str_writer.print(
                " ${X:02} = {X:02}",
                .{ ptr_addr, cpu.mem_read(ptr_addr) },
            ) catch @panic("ERROR formating string");
        },
        .ZeroPageX => {
            const base = cpu.mem_read(arg_addr);
            const ptr_addr = base +% cpu.register_x;
            str_writer.print(
                " ${X:02},X @ {X:02} = {X:02}",
                .{ base, ptr_addr, cpu.mem_read(ptr_addr) },
            ) catch @panic("ERROR formating string");
        },
        .ZeroPageY => {
            const base = cpu.mem_read(arg_addr);
            const ptr_addr = base +% cpu.register_y;
            str_writer.print(
                " ${X:02},Y @ {X:02} = {X:02}",
                .{ base, ptr_addr, cpu.mem_read(ptr_addr) },
            ) catch @panic("ERROR formating string");
        },
        .Indirect => {
            const ptr_addr = cpu.mem_read_u16(arg_addr);
            const jmp_addr = blk: {
                if (ptr_addr & 0xFF == 0xFF) {
                    const lo = cpu.mem_read(ptr_addr);
                    const hi = cpu.mem_read(ptr_addr & 0xFF00);
                    break :blk @as(u16, hi) << 8 | lo;
                } else {
                    break :blk cpu.mem_read_u16(ptr_addr);
                }
            };
            str_writer.print(
                " (${X:04}) = {X:04}",
                .{ ptr_addr, jmp_addr },
            ) catch @panic("ERROR formating string");
        },
        .IndirectX => {
            const base = cpu.mem_read(arg_addr);
            const ptr = base +% cpu.register_x;
            const lo = cpu.mem_read(ptr);
            const hi = cpu.mem_read(ptr +% 1);
            const ptr_addr = @as(u16, hi) << 8 | lo;
            str_writer.print(
                " (${X:02},X) @ {X:02} = {X:04} = {X:02}",
                .{ base, ptr, ptr_addr, cpu.mem_read(ptr_addr) },
            ) catch @panic("ERROR formating string");
        },
        .IndirectY => {
            const base = cpu.mem_read(arg_addr);
            const lo = cpu.mem_read(base);
            const hi = cpu.mem_read(base +% 1);
            const base_ptr_addr = @as(u16, hi) << 8 | lo;
            const ptr_addr = base_ptr_addr +% cpu.register_y;
            str_writer.print(
                " (${X:02}),Y = {X:04} @ {X:04} = {X:02}",
                .{ base, base_ptr_addr, ptr_addr, cpu.mem_read(ptr_addr) },
            ) catch @panic("ERROR formating string");
        },
        .Implicit => {
            switch (opcode) {
                .ASL, .LSR, .ROL, .ROR => str_writer.print(" A", .{}) catch @panic("ERROR formating string"),
                else => {},
            }
        },
    }

    var final_str: std.ArrayList(u8) = .empty;
    defer final_str.deinit(allocator);

    var final_str_writer = final_str.writer(allocator);
    final_str_writer.print(
        "{s:<47} A:{X:02} X:{X:02} Y:{X:02} P:{X:02} SP:{X:02}",
        .{ string.items, cpu.register_a, cpu.register_x, cpu.register_y, @as(u8, @bitCast(cpu.status)), cpu.sp },
    ) catch @panic("ERROR formating string");

    return final_str.toOwnedSlice(allocator) catch @panic("ERROR formating string");
}

test "format trace" {
    const rom = @import("rom.zig");
    const Bus = @import("bus.zig").Bus;

    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        &[_]u8{},
    );
    defer allocator.free(test_rom);

    var bus = Bus.init(try rom.Rom.load(test_rom));
    bus.mem_write(100, 0xA2);
    bus.mem_write(101, 0x01);
    bus.mem_write(102, 0xCA);
    bus.mem_write(103, 0x88);
    bus.mem_write(104, 0x00);

    var cpu = CPU.init(bus);
    cpu.pc = 100;
    cpu.register_a = 1;
    cpu.register_x = 2;
    cpu.register_y = 3;

    try std.testing.expect(std.mem.eql(
        u8,
        "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
        trace(cpu),
    ));
    _ = cpu.tick();
    try std.testing.expect(std.mem.eql(
        u8,
        "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
        trace(cpu),
    ));
    _ = cpu.tick();
    try std.testing.expect(std.mem.eql(
        u8,
        "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
        trace(cpu),
    ));
    _ = cpu.tick();
    try std.testing.expect(std.mem.eql(
        u8,
        "0068  00        BRK                             A:01 X:00 Y:02 P:24 SP:FD",
        trace(cpu),
    ));
}

test "format mem access" {
    const rom = @import("rom.zig");
    const Bus = @import("bus.zig").Bus;

    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        &[_]u8{},
    );
    defer allocator.free(test_rom);

    var bus = Bus.init(try rom.Rom.load(test_rom));
    bus.mem_write(100, 0x11);
    bus.mem_write(101, 0x33);

    //data
    bus.mem_write(0x33, 0x00);
    bus.mem_write(0x34, 0x04);

    //target cell
    bus.mem_write(0x400, 0xAA);

    var cpu = CPU.init(bus);
    cpu.pc = 100;
    cpu.register_y = 0;

    try std.testing.expect(std.mem.eql(
        u8,
        "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
        trace(cpu),
    ));
    _ = cpu.tick();
    try std.testing.expect(std.mem.eql(
        u8,
        "0066  00        BRK                             A:AA X:00 Y:00 P:A4 SP:FD",
        trace(cpu),
    ));
}
