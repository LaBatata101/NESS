const std = @import("std");

const rom = @import("rom.zig");

const Bus = @import("bus.zig").Bus;
const Rom = rom.Rom;
const opcodes = @import("opcodes.zig");
const AdressingMode = opcodes.AdressingMode;

const ProcessorStatus = packed struct(u8) {
    /// The carry flag is set if the last operation caused an overflow from bit 7 of the result or an underflow
    /// from bit 0. This condition is set during arithmetic, comparison and during logical shifts. It can be explicitly
    /// set using the 'Set Carry Flag' (`SEC`) instruction and cleared with 'Clear Carry Flag' (`CLC`).
    carry_flag: bool = false,

    /// The zero flag is set if the result of the last operation as was zero.
    zero_flag: bool = false,

    /// The interrupt disable flag is set if the program has executed a 'Set Interrupt Disable' (`SEI`) instruction.
    /// While this flag is set the processor will not respond to interrupts from devices until it is cleared by a
    /// 'Clear Interrupt Disable' (`CLI`) instruction.
    interrupt_disable: bool = false,

    /// While the decimal mode flag is set the processor will obey the rules of Binary Coded Decimal (`BCD`) arithmetic
    /// during addition and subtraction. The flag can be explicitly set using 'Set Decimal Flag' (`SED`) and cleared
    /// with 'Clear Decimal Flag' (`CLD`).
    decimal_mode: bool = false,

    /// The break command bit is set when a BRK instruction has been executed and an interrupt has been generated
    /// to process it.
    break_command: bool = false,
    break2: bool = false,

    /// The overflow flag is set during arithmetic operations if the result has yielded an invalid 2's complement
    /// result (e.g. adding to positive numbers and ending up with a negative result: 64 + 64 => -128). It is
    /// determined by looking at the carry between bits 6 and 7 and between bit 7 and the carry flag.
    overflow_flag: bool = false,

    /// The negative flag is set if the result of the last operation had bit 7 set to a one.
    negative_flag: bool = false,
};

pub const CPU = struct {
    /// *Program Counter*: points to the next instruction to be executed. The value of program counter is modified
    /// automatically as instructions are executed.
    ///
    /// The value of the program counter can be modified by executing a jump, a relative branch or a subroutine call to
    /// another memory address or by returning from a subroutine or interrupt.
    pc: u16,
    /// *Stack Pointer*: The processor supports a 256 byte stack located between `$0100` and `$01FF`. The stack pointer
    /// is an 8 bit register and holds the low 8 bits of the next free location on the stack. The location of the stack
    /// is fixed and cannot be moved.
    ///
    /// Pushing bytes to the stack causes the stack pointer to be decremented. Conversely pulling bytes causes it to be
    /// incremented.
    /// The CPU does not detect if the stack is overflowed by excessive pushing or pulling operations and will most
    /// likely result in the program crashing.
    sp: u8,
    /// *Accumulator*: used for all arithmetic and logical operations (with the exception of increments and decrements).
    /// The contents of the accumulator can be stored and retrieved either from memory or the stack.
    register_a: u8,
    /// *Index Register X*: most commonly used to hold counters or offsets for accessing memory. The value of the X
    /// register can be loaded and saved in memory, compared with values held in memory or incremented and decremented.
    ///
    /// The X register has one special function. It can be used to get a copy of the stack pointer or change its value.
    register_x: u8,
    /// *Index Register Y*: The Y register is similar to the X register in that it is available for holding counter or
    /// offsets memory access and supports the same set of memory load, save and compare operations as wells as
    /// increments and decrements. It has no special functions.
    register_y: u8,
    /// *Processor Status*: As instructions are executed a set of processor flags are set or clear to record the
    /// results of the operation. Each flag has a single bit within the register.
    status: ProcessorStatus,

    bus: Bus,

    /// The size in bytes of the program being currently executed by the CPU.
    program_len: usize,
    program_start_addr: u16,

    const Self = @This();

    const STACK_START: u16 = 0x0100;
    const STACK_END: u16 = 0x01FF;

    pub fn init(bus: Bus) CPU {
        var initial_status = ProcessorStatus{};
        initial_status.interrupt_disable = true;
        initial_status.break2 = true;

        return .{
            .sp = 0xFF,
            .pc = 0x8000,
            .register_a = 0,
            .register_x = 0,
            .register_y = 0,
            .status = initial_status,
            .program_len = bus.rom.prg_rom.len,
            .program_start_addr = 0x8000,
            // .program_start_addr = 0x0000,
            .bus = bus,
        };
    }

    fn update_zero_and_negative_flags(self: *Self, result: u8) void {
        self.status.zero_flag = result == 0;
        // Check if bit 7 is set
        self.status.negative_flag = result & 0b1000_0000 != 0;
    }

    pub fn mem_read(self: Self, addr: u16) u8 {
        return self.bus.mem_read(addr);
    }

    pub fn mem_write(self: *Self, addr: u16, data: u8) void {
        self.bus.mem_write(addr, data);
    }

    fn mem_read_u16(self: *Self, addr: u16) u16 {
        return self.bus.mem_read_u16(addr);
    }

    fn mem_write_u16(self: *Self, addr: u16, data: u16) void {
        self.bus.mem_write_u16(addr, data);
    }

    pub fn load(self: *Self, program: []const u8) void {
        self.program_len = program.len;
        // @memcpy(self.bus.ram[self.program_start_addr..(self.program_start_addr + program.len)], program);
        for (0..program.len) |i| {
            self.mem_write(self.program_start_addr + @as(u16, @intCast(i)), program[i]);
        }
        self.mem_write_u16(0xFFFC, self.program_start_addr);
    }

    pub fn reset(self: *Self) void {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.sp = 0xFF;

        self.status = .{};
        self.status.interrupt_disable = true;
        self.status.break2 = true;

        self.pc = self.mem_read_u16(0xFFFC);
    }

    pub fn load_and_run(self: *Self, program: []const u8) void {
        self.load(program);
        self.reset();
        self.run();
    }

    fn stack_push(self: *Self, data: u8) void {
        var addr = STACK_START + self.sp;
        self.mem_write(addr, data);
        addr -%= 1;

        // TODO: how to handle stack overflow properly??
        if (addr < STACK_START) {
            std.log.warn("stack overflow while pushing!\n", .{});
        }
        self.sp = @truncate(addr);
    }

    fn stack_pop(self: *Self) u8 {
        self.sp +%= 1;
        const addr = STACK_START + self.sp;
        const data = self.mem_read(addr);

        // TODO: how to handle stack overflow properly??
        if (addr > STACK_END) {
            std.log.warn("stack overflow while poping!\n", .{});
        }

        return data;
    }

    fn stack_push_u16(self: *Self, data: u16) void {
        const hi: u8 = @truncate(data >> 8);
        const lo: u8 = @truncate(data);
        // std.debug.print("data: 0x{X} | hi: 0x{X} | lo: 0x{X}\n\n", .{ data, hi, lo });
        self.stack_push(hi);
        self.stack_push(lo);
    }

    fn stack_pop_u16(self: *Self) u16 {
        const lo = self.stack_pop();
        const hi = self.stack_pop();
        return @as(u16, hi) << 8 | lo;
    }

    fn operand_address(self: *Self, mode: AdressingMode) u16 {
        return switch (mode) {
            AdressingMode.Immediate => self.pc,
            AdressingMode.ZeroPage => self.mem_read(self.pc),
            AdressingMode.ZeroPageX => self.mem_read(self.pc) +% self.register_x,
            AdressingMode.ZeroPageY => self.mem_read(self.pc) +% self.register_y,
            AdressingMode.Absolute => self.mem_read_u16(self.pc),
            AdressingMode.AbsoluteX => self.mem_read_u16(self.pc) +% self.register_x,
            AdressingMode.AbsoluteY => self.mem_read_u16(self.pc) +% self.register_y,
            AdressingMode.Relative => {
                const offset = @as(i8, @bitCast(self.mem_read(self.pc)));
                return self.pc +% 1 +% @as(u16, @bitCast(@as(i16, offset)));
            },
            AdressingMode.Indirect => {
                const ptr_addr = self.mem_read_u16(self.pc);

                // NOTE - 6502 bug mode with with page boundary:
                // if address $3000 contains $40, $30FF contains $80, and $3100 contains $50,
                // the result of JMP ($30FF) will be a transfer of control to $4080 rather than $5080 as you intended
                // i.e. the 6502 took the low byte of the address from $30FF and the high byte from $3000
                if (ptr_addr & 0xFF == 0xFF) {
                    const lo = self.mem_read(ptr_addr);
                    const hi = self.mem_read(ptr_addr & 0xFF00);

                    return @as(u16, hi) << 8 | @as(u16, lo);
                } else {
                    return self.mem_read_u16(ptr_addr);
                }
            },
            AdressingMode.IndirectX => {
                const base = self.mem_read(self.pc);
                const ptr = base +% self.register_x;
                const lo = self.mem_read(ptr);
                const hi = self.mem_read(ptr +% 1);

                return @as(u16, hi) << 8 | @as(u16, lo);
            },
            AdressingMode.IndirectY => {
                const base = self.mem_read(self.pc);
                const lo = self.mem_read(base);
                const hi = self.mem_read(base +% 1);
                const deref_base = @as(u16, hi) << 8 | @as(u16, lo);
                const deref = deref_base +% self.register_y;

                return deref;
            },
            AdressingMode.Implicit => unreachable,
        };
    }

    fn branch(self: *Self, mode: AdressingMode, condition: bool) void {
        if (condition) {
            const jump_addr = self.operand_address(mode);
            self.pc = jump_addr;
        }
    }

    fn compare(self: *Self, mode: AdressingMode, register: u8) void {
        const addr = self.operand_address(mode);
        const data = self.mem_read(addr);
        const result = register -% data;

        self.status.carry_flag = register >= data;
        self.update_zero_and_negative_flags(result);
    }

    fn adc(self: *Self, value: u8) void {
        const sum: u16 = @as(u16, self.register_a) + @as(u16, value) + @as(u16, @intFromBool(self.status.carry_flag));
        self.status.carry_flag = (sum > 0xFF);

        const result: u8 = @truncate(sum);

        self.status.overflow_flag = (value ^ result) & (result ^ self.register_a) & 0x80 != 0;

        self.register_a = result;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn run(self: *Self) void {
        const T = struct {
            fn callback(_: *Self) void {
                // do nothing
            }
        };
        self.run_with(T);
    }

    pub fn run_with(self: *Self, callback: anytype) void {
        while (true) {
            const code = self.mem_read(self.pc);
            const opcode = opcodes.get_opcode(code);
            self.pc += 1;
            const pc_state = self.pc;
            // std.debug.print("PC: 0x{X} code: 0x{X} {any}\n", .{ self.pc, code, opcode });

            switch (opcode) {
                .ADC => {
                    // NOTE: ignoring decimal mode
                    const addr = self.operand_address(opcode.addressing_mode());
                    const data = self.mem_read(addr);
                    self.adc(data);
                },
                .SBC => {
                    // NOTE: ignoring decimal mode
                    const addr = self.operand_address(opcode.addressing_mode());
                    const data = self.mem_read(addr);

                    self.adc(@as(u8, @bitCast(-%@as(i8, @bitCast(data)) -% 1)));
                },
                .AND => {
                    const addr = self.operand_address(opcode.addressing_mode());
                    const data = self.mem_read(addr);

                    self.register_a &= data;
                    self.update_zero_and_negative_flags(self.register_a);
                },
                .EOR => {
                    const data = self.mem_read(self.operand_address(opcode.addressing_mode()));
                    self.register_a ^= data;
                    self.update_zero_and_negative_flags(self.register_a);
                },
                .ASL => switch (opcode.addressing_mode()) {
                    .Implicit => {
                        const result = @mulWithOverflow(self.register_a, 2);

                        self.register_a = result[0];
                        self.status.carry_flag = result[1] == 1;
                        self.update_zero_and_negative_flags(self.register_a);
                    },
                    else => {
                        const addr = self.operand_address(opcode.addressing_mode());
                        const value = self.mem_read(addr);

                        const result = @mulWithOverflow(value, 2);

                        self.mem_write(addr, result[0]);
                        self.status.carry_flag = result[1] == 1;
                        self.update_zero_and_negative_flags(value);
                    },
                },
                .LSR => switch (opcode.addressing_mode()) {
                    .Implicit => {
                        self.status.carry_flag = self.register_a & 1 == 1;
                        self.register_a >>= 1;
                        self.update_zero_and_negative_flags(self.register_a);
                    },
                    else => {
                        const addr = self.operand_address(opcode.addressing_mode());
                        var value = self.mem_read(addr);
                        self.status.carry_flag = value & 1 == 1;
                        value >>= 1;
                        self.mem_write(addr, value);
                        self.update_zero_and_negative_flags(value);
                    },
                },
                .ROL => switch (opcode.addressing_mode()) {
                    .Implicit => {
                        const is_bit7_set = self.register_a & (1 << 7) != 0;

                        self.register_a <<= 1;
                        // set bit 0 to the value of carry flag
                        self.register_a = self.register_a | @intFromBool(self.status.carry_flag);
                        self.status.carry_flag = is_bit7_set;

                        self.update_zero_and_negative_flags(self.register_a);
                    },
                    else => {
                        const addr = self.operand_address(opcode.addressing_mode());
                        var value = self.mem_read(addr);
                        const is_bit7_set = value & (1 << 7) != 0;

                        value <<= 1;
                        // set bit 0 to the value of carry flag
                        value = value | @intFromBool(self.status.carry_flag);
                        self.mem_write(addr, value);
                        self.status.carry_flag = is_bit7_set;

                        self.update_zero_and_negative_flags(value);
                    },
                },
                .ROR => switch (opcode.addressing_mode()) {
                    .Implicit => {
                        const is_bit0_set = self.register_a & 1 != 0;

                        self.register_a >>= 1;
                        // set bit 7 to the value of carry flag
                        self.register_a = (self.register_a & ~@as(u8, 0x80)) | @as(u8, @intFromBool(self.status.carry_flag)) << 7;
                        self.status.carry_flag = is_bit0_set;

                        self.update_zero_and_negative_flags(self.register_a);
                    },
                    else => {
                        const addr = self.operand_address(opcode.addressing_mode());
                        var value = self.mem_read(addr);
                        const is_bit0_set = value & 1 != 0;

                        value >>= 1;
                        // set bit 7 to the value of carry flag
                        value = (value & ~@as(u8, 0x80)) | @as(u8, @intFromBool(self.status.carry_flag)) << 7;
                        self.mem_write(addr, value);
                        self.status.carry_flag = is_bit0_set;

                        self.update_zero_and_negative_flags(value);
                    },
                },
                .ORA => {
                    const value = self.mem_read(self.operand_address(opcode.addressing_mode()));
                    self.register_a |= value;
                    self.update_zero_and_negative_flags(self.register_a);
                },
                .LDA => {
                    const addr = self.operand_address(opcode.addressing_mode());
                    const value = self.mem_read(addr);
                    self.register_a = value;
                    self.update_zero_and_negative_flags(self.register_a);
                },
                .LDX => {
                    const addr = self.operand_address(opcode.addressing_mode());
                    const value = self.mem_read(addr);
                    self.register_x = value;
                    self.update_zero_and_negative_flags(self.register_x);
                },
                .LDY => {
                    const addr = self.operand_address(opcode.addressing_mode());
                    const value = self.mem_read(addr);
                    self.register_y = value;
                    self.update_zero_and_negative_flags(self.register_y);
                },
                .TAX => {
                    self.register_x = self.register_a;
                    self.update_zero_and_negative_flags(self.register_x);
                },
                .TAY => {
                    self.register_y = self.register_a;
                    self.update_zero_and_negative_flags(self.register_y);
                },
                .TXA => {
                    self.register_a = self.register_x;
                    self.update_zero_and_negative_flags(self.register_a);
                },
                .TYA => {
                    self.register_a = self.register_y;
                    self.update_zero_and_negative_flags(self.register_a);
                },
                .TSX => {
                    self.register_x = self.sp;
                    self.update_zero_and_negative_flags(self.register_x);
                },
                .TXS => {
                    self.sp = self.register_x;
                },
                .INX => {
                    self.register_x +%= 1;
                    self.update_zero_and_negative_flags(self.register_x);
                },
                .INY => {
                    self.register_y +%= 1;
                    self.update_zero_and_negative_flags(self.register_y);
                },
                .INC => {
                    const addr = self.operand_address(opcode.addressing_mode());
                    const value = self.mem_read(addr);
                    const result = value +% 1;

                    self.mem_write(addr, result);
                    self.update_zero_and_negative_flags(result);
                },
                .STA => {
                    const addr = self.operand_address(opcode.addressing_mode());
                    self.mem_write(addr, self.register_a);
                },
                .STX => {
                    const addr = self.operand_address(opcode.addressing_mode());
                    self.mem_write(addr, self.register_x);
                },
                .STY => {
                    const addr = self.operand_address(opcode.addressing_mode());
                    self.mem_write(addr, self.register_y);
                },
                .CMP => {
                    self.compare(opcode.addressing_mode(), self.register_a);
                },
                .CPX => {
                    self.compare(opcode.addressing_mode(), self.register_x);
                },
                .CPY => {
                    self.compare(opcode.addressing_mode(), self.register_y);
                },
                .BNE => {
                    self.branch(opcode.addressing_mode(), !self.status.zero_flag);
                },
                .BEQ => {
                    self.branch(opcode.addressing_mode(), self.status.zero_flag);
                },
                .BCC => {
                    self.branch(opcode.addressing_mode(), !self.status.carry_flag);
                },
                .BCS => {
                    self.branch(opcode.addressing_mode(), self.status.carry_flag);
                },
                .BPL => {
                    self.branch(opcode.addressing_mode(), !self.status.negative_flag);
                },
                .BMI => {
                    self.branch(opcode.addressing_mode(), self.status.negative_flag);
                },
                .BVC => {
                    self.branch(opcode.addressing_mode(), !self.status.overflow_flag);
                },
                .BVS => {
                    self.branch(opcode.addressing_mode(), self.status.overflow_flag);
                },
                .BIT => {
                    const data = self.mem_read(self.operand_address(opcode.addressing_mode()));

                    self.status.zero_flag = self.register_a & data == 0;
                    self.status.overflow_flag = data & 0b0100_0000 != 0;
                    self.status.negative_flag = data & 0b1000_0000 != 0;
                },
                .CLC => {
                    self.status.carry_flag = false;
                },
                .CLD => {
                    self.status.decimal_mode = false;
                },
                .SED => {
                    self.status.decimal_mode = true;
                },
                .CLI => {
                    self.status.interrupt_disable = false;
                },
                .CLV => {
                    self.status.overflow_flag = false;
                },
                .SEI => {
                    self.status.interrupt_disable = true;
                },
                .SEC => {
                    self.status.carry_flag = true;
                },
                .DEC => {
                    const addr = self.operand_address(opcode.addressing_mode());
                    const data = self.mem_read(addr);
                    const result = data -% 1;

                    self.mem_write(addr, result);
                    self.update_zero_and_negative_flags(result);
                },
                .JMP => {
                    const jmp_addr = self.operand_address(opcode.addressing_mode());
                    self.pc = jmp_addr;
                },
                .JSR => {
                    self.stack_push_u16(self.pc + 2 - 1);
                    const jmp_addr = self.operand_address(opcode.addressing_mode());
                    self.pc = jmp_addr;
                },
                .RTS => {
                    self.pc = self.stack_pop_u16() + 1;
                },
                .RTI => {
                    const s = self.stack_pop();
                    // std.debug.print("P: {any} | PC: 0x{X} | S: {X}\n", .{ self.status, self.pc, s });
                    self.status = @bitCast(s);
                    self.status.break_command = false;
                    self.status.break2 = true;

                    self.pc = self.stack_pop_u16();
                },
                .DEX => {
                    self.register_x -%= 1;
                    self.update_zero_and_negative_flags(self.register_x);
                },
                .DEY => {
                    self.register_y -%= 1;
                    self.update_zero_and_negative_flags(self.register_y);
                },
                .PHA => {
                    self.stack_push(self.register_a);
                },
                .PLA => {
                    self.register_a = self.stack_pop();
                    self.update_zero_and_negative_flags(self.register_a);
                },
                .PHP => {
                    var copy_status = self.status;
                    copy_status.break_command = true;
                    copy_status.break2 = true;
                    self.stack_push(@bitCast(copy_status));
                },
                .PLP => {
                    self.status = @bitCast(self.stack_pop());
                    self.status.break_command = false;
                    self.status.break2 = true;
                },
                .NOP => {},
                .BRK => {
                    // TODO: properly implement this
                    self.status.break_command = true;
                    // self.stack_push_u16(self.pc);
                    // self.stack_push(@bitCast(self.status));
                    // self.pc = self.mem_read_u16(0xFFFE);
                    break;
                },
            }

            if (self.pc == pc_state) {
                self.pc += opcode.size() - 1;
            }

            callback.callback(self);
        }
    }
};

// test "0x00: BRK Force Interrupt" {
//     const allocator = std.testing.allocator;
//     const test_rom = try rom.TestRom.testRom(
//         allocator,
//         //      SEI   SEC   BRK   LDA
//         &[_]u8{ 0x78, 0x38, 0x00, 0xA9, 0x01 },
//     );
//     const bus = Bus.init(try Rom.load(test_rom));
//     defer allocator.free(test_rom);
//
//     var cpu = CPU.init(bus);
//     cpu.mem_write_u16(0xFFFE, 0x1234);
//     cpu.run();
//
//     try std.testing.expect(cpu.status.break_command);
//     try std.testing.expectEqual(0x1234, cpu.pc);
//
//     const status: ProcessorStatus = @bitCast(cpu.stack_pop());
//
//     try std.testing.expect(status.interrupt_disable);
//     try std.testing.expect(status.carry_flag);
//     try std.testing.expectEqual(0x8003, cpu.stack_pop_u16());
// }

test "0xA9: LDA immediate load data" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         BRK
        &[_]u8{ 0xA9, 0x05, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x05, cpu.register_a);
    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);
}

test "0xA9: LDA zero flag" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         BRK
        &[_]u8{ 0xA9, 0x00, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x00, cpu.register_a);
    try std.testing.expect(cpu.status.zero_flag);
}

test "0xAA: TAX copies register A contents to X" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA        TAX   BRK
        &[_]u8{ 0xA9, 0xA, 0xAA, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(10, cpu.register_x);
    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);
}

test "0xA8: TAY Transfer Accumulator to Y" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         TAY   BRK
        &[_]u8{ 0xA9, 0x69, 0xA8, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x69, cpu.register_y);
    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);
}

test "0xBA: TSX Transfer Stack Pointer to X" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //     TSX   BRK
        &[_]u8{ 0xBA, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.sp = 0x05;
    cpu.run();

    try std.testing.expectEqual(0x05, cpu.register_x);
}

test "0x8A: TXA Transfer X to Accumulator" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDX         TXA   BRK
        &[_]u8{ 0xA2, 0x69, 0x8A, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x69, cpu.register_x);
    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDX         TXA   BRK
        &[_]u8{ 0xA2, 0x00, 0x8A, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expectEqual(0x00, cpu2.register_x);
    try std.testing.expect(cpu2.status.zero_flag);
    try std.testing.expect(!cpu2.status.negative_flag);
}

test "0x98: TYA Transfer Y to Accumulator" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDY         TYA   BRK
        &[_]u8{ 0xA0, 0x69, 0x98, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x69, cpu.register_a);
    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);
}

test "0x9A: TXS Transfer X to Stack Pointer" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDX         TXS   BRK
        &[_]u8{ 0xA2, 0x69, 0x9A, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x69, cpu.sp);
}

test "4 ops (LDA, TAX, INX, BRK) working together" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA        LDA         TAX   INX   BRK
        &[_]u8{ 0xA9, 0xA, 0xA9, 0xC0, 0xAA, 0xE8, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0xC0, cpu.register_a);
    try std.testing.expectEqual(0xC1, cpu.register_x);
    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(cpu.status.negative_flag);
}

test "INX overflow" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         TAX   INX   INX   BRK
        &[_]u8{ 0xA9, 0xFF, 0xAA, 0xE8, 0xE8, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(1, cpu.register_x);
}

test "0xA5: LDA from memory" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         BRK
        &[_]u8{ 0xA5, 0x10, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.mem_write(0x10, 0x55);
    cpu.run();

    try std.testing.expectEqual(0x55, cpu.register_a);
}

test "0xC9: CMP equal values" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         CMP         BRK
        &[_]u8{ 0xA9, 0x01, 0xC9, 0x01, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.mem_write(0x10, 0x55);
    cpu.run();

    try std.testing.expect(cpu.status.zero_flag);
}

test "0xC9: CMP different values" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         CMP         BRK
        &[_]u8{ 0xA9, 0x01, 0xC9, 0x03, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.mem_write(0x10, 0x55);
    cpu.run();

    try std.testing.expect(!cpu.status.zero_flag);
}

test "0x69: ADC add with carry - no overflow" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         TAX         ADC         BRK
        &[_]u8{ 0xa9, 0xc0, 0xaa, 0xe8, 0x69, 0xc4, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x84, cpu.register_a);
    try std.testing.expectEqual(0xc1, cpu.register_x);
    try std.testing.expect(!cpu.status.overflow_flag);
    try std.testing.expect(cpu.status.carry_flag);
}

test "0x65: ADC add with carry - overflow" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         ADC         BRK
        &[_]u8{ 0xa9, 0x80, 0x85, 0x01, 0x65, 0x01, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0, cpu.register_a);
    try std.testing.expectEqual(0, cpu.register_x);
    try std.testing.expect(cpu.status.overflow_flag);
    try std.testing.expect(cpu.status.carry_flag);
}

test "0xE9: SBC Subtract with Carry - no overflow" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         SBC   BRK
        &[_]u8{ 0xa9, 0xF0, 0xE9, 0x50, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x9F, cpu.register_a);
    try std.testing.expect(cpu.status.carry_flag);
    try std.testing.expect(cpu.status.negative_flag);
    try std.testing.expect(!cpu.status.overflow_flag);
    try std.testing.expect(!cpu.status.zero_flag);
}

test "0xE9: SBC Subtract with Carry - overflow" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         SBC   BRK
        &[_]u8{ 0xa9, 0xD0, 0xE9, 0x70, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x5F, cpu.register_a);
    try std.testing.expect(cpu.status.carry_flag);
    try std.testing.expect(cpu.status.overflow_flag);
    try std.testing.expect(!cpu.status.negative_flag);
    try std.testing.expect(!cpu.status.zero_flag);
}

test "0x29: logical AND - true result" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         AND        BRK
        &[_]u8{ 0xA9, 0x01, 0x29, 0x01, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x01, cpu.register_a);
}

test "0x29: logical AND - false result" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         AND         BRK
        &[_]u8{ 0xA9, 0x01, 0x29, 0x00, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0, cpu.register_a);
    try std.testing.expect(cpu.status.zero_flag);
}

test "0x0A: ASL Arithmetic Shift Left - carry flag set" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         ASL   BRK
        &[_]u8{ 0xA9, 0x80, 0x0A, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0, cpu.register_a);
    try std.testing.expect(cpu.status.carry_flag);
}

test "0x0A: ASL Arithmetic Shift Left - carry flag not set" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         ASL   BRK
        &[_]u8{ 0xA9, 0x01, 0x0A, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(2, cpu.register_a);
    try std.testing.expect(!cpu.status.carry_flag);
}

test "0x06: ASL Arithmetic Shift Left - read value from memory" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         ASL   BRK
        &[_]u8{ 0xA9, 0x02, 0x85, 0xFF, 0x06, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(4, cpu.mem_read(0xFF));
    try std.testing.expect(!cpu.status.carry_flag);
}

test "0x4A: LSR Logical Shift Right - Accumulator" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         LSR   BRK
        &[_]u8{ 0xA9, 0x02, 0x4A, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(1, cpu.register_a);
    try std.testing.expect(!cpu.status.carry_flag);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDA         LSR   BRK
        &[_]u8{ 0xA9, 0x03, 0x4A, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expectEqual(1, cpu2.register_a);
    try std.testing.expect(cpu2.status.carry_flag);
}

test "0x46: LSR Logical Shift Right - read value from memory" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LSR   BRK
        &[_]u8{ 0xA9, 0x02, 0x85, 0xFF, 0x46, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(1, cpu.mem_read(0xFF));
    try std.testing.expect(!cpu.status.carry_flag);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LSR   BRK
        &[_]u8{ 0xA9, 0x03, 0x85, 0xFF, 0x46, 0xFF, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expectEqual(1, cpu2.mem_read(0xFF));
    try std.testing.expect(cpu2.status.carry_flag);
}

test "0x09: ORA Logical Inclusive OR" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         ORA   BRK
        &[_]u8{ 0xA9, 0x02, 0x09, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0xFF, cpu.register_a);
}

test "0x2A: ROL Rotate Left" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         SEC   ROL   BRK
        &[_]u8{ 0xA9, 0x01, 0x38, 0x2A, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(3, cpu.register_a);
    try std.testing.expect(!cpu.status.carry_flag);
    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDA         ROL   BRK
        &[_]u8{ 0xA9, 0x80, 0x2A, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expectEqual(0, cpu2.register_a);
    try std.testing.expect(cpu2.status.carry_flag);
    try std.testing.expect(cpu2.status.zero_flag);
    try std.testing.expect(!cpu2.status.negative_flag);

    const test_rom3 = try rom.TestRom.testRom(
        allocator,
        //      LDA         ROL   BRK
        &[_]u8{ 0xA9, 0x40, 0x2A, 0x00 },
    );
    const bus3 = Bus.init(try Rom.load(test_rom3));
    defer allocator.free(test_rom3);

    var cpu3 = CPU.init(bus3);
    cpu3.run();

    try std.testing.expectEqual(0x80, cpu3.register_a);
    try std.testing.expect(!cpu3.status.carry_flag);
    try std.testing.expect(!cpu3.status.zero_flag);
    try std.testing.expect(cpu3.status.negative_flag);
}

test "0x26: ROL Rotate Left - read value from memory" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         SEC   ROL   BRK
        &[_]u8{ 0xA9, 0x01, 0x85, 0xFF, 0x38, 0x26, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(3, cpu.mem_read(0xFF));
    try std.testing.expect(!cpu.status.carry_flag);
    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         ROL   BRK
        &[_]u8{ 0xA9, 0x80, 0x85, 0xFF, 0x26, 0xFF, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expectEqual(0, cpu2.mem_read(0xFF));
    try std.testing.expect(cpu2.status.carry_flag);
    try std.testing.expect(cpu2.status.zero_flag);
    try std.testing.expect(!cpu2.status.negative_flag);
}

test "0x6A: ROR Rotate Right" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         SEC   ROR   BRK
        &[_]u8{ 0xA9, 0x01, 0x38, 0x6A, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x80, cpu.register_a);
    try std.testing.expect(cpu.status.carry_flag);
    try std.testing.expect(cpu.status.negative_flag);
    try std.testing.expect(!cpu.status.zero_flag);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDA         ROR   BRK
        &[_]u8{ 0xA9, 0x1, 0x6A, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expectEqual(0, cpu2.register_a);
    try std.testing.expect(cpu2.status.carry_flag);
    try std.testing.expect(cpu2.status.zero_flag);
    try std.testing.expect(!cpu2.status.negative_flag);

    const test_rom3 = try rom.TestRom.testRom(
        allocator,
        //      LDA         ROR   BRK
        &[_]u8{ 0xA9, 0x80, 0x6A, 0x00 },
    );
    const bus3 = Bus.init(try Rom.load(test_rom3));
    defer allocator.free(test_rom3);

    var cpu3 = CPU.init(bus3);
    cpu3.run();

    try std.testing.expectEqual(0x40, cpu3.register_a);
    try std.testing.expect(!cpu3.status.carry_flag);
    try std.testing.expect(!cpu3.status.zero_flag);
    try std.testing.expect(!cpu3.status.negative_flag);
}

test "0x66: ROR Rotate Right - Read Value from Memory" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         SEC   ROR   BRK
        &[_]u8{ 0xA9, 0x01, 0x85, 0xFF, 0x38, 0x66, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x80, cpu.mem_read(0xFF));
    try std.testing.expect(cpu.status.carry_flag);
    try std.testing.expect(cpu.status.negative_flag);
    try std.testing.expect(!cpu.status.zero_flag);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDA        STA         ROR   BRK
        &[_]u8{ 0xA9, 0x1, 0x85, 0xFF, 0x66, 0xFF, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expectEqual(0, cpu2.mem_read(0xFF));
    try std.testing.expect(cpu2.status.carry_flag);
    try std.testing.expect(cpu2.status.zero_flag);
    try std.testing.expect(!cpu2.status.negative_flag);

    const test_rom3 = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         ROR   BRK
        &[_]u8{ 0xA9, 0x80, 0x85, 0xFF, 0x66, 0xFF, 0x00 },
    );
    const bus3 = Bus.init(try Rom.load(test_rom3));
    defer allocator.free(test_rom3);

    var cpu3 = CPU.init(bus3);
    cpu3.run();

    try std.testing.expectEqual(0x40, cpu3.mem_read(0xFF));
    try std.testing.expect(!cpu3.status.carry_flag);
    try std.testing.expect(!cpu3.status.zero_flag);
    try std.testing.expect(!cpu3.status.negative_flag);
}

test "0xD0: BNE jump if not equal - skip INX instruction" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         CMP         BNE         INX   BRK
        &[_]u8{ 0xA9, 0x01, 0xC9, 0x03, 0xD0, 0x01, 0xE8, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.mem_write(0x10, 0x55);
    cpu.run();

    try std.testing.expectEqual(0, cpu.register_x);
}

test "0xD0: BNE jump if not equal - execute INX instruction" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         CMP         BNE         INX   BRK
        &[_]u8{ 0xA9, 0x01, 0xC9, 0x01, 0xD0, 0x01, 0xE8, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.mem_write(0x10, 0x55);
    cpu.run();

    try std.testing.expectEqual(1, cpu.register_x);
}

test "0x90: BCC Branch if Carry Clear - branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         ASL   BCC         LDA         BRK
        &[_]u8{ 0xA9, 0x01, 0x0A, 0x90, 0x02, 0xA9, 0x03, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 2 that means we did branch, otherwise we would have the value 3.
    try std.testing.expectEqual(2, cpu.register_a);
}

test "0x90: BCC Branch if Carry Clear - no branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         ASL   BCC         LDA         BRK
        &[_]u8{ 0xA9, 0x80, 0x0A, 0x90, 0x02, 0xA9, 0x03, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 3 that means we didn't branch, otherwise we would have the value 0.
    try std.testing.expectEqual(3, cpu.register_a);
}

test "0xB0: BCS Branch if Carry Set - branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         ASL   BCS         LDA         BRK
        &[_]u8{ 0xA9, 0x80, 0x0A, 0xB0, 0x02, 0xA9, 0x03, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 0 that means we did branch, otherwise we would have the value 3.
    try std.testing.expectEqual(0, cpu.register_a);
}

test "0xB0: BCS Branch if Carry Set - no branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         ASL   BCS         LDA         BRK
        &[_]u8{ 0xA9, 0x01, 0x0A, 0xB0, 0x02, 0xA9, 0x03, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 3 that means we did branch, otherwise we would have the value 1.
    try std.testing.expectEqual(3, cpu.register_a);
}

test "0xF0: BEQ Branch if Equal - branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         CMP         BEQ         LDA         BRK
        &[_]u8{ 0xA9, 0x01, 0xC9, 0x01, 0xF0, 0x02, 0xA9, 0x03, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 1 that means we did branch, otherwise we would have the value 3.
    try std.testing.expectEqual(1, cpu.register_a);
}

test "0xF0: BEQ Branch if Equal - no branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         CMP         BEQ         LDA         BRK
        &[_]u8{ 0xA9, 0x01, 0xC9, 0x02, 0xF0, 0x02, 0xA9, 0x03, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 3 that means we did branch, otherwise we would have the value 1.
    try std.testing.expectEqual(3, cpu.register_a);
}

test "0x30: BMI Branch if Minus - branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LDA         BIT         BMI         LDA         BRK
        &[_]u8{ 0xA9, 0x81, 0x85, 0xFF, 0xA9, 0x01, 0x24, 0xFF, 0x30, 0x02, 0xA9, 0x04, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 1 that means we did branch, otherwise we would have the value 4.
    try std.testing.expectEqual(1, cpu.register_a);
}

test "0x30: BMI Branch if Minus - no branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LDA         BIT         BMI         LDA         BRK
        &[_]u8{ 0xA9, 0x01, 0x85, 0xFF, 0xA9, 0x01, 0x24, 0xFF, 0x30, 0x02, 0xA9, 0x04, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 4 that means we didn't branch, otherwise we would have the value 1.
    try std.testing.expectEqual(4, cpu.register_a);
}

test "0x10: BPL Branch if Positive - no branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LDA         BIT         BPL         LDA         BRK
        &[_]u8{ 0xA9, 0x81, 0x85, 0xFF, 0xA9, 0x01, 0x24, 0xFF, 0x10, 0x02, 0xA9, 0x04, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 4 that means we didn't branch, otherwise we would have the value 1.
    try std.testing.expectEqual(4, cpu.register_a);
}

test "0x10: BPL Branch if Positive - branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LDA         BIT         BPL         LDA         BRK
        &[_]u8{ 0xA9, 0x01, 0x85, 0xFF, 0xA9, 0x01, 0x24, 0xFF, 0x10, 0x02, 0xA9, 0x04, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 1 that means we did branch, otherwise we would have the value 4.
    try std.testing.expectEqual(1, cpu.register_a);
}

test "0x50: BVC Branch if Overflow Clear - no branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LDA         BIT         BVC         LDA         BRK
        &[_]u8{ 0xA9, 0x40, 0x85, 0xFF, 0xA9, 0x01, 0x24, 0xFF, 0x50, 0x02, 0xA9, 0x04, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 4 that means we didn't branch, otherwise we would have the value 1.
    try std.testing.expectEqual(4, cpu.register_a);
}

test "0x50: BVC Branch if Overflow Clear  - branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LDA         BIT         BVC         LDA         BRK
        &[_]u8{ 0xA9, 0x01, 0x85, 0xFF, 0xA9, 0x01, 0x24, 0xFF, 0x50, 0x02, 0xA9, 0x04, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 1 that means we did branch, otherwise we would have the value 4.
    try std.testing.expectEqual(1, cpu.register_a);
}

test "0x70: BVS Branch if Overflow Set - branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LDA         BIT         BVS         LDA         BRK
        &[_]u8{ 0xA9, 0x40, 0x85, 0xFF, 0xA9, 0x01, 0x24, 0xFF, 0x70, 0x02, 0xA9, 0x04, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 1 that means we did branch, otherwise we would have the value 4.
    try std.testing.expectEqual(1, cpu.register_a);
}

test "0x70: BVS Branch if Overflow Set - no branch" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LDA         BIT         BVS         LDA         BRK
        &[_]u8{ 0xA9, 0x01, 0x85, 0xFF, 0xA9, 0x01, 0x24, 0xFF, 0x70, 0x02, 0xA9, 0x04, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    // if the register A contains the value 4 that means we didn't branch, otherwise we would have the value 1.
    try std.testing.expectEqual(4, cpu.register_a);
}

test "0x24: BIT test - set N,V,Z flags" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LDA         BIT         BRK
        &[_]u8{ 0xA9, 0xC0, 0x85, 0xFF, 0xA9, 0x02, 0x24, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(2, cpu.register_a);
    try std.testing.expect(cpu.status.negative_flag);
    try std.testing.expect(cpu.status.overflow_flag);
    try std.testing.expect(cpu.status.zero_flag);
}

test "0x24: BIT test - set N,V flags" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LDA         BIT         BRK
        &[_]u8{ 0xA9, 0xC2, 0x85, 0xFF, 0xA9, 0x02, 0x24, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(2, cpu.register_a);
    try std.testing.expect(cpu.status.negative_flag);
    try std.testing.expect(cpu.status.overflow_flag);
    try std.testing.expect(!cpu.status.zero_flag);
}

test "0x24: BIT test - set N flags" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         LDA         BIT         BRK
        &[_]u8{ 0xA9, 0x81, 0x85, 0xFF, 0xA9, 0x01, 0x24, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(1, cpu.register_a);
    try std.testing.expect(cpu.status.negative_flag);
    try std.testing.expect(!cpu.status.overflow_flag);
    try std.testing.expect(!cpu.status.zero_flag);
}

test "0x18: CLC Clear Carry Flag" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         ASL   CLC   BRK
        &[_]u8{ 0xA9, 0x80, 0x0A, 0x18, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(!cpu.status.carry_flag);
}

test "0xF8: SED Set Decimal Flag" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      SED   BRK
        &[_]u8{ 0xF8, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(cpu.status.decimal_mode);
}

test "0xD8: CLD Clear Decimal Flag" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      SED   CLD   BRK
        &[_]u8{ 0xF8, 0xD8, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(!cpu.status.decimal_mode);
}

test "0x78: SEI Set Interrupt Disable" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //     SEI   BRK
        &[_]u8{ 0x78, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(cpu.status.interrupt_disable);
}

test "0x38: SEC Set Carry Flag" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //     SEC   BRK
        &[_]u8{ 0x38, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(cpu.status.carry_flag);
}

test "0x58: CLI Clear Interrupt Disable" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      SEI   CLI   BRK
        &[_]u8{ 0x78, 0x58, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(!cpu.status.interrupt_disable);
}

test "0xB8: CLV Clear Overflow Flag" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         BIT         CLV   BRK
        &[_]u8{ 0xA9, 0xC2, 0x85, 0xFF, 0x24, 0xFF, 0xB8, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(!cpu.status.overflow_flag);
}

test "0xA2: LDX Load X Register" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDX         BRK
        &[_]u8{ 0xA2, 0x01, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(1, cpu.register_x);
}

test "0xA0: LDY Load Y Register" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDY         BRK
        &[_]u8{ 0xA0, 0x01, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(1, cpu.register_y);
}

test "0xE0: CPX Compare X Register" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDX         CPX         BRK
        &[_]u8{ 0xA2, 0x01, 0xE0, 0x01, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(cpu.status.zero_flag);
    try std.testing.expect(cpu.status.carry_flag);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDX         CPX         BRK
        &[_]u8{ 0xA2, 0x01, 0xE0, 0x02, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expect(cpu2.status.negative_flag);
}

test "0xC0: CPY Compare Y Register" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDY         CPY         BRK
        &[_]u8{ 0xA0, 0x01, 0xC0, 0x01, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(cpu.status.zero_flag);
    try std.testing.expect(cpu.status.carry_flag);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDY         CPY         BRK
        &[_]u8{ 0xA0, 0x01, 0xC0, 0x02, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expect(cpu2.status.negative_flag);
}

test "0xC6: DEC Decrement Memory" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         DEC         BRK
        &[_]u8{ 0xA9, 0x01, 0x85, 0xFF, 0xC6, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);
    try std.testing.expectEqual(0, cpu.mem_read(0xff));

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         DEC         BRK
        &[_]u8{ 0xA9, 0x02, 0x85, 0xFF, 0xC6, 0xFF, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expect(!cpu2.status.zero_flag);
    try std.testing.expect(!cpu2.status.negative_flag);
    try std.testing.expectEqual(1, cpu2.mem_read(0xff));

    const test_rom3 = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         DEC         BRK
        &[_]u8{ 0xA9, 0x81, 0x85, 0xFF, 0xC6, 0xFF, 0x00 },
    );
    const bus3 = Bus.init(try Rom.load(test_rom3));
    defer allocator.free(test_rom3);

    var cpu3 = CPU.init(bus3);
    cpu3.run();

    try std.testing.expect(!cpu3.status.zero_flag);
    try std.testing.expect(cpu3.status.negative_flag);
    try std.testing.expectEqual(0x80, cpu3.mem_read(0xff));
}

test "0xCA: DEX Decrement X Register" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDX         DEX   BRK
        &[_]u8{ 0xA2, 0x02, 0xCA, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);
    try std.testing.expectEqual(1, cpu.register_x);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDX         DEX   BRK
        &[_]u8{ 0xA2, 0x01, 0xCA, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expect(cpu2.status.zero_flag);
    try std.testing.expect(!cpu2.status.negative_flag);
    try std.testing.expectEqual(0, cpu2.register_x);

    const test_rom3 = try rom.TestRom.testRom(
        allocator,
        //      LDX         DEX   BRK
        &[_]u8{ 0xA2, 0x81, 0xCA, 0x00 },
    );
    const bus3 = Bus.init(try Rom.load(test_rom3));
    defer allocator.free(test_rom3);

    var cpu3 = CPU.init(bus3);
    cpu3.run();

    try std.testing.expect(!cpu3.status.zero_flag);
    try std.testing.expect(cpu3.status.negative_flag);
    try std.testing.expectEqual(0x80, cpu3.register_x);

    const test_rom4 = try rom.TestRom.testRom(
        allocator,
        //      LDX         DEX   BRK
        &[_]u8{ 0xA2, 0x00, 0xCA, 0x00 },
    );
    const bus4 = Bus.init(try Rom.load(test_rom4));
    defer allocator.free(test_rom4);

    var cpu4 = CPU.init(bus4);
    cpu4.run();

    try std.testing.expect(!cpu4.status.zero_flag);
    try std.testing.expect(cpu4.status.negative_flag);
    try std.testing.expectEqual(0xFF, cpu4.register_x);
}

test "0x88: DEY Decrement Y Register" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDY         DEY   BRK
        &[_]u8{ 0xA0, 0x02, 0x88, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);
    try std.testing.expectEqual(1, cpu.register_y);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDY         DEY   BRK
        &[_]u8{ 0xA0, 0x01, 0x88, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expect(cpu2.status.zero_flag);
    try std.testing.expect(!cpu2.status.negative_flag);
    try std.testing.expectEqual(0, cpu2.register_y);

    const test_rom3 = try rom.TestRom.testRom(
        allocator,
        //      LDY         DEY   BRK
        &[_]u8{ 0xA0, 0x81, 0x88, 0x00 },
    );
    const bus3 = Bus.init(try Rom.load(test_rom3));
    defer allocator.free(test_rom3);

    var cpu3 = CPU.init(bus3);
    cpu3.run();

    try std.testing.expect(!cpu3.status.zero_flag);
    try std.testing.expect(cpu3.status.negative_flag);
    try std.testing.expectEqual(0x80, cpu3.register_y);
}

test "0x49: EOR Exclusive OR" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         EOR         BRK
        &[_]u8{ 0xA9, 0x01, 0x49, 0x01, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);

    const test_rom2 = try rom.TestRom.testRom(
        allocator,
        //      LDA         EOR         BRK
        &[_]u8{ 0xA9, 0x01, 0x49, 0x80, 0x00 },
    );
    const bus2 = Bus.init(try Rom.load(test_rom2));
    defer allocator.free(test_rom2);

    var cpu2 = CPU.init(bus2);
    cpu2.run();

    try std.testing.expect(!cpu2.status.zero_flag);
    try std.testing.expect(cpu2.status.negative_flag);
}

test "0xE8: INX increments X register by 1" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         TAX   INX   BRK
        &[_]u8{ 0xA9, 0x01, 0xAA, 0xE8, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(2, cpu.register_x);
    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);
}

test "0xE6: INC Increment Memory" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA         INC         LDA         BRK
        &[_]u8{ 0xA9, 0x01, 0x85, 0x10, 0xE6, 0x10, 0xA5, 0x10, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(2, cpu.register_a);
    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);
}

test "0xC8: INY Increment Y Register" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDY         INY   BRK
        &[_]u8{ 0xA0, 0x01, 0xC8, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(2, cpu.register_y);
    try std.testing.expect(!cpu.status.zero_flag);
    try std.testing.expect(!cpu.status.negative_flag);
}

test "0x6C: JMP Jump to address" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         JMP               LDA         BRK
        &[_]u8{ 0xA9, 0x42, 0x6C, 0x11, 0x11, 0xA9, 0x69, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.mem_write_u16(0x1111, 0x8007);
    cpu.run();

    try std.testing.expectEqual(0x8008, cpu.pc);
    // if the register A contains the value 0x42 that means we jumped, otherwise we would have the value 0x69.
    try std.testing.expectEqual(0x42, cpu.register_a);
}

test "0x48: PHA Push Accumulator to Stack" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         PHA   LDA         PHA   LDA         PHA   BRK
        &[_]u8{ 0xA9, 0x01, 0x48, 0xA9, 0x02, 0x48, 0xA9, 0x03, 0x48, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x03, cpu.stack_pop());
    try std.testing.expectEqual(0x02, cpu.stack_pop());
    try std.testing.expectEqual(0x01, cpu.stack_pop());
}

test "0x68: PLA Pop Accumulator from Stack" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         PHA   LDA         PLA   BRK
        &[_]u8{ 0xA9, 0x01, 0x48, 0xA9, 0x02, 0x68, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x01, cpu.register_a);
}

test "0x20: JSR Jump to Subroutine" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      JSR               LDX         LDX         BRK   LDX   BRK
        &[_]u8{ 0x20, 0x05, 0x80, 0xA2, 0x01, 0xA2, 0x02, 0x00, 0xA2, 0x03, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(2, cpu.register_x);
    try std.testing.expectEqual(0x8008, cpu.pc);
}

test "0x60: RTS Return from Subroutine" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      JSR               JSR               JSR               LDX         RTS   INX   RTS   BRK
        &[_]u8{ 0x20, 0x09, 0x80, 0x20, 0x0C, 0x80, 0x20, 0x0E, 0x80, 0xA2, 0x00, 0x60, 0xE8, 0x60, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(1, cpu.register_x);
    try std.testing.expectEqual(0x800F, cpu.pc);
}

// FIX
// test "0x40: RTI Return from Interrupt" {
//     const allocator = std.testing.allocator;
//     const test_rom = try rom.TestRom.testRom(
//         allocator,
//         //     RTI   BRK
//         &[_]u8{0x40, 0x00},
//     );
//     const bus = Bus.init(try Rom.load(test_rom));
//     defer allocator.free(test_rom);
//
//     var cpu = CPU.init(bus);
//     cpu.status.carry_flag = true;
//     cpu.status.negative_flag = true;
//
//     cpu.stack_push_u16(cpu.pc);
//     std.debug.print("AFTER FIRST PUSH: {any}\n", .{cpu.memory[0x0100..0x01FF]});
//     cpu.stack_push(@bitCast(cpu.status));
//     std.debug.print("AFTER SECOND PUSH: {any}\n", .{cpu.memory[0x0100..0x01FF]});
//
//     cpu.run();
//
//     try std.testing.expect(cpu.status.carry_flag);
//     try std.testing.expect(cpu.status.negative_flag);
//     try std.testing.expectEqual(PROGRAM_START_ADDR, cpu.pc);
// }

test "0x08: PHP Push Processor Status" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      SED   SEI   PHP   BRK
        &[_]u8{ 0xF8, 0x78, 0x08, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    const status: ProcessorStatus = @bitCast(cpu.stack_pop());
    try std.testing.expect(status.decimal_mode);
    try std.testing.expect(status.interrupt_disable);
}

test "0x28: PLP Pull Processor Status" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      SED   SEI   PHP   PLP   BRK
        &[_]u8{ 0xF8, 0x78, 0x08, 0x28, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expect(cpu.status.decimal_mode);
    try std.testing.expect(cpu.status.interrupt_disable);
}

test "0x85: STA Store Accumulator" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDA         STA   BRK
        &[_]u8{ 0xA9, 0x69, 0x85, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x69, cpu.mem_read(0x00FF));
}

test "0x86: STX Store X Register" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDX         STX   BRK
        &[_]u8{ 0xA2, 0x69, 0x86, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x69, cpu.mem_read(0x00FF));
}

test "0x84: STY Store Y Register" {
    const allocator = std.testing.allocator;
    const test_rom = try rom.TestRom.testRom(
        allocator,
        //      LDY         STY         BRK
        &[_]u8{ 0xA0, 0x69, 0x84, 0xFF, 0x00 },
    );
    const bus = Bus.init(try Rom.load(test_rom));
    defer allocator.free(test_rom);

    var cpu = CPU.init(bus);
    cpu.run();

    try std.testing.expectEqual(0x69, cpu.mem_read(0x00FF));
}
