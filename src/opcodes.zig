const std = @import("std");

pub const AdressingMode = enum {
    /// Immediate addressing allows the programmer to directly specify an `8 bit` constant within the instruction. It is
    /// indicated by a `#` symbol followed by an numeric expression.
    Immediate,

    /// An instruction using zero page addressing mode has only an `8 bit` address operand. This limits it to addressing
    /// only the first `256 bytes` of memory (e.g. `$0000` to `$00FF`) where the most significant byte of the address is
    /// always zero. In zero page mode only the least significant byte of the address is held in the instruction making
    /// it shorter by one byte (important for space saving) and one less memory fetch during execution (important for
    /// speed).
    ZeroPage,

    /// The address to be accessed by an instruction using indexed zero page addressing is calculated by taking the
    /// `8 bit` zero page address from the instruction and adding the current value of the X register to it. For
    /// example if the `X` register contains `$0F` and the instruction `LDA $80,X` is executed then the accumulator will
    /// be loaded from `$008F` (e.g. `$80 + $0F => $8F`).
    ///
    /// NB: The address calculation wraps around if the sum of the base address and the register exceed `$FF`. If we
    /// repeat the last example but with `$FF` in the `X` register then the accumulator will be loaded from `$007F`
    /// (e.g. `$80 + $FF => $7F`) and not `$017F`.
    ZeroPageX,

    /// The address to be accessed by an instruction using indexed zero page addressing is calculated by taking the
    /// `8 bit` zero page address from the instruction and adding the current value of the `Y` register to it. This
    /// mode can only be used with the `LDX` and `STX` instructions.
    ZeroPageY,

    /// Relative addressing mode is used by branch instructions (e.g. `BEQ`, `BNE`, etc.) which contain a signed `8 bit`
    /// relative offset (e.g. `-128` to `+127`) which is added to program counter if the condition is true. As the
    /// program counter itself is incremented during instruction execution by two the effective address range for the
    /// target instruction must be with `-126` to `+129` bytes of the branch.
    Relative,

    /// Instructions using absolute addressing contain a full 16 bit address to identify the target location.
    Absolute,

    /// The address to be accessed by an instruction using `X` register indexed absolute addressing is computed by
    /// taking the `16 bit` address from the instruction and added the contents of the `X` register. For example if `X`
    /// contains `$92` then an `STA $2000,X` instruction will store the accumulator at `$2092` (e.g. `$2000 + $92`).
    AbsoluteX,

    /// The `Y` register indexed absolute addressing mode is the same as the previous mode only with the contents of
    /// the `Y` register added to the `16 bit` address from the instruction.
    AbsoluteY,

    /// `JMP` is the only `6502` instruction to support indirection. The instruction contains a `16 bit` address which
    /// identifies the location of the least significant byte of another `16 bit` memory address which is the real
    /// target of the instruction.
    ///
    /// For example if location `$0120` contains `$FC` and location `$0121` contains `$BA` then the instruction
    /// `JMP` (`$0120`) will cause the next instruction execution to occur at `$BAFC` (e.g. the contents of `$0120` and
    /// `$0121`).
    Indirect,

    /// Indexed indirect addressing is normally used in conjunction with a table of address held on zero page. The
    /// address of the table is taken from the instruction and the `X` register added to it (with zero page wrap
    /// around) to give the location of the least significant byte of the target address.
    IndirectX,

    /// Indirect indexed addressing is the most common indirection mode used on the `6502`. In instruction that contains
    /// the zero page location of the least significant byte of a `16 bit` address. The `Y` register is dynamically
    /// added to this value to generate the actual target address for operation.
    IndirectY,

    /// Some instructions don’t deal with memory locations (e.g. `INX` - increment the `X` register). These are said to
    /// have implicit addressing - the argument is implied by the instruction.
    Implicit,
};

const Instruction = struct {
    /// The opcode of the instruction.
    code: u8,
    /// The size in bytes of the instruction.
    size: u8,
    /// How many cycles this instruction takes to execute.
    cycles: u8,
    /// The addressing mode this instruction is going to be executed with.
    addressing_mode: AdressingMode,
};

const OpCode = union(enum) {
    /// *Add with Carry*:
    /// This instruction adds the contents of a memory location to the accumulator together with the carry bit. If
    /// overflow occurs the carry bit is set, this enables multiple byte addition to be performed.
    ADC: Instruction,

    /// *Subtract with Carry*:
    /// This instruction subtracts the contents of a memory location to the accumulator together with the not of the
    /// carry bit. If overflow occurs the carry bit is clear, this enables multiple byte subtraction to be performed.
    SBC: Instruction,

    /// *Load Accumulator*:
    /// Loads a byte of memory into the accumulator setting the zero and negative flags as appropriate.
    LDA: Instruction,

    /// *Load X Register*:
    /// Loads a byte of memory into the `X` register setting the zero and negative flags as appropriate.
    LDX: Instruction,

    /// *Load Y Register*:
    /// Loads a byte of memory into the `Y` register setting the zero and negative flags as appropriate.
    LDY: Instruction,

    /// *Transfer Accumulator to X*:
    /// Copies the current contents of the accumulator into the X register and sets the zero and negative flags as
    /// appropriate.
    TAX: Instruction,

    /// *Transfer Accumulator to Y*:
    /// Copies the current contents of the accumulator into the Y register and sets the zero and negative flags as
    /// appropriate.
    TAY: Instruction,

    /// *Transfer X to Accumulator*:
    /// Copies the current contents of the X register into the accumulator and sets the zero and negative flags as
    /// appropriate.
    TXA: Instruction,

    /// *Transfer X to Stack Pointer*:
    /// Copies the current contents of the X register into the stack register.
    TXS: Instruction,

    /// *Transfer Y to Accumulator*:
    /// Copies the current contents of the Y register into the accumulator and sets the zero and negative flags as
    /// appropriate.
    TYA: Instruction,

    /// *Transfer Stack Pointer to X*:
    /// Copies the current contents of the stack register into the X register and sets the zero and negative flags as
    /// appropriate.
    TSX: Instruction,

    /// *Increment X Register*:
    /// Adds one to the X register setting the zero and negative flags as appropriate.
    INX: Instruction,

    /// *Increment Memory*:
    /// Adds one to the value held at a specified memory location setting the zero and negative flags as appropriate.
    INC: Instruction,

    /// *Increment Y Register*:
    /// Adds one to the Y register setting the zero and negative flags as appropriate.
    INY: Instruction,

    /// *Store Accumulator*:
    /// Stores the contents of the accumulator into memory.
    STA: Instruction,

    /// *Store X Register*:
    /// Stores the contents of the X register into memory.
    STX: Instruction,

    /// *Store Y Register*:
    /// Stores the contents of the Y register into memory.
    STY: Instruction,

    /// *Force Interrupt*:
    /// The BRK instruction forces the generation of an interrupt request. The program counter and processor status are
    /// pushed on the stack then the IRQ interrupt vector at $FFFE/F is loaded into the PC and the break flag in the
    /// status set to one.
    BRK: Instruction,

    /// *Compare*:
    /// This instruction compares the contents of the accumulator with another memory held value and sets the zero and
    /// carry flags as appropriate.
    CMP: Instruction,

    /// *Logical AND*:
    /// A logical AND is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
    AND: Instruction,

    /// *Exclusive OR*:
    /// An exclusive OR is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
    EOR: Instruction,

    /// *Arithmetic Shift Left*:
    /// This operation shifts all the bits of the accumulator or memory contents one bit left. `Bit 0` is set to 0 and
    /// `bit 7` is placed in the carry flag. The effect of this operation is to multiply the memory contents by 2
    /// (ignoring 2's complement considerations), setting the carry if the result will not fit in 8 bits.
    ASL: Instruction,

    /// *Logical Shift Right*:
    /// Each of the bits in `A` or `M` is shift one place to the right. The bit that was in bit 0 is shifted into the
    /// carry flag. Bit 7 is set to zero.
    LSR: Instruction,

    /// *Rotate Left*:
    /// Move each of the bits in either A or M one place to the left. Bit 0 is filled with the current value of the
    /// carry flag whilst the old bit 7 becomes the new carry flag value.
    ROL: Instruction,

    /// *Rotate Right*:
    /// Move each of the bits in either A or M one place to the right. Bit 7 is filled with the current value of the
    /// carry flag whilst the old bit 0 becomes the new carry flag value.
    ROR: Instruction,

    /// *Logical Inclusive OR*:
    /// An inclusive `OR` is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
    ORA: Instruction,

    /// *Branch if Not Equal*:
    /// If the zero flag is clear then add the relative displacement to the program counter to cause a branch to a new
    /// location.
    BNE: Instruction,

    /// *Branch if Equal*:
    /// If the zero flag is set then add the relative displacement to the program counter to cause a branch to a new
    /// location.
    BEQ: Instruction,

    /// *Branch if Carry Clear*:
    /// If the carry flag is clear then add the relative displacement to the program counter to cause a branch to a new
    /// location.
    BCC: Instruction,

    /// *Branch if Carry Set*:
    /// If the carry flag is set then add the relative displacement to the program counter to cause a branch to a new
    /// location.
    BCS: Instruction,

    /// *Branch if Minus*:
    /// If the negative flag is set then add the relative displacement to the program counter to cause a branch to a
    /// new location.
    BMI: Instruction,

    /// *Branch if Positive*:
    /// If the negative flag is clear then add the relative displacement to the program counter to cause a branch to a
    /// new location.
    BPL: Instruction,

    /// *Branch if Overflow Clear*:
    /// If the overflow flag is clear then add the relative displacement to the program counter to cause a branch to a
    /// new location.
    BVC: Instruction,

    /// *Branch if Overflow Set*:
    /// If the overflow flag is set then add the relative displacement to the program counter to cause a branch to a
    /// new location.
    BVS: Instruction,

    /// *Bit Test*:
    /// This instructions is used to test if one or more bits are set in a target memory location. The mask pattern in
    /// `A` is ANDed with the value in memory to set or clear the zero flag, but the result is not kept. `Bits 7` and
    /// `6` of the value from memory are copied into the N and V flags.
    BIT: Instruction,

    /// *Clear Carry Flag*:
    /// Set the carry flag to zero.
    CLC: Instruction,

    /// *Clear Decimal Mode*:
    /// Sets the decimal mode flag to zero.
    CLD: Instruction,

    /// *Set Decimal Flag*:
    /// Set the decimal mode flag to one.
    SED: Instruction,

    /// *Clear Interrupt Disable*:
    /// Clears the interrupt disable flag allowing normal interrupt requests to be serviced.
    CLI: Instruction,

    /// *Clear Overflow Flag*:
    /// Clears the overflow flag.
    CLV: Instruction,

    /// *Set Interrupt Disable*:
    /// Set the interrupt disable flag to one.
    SEI: Instruction,

    /// *Set Carry Flag*:
    /// Set the carry flag to one.
    SEC: Instruction,

    /// *Compare X Register*:
    /// This instruction compares the contents of the X register with another memory held value and sets the zero and
    /// carry flags as appropriate.
    CPX: Instruction,

    /// *Compare Y Register*:
    /// This instruction compares the contents of the Y register with another memory held value and sets the zero and
    /// carry flags as appropriate.
    CPY: Instruction,

    /// *Decrement Memory*:
    /// Subtracts one from the value held at a specified memory location setting the zero and negative flags as
    /// appropriate.
    DEC: Instruction,

    /// *Decrement X Register*:
    /// Subtracts one from the X register setting the zero and negative flags as appropriate.
    DEX: Instruction,

    /// *Decrement Y Register*:
    /// Subtracts one from the Y register setting the zero and negative flags as appropriate.
    DEY: Instruction,

    /// *Jump*:
    /// Sets the program counter to the address specified by the operand.
    JMP: Instruction,

    /// *Jump to Subroutine*:
    /// The JSR instruction pushes the address (minus one) of the return point on to the stack and then sets the
    /// program counter to the target memory address. The return point is the address after the `JSR` parameters.
    JSR: Instruction,

    /// *Return from Subroutine*:
    /// The RTS instruction is used at the end of a subroutine to return to the calling routine. It pulls the program
    /// counter (minus one) from the stack.
    RTS: Instruction,

    /// *Return from Interrupt*:
    /// The `RTI` instruction is used at the end of an interrupt processing routine. It pulls the processor flags from
    /// the stack followed by the program counter.
    RTI: Instruction,

    /// *Push Accumulator*:
    /// Pushes a copy of the accumulator on to the stack.
    PHA: Instruction,

    /// *Pull Accumulator*:
    /// Pulls an 8 bit value from the stack and into the accumulator. The zero and negative flags are set as
    /// appropriate.
    PLA: Instruction,

    /// *Push Processor Status*:
    /// Pushes a copy of the status flags on to the stack.
    PHP: Instruction,

    /// *Pull Processor Status*:
    /// Pulls an 8 bit value from the stack and into the processor flags. The flags will take on new states as
    /// determined by the value pulled.
    PLP: Instruction,

    /// *No Operation*:
    /// The `NOP` instruction causes no changes to the processor other than the normal incrementing of the program
    /// counter to the next instruction.
    NOP: Instruction,

    pub fn code(self: @This()) u8 {
        switch (self) {
            inline else => |meta| return meta.code,
        }
    }

    pub fn addressing_mode(self: @This()) AdressingMode {
        switch (self) {
            inline else => |meta| return meta.addressing_mode,
        }
    }

    pub fn size(self: @This()) u8 {
        switch (self) {
            inline else => |meta| return meta.size,
        }
    }
};

// zig fmt: off
pub const OP_CODES = [_]OpCode{
    .{
        .ADC = .{ .code = 0x69, .size = 2, .cycles = 2, .addressing_mode = AdressingMode.Immediate }
    },
    .{
        .ADC = .{ .code = 0x65, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .ADC = .{ .code = 0x75, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .ADC = .{ .code = 0x6D, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .ADC = .{ .code = 0x7D, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteX }
    },
    .{
        .ADC = .{ .code = 0x79, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteY }
    },
    .{
        .ADC = .{ .code = 0x61, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.IndirectX }
    },
    .{
        .ADC = .{ .code = 0x71, .size = 2, .cycles = 5, // (+1 if page crossed) 
            .addressing_mode = AdressingMode.IndirectY }
    },

    .{
        .SBC = .{ .code = 0xE9, .size = 2, .cycles = 2, .addressing_mode = AdressingMode.Immediate }
    },
    .{
        .SBC = .{ .code = 0xE5, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .SBC = .{ .code = 0xF5, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .SBC = .{ .code = 0xED, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .SBC = .{ .code = 0xFD, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteX }
    },
    .{
        .SBC = .{ .code = 0xF9, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteY }
    },
    .{
        .SBC = .{ .code = 0xE1, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.IndirectX }
    },
    .{
        .SBC = .{ .code = 0xF1, .size = 2, .cycles = 5, // (+1 if page crossed) 
            .addressing_mode = AdressingMode.IndirectY }
    },

    .{
        .BCC = .{ .code = 0x90, .size = 2, .cycles = 2, // (+1 if branch succeeds +2 if to a new page) 
            .addressing_mode = AdressingMode.Relative }
    },
    .{
        .BCS = .{ .code = 0xB0, .size = 2, .cycles = 2, // (+1 if branch succeeds +2 if to a new page) 
            .addressing_mode = AdressingMode.Relative }
    },
    .{
        .BNE = .{ .code = 0xD0, .size = 2, .cycles = 2, // (+1 if branch succeeds +2 if to a new page)
        .addressing_mode = AdressingMode.Relative }
    },
    .{
        .BEQ = .{ .code = 0xF0, .size = 2, .cycles = 2, // (+1 if branch succeeds +2 if to a new page)
        .addressing_mode = AdressingMode.Relative }
    },
    .{
        .BMI = .{ .code = 0x30, .size = 2, .cycles = 2, // (+1 if branch succeeds +2 if to a new page)
        .addressing_mode = AdressingMode.Relative }
    },
    .{
        .BPL = .{ .code = 0x10, .size = 2, .cycles = 2, // (+1 if branch succeeds +2 if to a new page)
        .addressing_mode = AdressingMode.Relative }
    },
    .{
        .BVC = .{ .code = 0x50, .size = 2, .cycles = 2, // (+1 if branch succeeds +2 if to a new page)
        .addressing_mode = AdressingMode.Relative }
    },
    .{
        .BVS = .{ .code = 0x70, .size = 2, .cycles = 2, // (+1 if branch succeeds +2 if to a new page)
        .addressing_mode = AdressingMode.Relative }
    },

    .{
        .CLC = .{ .code = 0x18, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .CLD = .{ .code = 0xD8, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .CLI = .{ .code = 0x58, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .CLV = .{ .code = 0xB8, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },

    .{
        .SEI = .{ .code = 0x78, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .SEC = .{ .code = 0x38, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .SED = .{ .code = 0xF8, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },

    .{
        .CPX = .{ .code = 0xE0, .size = 2, .cycles = 2, .addressing_mode = AdressingMode.Immediate }
    },
    .{
        .CPX = .{ .code = 0xE4, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .CPX = .{ .code = 0xEC, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .CPY = .{ .code = 0xC0, .size = 2, .cycles = 2, .addressing_mode = AdressingMode.Immediate }
    },
    .{
        .CPY = .{ .code = 0xC4, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .CPY = .{ .code = 0xCC, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },

    .{
        .DEC = .{ .code = 0xC6, .size = 2, .cycles = 5, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .DEC = .{ .code = 0xD6, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .DEC = .{ .code = 0xCE, .size = 3, .cycles = 6, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .DEC = .{ .code = 0xDE, .size = 3, .cycles = 7, .addressing_mode = AdressingMode.AbsoluteX }
    },
    .{
        .DEX = .{ .code = 0xCA, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .DEY = .{ .code = 0x88, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },

    .{
        .AND = .{ .code = 0x29, .size = 2, .cycles = 2, .addressing_mode = AdressingMode.Immediate }
    },
    .{
        .AND = .{ .code = 0x25, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .AND = .{ .code = 0x35, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .AND = .{ .code = 0x2D, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .AND = .{ .code = 0x3D, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteX }
    },
    .{
        .AND = .{ .code = 0x39, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteY }
    },
    .{
        .AND = .{ .code = 0x21, .size = 2, .cycles = 6,
            .addressing_mode = AdressingMode.IndirectX }
    },
    .{
        .AND = .{ .code = 0x31, .size = 2, .cycles = 5, // (+1 if page crossed)
            .addressing_mode = AdressingMode.IndirectY }
    },
    .{
        .EOR = .{ .code = 0x49, .size = 2, .cycles = 2, .addressing_mode = AdressingMode.Immediate }
    },
    .{
        .EOR = .{ .code = 0x45, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .EOR = .{ .code = 0x55, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .EOR = .{ .code = 0x4D, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .EOR = .{ .code = 0x5D, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteX }
    },
    .{
        .EOR = .{ .code = 0x59, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteY }
    },
    .{
        .EOR = .{ .code = 0x41, .size = 2, .cycles = 6,
            .addressing_mode = AdressingMode.IndirectX }
    },
    .{
        .EOR = .{ .code = 0x51, .size = 2, .cycles = 5, // (+1 if page crossed)
            .addressing_mode = AdressingMode.IndirectY }
    },

    .{
        .ASL = .{ .code = 0x0A, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .ASL = .{ .code = 0x06, .size = 2, .cycles = 5, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .ASL = .{ .code = 0x16, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .ASL = .{ .code = 0x0E, .size = 3, .cycles = 6, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .ASL = .{ .code = 0x1E, .size = 3, .cycles = 7, .addressing_mode = AdressingMode.AbsoluteX }
    },

    .{
        .LSR = .{ .code = 0x4A, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .LSR = .{ .code = 0x46, .size = 2, .cycles = 5, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .LSR = .{ .code = 0x56, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .LSR = .{ .code = 0x4E, .size = 3, .cycles = 6, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .LSR = .{ .code = 0x5E, .size = 3, .cycles = 7, .addressing_mode = AdressingMode.AbsoluteX }
    },

    .{
        .ROL = .{ .code = 0x2A, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .ROL = .{ .code = 0x26, .size = 2, .cycles = 5, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .ROL = .{ .code = 0x36, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .ROL = .{ .code = 0x2E, .size = 3, .cycles = 6, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .ROL = .{ .code = 0x3E, .size = 3, .cycles = 7, .addressing_mode = AdressingMode.AbsoluteX }
    },

    .{
        .ROR = .{ .code = 0x6A, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .ROR = .{ .code = 0x66, .size = 2, .cycles = 5, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .ROR = .{ .code = 0x76, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .ROR = .{ .code = 0x6E, .size = 3, .cycles = 6, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .ROR = .{ .code = 0x7E, .size = 3, .cycles = 7, .addressing_mode = AdressingMode.AbsoluteX }
    },


    .{
        .ORA = .{ .code = 0x09, .size = 2, .cycles = 2, .addressing_mode = AdressingMode.Immediate }
    },
    .{
        .ORA = .{ .code = 0x05, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .ORA = .{ .code = 0x15, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .ORA = .{ .code = 0x0D, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .ORA = .{ .code = 0x1D, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteX }
    },
    .{
        .ORA = .{ .code = 0x19, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteY }
    },
    .{
        .ORA = .{ .code = 0x01, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.IndirectX }
    },
    .{
        .ORA = .{ .code = 0x11, .size = 2, .cycles = 5, // (+1 if page crossed)
            .addressing_mode = AdressingMode.IndirectY }
    },

    .{
        .BRK = .{ .code = 0x00, .size = 1, .cycles = 7, .addressing_mode = AdressingMode.Implicit }
    },

    .{
        .TAX = .{ .code = 0xAA, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .TAY = .{ .code = 0xA8, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .TSX = .{ .code = 0xBA, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .TXA = .{ .code = 0x8A, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .TXS = .{ .code = 0x9A, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .TYA = .{ .code = 0x98, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },


    .{
        .INX = .{ .code = 0xE8, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .INY = .{ .code = 0xC8, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .INC = .{ .code = 0xE6, .size = 2, .cycles = 5, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .INC = .{ .code = 0xF6, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .INC = .{ .code = 0xEE, .size = 3, .cycles = 6, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .INC = .{ .code = 0xFE, .size = 3, .cycles = 7, .addressing_mode = AdressingMode.AbsoluteX }
    },

    .{
        .LDA = .{ .code = 0xA9, .size = 2, .cycles = 2, .addressing_mode = AdressingMode.Immediate } 
    },
    .{
        .LDA = .{ .code = 0xA5, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage } 
    },
    .{
        .LDA = .{ .code = 0xB5, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageX } 
    },
    .{
        .LDA = .{ .code = 0xAD, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute } 
    },
    .{
        .LDA = .{ .code = 0xBD, .size = 3, .cycles = 4, // (+1 if page crossed)
        .addressing_mode = AdressingMode.AbsoluteX } 
    },
    .{
        .LDA = .{ .code = 0xB9, .size = 3, .cycles = 4, // (+1 if page crossed)
        .addressing_mode = AdressingMode.AbsoluteY } 
    },
    .{
        .LDA = .{ .code = 0xA1, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.IndirectX } 
    },
    .{
        .LDA = .{ .code = 0xB1, .size = 2, .cycles = 5, // (+1 if page crossed)
        .addressing_mode = AdressingMode.IndirectY } 
    },
    .{
        .LDX = .{ .code = 0xA2, .size = 2, .cycles = 2, .addressing_mode = AdressingMode.Immediate } 
    },
    .{
        .LDX = .{ .code = 0xA6, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage } 
    },
    .{
        .LDX = .{ .code = 0xB6, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageY } 
    },
    .{
        .LDX = .{ .code = 0xAE, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute } 
    },
    .{
        .LDX = .{ .code = 0xBE, .size = 3, .cycles = 4, // (+1 if page crossed)
        .addressing_mode = AdressingMode.AbsoluteY } 
    },
    .{
        .LDY = .{ .code = 0xA0, .size = 2, .cycles = 2, .addressing_mode = AdressingMode.Immediate } 
    },
    .{
        .LDY = .{ .code = 0xA4, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage } 
    },
    .{
        .LDY = .{ .code = 0xB4, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageX } 
    },
    .{
        .LDY = .{ .code = 0xAC, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute } 
    },
    .{
        .LDY = .{ .code = 0xBC, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteX } 
    },

    .{
        .STA = .{ .code = 0x85, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .STA = .{ .code = 0x95, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .STA = .{ .code = 0x8D, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .STA = .{ .code = 0x9D, .size = 3, .cycles = 5, .addressing_mode = AdressingMode.AbsoluteX }
    },
    .{
        .STA = .{ .code = 0x99, .size = 3, .cycles = 5, .addressing_mode = AdressingMode.AbsoluteY }
    },
    .{
        .STA = .{ .code = 0x81, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.IndirectX }
    },
    .{
        .STA = .{ .code = 0x91, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.IndirectY }
    },
    .{
        .STX = .{ .code = 0x86, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .STX = .{ .code = 0x96, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageY }
    },
    .{
        .STX = .{ .code = 0x8E, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .STY = .{ .code = 0x84, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .STY = .{ .code = 0x94, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .STY = .{ .code = 0x8C, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },

    .{
        .CMP = .{ .code = 0xC9, .size = 2, .cycles = 2, .addressing_mode = AdressingMode.Immediate }
    },
    .{
        .CMP = .{ .code = 0xC5, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .CMP = .{ .code = 0xD5, .size = 2, .cycles = 4, .addressing_mode = AdressingMode.ZeroPageX }
    },
    .{
        .CMP = .{ .code = 0xCD, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .CMP = .{ .code = 0xDD, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteX }
    },
    .{
        .CMP = .{ .code = 0xD9, .size = 3, .cycles = 4, // (+1 if page crossed)
            .addressing_mode = AdressingMode.AbsoluteY }
    },
    .{
        .CMP = .{ .code = 0xC1, .size = 2, .cycles = 6, .addressing_mode = AdressingMode.IndirectX }
    },
    .{
        .CMP = .{ .code = 0xD1, .size = 2, .cycles = 5, .addressing_mode = AdressingMode.IndirectY }
    },

    .{
        .JMP = .{ .code = 0x4C, .size = 3, .cycles = 3, .addressing_mode = AdressingMode.Absolute }
    },
    .{
        .JMP = .{ .code = 0x6C, .size = 3, .cycles = 5, .addressing_mode = AdressingMode.Indirect }
    },
    .{
        .JSR = .{ .code = 0x20, .size = 3, .cycles = 6, .addressing_mode = AdressingMode.Absolute }
    },

    .{
        .RTS = .{ .code = 0x60, .size = 1, .cycles = 6, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .RTI = .{ .code = 0x40, .size = 1, .cycles = 6, .addressing_mode = AdressingMode.Implicit }
    },

    .{
        .BIT = .{ .code = 0x24, .size = 2, .cycles = 3, .addressing_mode = AdressingMode.ZeroPage }
    },
    .{
        .BIT = .{ .code = 0x2C, .size = 3, .cycles = 4, .addressing_mode = AdressingMode.Absolute }
    },

    .{
        .PHA = .{ .code = 0x48, .size = 1, .cycles = 3, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .PLA = .{ .code = 0x68, .size = 1, .cycles = 4, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .PHP = .{ .code = 0x08, .size = 1, .cycles = 3, .addressing_mode = AdressingMode.Implicit }
    },
    .{
        .PLP = .{ .code = 0x28, .size = 1, .cycles = 4, .addressing_mode = AdressingMode.Implicit }
    },

    .{
        .NOP = .{ .code = 0xEA, .size = 1, .cycles = 2, .addressing_mode = AdressingMode.Implicit }
    },
};
// zig fmt: on

pub fn get_opcode(code: u8) OpCode {
    inline for (OP_CODES) |opcode| {
        if (opcode.code() == code) {
            return opcode;
        }
    }
    std.debug.panic("OPCODE 0x{X:02} not supported!", .{code});
}
