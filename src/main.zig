const std = @import("std");
const _8bit_emulator = @import("8bit_emulator");

pub fn main() !void {
    for (_8bit_emulator.opcodes.OP_CODES) |opcode| {
        std.debug.print("0x{X}, ", .{opcode.code()});
    }
}
