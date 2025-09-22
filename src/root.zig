const std = @import("std");

pub const CPU = @import("cpu.zig").CPU;
pub const opcodes = @import("opcodes.zig");

test {
    std.testing.refAllDecls(@This());
}
