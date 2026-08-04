const std = @import("std");
const builtin = @import("builtin");

var variables: ?*const std.process.Environ.Map = null;

pub fn init(env: *const std.process.Environ.Map) void {
    variables = env;
}

pub fn get(key: []const u8) ?[]const u8 {
    const environ = variables orelse return null;
    return environ.get(key);
}
