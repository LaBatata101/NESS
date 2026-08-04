const std = @import("std");
const builtin = @import("builtin");
const c = @import("../root.zig").c;

pub fn readFile(alloc: std.mem.Allocator, io: std.Io, uri: []const u8) ![]u8 {
    if (builtin.abi.isAndroid()) {
        const uriz = try alloc.dupeZ(u8, uri);
        defer alloc.free(uriz);
        var size: usize = 0;

        const data = c.SDL_LoadFile(uriz.ptr, &size) orelse {
            std.log.err("Failed to read file '{s}': {s}", .{ uri, c.SDL_GetError() });
            return error.FileReadFailed;
        };
        defer c.SDL_free(data);

        return try alloc.dupe(u8, @as([*]const u8, @ptrCast(data))[0..size]);
    } else {
        return std.Io.Dir.cwd().readFileAlloc(io, uri, alloc, .unlimited);
    }
}
