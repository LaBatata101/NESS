const std = @import("std");
const builtin = @import("builtin");
const mmap = @import("../root.zig").mmap.mmap;
const munmap = @import("../root.zig").mmap.munmap;
const paths = @import("../utils/paths.zig");
const android = if (builtin.abi.isAndroid()) @import("../utils/android.zig") else struct {};

pub const Ram = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    const Self = @This();
    pub const VTable = struct {
        deinit: *const fn (ptr: *anyopaque) void,
        read: *const fn (ptr: *const anyopaque, addr: u16) u8,
        write: *const fn (ptr: *const anyopaque, addr: u16, value: u8) void,
        save_state: *const fn (ptr: *const anyopaque, alloc: std.mem.Allocator) anyerror![]u8,
        load_state: *const fn (ptr: *anyopaque, data: []const u8) anyerror!void,
    };

    pub fn deinit(self: *Self) void {
        self.vtable.deinit(self.ptr);
    }

    pub fn write(self: *Self, addr: u16, value: u8) void {
        self.vtable.write(self.ptr, addr, value);
    }

    pub fn read(self: *const Self, addr: u16) u8 {
        return self.vtable.read(self.ptr, addr);
    }

    pub fn saveState(self: *const Self, alloc: std.mem.Allocator) ![]u8 {
        return self.vtable.save_state(self.ptr, alloc);
    }

    pub fn loadState(self: *Self, data: []const u8) !void {
        return self.vtable.load_state(self.ptr, data);
    }
};

pub const VolatileRam = struct {
    alloc: std.mem.Allocator,
    buffer: []u8,

    const Self = @This();
    const VTable = Ram.VTable{
        .deinit = @ptrCast(&Self.deinit),
        .read = @ptrCast(&Self.read),
        .write = @ptrCast(&Self.write),
        .save_state = @ptrCast(&Self.saveState),
        .load_state = @ptrCast(&Self.loadState),
    };

    pub fn init(alloc: std.mem.Allocator, size: usize) !*Self {
        const self = try alloc.create(Self);
        const buffer = try alloc.alloc(u8, size);
        @memset(buffer, 0);

        self.* = .{
            .alloc = alloc,
            .buffer = buffer,
        };
        return self;
    }

    fn deinit(self: *Self) void {
        self.alloc.free(self.buffer);
        self.alloc.destroy(self);
    }

    fn write(self: *Self, addr: u16, value: u8) void {
        self.buffer[addr % self.buffer.len] = value;
    }

    fn read(self: *const Self, addr: u16) u8 {
        return self.buffer[addr % self.buffer.len];
    }

    fn saveState(self: *const Self, alloc: std.mem.Allocator) ![]u8 {
        return alloc.dupe(u8, self.buffer);
    }

    fn loadState(self: *Self, data: []const u8) !void {
        if (data.len != self.buffer.len) return error.InvalidSnapshot;
        @memcpy(self.buffer, data);
    }

    pub fn as_ram(self: *Self) Ram {
        return .{ .ptr = self, .vtable = &VTable };
    }
};

pub const BatteryBackedRam = struct {
    alloc: std.mem.Allocator,
    file: std.fs.File,
    buffer: []u8,
    save_file_path: []u8,

    const Self = @This();
    const VTable = Ram.VTable{
        .deinit = @ptrCast(&Self.deinit),
        .read = @ptrCast(&Self.read),
        .write = @ptrCast(&Self.write),
        .save_state = @ptrCast(&Self.saveState),
        .load_state = @ptrCast(&Self.loadState),
    };

    pub fn init(alloc: std.mem.Allocator, path: []const u8, size: usize) !*Self {
        const self = try alloc.create(Self);

        const save_path = try savePath(alloc, path);
        const file = try std.fs.createFileAbsolute(save_path, .{
            .read = true,
            .truncate = false,
        });

        const stat = try file.stat();
        if (stat.size < size) {
            try file.setEndPos(size);
        }

        const buffer = try mmap(&file, size);
        errdefer munmap(buffer);

        self.* = .{
            .alloc = alloc,
            .file = file,
            .buffer = buffer,
            .save_file_path = save_path,
        };
        return self;
    }

    fn deinit(self: *Self) void {
        munmap(self.buffer);
        self.file.close();
        self.alloc.free(self.save_file_path);
        self.alloc.destroy(self);
    }

    fn write(self: *Self, addr: u16, value: u8) void {
        self.buffer[addr % self.buffer.len] = value;
    }

    fn read(self: *const Self, addr: u16) u8 {
        return self.buffer[addr % self.buffer.len];
    }

    fn saveState(self: *const Self, alloc: std.mem.Allocator) ![]u8 {
        return alloc.dupe(u8, self.buffer);
    }

    fn loadState(self: *Self, data: []const u8) !void {
        if (data.len != self.buffer.len) return error.InvalidSnapshot;
        @memcpy(self.buffer, data);
    }

    pub fn as_ram(self: *Self) Ram {
        return .{ .ptr = self, .vtable = &VTable };
    }

    fn savePath(alloc: std.mem.Allocator, rom_path: []const u8) ![]u8 {
        if (builtin.abi.isAndroid()) {
            const data_dir_path = try paths.getDataDir(alloc);
            defer alloc.free(data_dir_path);

            std.fs.makeDirAbsolute(data_dir_path) catch |err| switch (err) {
                error.PathAlreadyExists => {},
                else => return err,
            };

            var data_dir = try std.fs.openDirAbsolute(data_dir_path, .{});
            defer data_dir.close();

            // Ensure <data-dir>/neskwik/save-files exists.
            data_dir.makeDir("save-files") catch |err| switch (err) {
                error.PathAlreadyExists => {},
                else => return err,
            };

            const rom_name = if (builtin.abi.isAndroid())
                (android.displayName(alloc, rom_path) catch @panic("JNI error")).?
            else
                std.fs.path.stem(rom_path);
            defer if (builtin.abi.isAndroid()) alloc.free(rom_name);

            const dir = try std.fs.path.join(alloc, &.{ data_dir_path, "save-files" });
            defer alloc.free(dir);

            return std.fmt.allocPrint(
                alloc,
                "{s}" ++ std.fs.path.sep_str ++ "{s}.sav",
                .{ dir, rom_name },
            );
        }

        const dir = std.fs.path.dirname(rom_path) orelse return error.InvalidRomPath;
        const base = std.fs.path.stem(rom_path);
        return std.fmt.allocPrint(alloc, "{s}" ++ std.fs.path.sep_str ++ "{s}.sav", .{ dir, base });
    }
};
