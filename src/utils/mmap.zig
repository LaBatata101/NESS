const std = @import("std");
const builtin = @import("builtin");

const HANDLE = std.os.windows.HANDLE;
const DWORD = std.os.windows.DWORD;
const BOOL = std.os.windows.BOOL;
const LPCWSTR = std.os.windows.LPCWSTR;
const LPSECURITY_ATTRIBUTES = ?*anyopaque;

const PAGE_READWRITE = 0x04;
pub extern "kernel32" fn CreateFileMappingW(
    hFile: HANDLE,
    lpFileMappingAttributes: LPSECURITY_ATTRIBUTES,
    flProtect: DWORD,
    dwMaximumSizeHigh: DWORD,
    dwMaximumSizeLow: DWORD,
    lpName: ?LPCWSTR,
) callconv(.winapi) ?HANDLE;

const FILE_MAP_READ = 0x04;
const FILE_MAP_WRITE = 0x02;
pub extern "kernel32" fn MapViewOfFile(
    hFileMappingObject: ?HANDLE,
    dwDesiredAccess: DWORD,
    dwFileOffsetHigh: DWORD,
    dwFileOffsetLow: DWORD,
    dwNumberOfBytesToMap: std.os.windows.SIZE_T,
) callconv(.winapi) ?std.os.windows.LPVOID;

pub extern "kernel32" fn UnmapViewOfFile(lpBaseAddress: std.os.windows.LPCVOID) callconv(.winapi) BOOL;

pub fn mmap(file: *const std.Io.File, size: usize) ![]u8 {
    return if (builtin.target.os.tag == .windows) blk: {
        const map_handle = CreateFileMappingW(
            file.handle,
            null,
            PAGE_READWRITE,
            if (@sizeOf(usize) == 8) @as(u32, @truncate(size >> 32)) else 0,
            @as(u32, @truncate(size)),
            null,
        );
        if (map_handle == null) return error.FailedToCreateFileMapping;
        defer std.os.windows.CloseHandle(map_handle.?);

        const ptr = MapViewOfFile(map_handle, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, size);
        if (ptr == null) return error.FailedToMapViewOfFile;

        break :blk @as([*]u8, @ptrCast(@alignCast(ptr.?)))[0..size];
    } else try std.posix.mmap(
        null,
        size,
        .{ .READ = true, .WRITE = true },
        .{ .TYPE = .SHARED },
        file.handle,
        0,
    );
}

pub fn munmap(buffer: []u8) void {
    if (builtin.target.os.tag == .windows) {
        _ = UnmapViewOfFile(@ptrCast(buffer));
    } else {
        std.posix.munmap(@alignCast(buffer));
    }
}
