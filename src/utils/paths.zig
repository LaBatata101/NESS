const std = @import("std");
const builtin = @import("builtin");
const c = @import("../root.zig").c;
const android = @import("android.zig");
const sdlError = @import("sdl.zig").sdlError;
const env = @import("../env.zig");

pub const APP_NAME = "neskwik";

pub fn getConfigDir(alloc: std.mem.Allocator) ![]u8 {
    switch (builtin.os.tag) {
        .windows => {
            const local_app_data_dir = env.get("LOCALAPPDATA") orelse return error.AppConfigDirUnavailable;
            return std.fs.path.join(alloc, &.{ local_app_data_dir, APP_NAME });
        },
        .macos => {
            const home_dir = env.get("HOME") orelse return error.AppConfigDirUnavailable;
            return std.fs.path.join(alloc, &.{ home_dir, "Library", "Application Support", APP_NAME });
        },
        .linux, .serenity => {
            if (builtin.abi.isAndroid()) {
                const path = sdlError(c.SDL_GetAndroidExternalStoragePath());
                return std.fs.path.join(alloc, &.{ std.mem.span(path), "config" });
            }

            if (env.get("XDG_CONFIG_HOME")) |xdg| {
                if (xdg.len > 0) {
                    return std.fs.path.join(alloc, &.{ xdg, APP_NAME });
                }
            }

            const home_dir = env.get("HOME") orelse return error.AppConfigDirUnavailable;
            return std.fs.path.join(alloc, &.{ home_dir, ".config", APP_NAME });
        },
        else => @compileError("Unsupported OS"),
    }
}

pub fn getDataDir(alloc: std.mem.Allocator) ![]u8 {
    if (builtin.abi.isAndroid()) {
        const path = sdlError(c.SDL_GetAndroidExternalStoragePath());
        return try alloc.dupe(u8, std.mem.span(path));
    } else {
        switch (builtin.os.tag) {
            .windows => {
                const local_app_data_dir = env.get("LOCALAPPDATA") orelse return error.AppDataDirUnavailable;
                return std.fs.path.join(alloc, &.{ local_app_data_dir, APP_NAME });
            },
            .macos => {
                const home_dir = env.get("HOME") orelse return error.AppDataDirUnavailable;
                return std.fs.path.join(alloc, &.{ home_dir, "Library", "Application Support", APP_NAME });
            },
            .linux, .serenity => {
                if (env.get("XDG_DATA_HOME")) |xdg| {
                    if (xdg.len > 0) return std.fs.path.join(alloc, &.{ xdg, APP_NAME });
                }
                const home_dir = env.get("HOME") orelse return error.AppDataDirUnavailable;
                return std.fs.path.join(alloc, &.{ home_dir, ".local", "share", APP_NAME });
            },
            else => @compileError("Unsupported OS"),
        }
    }
}

pub fn getLogDir(alloc: std.mem.Allocator) ![]u8 {
    switch (builtin.os.tag) {
        .windows => {
            const local_app_data_dir = env.get("LOCALAPPDATA") orelse return error.AppLogDirUnavailable;
            return std.fs.path.join(alloc, &.{ local_app_data_dir, APP_NAME, "logs" });
        },
        .macos => {
            const home_dir = env.get("HOME") orelse return error.AppLogDirUnavailable;
            return std.fs.path.join(alloc, &.{ home_dir, "Library", "Logs", APP_NAME });
        },
        .linux, .serenity => {
            if (builtin.abi.isAndroid()) {
                const path = sdlError(c.SDL_GetAndroidExternalStoragePath());
                return std.fs.path.join(alloc, &.{ std.mem.span(path), "logs" });
            }

            if (env.get("XDG_STATE_HOME")) |xdg| {
                if (xdg.len > 0) {
                    return std.fs.path.join(alloc, &.{ xdg, APP_NAME, "logs" });
                }
            }

            const home_dir = env.get("HOME") orelse return error.AppLogDirUnavailable;
            return std.fs.path.join(alloc, &.{ home_dir, ".local", "state", APP_NAME, "logs" });
        },
        else => @compileError("Unsupported OS"),
    }
}

/// Returns the OS-appropriate shader cache directory (owned by caller).
pub fn getCacheDir(alloc: std.mem.Allocator) ![]u8 {
    switch (builtin.os.tag) {
        .windows => {
            const local_app_data_dir = env.get("LOCALAPPDATA") orelse return error.AppCacheDirUnavailable;
            return std.fs.path.join(alloc, &.{ local_app_data_dir, APP_NAME, "cache" });
        },
        .macos => {
            const home_dir = env.get("HOME") orelse return error.AppCacheDirUnavailable;
            return std.fs.path.join(alloc, &.{ home_dir, "Library", "Caches", APP_NAME });
        },
        .linux, .serenity => {
            if (builtin.abi.isAndroid()) {
                return (try android.getExternalCacheDir(alloc)).?;
            }

            if (env.get("XDG_CACHE_HOME")) |xdg| {
                return std.fs.path.join(alloc, &.{ xdg, APP_NAME });
            }

            const home_dir = env.get("HOME") orelse return error.AppCacheDirUnavailable;
            return std.fs.path.join(alloc, &.{ home_dir, ".cache", APP_NAME });
        },
        else => @compileError("Unsupported OS"),
    }
}

pub fn shaderDownloadAndroidPath(alloc: std.mem.Allocator) ![]u8 {
    const data_dir = try getDataDir(alloc);
    defer alloc.free(data_dir);
    return std.fs.path.join(alloc, &.{ data_dir, "shaders", "slang-shaders" });
}
