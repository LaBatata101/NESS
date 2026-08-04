const std = @import("std");
const sdlError = @import("utils/sdl.zig").sdlError;
const c = @import("root.zig").c;
const paths = @import("utils/paths.zig");

pub const THUMBNAIL_WIDTH: u32 = 256;
pub const THUMBNAIL_HEIGHT: u32 = 224;
pub const THUMBNAIL_BYTES: usize = THUMBNAIL_WIDTH * THUMBNAIL_HEIGHT * 4;
const THUMBNAIL_PITCH: c_int = THUMBNAIL_WIDTH * 4;

pub const GameEntry = struct {
    alloc: std.mem.Allocator,
    name: []u8,
    rom_path: []u8,
    play_time_secs: u64,
    last_played: i64,
    thumbnail: ?[]u8, // THUMBNAIL_BYTES bytes of ABGR8888, or null

    pub fn deinit(self: *GameEntry) void {
        self.alloc.free(self.name);
        self.alloc.free(self.rom_path);
        if (self.thumbnail) |t| self.alloc.free(t);
    }
};

pub const GameHistory = struct {
    alloc: std.mem.Allocator,
    io: std.Io,
    entries: std.ArrayList(GameEntry),
    data_dir: ?[]u8, // null when the OS data dir is unavailable

    const HISTORY_SUBDIR = "history";

    pub fn init(alloc: std.mem.Allocator, io: std.Io) GameHistory {
        const data_dir = paths.getDataDir(alloc) catch null;
        return .{
            .alloc = alloc,
            .io = io,
            .entries = .empty,
            .data_dir = data_dir,
        };
    }

    pub fn deinit(self: *GameHistory) void {
        if (self.data_dir) |d| self.alloc.free(d);
        for (self.entries.items) |*e| e.deinit();
        self.entries.deinit(self.alloc);
    }

    pub fn load(self: *GameHistory) void {
        self.loadImpl() catch |err|
            std.log.err("game history load failed: {any}", .{err});

        // Show most-recently-played games first.
        std.sort.pdq(GameEntry, self.entries.items, {}, struct {
            fn lt(_: void, a: GameEntry, b: GameEntry) bool {
                return a.last_played > b.last_played;
            }
        }.lt);
    }

    fn loadImpl(self: *GameHistory) !void {
        var arena = std.heap.ArenaAllocator.init(self.alloc);
        defer arena.deinit();
        const alloc = arena.allocator();

        const data_dir_path = self.data_dir orelse return;
        const hist_dir_path = try std.fs.path.join(alloc, &.{ data_dir_path, HISTORY_SUBDIR });

        var hist_dir = std.Io.Dir.openDirAbsolute(self.io, hist_dir_path, .{ .iterate = true }) catch return;
        defer hist_dir.close(self.io);

        var it = hist_dir.iterate();
        while (try it.next(self.io)) |entry| {
            if (entry.kind != .file or !std.mem.endsWith(u8, entry.name, ".json")) continue;

            const stem = entry.name[0 .. entry.name.len - ".json".len];
            const json_bytes = hist_dir.readFileAlloc(self.io, entry.name, alloc, .limited(64 * 1024)) catch continue;

            const Meta = struct {
                name: []const u8,
                rom_path: []const u8,
                play_time_secs: u64,
                last_played: i64 = 0,
            };
            const parsed = std.json.parseFromSlice(Meta, alloc, json_bytes, .{}) catch continue;

            const thumbnail = blk: {
                const png_path = try std.fs.path.join(
                    alloc,
                    &.{ hist_dir_path, try std.fmt.allocPrint(alloc, "{s}.png", .{stem}) },
                );
                if (loadThumbnailPng(self.alloc, png_path)) |png| break :blk png;

                break :blk try createPlaceholderThumbnail(self.alloc);
            };

            try self.entries.append(self.alloc, .{
                .alloc = self.alloc,
                .name = try self.alloc.dupe(u8, parsed.value.name),
                .rom_path = try self.alloc.dupe(u8, parsed.value.rom_path),
                .play_time_secs = parsed.value.play_time_secs,
                .last_played = parsed.value.last_played,
                .thumbnail = thumbnail,
            });
        }
    }

    pub fn save(
        self: *GameHistory,
        name: []const u8,
        rom_path: []const u8,
        play_time_secs: u64,
        pixels: []const u8,
    ) void {
        const now = std.Io.Timestamp.now(self.io, .real).toSeconds();
        self.saveImpl(name, rom_path, play_time_secs, now, pixels) catch |err|
            std.log.err("game history save failed: {any}", .{err});
    }

    fn saveImpl(
        self: *GameHistory,
        name: []const u8,
        rom_path: []const u8,
        play_time_secs: u64,
        last_played: i64,
        pixels: []const u8,
    ) !void {
        var arena = std.heap.ArenaAllocator.init(self.alloc);
        defer arena.deinit();
        const alloc = arena.allocator();

        const data_dir_path = self.data_dir orelse return error.DataDirUnavailable;
        const hist_dir_path = try std.fs.path.join(alloc, &.{ data_dir_path, HISTORY_SUBDIR });

        std.Io.Dir.createDirAbsolute(self.io, data_dir_path, .default_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        var data_dir = try std.Io.Dir.openDirAbsolute(self.io, data_dir_path, .{});
        defer data_dir.close(self.io);

        // Ensure <data-dir>/neskwik/history exists.
        data_dir.createDir(self.io, HISTORY_SUBDIR, .default_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        var hist_dir = try data_dir.openDir(self.io, HISTORY_SUBDIR, .{});
        defer hist_dir.close(self.io);

        // Write metadata JSON.
        {
            const json_name = try std.fmt.allocPrint(alloc, "{s}.json", .{name});
            const json_bytes = try std.json.Stringify.valueAlloc(alloc, .{
                .name = name,
                .rom_path = rom_path,
                .play_time_secs = play_time_secs,
                .last_played = last_played,
            }, .{});
            const f = try hist_dir.createFile(self.io, json_name, .{});
            defer f.close(self.io);
            try f.writeStreamingAll(self.io, json_bytes);
        }

        // Write thumbnail PNG.
        if (pixels.len == THUMBNAIL_BYTES) {
            const png_path = try std.fs.path.join(
                alloc,
                &.{ hist_dir_path, try std.fmt.allocPrint(alloc, "{s}.png", .{name}) },
            );
            try saveThumbnailPng(alloc, pixels, png_path);
        }

        // Update or append the in-memory entry.
        for (self.entries.items) |*entry| {
            if (std.mem.eql(u8, entry.rom_path, rom_path)) {
                entry.play_time_secs = play_time_secs;
                entry.last_played = last_played;
                if (pixels.len == THUMBNAIL_BYTES) {
                    if (entry.thumbnail) |old| self.alloc.free(old);
                    entry.thumbnail = try self.alloc.dupe(u8, pixels);
                }
                return;
            }
        }

        try self.entries.append(self.alloc, .{
            .alloc = self.alloc,
            .name = try self.alloc.dupe(u8, name),
            .rom_path = try self.alloc.dupe(u8, rom_path),
            .play_time_secs = play_time_secs,
            .last_played = last_played,
            .thumbnail = if (pixels.len == THUMBNAIL_BYTES)
                try self.alloc.dupe(u8, pixels)
            else
                null,
        });
    }
};

fn loadThumbnailPng(alloc: std.mem.Allocator, png_path: []const u8) ?[]u8 {
    const png_path_z = alloc.dupeZ(u8, png_path) catch return null;
    defer alloc.free(png_path_z);

    var surface = c.SDL_LoadPNG(png_path_z.ptr) orelse return null;
    defer c.SDL_DestroySurface(surface);

    if (surface.*.w != THUMBNAIL_WIDTH or surface.*.h != THUMBNAIL_HEIGHT) return null;

    if (surface.*.format != c.SDL_PIXELFORMAT_ABGR8888) {
        const converted = c.SDL_ConvertSurface(surface, c.SDL_PIXELFORMAT_ABGR8888) orelse return null;
        c.SDL_DestroySurface(surface);
        surface = converted;
    }

    const pixels = alloc.alloc(u8, THUMBNAIL_BYTES) catch @panic("OOM");
    errdefer alloc.free(pixels);

    const source_pixels: [*]const u8 = @ptrCast(surface.*.pixels);
    @memcpy(pixels[0..], source_pixels[0..]);

    return pixels;
}

fn saveThumbnailPng(alloc: std.mem.Allocator, pixels: []const u8, png_path: []const u8) !void {
    const png_path_z = try alloc.dupeZ(u8, png_path);
    defer alloc.free(png_path_z);

    const surface = c.SDL_CreateSurfaceFrom(
        THUMBNAIL_WIDTH,
        THUMBNAIL_HEIGHT,
        c.SDL_PIXELFORMAT_ABGR8888,
        @ptrCast(@constCast(pixels.ptr)),
        THUMBNAIL_PITCH,
    ) orelse return error.SdlCreateSurfaceFailed;
    defer c.SDL_DestroySurface(surface);

    if (!c.SDL_SavePNG(surface, png_path_z.ptr)) return error.SdlSavePngFailed;
}

fn createPlaceholderThumbnail(alloc: std.mem.Allocator) ![]u8 {
    const pixels = try alloc.alloc(u8, THUMBNAIL_BYTES);
    errdefer alloc.free(pixels);

    fillRect(pixels, 0, 0, THUMBNAIL_WIDTH, THUMBNAIL_HEIGHT, .{ .r = 10, .g = 12, .b = 16 });

    var y: u32 = 0;
    while (y < THUMBNAIL_HEIGHT) : (y += 1) {
        var x: u32 = 0;
        while (x < THUMBNAIL_WIDTH) : (x += 1) {
            if (((x + y) % 18) == 0) setPixel(pixels, x, y, .{ .r = 20, .g = 24, .b = 31 });
        }
    }

    fillRect(pixels, 56, 54, 144, 104, .{ .r = 24, .g = 29, .b = 38 });
    fillRect(pixels, 64, 62, 128, 70, .{ .r = 7, .g = 9, .b = 13 });
    fillRect(pixels, 78, 78, 100, 38, .{ .r = 18, .g = 36, .b = 45 });
    fillRect(pixels, 64, 140, 128, 10, .{ .r = 50, .g = 57, .b = 68 });

    fillRect(pixels, 82, 164, 92, 8, .{ .r = 52, .g = 62, .b = 76 });
    fillRect(pixels, 96, 176, 64, 6, .{ .r = 36, .g = 44, .b = 55 });

    fillRect(pixels, 72, 146, 18, 4, .{ .r = 84, .g = 94, .b = 110 });
    fillRect(pixels, 166, 144, 8, 8, .{ .r = 97, .g = 73, .b = 57 });
    fillRect(pixels, 178, 144, 8, 8, .{ .r = 97, .g = 73, .b = 57 });

    return pixels;
}

const PlaceholderColor = struct {
    r: u8,
    g: u8,
    b: u8,
};

fn fillRect(pixels: []u8, x: u32, y: u32, w: u32, h: u32, color: PlaceholderColor) void {
    const x_end = @min(x + w, THUMBNAIL_WIDTH);
    const y_end = @min(y + h, THUMBNAIL_HEIGHT);
    var py = y;
    while (py < y_end) : (py += 1) {
        var px = x;
        while (px < x_end) : (px += 1) {
            setPixel(pixels, px, py, color);
        }
    }
}

fn setPixel(pixels: []u8, x: u32, y: u32, color: PlaceholderColor) void {
    const idx: usize = (@as(usize, y) * THUMBNAIL_WIDTH + x) * 4;
    pixels[idx + 0] = color.r;
    pixels[idx + 1] = color.g;
    pixels[idx + 2] = color.b;
    pixels[idx + 3] = 255;
}
