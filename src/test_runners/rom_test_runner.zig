const std = @import("std");
const ness = @import("ness");

const c = ness.c;
const Rom = ness.Rom;
const System = ness.System;
const ThreadPool = ness.ThreadPool;

const ADDR_STATUS = 0x6000;
const ADDR_MAGIC_START = 0x6001; // $DE $B0 $61
const ADDR_TEXT_START = 0x6004;

const MAGIC_BYTES = [_]u8{ 0xDE, 0xB0, 0x61 };

const STATUS_RUNNING = 0x80;
const STATUS_NEEDS_RESET = 0x81;
const STATUS_PASSED = 0x00;

const CPU_ONE_SEC_CYCLES: u64 = 1_790_000;

const TESTS_DIR = "test_roms";

const BUILTIN_SKIPPED_TESTS = [_]SkippedTest{
    .{ .pattern = "cpu_interrupts", .reason = "Doesn't pass all test cases yet" },
};

const SkippedTest = struct {
    pattern: []const u8,
    reason: []const u8 = "",
};

const CliOptions = struct {
    filters: std.ArrayList([]const u8) = .empty,
    skip_patterns: std.ArrayList([]const u8) = .empty,

    fn deinit(self: *CliOptions, allocator: std.mem.Allocator) void {
        self.filters.deinit(allocator);
        self.skip_patterns.deinit(allocator);
    }
};

pub fn suppressLog(
    comptime message_level: std.log.Level,
    comptime scope: @EnumLiteral(),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = message_level;
    _ = scope;
    _ = format;
    _ = args;
}

pub const std_options: std.Options = .{
    .log_level = .err,
    .logFn = suppressLog,
};

const TestState = struct {
    mutex: std.Io.Mutex = .init,
    passed_count: usize = 0,
    total_count: usize = 0,
    skipped_count: usize = 0,
    failed_tests: std.ArrayList(FailedTest),
    allocator: std.mem.Allocator,
    io: std.Io,

    pub fn init(allocator: std.mem.Allocator, io: std.Io) TestState {
        return .{
            .failed_tests = .empty,
            .allocator = allocator,
            .io = io,
        };
    }

    pub fn deinit(self: *TestState) void {
        // Clean up the failed test data
        for (self.failed_tests.items) |failed| {
            self.allocator.free(failed.output);
            // The name is the path, which was also allocated
            self.allocator.free(failed.name);
        }
        self.failed_tests.deinit(self.allocator);
    }
};

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa;
    const io = init.io;

    var args = try init.minimal.args.iterateAllocator(allocator);
    defer args.deinit();

    var options = try parseArgs(allocator, &args);
    defer options.deinit(allocator);

    _ = c.SDL_SetHint(c.SDL_HINT_NO_SIGNAL_HANDLERS, "1");

    if (!c.SDL_Init(c.SDL_INIT_AUDIO)) {
        std.debug.print("Failed to initialize SDL for headless tests!\n", .{});
        return error.SDLInitFailed;
    }
    defer c.SDL_Quit();

    var dir = std.Io.Dir.cwd().openDir(io, TESTS_DIR, .{ .iterate = true }) catch |err| {
        std.debug.print("Error: Couldn't find '{s}'.\nError: {}\n", .{ TESTS_DIR, err });
        return;
    };
    defer dir.close(io);

    var pool: ThreadPool = undefined;
    try pool.init(.{ .allocator = allocator, .io = io });

    var state = TestState.init(allocator, io);
    defer state.deinit();

    std.debug.print("\n=== RUNNING TEST ROMS ===\n\n", .{});

    var walker = try dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.basename, ".nes")) continue;

        if (!matchesAnyFilter(entry.path, options.filters.items)) continue;

        if (skipReason(entry.path, options.skip_patterns.items)) |reason| {
            const name_stem = displayName(entry.path);
            state.mutex.lockUncancelable(io);
            state.skipped_count += 1;
            if (reason.len > 0) {
                std.debug.print("{s: <30} \x1b[33mSKIP\x1b[0m \t\x1b[1m{s}\x1b[0m\n", .{ name_stem, reason });
            } else {
                std.debug.print("{s: <30} \x1b[33mSKIP\x1b[0m\n", .{name_stem});
            }
            state.mutex.unlock(io);
            continue;
        }

        // We must duplicate the path because the thread will run after the
        // walker moves to the next entry or closes.
        const rom_path = try std.fs.path.join(allocator, &.{ TESTS_DIR, entry.path });

        // Spawn the job
        try pool.spawn(run_test_wrapper, .{ &state, rom_path });
    }

    const timer = std.Io.Timestamp.now(io, .awake);
    pool.deinit(); // This will wait for all threads to finish
    const exec_time = timer.untilNow(io, .awake).toNanoseconds();

    if (state.failed_tests.items.len > 0) {
        std.debug.print("\nFailed Tests:\n", .{});
        for (state.failed_tests.items) |failed_test| {
            std.debug.print("\n{s}", .{failed_test.name});
            if (failed_test.output.len > 0) {
                std.debug.print(" - output:\n{s}\n{s}", .{ failed_test.output, "=" ** 15 });
            }
            std.debug.print("\n", .{});
        }
        std.debug.print("Failed: {}\n", .{state.failed_tests.items.len});
    }

    std.debug.print("\nPassed: {}/{}\n", .{ state.passed_count, state.total_count });
    if (state.skipped_count > 0) {
        std.debug.print("Skipped: {}\n", .{state.skipped_count});
    }
    std.debug.print("Total time: {d:.2}ms\n", .{
        @as(f64, @floatFromInt(exec_time)) / std.time.ns_per_ms,
    });

    if (state.failed_tests.items.len > 0) {
        std.process.exit(1);
    }
}

fn parseArgs(allocator: std.mem.Allocator, args: *std.process.Args.Iterator) !CliOptions {
    var options: CliOptions = .{};
    errdefer options.deinit(allocator);

    _ = args.skip();
    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--skip")) {
            try options.skip_patterns.append(allocator, args.next() orelse return error.MissingSkipPattern);
        } else if (std.mem.startsWith(u8, arg, "--skip=")) {
            try options.skip_patterns.append(allocator, arg["--skip=".len..]);
        } else {
            try options.filters.append(allocator, arg);
        }
    }

    return options;
}

fn matchesAnyFilter(path: []const u8, filters: []const []const u8) bool {
    if (filters.len == 0) return true;
    for (filters) |filter| {
        if (std.mem.indexOf(u8, path, filter) != null) return true;
    }
    return false;
}

fn skipReason(path: []const u8, cli_skip_patterns: []const []const u8) ?[]const u8 {
    for (&BUILTIN_SKIPPED_TESTS) |skipped| {
        if (std.mem.indexOf(u8, path, skipped.pattern) != null) return skipped.reason;
    }
    for (cli_skip_patterns) |pattern| {
        if (std.mem.indexOf(u8, path, pattern) != null) return "requested by --skip";
    }
    return null;
}

fn displayName(path: []const u8) []const u8 {
    return if (std.mem.lastIndexOfScalar(u8, path, '.')) |i| path[0..i] else path;
}

// Wrapper function executed by the thread pool
fn run_test_wrapper(state: *TestState, rom_path: []const u8) void {
    // Ensure we free the path we allocated in the main loop,
    // UNLESS it's saved into failed_tests (handled below)
    var should_free_path = true;
    defer if (should_free_path) state.allocator.free(rom_path);

    const timer = std.Io.Timestamp.now(state.io, .awake);
    const result = run_test_rom(state.allocator, state.io, rom_path) catch |err| {
        // Handle unexpected runtime errors (file not found, etc)
        state.mutex.lockUncancelable(state.io);
        defer state.mutex.unlock(state.io);
        // Strip "test_roms/" prefix then the extension for a readable path-aware name
        const rel = rom_path[TESTS_DIR.len + 1 ..];
        const display = if (std.mem.lastIndexOfScalar(u8, rel, '.')) |i| rel[0..i] else rel;
        std.debug.print("{s}... \x1b[31mERROR ({})\x1b[0m\n", .{ display, err });
        return;
    };
    const exec_time = timer.untilNow(state.io, .awake).toNanoseconds();

    // Lock mutex to safely print to stdout and update stats
    state.mutex.lockUncancelable(state.io);
    defer state.mutex.unlock(state.io);

    state.total_count += 1;

    // Strip the leading "test_roms/" prefix so subdirectory tests show "subdir/test"
    // instead of just "test"; then strip the .nes extension.
    const rel = rom_path[TESTS_DIR.len + 1 ..];
    const name_stem = displayName(rel);

    switch (result) {
        .passed => {
            state.passed_count += 1;
            std.debug.print("{s: <30} \x1b[32mOK\x1b[0m \ttook {d:.2}ms\n", .{
                name_stem,
                @as(f64, @floatFromInt(exec_time)) / std.time.ns_per_ms,
            });
        },
        .failed => |fail_data| {
            std.debug.print("{s: <30} \x1b[31mFAILED\x1b[0m\n", .{name_stem});

            // We transfer ownership of the allocated rom_path string to the struct
            // so it can be printed in the summary later.
            should_free_path = false;

            // We need to construct a FailedTest that owns its data
            const stored_failure = FailedTest{
                .code = fail_data.code,
                .name = rom_path, // Store full path/buffer ownership
                .output = fail_data.output,
            };

            state.failed_tests.append(state.allocator, stored_failure) catch {
                std.debug.print("CRITICAL: Failed to append test failure to list.\n", .{});
            };
        },
    }
}

const FailedTest = struct {
    code: u8,
    name: []const u8,
    output: []const u8,
};

const TestResult = union(enum) {
    passed,
    failed: FailedTest,
};

fn run_test_rom(allocator: std.mem.Allocator, io: std.Io, path: []const u8) !TestResult {
    const buffer = try std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .unlimited);
    defer allocator.free(buffer);

    var rom = try Rom.init(allocator, io, path, buffer);
    defer rom.deinit();

    var system = try System.init(allocator, io, &rom, .{ .disable_audio = true });
    defer system.deinit();
    system.reset();

    const speed = ness.settings.EmulationSpeed.quadruple;
    var frame_acc: f32 = 0;

    var test_started = false;
    var timestamp: u64 = 0;
    var status = system.bus.mem_read(ADDR_STATUS);
    while (true) {
        while (system.bus.cycles <= (CPU_ONE_SEC_CYCLES / 2) + timestamp) {
            frame_acc += speed.multiplier();
            const frames_to_run: u32 = @intFromFloat(frame_acc);
            frame_acc -= @floatFromInt(frames_to_run);
            for (0..frames_to_run) |_| {
                system.run_frame();
            }
        }
        timestamp = system.bus.cycles;

        if (!test_started) {
            const m1 = system.bus.mem_read(ADDR_MAGIC_START);
            const m2 = system.bus.mem_read(ADDR_MAGIC_START + 1);
            const m3 = system.bus.mem_read(ADDR_MAGIC_START + 2);

            test_started = m1 == MAGIC_BYTES[0] and m2 == MAGIC_BYTES[1] and m3 == MAGIC_BYTES[2];
        }

        if (test_started and status == STATUS_NEEDS_RESET) {
            system.reset();
            continue;
        }

        status = system.bus.mem_read(ADDR_STATUS);
        if (status != STATUS_RUNNING) {
            break;
        }
    }

    if (status != STATUS_PASSED) {
        var output_buffer = std.ArrayList(u8).empty;
        var char_idx: u16 = 0;
        while (true) : (char_idx += 1) {
            const char = system.bus.mem_read(ADDR_TEXT_START + char_idx);
            if (char == 0) break;

            if (char_idx >= output_buffer.items.len) {
                try output_buffer.append(allocator, char);
            }
        }
        return .{
            .failed = .{
                .code = status,
                .name = std.fs.path.stem(path),
                .output = try output_buffer.toOwnedSlice(allocator),
            },
        };
    } else {
        return .passed;
    }
}
