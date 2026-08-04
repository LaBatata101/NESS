const std = @import("std");
const builtin = @import("builtin");

pub fn build(b: *std.Build) void {
    // An explicit default target makes Zig use its bundled libc startup files.
    // This also avoids host GCC startup-object incompatibilities on newer Linux
    // distributions while remaining overridable with `-Dtarget=...`.
    const target = b.standardTargetOptions(.{ .default_target = .{
        .cpu_arch = builtin.cpu.arch,
        .os_tag = builtin.os.tag,
        .abi = builtin.abi,
    } });
    const optimize = b.standardOptimizeOption(.{});
    const lib_dir_override = b.option([]const u8, "iroh_lib_dir", "Directory containing a prebuilt iroh-ffi static archive");

    switch (target.result.os.tag) {
        .linux, .macos, .windows => {},
        else => @panic("iroh Zig bindings support only Linux, macOS, and Windows"),
    }

    const is_native = target.result.cpu.arch == builtin.cpu.arch and
        target.result.os.tag == builtin.os.tag and target.result.abi == builtin.abi;
    const rust_target = rustTarget(target);
    if (!is_native and rust_target == null and lib_dir_override == null) {
        @panic("unsupported Rust cross target; provide -Diroh_lib_dir=/absolute/path/to/libiroh_ffi");
    }

    // iroh-relay also emits a cdylib. Its unoptimized Windows build exceeds
    // PE's 65535-export limit, so Windows GNU builds always use Cargo's
    // release profile. This does not change the Zig module's optimize mode.
    const cargo_release = optimize != .Debug or rust_target != null;
    const profile = if (cargo_release) "release" else "debug";
    const lib_dir: std.Build.LazyPath = if (lib_dir_override) |path|
        .{ .cwd_relative = path }
    else blk: {
        const cargo = if (target.result.abi.isAndroid()) android: {
            _ = b.findProgram(&.{"cargo-ndk"}, &.{}) catch
                @panic("cargo-ndk is required; install it with `cargo install --locked cargo-ndk`");
            const command = b.addSystemCommand(&.{
                "cargo",
                "ndk",
                "--target",
                androidNdkTarget(target),
                "--platform",
                "24",
            });
            command.setCwd(b.path("."));
            command.addArg("build");
            break :android command;
        } else non_android: {
            const cargo_action = if (rust_target != null and rust_target.?.use_zigbuild) "zigbuild" else "build";
            const command = b.addSystemCommand(&.{ "cargo", cargo_action });
            command.addPrefixedFileArg("--manifest-path=", b.path("Cargo.toml"));
            break :non_android command;
        };
        cargo.addArgs(&.{ "--locked", "--lib", "-p", "iroh-ffi" });
        if (cargo_release) cargo.addArg("--release");

        if (rust_target) |cross_target| {
            if (cross_target.use_zigbuild) {
                _ = b.findProgram(&.{"cargo-zigbuild"}, &.{}) catch
                    @panic("cargo-zigbuild is required; install it with `cargo install --locked cargo-zigbuild`");
            }

            const rustup = b.addSystemCommand(&.{ "rustup", "target", "list", "--installed" });
            // Installed Rust targets are external toolchain state and can change
            // without any build input changing, so this probe must not be cached.
            rustup.has_side_effects = true;
            const check_rust_target = b.addCheckFile(rustup.captureStdOut(.{}), .{
                .expected_matches = &.{cross_target.rustup},
            });
            check_rust_target.setName(b.fmt(
                "check Rust target {s} (install with `rustup target add {s}`)",
                .{ cross_target.rustup, cross_target.rustup },
            ));
            cargo.step.dependOn(&check_rust_target.step);

            if (!target.result.abi.isAndroid()) cargo.addArgs(&.{ "--target", cross_target.cargo });
        }

        cargo.addFileInput(b.path("Cargo.lock"));
        cargo.addFileInput(b.path("build.rs"));
        cargo.addFileInput(b.path("iroh.pc.in"));
        addCargoSourceInputs(b, cargo, "src");

        const cargo_target = cargo.addPrefixedOutputDirectoryArg("--target-dir=", "iroh-target");
        const lib_subdir = if (rust_target) |cross_target|
            b.fmt("{s}/{s}", .{ cross_target.rustup, profile })
        else
            profile;
        break :blk cargo_target.path(b, lib_subdir);
    };

    const iroh = b.addModule("iroh", .{
        .root_source_file = b.path("zig/iroh.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    iroh.addIncludePath(b.path("include"));
    iroh.addLibraryPath(lib_dir);
    iroh.linkSystemLibrary("iroh_ffi", .{
        .preferred_link_mode = .static,
        .search_strategy = .no_fallback,
        .use_pkg_config = .no,
    });

    if (target.result.os.tag == .macos) {
        iroh.linkFramework("Security", .{});
        iroh.linkFramework("SystemConfiguration", .{});
    }

    const tests = b.addTest(.{ .root_module = iroh });
    const run_tests = b.addRunArtifact(tests);

    const test_step = b.step("test", "Build Rust and run the Zig binding tests");
    test_step.dependOn(&run_tests.step);
}

const RustTarget = struct {
    cargo: []const u8,
    rustup: []const u8,
    use_zigbuild: bool,
};

fn rustTarget(target: std.Build.ResolvedTarget) ?RustTarget {
    if (target.result.abi.isAndroid()) {
        return switch (target.result.cpu.arch) {
            .aarch64 => .{
                .cargo = "aarch64-linux-android",
                .rustup = "aarch64-linux-android",
                .use_zigbuild = false,
            },
            .x86_64 => .{
                .cargo = "x86_64-linux-android",
                .rustup = "x86_64-linux-android",
                .use_zigbuild = false,
            },
            else => null,
        };
    }

    if (target.result.cpu.arch == .x86_64 and
        target.result.os.tag == .windows and
        target.result.abi == .gnu)
    {
        return .{
            .cargo = "x86_64-pc-windows-gnu",
            .rustup = "x86_64-pc-windows-gnu",
            .use_zigbuild = true,
        };
    }
    return null;
}

fn androidNdkTarget(target: std.Build.ResolvedTarget) []const u8 {
    return switch (target.result.cpu.arch) {
        .aarch64 => "arm64-v8a",
        .x86_64 => "x86_64",
        else => @panic("unsupported Android architecture"),
    };
}

fn addCargoSourceInputs(b: *std.Build, cargo: *std.Build.Step.Run, source_dir: []const u8) void {
    var dir = b.build_root.handle.openDir(b.graph.io, source_dir, .{ .iterate = true }) catch |err|
        @panic(b.fmt("failed to open Cargo source directory '{s}': {s}", .{ source_dir, @errorName(err) }));
    defer dir.close(b.graph.io);

    var walker = dir.walk(b.allocator) catch @panic("out of memory while walking Cargo sources");
    defer walker.deinit();

    var paths: std.ArrayList([]const u8) = .empty;
    while (walker.next(b.graph.io) catch |err|
        @panic(b.fmt("failed to walk Cargo source directory '{s}': {s}", .{ source_dir, @errorName(err) }))) |entry|
    {
        if (entry.kind != .file) continue;
        paths.append(b.allocator, b.dupe(entry.path)) catch @panic("out of memory while collecting Cargo sources");
    }

    std.mem.sort([]const u8, paths.items, {}, struct {
        fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
            return std.mem.lessThan(u8, lhs, rhs);
        }
    }.lessThan);

    for (paths.items) |path| {
        cargo.addFileInput(b.path(b.fmt("{s}/{s}", .{ source_dir, path })));
    }
}
