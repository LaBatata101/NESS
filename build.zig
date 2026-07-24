const std = @import("std");
const android = @import("android");

pub fn build(b: *std.Build) !void {
    const exe_name = "neskwik";
    const package_name = "com.labatata.neskwik";

    const root_target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const android_targets = android.standardTargets(b, root_target);

    var root_target_single = [_]std.Build.ResolvedTarget{root_target};
    const targets: []std.Build.ResolvedTarget = if (android_targets.len == 0)
        root_target_single[0..]
    else
        android_targets;

    const android_apk: ?*android.Apk = blk: {
        if (android_targets.len == 0) break :blk null;

        const android_sdk = android.Sdk.create(b, .{});
        const apk = android_sdk.createApk(.{
            .name = exe_name,
            .api_level = .android15,
            .build_tools_version = "36.1.0",
            .ndk_version = "28.2.13676358",
        });

        apk.setKeyStore(android_sdk.createKeyStore(.example));
        apk.setAndroidManifest(b.path("android/AndroidManifest.xml"));
        apk.addResourceDirectory(b.path("android/res"));
        apk.addJavaSourceFile(.{ .file = b.path("android/src/NeskwikActivity.java") });
        addAndroidLibcxxShared(b, apk, android_targets);

        const sdl_java_dep = b.dependency("sdl", .{
            .target = android_targets[0],
            .optimize = optimize,
            .preferred_linkage = .static,
        });
        const sdl_java_files = sdl_java_dep.namedWriteFiles("sdljava");
        for (sdl_java_files.files.items) |file| {
            apk.addJavaSourceFile(.{ .file = file.contents.copy });
        }

        break :blk apk;
    };

    for (targets) |target| {
        if (target.result.cpu.arch == .x86 or target.result.cpu.arch == .arm) continue;

        const deps = try createNessModule(b, target, optimize);

        const app_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .strip = optimize == .ReleaseFast,
            .imports = &.{
                .{ .name = "ness", .module = deps.mod },
            },
        });

        if (target.result.abi.isAndroid()) {
            const apk = android_apk orelse @panic("Android APK should be initialized");
            const android_dep = b.dependency("android", .{
                .optimize = optimize,
                .target = target,
            });
            app_module.addImport("android", android_dep.module("android"));

            const app_lib = b.addLibrary(.{
                .name = "main",
                .root_module = app_module,
                .linkage = .dynamic,
                .use_llvm = true,
            });
            linkAppLibraries(app_lib, deps);
            app_lib.root_module.linkSystemLibrary("c++_shared", .{});
            apk.addArtifact(app_lib);
        } else {
            const exe = b.addExecutable(.{
                .name = exe_name,
                .use_llvm = true,
                .root_module = app_module,
            });

            if (target.result.os.tag == .windows) {
                exe.subsystem = .Windows;
            }

            linkAppLibraries(exe, deps);

            b.installArtifact(exe);

            const run_step = b.step("run", "Run the app");
            const run_cmd = b.addRunArtifact(exe);
            run_step.dependOn(&run_cmd.step);

            run_cmd.step.dependOn(b.getInstallStep());

            if (b.args) |args| {
                run_cmd.addArgs(args);
            }

            addTestStep(b, target, optimize, deps, exe);
        }
    }

    if (android_apk) |apk| {
        const installed_apk = apk.addInstallApk();
        b.getInstallStep().dependOn(&installed_apk.step);

        const run_step = b.step("run", "Install and run the app on an Android device");
        const adb_install = apk.sdk.addAdbInstall(installed_apk.source);
        const adb_start = apk.sdk.addAdbStart(package_name ++ "/" ++ package_name ++ ".NeskwikActivity");
        adb_start.step.dependOn(&adb_install.step);
        run_step.dependOn(&adb_start.step);
    }
}

fn addAndroidLibcxxShared(b: *std.Build, apk: *android.Apk, targets: []const std.Build.ResolvedTarget) void {
    for (targets) |target| {
        if (target.result.cpu.arch == .x86 or target.result.cpu.arch == .arm) continue;

        const system_triple = androidSystemTriple(b, target);
        const libcxx_path: std.Build.LazyPath = .{
            .cwd_relative = b.fmt("{s}/usr/lib/{s}/libc++_shared.so", .{
                apk.ndk.sysroot_path,
                system_triple,
            }),
        };

        switch (target.result.cpu.arch) {
            .aarch64 => apk.addLibraryFile(.arm64_v8a, libcxx_path),
            .x86_64 => apk.addLibraryFile(.x86_64, libcxx_path),
            else => @panic(b.fmt("unsupported Android target arch: {s}", .{@tagName(target.result.cpu.arch)})),
        }
    }
}

fn androidSystemTriple(b: *std.Build, target: std.Build.ResolvedTarget) []const u8 {
    if (!target.result.abi.isAndroid()) {
        @panic("expected Android target");
    }
    return switch (target.result.cpu.arch) {
        .aarch64 => "aarch64-linux-android",
        .x86_64 => "x86_64-linux-android",
        else => @panic(b.fmt("unsupported Android target arch: {s}", .{@tagName(target.result.cpu.arch)})),
    };
}

const NessDeps = struct {
    mod: *std.Build.Module,
    sdl_lib: *std.Build.Step.Compile,
    blip_buf_lib: *std.Build.Step.Compile,
    clay_lib: *std.Build.Step.Compile,
    font_raster_lib: *std.Build.Step.Compile,
    glslang_lib: *std.Build.Step.Compile,
    spirv_cross_lib: *std.Build.Step.Compile,
};

fn createNessModule(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) !NessDeps {
    const preferred_linkage: std.builtin.LinkMode = if (target.result.abi.isAndroid()) .dynamic else .static;

    const mod = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
    });

    const sdl_dep = b.dependency("sdl", .{
        .target = target,
        .optimize = optimize,
        .preferred_linkage = preferred_linkage,
    });
    const sdl_lib = sdl_dep.artifact("SDL3");
    mod.linkLibrary(sdl_lib);

    const blip_buf_lib = b.addLibrary(
        .{ .name = "blip_buf", .linkage = .static, .root_module = b.createModule(
            .{
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            },
        ) },
    );
    blip_buf_lib.addCSourceFile(.{ .file = b.path("third-party/blip_buf-1.1.0/blip_buf.c") });
    mod.addIncludePath(b.path("third-party/blip_buf-1.1.0"));
    mod.linkLibrary(blip_buf_lib);

    const clay_mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    clay_mod.addIncludePath(b.path("third-party/clay"));

    const clay_lib = b.addLibrary(.{
        .name = "clay",
        .linkage = .static,
        .root_module = clay_mod,
    });
    clay_lib.addCSourceFile(.{ .file = b.path("third-party/clay/clay_impl.c") });
    mod.linkLibrary(clay_lib);

    const font_raster_dep = b.dependency("font_raster", .{
        .target = target,
        .optimize = optimize,
        .@"enable-harfbuzz" = false,
        .@"enable-libpng" = false,
    });
    const font_raster_mod = font_raster_dep.module("font_raster");
    mod.addImport("font_raster", font_raster_mod);
    const font_raster_lib = blk: {
        for (font_raster_mod.link_objects.items) |link_object| {
            switch (link_object) {
                .other_step => |compile| break :blk compile,
                else => {},
            }
        }
        @panic("font_raster module did not link its FreeType artifact");
    };

    const glslang_dep = b.dependency("glslang", .{
        .target = target,
        .optimize = optimize,
    });
    const glslang_lib = glslang_dep.artifact("glslang");
    mod.linkLibrary(glslang_lib);

    const spirv_cross_dep = b.dependency("spirv_cross", .{
        .target = target,
        .optimize = optimize,
    });
    const spirv_cross_lib = spirv_cross_dep.artifact("spirv-cross-c");
    mod.linkLibrary(spirv_cross_lib);

    const zeit_dep = b.dependency("zeit", .{
        .target = target,
        .optimize = optimize,
    });
    mod.addImport("zeit", zeit_dep.module("zeit"));

    const vk_headers = b.dependency("vulkan_headers", .{});
    mod.addIncludePath(vk_headers.path("include"));

    const override = b.option([]const u8, "iroh-lib-dir", "Directory containing a prebuilt iroh-ffi static archive");
    const iroh_dep = if (override) |lib_dir|
        b.dependency("iroh", .{
            .target = target,
            .optimize = optimize,
            .iroh_lib_dir = lib_dir,
        })
    else
        b.dependency("iroh", .{
            .target = target,
            .optimize = optimize,
        });
    mod.addImport("iroh", iroh_dep.module("iroh"));

    mod.addAnonymousImport("pixeloid_font", .{ .root_source_file = b.path("resources/fonts/PixeloidSans.ttf") });
    mod.addAnonymousImport("nes_controller_img", .{ .root_source_file = b.path("resources/images/nes-controller.png") });
    mod.addAnonymousImport("app_icon", .{ .root_source_file = b.path("resources/icons/nes-icon-256x256.png") });
    mod.addAnonymousImport("fast_forward_icon", .{ .root_source_file = b.path("resources/icons/fast_forward_32x32.png") });
    mod.addAnonymousImport("skip_next_icon", .{ .root_source_file = b.path("resources/icons/skip_next_32x32.png") });
    mod.addAnonymousImport("play_icon", .{ .root_source_file = b.path("resources/icons/play_arrow_32x32.png") });
    mod.addAnonymousImport("stop_icon", .{ .root_source_file = b.path("resources/icons/stop_32x32.png") });
    mod.addAnonymousImport("menu_icon", .{ .root_source_file = b.path("resources/icons/menu_32x32.png") });
    mod.addAnonymousImport("controller_icon", .{ .root_source_file = b.path("resources/icons/controller_32x32.png") });
    mod.addAnonymousImport("keyboard_icon", .{ .root_source_file = b.path("resources/icons/keyboard_32x32.png") });
    mod.addAnonymousImport("dpad_up_icon", .{ .root_source_file = b.path("resources/icons/dpad_up_32x32.png") });
    mod.addAnonymousImport("dpad_down_icon", .{ .root_source_file = b.path("resources/icons/dpad_down_32x32.png") });
    mod.addAnonymousImport("dpad_left_icon", .{ .root_source_file = b.path("resources/icons/dpad_left_32x32.png") });
    mod.addAnonymousImport("dpad_right_icon", .{ .root_source_file = b.path("resources/icons/dpad_right_32x32.png") });
    mod.addAnonymousImport("copy_icon", .{ .root_source_file = b.path("resources/icons/copy_32x32.png") });

    try addBorderShaderImports(b, mod);

    return .{
        .mod = mod,
        .sdl_lib = sdl_lib,
        .blip_buf_lib = blip_buf_lib,
        .clay_lib = clay_lib,
        .font_raster_lib = font_raster_lib,
        .glslang_lib = glslang_lib,
        .spirv_cross_lib = spirv_cross_lib,
    };
}

const BorderShaderFile = struct {
    rel_path: []const u8,
    import_name: []const u8,
};

fn addBorderShaderImports(b: *std.Build, mod: *std.Build.Module) !void {
    const shader_dir_path = "resources/shaders";

    var files: std.ArrayList(BorderShaderFile) = .empty;
    try collectBorderShaderFiles(b, &files, shader_dir_path, "");

    std.mem.sort(BorderShaderFile, files.items, {}, struct {
        fn lessThan(_: void, lhs: BorderShaderFile, rhs: BorderShaderFile) bool {
            return std.mem.lessThan(u8, lhs.rel_path, rhs.rel_path);
        }
    }.lessThan);

    const generated_source = try renderBorderShaderEntries(b, files.items);
    const generated_path = b.addWriteFiles().add("border_shader_entries.zig", generated_source);
    const generated_mod = b.createModule(.{
        .root_source_file = generated_path,
    });

    for (files.items) |file| {
        const file_path = try std.fs.path.join(b.allocator, &.{ shader_dir_path, file.rel_path });
        generated_mod.addAnonymousImport(
            file.import_name,
            .{ .root_source_file = b.path(file_path) },
        );
    }

    mod.addImport("border_shader_entries", generated_mod);
}

fn collectBorderShaderFiles(
    b: *std.Build,
    files: *std.ArrayList(BorderShaderFile),
    shader_dir_path: []const u8,
    rel_dir: []const u8,
) !void {
    const dir_path = if (rel_dir.len == 0)
        shader_dir_path
    else
        try std.fs.path.join(b.allocator, &.{ shader_dir_path, rel_dir });

    var shader_dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err|
        @panic(b.fmt("failed to open border shader directory '{s}': {s}", .{ dir_path, @errorName(err) }));
    defer shader_dir.close();

    var iter = shader_dir.iterate();
    while (try iter.next()) |entry| {
        const rel_path = if (rel_dir.len == 0)
            try b.allocator.dupe(u8, entry.name)
        else
            try std.fmt.allocPrint(b.allocator, "{s}/{s}", .{ rel_dir, entry.name });

        if (entry.kind == .directory) {
            try collectBorderShaderFiles(b, files, shader_dir_path, rel_path);
            continue;
        }
        if (entry.kind != .file) continue;

        const ext = std.fs.path.extension(entry.name);
        if (!isBorderShaderSourceFile(ext)) continue;

        try files.append(b.allocator, .{
            .rel_path = rel_path,
            .import_name = try borderShaderImportName(b, rel_path, ext),
        });
    }
}

fn isBorderShaderSourceFile(ext: []const u8) bool {
    return std.mem.eql(u8, ext, ".slang") or
        std.mem.eql(u8, ext, ".slangp") or
        std.mem.eql(u8, ext, ".h") or
        std.mem.eql(u8, ext, ".inc");
}

fn borderShaderImportName(b: *std.Build, rel_path: []const u8, ext: []const u8) ![]const u8 {
    var import_name: std.ArrayList(u8) = .empty;
    try import_name.appendSlice(b.allocator, "border_shader_");
    for (rel_path) |ch| {
        try import_name.append(b.allocator, if (std.ascii.isAlphanumeric(ch) or ch == '_') ch else '_');
    }

    const suffix = if (std.mem.eql(u8, ext, ".slangp"))
        "_preset"
    else if (std.mem.eql(u8, ext, ".slang"))
        "_source"
    else if (std.mem.eql(u8, ext, ".h") or std.mem.eql(u8, ext, ".inc"))
        "_header"
    else
        unreachable;

    try import_name.appendSlice(b.allocator, suffix);
    return import_name.toOwnedSlice(b.allocator);
}

fn renderBorderShaderEntries(b: *std.Build, files: []const BorderShaderFile) ![]const u8 {
    var source: std.ArrayList(u8) = .empty;
    try source.appendSlice(b.allocator,
        \\pub const Entry = struct {
        \\    path: []const u8,
        \\    source: []const u8,
        \\};
        \\
        \\pub const entries = [_]Entry{
        \\
    );

    for (files) |file| {
        try source.appendSlice(b.allocator, "    .{ .path = \"builtin://border-shaders/");
        try appendZigStringContent(b, &source, file.rel_path);
        try source.appendSlice(b.allocator, "\", .source = @embedFile(\"");
        try appendZigStringContent(b, &source, file.import_name);
        try source.appendSlice(b.allocator, "\") },\n");
    }

    try source.appendSlice(b.allocator, "};\n");
    return source.toOwnedSlice(b.allocator);
}

fn appendZigStringContent(b: *std.Build, source: *std.ArrayList(u8), value: []const u8) !void {
    for (value) |ch| {
        switch (ch) {
            '\\' => try source.appendSlice(b.allocator, "\\\\"),
            '"' => try source.appendSlice(b.allocator, "\\\""),
            '\n' => try source.appendSlice(b.allocator, "\\n"),
            '\r' => try source.appendSlice(b.allocator, "\\r"),
            '\t' => try source.appendSlice(b.allocator, "\\t"),
            else => try source.append(b.allocator, ch),
        }
    }
}

fn linkAppLibraries(compile: *std.Build.Step.Compile, deps: NessDeps) void {
    compile.linkLibrary(deps.sdl_lib);
    compile.linkLibrary(deps.blip_buf_lib);
    compile.linkLibrary(deps.clay_lib);
    compile.linkLibrary(deps.font_raster_lib);
    compile.linkLibrary(deps.glslang_lib);
    compile.linkLibrary(deps.spirv_cross_lib);
}

fn addTestStep(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    deps: NessDeps,
    exe: *std.Build.Step.Compile,
) void {
    const test_filters = b.option([]const []const u8, "test-filter", "Skip tests that do not match any filter") orelse &[0][]const u8{};
    const no_run = b.option(bool, "no-run", "Don't run the test") orelse false;
    const rom_tests_enabled = b.option(bool, "rom-tests", "Run ROM tests. This is very slow!") orelse false;
    const skipped_rom_tests = b.option([]const []const u8, "skip-rom-test", "Skip ROM tests matching any pattern") orelse &[0][]const u8{};

    const test_step = b.step("test", "Run tests");
    if (!no_run and rom_tests_enabled) {
        const test_exe = b.addExecutable(.{
            .name = "rom-tests",
            .use_llvm = true,
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/test_runners/rom_test_runner.zig"),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "ness", .module = deps.mod },
                },
            }),
        });
        const rom_mod_test_artifacts = b.addInstallArtifact(test_exe, .{
            .dest_dir = .{
                .override = .{
                    .custom = "tests",
                },
            },
        });
        test_exe.root_module.addIncludePath(b.path("third-party/blip_buf-1.1.0"));

        test_exe.root_module.linkLibrary(deps.sdl_lib);
        test_exe.root_module.linkLibrary(deps.blip_buf_lib);
        test_exe.root_module.link_libc = true;

        const run_rom_tests = b.addRunArtifact(test_exe);

        for (test_filters) |filter| {
            run_rom_tests.addArg(filter);
        }
        for (skipped_rom_tests) |skip| {
            run_rom_tests.addArg("--skip");
            run_rom_tests.addArg(skip);
        }

        test_step.dependOn(&rom_mod_test_artifacts.step);
        test_step.dependOn(&run_rom_tests.step);
    } else {
        const mod_tests = b.addTest(.{
            .name = "ness-mod-test",
            .root_module = deps.mod,
            .filters = test_filters,
            .use_llvm = true,
            .test_runner = .{ .path = b.path("src/test_runners/test_runner.zig"), .mode = .simple },
        });
        const mod_test_artifacts = b.addInstallArtifact(mod_tests, .{
            .dest_dir = .{
                .override = .{
                    .custom = "tests",
                },
            },
        });

        const run_mod_tests = b.addRunArtifact(mod_tests);
        const exe_tests = b.addTest(.{
            .name = "exe-test",
            .root_module = exe.root_module,
            .filters = test_filters,
            .use_llvm = true,
            .test_runner = .{ .path = b.path("src/test_runners/test_runner.zig"), .mode = .simple },
        });
        b.installArtifact(exe_tests);

        const run_exe_tests = b.addRunArtifact(exe_tests);

        test_step.dependOn(&mod_test_artifacts.step);
        if (!no_run) {
            test_step.dependOn(&run_mod_tests.step);
            test_step.dependOn(&run_exe_tests.step);
        }
    }
}
