/// RetroArch-compatible shader pipeline for NES emulation.
///
/// Supports multi-pass `.slangp` preset files with full SPIR-V compilation,
/// shader reflection, feedback loops, LUT textures, and frame history.
const std = @import("std");
const builtin = @import("builtin");

const c = @import("../root.zig").c;
const builtin_shaders = @import("builtin.zig");
const slangp = @import("slangp.zig");
const parser = @import("parser.zig");
const ShaderCache = @import("cache.zig").ShaderCache;
const SpirvPair = @import("cache.zig").SpirvPair;
const ThreadPool = @import("../utils/pool.zig");
const sdlError = @import("../utils/sdl.zig").sdlError;
const vulkan = @import("../utils/vulkan.zig");

const Vertex = struct {
    x: f32,
    y: f32,
    z: f32,
    u: f32,
    v: f32,
};

// Full-screen quad vertices (NDC space, UV flipped vertically for Vulkan).
const QUAD_VERTICES = [_]Vertex{
    .{ .x = -1, .y = -1, .z = 0, .u = 0, .v = 1 }, .{ .x = 1, .y = -1, .z = 0, .u = 1, .v = 1 },
    .{ .x = 1, .y = 1, .z = 0, .u = 1, .v = 0 },   .{ .x = -1, .y = -1, .z = 0, .u = 0, .v = 1 },
    .{ .x = 1, .y = 1, .z = 0, .u = 1, .v = 0 },   .{ .x = -1, .y = 1, .z = 0, .u = 0, .v = 0 },
};

pub const Viewport = struct { x: i32, y: i32, w: u32, h: u32 };

pub fn calculateLetterboxViewport(win_w: u32, win_h: u32, desired_aspect: f32) Viewport {
    const device_aspect = @as(f32, @floatFromInt(win_w)) / @as(f32, @floatFromInt(win_h));

    var x: i32 = 0;
    var y: i32 = 0;
    var viewport_w: u32 = win_w;
    var viewport_h: u32 = win_h;

    if (@abs(device_aspect - desired_aspect) >= 0.0001) {
        if (device_aspect > desired_aspect) {
            const delta = (desired_aspect / device_aspect - 1.0) / 2.0 + 0.5;
            x = @intFromFloat(@round(@as(f32, @floatFromInt(win_w)) * (0.5 - delta)));
            viewport_w = @intFromFloat(@round(2.0 * @as(f32, @floatFromInt(win_w)) * delta));
        } else {
            const delta = (device_aspect / desired_aspect - 1.0) / 2.0 + 0.5;
            y = @intFromFloat(@round(@as(f32, @floatFromInt(win_h)) * (0.5 - delta)));
            viewport_h = @intFromFloat(@round(2.0 * @as(f32, @floatFromInt(win_h)) * delta));
        }
    }

    return .{ .x = x, .y = y, .w = viewport_w, .h = viewport_h };
}

pub const Texture = struct {
    alloc: std.mem.Allocator,
    ptr: ?*c.SDL_GPUTexture = null,
    width: u32 = 0,
    height: u32 = 0,
    format: c.SDL_GPUTextureFormat = c.SDL_GPU_TEXTUREFORMAT_INVALID,
    num_levels: u32 = 1,
    refcount: u32 = 0,

    pub fn init(
        alloc: std.mem.Allocator,
        ptr: ?*c.SDL_GPUTexture,
        w: u32,
        h: u32,
        format: c.SDL_GPUTextureFormat,
        num_levels: u32,
    ) !*Texture {
        const obj = try alloc.create(Texture);
        obj.* = .{
            .alloc = alloc,
            .ptr = ptr,
            .width = w,
            .height = h,
            .format = format,
            .num_levels = num_levels,
            .refcount = 1,
        };
        return obj;
    }

    pub fn matches(self: *const Texture, w: u32, h: u32, format: c.SDL_GPUTextureFormat, num_levels: u32) bool {
        return self.ptr != null and
            self.width == w and
            self.height == h and
            self.format == format and
            self.num_levels == num_levels;
    }

    pub fn release(self: *Texture, device: ?*c.SDL_GPUDevice) void {
        if (self.refcount > 0) self.refcount -= 1;
        if (self.refcount == 0) {
            if (self.ptr != null) {
                c.SDL_ReleaseGPUTexture(device, self.ptr);
                self.ptr = null;
            }
            self.alloc.destroy(self);
        }
    }

    pub fn ref(self: *Texture) *Texture {
        self.refcount += 1;
        return self;
    }

    /// Returns `[width, height, 1/width, 1/height]` as expected by RetroArch uniforms.
    pub fn sizeVec4(self: Texture) [4]f32 {
        return [_]f32{
            @floatFromInt(self.width),
            @floatFromInt(self.height),
            1.0 / @as(f32, @floatFromInt(self.width)),
            1.0 / @as(f32, @floatFromInt(self.height)),
        };
    }
};

const BuiltinUniforms = struct {
    MVP: [16]f32 = .{
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1,
    },
    OriginalSize: [4]f32 = undefined,
    OutputSize: [4]f32 = undefined,
    SourceSize: [4]f32 = undefined,
    FinalViewportSize: [4]f32 = undefined,
    FrameCount: u32 = 0,
};

const FieldType = union(enum) {
    MVP,
    SourceSize,
    OutputSize,
    OriginalSize,
    FinalViewportSize,
    FrameCount,
    OriginalFPS,
    OriginalAspect,
    OriginalAspectRotated,
    FrameTimeDelta,
    Rotation,
    /// A size uniform for a named alias (e.g. `MyPassSize`).
    SizeVariant: []const u8, // owned
    /// A size uniform for a numbered alias (e.g. `MyPass0Size`).
    SizeVariantWithId: struct { name: []const u8, id: usize }, // name owned
    /// Any other custom parameter.
    Other: []const u8, // owned

    fn deinit(self: FieldType, alloc: std.mem.Allocator) void {
        switch (self) {
            .SizeVariant => |name| alloc.free(name),
            .SizeVariantWithId => |it| alloc.free(it.name),
            .Other => |name| alloc.free(name),
            else => {},
        }
    }
};

const MemberInfo = struct {
    field_type: FieldType,
    offset: u32,
    size: u32,
    param_data: ?[]u8 = null,
};

const UniformBufferLayout = struct {
    size: u32,
    members: std.ArrayList(MemberInfo),

    fn deinit(self: *UniformBufferLayout, alloc: std.mem.Allocator) void {
        for (self.members.items) |member| {
            member.field_type.deinit(alloc);
        }
        self.members.deinit(alloc);
    }
};

const SamplerType = union(enum) {
    Original,
    Source,
    OriginalHistory: usize,
    PassOutput: usize,
    PassFeedback: usize,
    Alias: []const u8, // not owned (points into reflection name)
};

const BindingType = union(enum) {
    push_params: UniformBufferLayout,
    UBO: UniformBufferLayout,
    sampler2D: SamplerType,
};

const BindingInfo = struct {
    name: []const u8, // owned
    binding: u32,
    binding_type: BindingType,
    uniform_payload: []u8,
};

const DescriptorSetInfo = struct {
    set_number: u32,
    bindings: std.ArrayList(BindingInfo),

    fn deinit(self: *DescriptorSetInfo, alloc: std.mem.Allocator) void {
        for (self.bindings.items) |*binding| {
            alloc.free(binding.name);
            if (binding.uniform_payload.len > 0) alloc.free(binding.uniform_payload);
            switch (binding.binding_type) {
                .push_params, .UBO => |*layout| layout.deinit(alloc),
                .sampler2D => {},
            }
        }
        self.bindings.deinit(alloc);
    }
};

const ShaderReflection = struct {
    descriptor_sets: std.ArrayList(DescriptorSetInfo),

    fn deinit(self: *ShaderReflection, alloc: std.mem.Allocator) void {
        for (self.descriptor_sets.items) |*set| {
            set.deinit(alloc);
        }
        self.descriptor_sets.deinit(alloc);
    }
};

/// Look-up texture
const Lut = struct {
    texture: *Texture,
    sampler: ?*c.SDL_GPUSampler,
};

/// Per-pass GPU state
const ShaderPass = struct {
    /// Set to true only after the pass is fully initialized by a worker.
    /// Guards against calling deinit on uninitialized passes.
    initialized: bool = false,
    id: usize,
    pipeline: ?*c.SDL_GPUGraphicsPipeline,
    sampler: ?*c.SDL_GPUSampler,
    output_texture: ?*Texture,
    fb_feedback: ?*Texture,
    scale_params: struct {
        scale_type: ?slangp.ScaleType = null,
        scale_type_x: ?slangp.ScaleType = null,
        scale_type_y: ?slangp.ScaleType = null,
        scale: ?f32 = null,
        scale_x: ?f32 = null,
        scale_y: ?f32 = null,
    },
    wrap_mode: c.SDL_GPUSamplerAddressMode,
    mipmap_input: bool,
    float_framebuffer: bool,
    srgb_framebuffer: bool,
    frame_count_mod: ?u32,
    alias: ?[]const u8, // owned
    feedback_alias: ?[]const u8, // owned
    texture_format: c.SDL_GPUTextureFormat,
    filter: c.SDL_GPUFilter,
    vertex_reflection: ShaderReflection,
    fragment_reflection: ShaderReflection,

    fn deinit(self: *ShaderPass, alloc: std.mem.Allocator, device: ?*c.SDL_GPUDevice) void {
        if (!self.initialized) return;
        c.SDL_ReleaseGPUGraphicsPipeline(device, self.pipeline);
        c.SDL_ReleaseGPUSampler(device, self.sampler);
        if (self.output_texture) |t| t.release(device);
        if (self.fb_feedback) |t| t.release(device);
        if (self.alias) |a| alloc.free(a);
        if (self.feedback_alias) |a| alloc.free(a);
        self.vertex_reflection.deinit(alloc);
        self.fragment_reflection.deinit(alloc);
    }

    fn calculateOutputSize(
        self: *const ShaderPass,
        input_w: u32,
        input_h: u32,
        viewport_w: u32,
        viewport_h: u32,
    ) struct { w: u32, h: u32 } {
        const scale_type_x = self.scale_params.scale_type_x orelse
            self.scale_params.scale_type orelse .source;
        const sx = self.scale_params.scale_x orelse self.scale_params.scale orelse 1.0;
        const out_w: u32 = switch (scale_type_x) {
            .source => @intFromFloat(@as(f32, @floatFromInt(input_w)) * sx),
            .viewport => @intFromFloat(@as(f32, @floatFromInt(viewport_w)) * sx),
            .absolute => @intFromFloat(sx),
        };

        const scale_type_y = self.scale_params.scale_type_y orelse
            self.scale_params.scale_type orelse .source;
        const sy = self.scale_params.scale_y orelse self.scale_params.scale orelse 1.0;
        const out_h: u32 = switch (scale_type_y) {
            .source => @intFromFloat(@as(f32, @floatFromInt(input_h)) * sy),
            .viewport => @intFromFloat(@as(f32, @floatFromInt(viewport_h)) * sy),
            .absolute => @intFromFloat(sy),
        };

        return .{ .w = out_w, .h = out_h };
    }
};

/// Public metadata for a single shader parameter, used by the UI.
pub const ParamInfo = struct {
    /// Key used to read/write the value (points into pass reflection data).
    name: []const u8,
    /// Human-readable label from `#pragma parameter` (owned by LoadedPreset).
    display_name: []const u8,
    min: f32,
    max: f32,
    step: ?f32,
};

const LoadedPreset = struct {
    passes: []ShaderPass,
    /// Parameter name -> bytes of the f32 value.
    param_data: std.StringHashMap([]u8),
    /// Ordered list of parameter metadata for UI display.
    param_meta: std.ArrayList(ParamInfo) = .empty,
    luts: std.StringHashMap(Lut),

    fn deinit(self: *LoadedPreset, alloc: std.mem.Allocator, device: ?*c.SDL_GPUDevice) void {
        for (self.passes) |*pass| {
            pass.deinit(alloc, device);
        }
        alloc.free(self.passes);

        var pd_it = self.param_data.iterator();
        while (pd_it.next()) |entry| {
            alloc.free(entry.value_ptr.*);
        }
        self.param_data.deinit();

        for (self.param_meta.items) |info| alloc.free(info.display_name);
        self.param_meta.deinit(alloc);

        var lut_it = self.luts.iterator();
        while (lut_it.next()) |entry| {
            alloc.free(entry.key_ptr.*);
            entry.value_ptr.texture.release(device);
            if (entry.value_ptr.sampler) |s| c.SDL_ReleaseGPUSampler(device, s);
        }
        self.luts.deinit();
    }
};

fn parseTextureFormat(fmt: []const u8) c.SDL_GPUTextureFormat {
    if (std.mem.eql(u8, fmt, "R8_UNORM")) return c.SDL_GPU_TEXTUREFORMAT_R8_UNORM;
    if (std.mem.eql(u8, fmt, "R8G8B8A8_UNORM")) return c.SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM;
    if (std.mem.eql(u8, fmt, "R8G8B8A8_SRGB")) return c.SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM_SRGB;
    if (std.mem.eql(u8, fmt, "R16_SFLOAT")) return c.SDL_GPU_TEXTUREFORMAT_R16_FLOAT;
    if (std.mem.eql(u8, fmt, "R16G16_SFLOAT")) return c.SDL_GPU_TEXTUREFORMAT_R16G16_FLOAT;
    if (std.mem.eql(u8, fmt, "R16G16B16A16_SFLOAT")) return c.SDL_GPU_TEXTUREFORMAT_R16G16B16A16_FLOAT;
    if (std.mem.eql(u8, fmt, "R32_SFLOAT")) return c.SDL_GPU_TEXTUREFORMAT_R32_FLOAT;
    if (std.mem.eql(u8, fmt, "R32G32_SFLOAT")) return c.SDL_GPU_TEXTUREFORMAT_R32G32_FLOAT;
    if (std.mem.eql(u8, fmt, "R32G32B32A32_SFLOAT")) return c.SDL_GPU_TEXTUREFORMAT_R32G32B32A32_FLOAT;
    if (std.mem.eql(u8, fmt, "A2B10G10R10_UNORM_PACK32")) return c.SDL_GPU_TEXTUREFORMAT_R10G10B10A2_UNORM;
    std.log.warn("Unknown texture format '{s}', defaulting to R8G8B8A8_UNORM", .{fmt});
    return c.SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM;
}

fn spvcError(result: c.spvc_result) void {
    switch (result) {
        c.SPVC_ERROR_INVALID_SPIRV => std.debug.panic("SPVC: Invalid SPIRV", .{}),
        c.SPVC_ERROR_UNSUPPORTED_SPIRV => std.debug.panic("SPVC: Unsupported SPIRV", .{}),
        c.SPVC_ERROR_OUT_OF_MEMORY => std.debug.panic("SPVC: Out of memory", .{}),
        c.SPVC_ERROR_INVALID_ARGUMENT => std.debug.panic("SPVC: Invalid argument", .{}),
        else => {},
    }
}

fn getSpirvVersion(vk_version: c_uint) c_uint {
    return switch (vk_version) {
        c.GLSLANG_TARGET_VULKAN_1_0 => c.GLSLANG_TARGET_SPV_1_0,
        c.GLSLANG_TARGET_VULKAN_1_1 => c.GLSLANG_TARGET_SPV_1_3,
        c.GLSLANG_TARGET_VULKAN_1_2 => c.GLSLANG_TARGET_SPV_1_5,
        c.GLSLANG_TARGET_VULKAN_1_3, c.GLSLANG_TARGET_VULKAN_1_4 => c.GLSLANG_TARGET_SPV_1_6,
        else => c.GLSLANG_TARGET_SPV_1_0,
    };
}

fn compileShader(
    alloc: std.mem.Allocator,
    vk_version: c_uint,
    glsl_version: u32,
    source: []const u8,
    stage: parser.ShaderStage,
) ![]u8 {
    const source_z = try alloc.dupeZ(u8, source);
    defer alloc.free(source_z);

    const target_version = vulkan.vk_to_glslang_version(vk_version);
    const glslang_input: c.glslang_input_t = .{
        .client = c.GLSLANG_CLIENT_VULKAN,
        .language = c.GLSLANG_SOURCE_GLSL,
        .stage = if (stage == .Vertex) c.GLSLANG_STAGE_VERTEX else c.GLSLANG_STAGE_FRAGMENT,
        .client_version = target_version,
        .target_language = c.GLSLANG_TARGET_SPV,
        .target_language_version = getSpirvVersion(target_version),
        .code = source_z.ptr,
        .default_version = @intCast(glsl_version),
        .default_profile = c.GLSLANG_NO_PROFILE,
        .messages = c.GLSLANG_MSG_DEFAULT_BIT,
        .resource = c.glslang_default_resource(),
    };

    const glslang_shader = c.glslang_shader_create(&glslang_input);
    defer c.glslang_shader_delete(glslang_shader);

    if (c.glslang_shader_preprocess(glslang_shader, &glslang_input) != 1) {
        std.log.err("GLSL preprocess failed:\n{s}\n{s}", .{
            c.glslang_shader_get_info_log(glslang_shader),
            c.glslang_shader_get_info_debug_log(glslang_shader),
        });
        return error.GLSL_PreprocessFailed;
    }

    const preprocessed_code = c.glslang_shader_get_preprocessed_code(glslang_shader);
    const patched = try parser.patchShaderSource(alloc, std.mem.span(preprocessed_code), stage);
    defer alloc.free(patched);
    const patched_z = try alloc.dupeZ(u8, patched);
    defer alloc.free(patched_z);

    c.glslang_shader_set_preprocessed_code(glslang_shader, patched_z.ptr);

    if (c.glslang_shader_parse(glslang_shader, &glslang_input) != 1) {
        std.log.err("GLSL parse failed:\n{s}", .{c.glslang_shader_get_info_log(glslang_shader)});
        return error.GLSL_ParsingFailed;
    }

    const program = c.glslang_program_create();
    defer c.glslang_program_delete(program);

    c.glslang_program_add_shader(program, glslang_shader);

    if (c.glslang_program_link(program, c.GLSLANG_MSG_SPV_RULES_BIT | c.GLSLANG_MSG_VULKAN_RULES_BIT) != 1) {
        std.log.err("GLSL link failed:\n{s}", .{c.glslang_program_get_info_log(program)});
        return error.GLSL_LinkingFailed;
    }

    if (c.glslang_program_map_io(program) != 1) {
        std.log.err("GLSL IO mapping failed:\n{s}", .{c.glslang_program_get_info_log(program)});
        return error.GLSL_IOMappingFailed;
    }

    var options = [_]c.struct_glslang_spv_options_s{.{
        .generate_debug_info = builtin.mode == .Debug,
        .strip_debug_info = false,
        .disable_optimizer = builtin.mode == .Debug,
        .optimize_size = false,
        .disassemble = false,
        .validate = true,
        .emit_nonsemantic_shader_debug_info = false,
        .emit_nonsemantic_shader_debug_source = false,
        .compile_only = false,
        .optimize_allow_expanded_id_bound = false,
    }};
    c.glslang_program_SPIRV_generate_with_options(program, glslang_input.stage, &options);

    if (c.glslang_program_SPIRV_get_messages(program)) |msg| {
        std.log.debug("SPIRV messages: {s}", .{msg});
    }

    const size = c.glslang_program_SPIRV_get_size(program);
    const ptr = c.glslang_program_SPIRV_get_ptr(program);
    return try alloc.dupe(u8, std.mem.sliceAsBytes(ptr[0..size]));
}

fn reflectResources(
    alloc: std.mem.Allocator,
    resources: c.spvc_resources,
    compiler: c.spvc_compiler,
    resource_type: c.spvc_resource_type,
    reflection: *ShaderReflection,
) !void {
    var resource_list: [*c]const c.spvc_reflected_resource = undefined;
    var count: usize = 0;
    spvcError(c.spvc_resources_get_resource_list_for_type(resources, resource_type, &resource_list, &count));

    for (resource_list[0..count]) |res| {
        const name = try alloc.dupe(u8, std.mem.span(res.name));
        const set = c.spvc_compiler_get_decoration(compiler, res.id, c.SpvDecorationDescriptorSet);
        const binding = c.spvc_compiler_get_decoration(compiler, res.id, c.SpvDecorationBinding);

        var set_info = DescriptorSetInfo{
            .set_number = @intCast(set),
            .bindings = .empty,
        };

        switch (resource_type) {
            c.SPVC_RESOURCE_TYPE_UNIFORM_BUFFER => {
                const struct_type_id = res.base_type_id;
                const struct_type = c.spvc_compiler_get_type_handle(compiler, struct_type_id);

                var block_size: usize = 0;
                spvcError(c.spvc_compiler_get_declared_struct_size(compiler, struct_type, &block_size));

                var layout = UniformBufferLayout{
                    .size = @intCast(block_size),
                    .members = .empty,
                };

                const member_count = c.spvc_type_get_num_member_types(struct_type);
                for (0..member_count) |i| {
                    var offset: u32 = 0;
                    spvcError(c.spvc_compiler_type_struct_member_offset(compiler, struct_type, @intCast(i), &offset));

                    var msize: usize = 0;
                    spvcError(c.spvc_compiler_get_declared_struct_member_size(compiler, struct_type, @intCast(i), &msize));

                    const member_name_raw = c.spvc_compiler_get_member_name(compiler, struct_type_id, @intCast(i));
                    const member_name = try alloc.dupe(u8, std.mem.span(member_name_raw));
                    defer alloc.free(member_name);

                    var field_type: FieldType = classifyFieldName(member_name) orelse
                        .{ .Other = try alloc.dupe(u8, member_name) };

                    // Detect size variants for pass aliases (e.g. `MyPassSize`, `MyPass0Size`)
                    if (field_type == .Other) {
                        if (std.mem.indexOf(u8, member_name, "Size")) |idx| {
                            const var_name = member_name[0..idx];
                            const is_builtin = std.mem.eql(u8, var_name, "Original") or
                                std.mem.eql(u8, var_name, "Source") or
                                std.mem.eql(u8, var_name, "Output") or
                                std.mem.eql(u8, var_name, "Final");
                            if (!is_builtin) {
                                // Free what classifyFieldName set (nothing, we set Other above)
                                alloc.free(field_type.Other);
                                const suffix = member_name[idx + 4 ..];
                                if (suffix.len > 0 and std.ascii.isDigit(suffix[0])) {
                                    field_type = .{ .SizeVariantWithId = .{
                                        .name = try alloc.dupe(u8, var_name),
                                        .id = try std.fmt.parseInt(usize, suffix[0..1], 10),
                                    } };
                                } else {
                                    field_type = .{ .SizeVariant = try alloc.dupe(u8, var_name) };
                                }
                            }
                        }
                    }

                    try layout.members.append(alloc, .{
                        .field_type = field_type,
                        .offset = offset,
                        .size = @intCast(msize),
                    });
                }

                const uniform_payload = try alloc.alloc(u8, block_size);
                var payload_transferred = false;
                defer if (!payload_transferred) alloc.free(uniform_payload);

                try set_info.bindings.append(alloc, .{
                    .name = name,
                    .binding = @intCast(binding),
                    .binding_type = if (std.mem.eql(u8, name, "Push"))
                        .{ .push_params = layout }
                    else
                        .{ .UBO = layout },
                    .uniform_payload = uniform_payload,
                });
                payload_transferred = true;
            },

            c.SPVC_RESOURCE_TYPE_SAMPLED_IMAGE => {
                const sampler_type: SamplerType = classifySamplerName(name);
                try set_info.bindings.append(alloc, .{
                    .name = name,
                    .binding = @intCast(binding),
                    .binding_type = .{ .sampler2D = sampler_type },
                    .uniform_payload = &.{},
                });
            },
            else => {
                alloc.free(name);
                continue;
            },
        }

        try reflection.descriptor_sets.append(alloc, set_info);
    }
}

fn classifyFieldName(name: []const u8) ?FieldType {
    if (std.mem.eql(u8, name, "MVP")) return .MVP;
    if (std.mem.eql(u8, name, "Rotation")) return .Rotation;
    if (std.mem.eql(u8, name, "SourceSize")) return .SourceSize;
    if (std.mem.eql(u8, name, "OutputSize")) return .OutputSize;
    if (std.mem.eql(u8, name, "OriginalSize")) return .OriginalSize;
    if (std.mem.eql(u8, name, "FinalViewportSize")) return .FinalViewportSize;
    if (std.mem.eql(u8, name, "FrameCount")) return .FrameCount;
    if (std.mem.eql(u8, name, "FrameTimeDelta")) return .FrameTimeDelta;
    if (std.mem.eql(u8, name, "OriginalFPS")) return .OriginalFPS;
    if (std.mem.eql(u8, name, "OriginalAspect")) return .OriginalAspect;
    if (std.mem.eql(u8, name, "OriginalAspectRotated")) return .OriginalAspectRotated;
    return null;
}

fn classifySamplerName(name: []const u8) SamplerType {
    if (std.mem.eql(u8, name, "Original")) return .Original;
    if (std.mem.eql(u8, name, "Source")) return .Source;
    if (std.mem.startsWith(u8, name, "PassFeedback")) {
        const id = slangp.get_field_pass_id(name) catch return .{ .Alias = name };
        return .{ .PassFeedback = id };
    }
    if (std.mem.startsWith(u8, name, "PassOutput")) {
        const id = slangp.get_field_pass_id(name) catch return .{ .Alias = name };
        return .{ .PassOutput = id };
    }
    if (std.mem.startsWith(u8, name, "OriginalHistory")) {
        const id = slangp.get_field_pass_id(name) catch return .{ .Alias = name };
        return .{ .OriginalHistory = id };
    }
    return .{ .Alias = name };
}

fn reflectShaderInfo(alloc: std.mem.Allocator, spirv: []const u8) !ShaderReflection {
    var reflection = ShaderReflection{ .descriptor_sets = .empty };
    errdefer reflection.deinit(alloc);

    var context: c.spvc_context = undefined;
    spvcError(c.spvc_context_create(&context));
    defer c.spvc_context_destroy(context);

    var parsed_ir: c.spvc_parsed_ir = undefined;
    spvcError(c.spvc_context_parse_spirv(
        context,
        @ptrCast(@alignCast(spirv.ptr)),
        spirv.len / @sizeOf(u32),
        &parsed_ir,
    ));

    var compiler: c.spvc_compiler = undefined;
    spvcError(c.spvc_context_create_compiler(
        context,
        c.SPVC_BACKEND_NONE,
        parsed_ir,
        c.SPVC_CAPTURE_MODE_TAKE_OWNERSHIP,
        &compiler,
    ));

    var resources: c.spvc_resources = undefined;
    spvcError(c.spvc_compiler_create_shader_resources(compiler, &resources));

    try reflectResources(alloc, resources, compiler, c.SPVC_RESOURCE_TYPE_UNIFORM_BUFFER, &reflection);
    try reflectResources(alloc, resources, compiler, c.SPVC_RESOURCE_TYPE_SAMPLED_IMAGE, &reflection);

    return reflection;
}

fn countBindings(reflection: *const ShaderReflection) struct { samplers: u32, ubos: u32 } {
    var samplers: u32 = 0;
    var ubos: u32 = 0;
    for (reflection.descriptor_sets.items) |set| {
        for (set.bindings.items) |binding| {
            switch (binding.binding_type) {
                .UBO, .push_params => ubos += 1,
                .sampler2D => samplers += 1,
            }
        }
    }
    return .{ .samplers = samplers, .ubos = ubos };
}

fn createGPUShader(
    device: ?*c.SDL_GPUDevice,
    spirv: []const u8,
    stage: c.SDL_GPUShaderStage,
    reflection: *const ShaderReflection,
) ?*c.SDL_GPUShader {
    const counts = countBindings(reflection);
    return sdlError(c.SDL_CreateGPUShader(device, &.{
        .code_size = spirv.len,
        .code = spirv.ptr,
        .entrypoint = "main",
        .format = c.SDL_GPU_SHADERFORMAT_SPIRV,
        .stage = stage,
        .num_samplers = counts.samplers,
        .num_uniform_buffers = counts.ubos,
        .num_storage_buffers = 0,
        .num_storage_textures = 0,
    }));
}

const CompilePassResult = struct {
    pipeline: ?*c.SDL_GPUGraphicsPipeline,
    params: []Param, // caller owns
    alias: ?[]const u8, // caller owns
    texture_format: c.SDL_GPUTextureFormat,
    reflection: struct {
        vertex: ShaderReflection,
        fragment: ShaderReflection,
    },
};

const Param = struct {
    name: []const u8, // borrowed from reflection (not owned here)
    display_name: []const u8, // owned (duped from ShaderParam.option_name)
    value: f32,
    min: f32,
    max: f32,
    step: ?f32,
};

/// Returns SPIR-V for both stages, consulting the cache when available.
fn getOrCompileSpirv(
    alloc: std.mem.Allocator,
    vk_version: c_uint,
    shader: *const parser.ParsedShader,
    shader_cache: *ShaderCache,
    pass_path: []const u8,
    pass_id: usize,
) !SpirvPair {
    const key = ShaderCache.computeKey(
        @intCast(vk_version),
        shader.version,
        shader.vertex,
        shader.fragment,
    );

    if (shader_cache.lookup(alloc, key)) |pair| {
        std.log.info("Loading shader pass {} from cache", .{pass_id});
        return pair;
    }

    std.log.info("Compiling shader pass {}: {s}", .{ pass_id, pass_path });
    const vert = try compileShader(alloc, vk_version, shader.version, shader.vertex, .Vertex);
    const frag = try compileShader(alloc, vk_version, shader.version, shader.fragment, .Fragment);

    try shader_cache.store(key, vert, frag);

    return .{ .vert = vert, .frag = frag };
}

fn compileAndCreatePipeline(
    alloc: std.mem.Allocator,
    io: std.Io,
    device: ?*c.SDL_GPUDevice,
    vk_version: c_uint,
    path: []const u8,
    pass_id: usize,
    texture_format: c.SDL_GPUTextureFormat,
    shader_cache: *ShaderCache,
) !CompilePassResult {
    const embedded_source = builtin_shaders.sourceForPath(path);
    const raw_source = if (embedded_source) |source| source else blk: {
        break :blk try std.Io.Dir.cwd().readFileAlloc(io, path, alloc, .limited(1024 * 1024));
    };
    defer if (embedded_source == null) alloc.free(raw_source);

    var shader = try parser.parseShader(alloc, io, std.fs.path.dirname(path).?, raw_source);
    defer shader.deinit(alloc);

    const spirv = try getOrCompileSpirv(alloc, vk_version, &shader, shader_cache, path, pass_id);
    defer spirv.deinit(alloc);

    const vert_reflection = try reflectShaderInfo(alloc, spirv.vert);
    const frag_reflection = try reflectShaderInfo(alloc, spirv.frag);

    // Collect shader parameters defined via `#pragma parameter`
    var params = std.ArrayList(Param).empty;
    if (shader.config) |config| {
        for (vert_reflection.descriptor_sets.items) |set_info| {
            for (set_info.bindings.items) |binding| {
                switch (binding.binding_type) {
                    .push_params, .UBO => |layout| {
                        for (layout.members.items) |member| {
                            if (member.field_type == .Other) {
                                const pname = member.field_type.Other;
                                const field = config.getPtr(pname) orelse blk: {
                                    std.log.warn("UBO field '{s}' has no #pragma parameter, defaulting to 0", .{pname});
                                    break :blk &parser.ShaderParam{ .option_name = pname };
                                };
                                try params.append(alloc, .{
                                    .name = pname,
                                    .display_name = try alloc.dupe(u8, field.option_name),
                                    .value = field.initial,
                                    .min = field.min,
                                    .max = field.max,
                                    .step = field.step,
                                });
                            }
                        }
                    },
                    .sampler2D => {},
                }
            }
        }
    }

    const vert_shader = createGPUShader(device, spirv.vert, c.SDL_GPU_SHADERSTAGE_VERTEX, &vert_reflection);
    const frag_shader = createGPUShader(device, spirv.frag, c.SDL_GPU_SHADERSTAGE_FRAGMENT, &frag_reflection);
    defer {
        c.SDL_ReleaseGPUShader(device, vert_shader);
        c.SDL_ReleaseGPUShader(device, frag_shader);
    }

    const format = if (shader.texture_format) |fmt_name|
        parseTextureFormat(fmt_name)
    else
        texture_format;

    const color_desc = c.SDL_GPUColorTargetDescription{
        .format = format,
        .blend_state = .{ .enable_blend = false },
    };
    const vertex_attrs = [_]c.SDL_GPUVertexAttribute{
        .{ .location = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3, .offset = 0 },
        .{ .location = 1, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, .offset = 12 },
    };
    const vb_desc = c.SDL_GPUVertexBufferDescription{
        .slot = 0,
        .pitch = @sizeOf(Vertex),
        .input_rate = c.SDL_GPU_VERTEXINPUTRATE_VERTEX,
    };
    const pipeline = sdlError(c.SDL_CreateGPUGraphicsPipeline(device, &.{
        .vertex_shader = vert_shader,
        .fragment_shader = frag_shader,
        .vertex_input_state = .{
            .vertex_buffer_descriptions = &vb_desc,
            .num_vertex_buffers = 1,
            .vertex_attributes = &vertex_attrs,
            .num_vertex_attributes = vertex_attrs.len,
        },
        .primitive_type = c.SDL_GPU_PRIMITIVETYPE_TRIANGLELIST,
        .target_info = .{ .color_target_descriptions = &color_desc, .num_color_targets = 1 },
    }));

    const alias: ?[]u8 = if (shader.alias) |a| try alloc.dupe(u8, a) else null;

    return .{
        .pipeline = pipeline,
        .params = try params.toOwnedSlice(alloc),
        .alias = alias,
        .texture_format = format,
        .reflection = .{
            .fragment = frag_reflection,
            .vertex = vert_reflection,
        },
    };
}

const WorkerArgs = struct {
    alloc: std.mem.Allocator,
    io: std.Io,
    preset: *LoadedPreset,
    shader_dir: []const u8,
    pass: *slangp.ShaderPass,
    device: ?*c.SDL_GPUDevice,
    vk_version: c_uint,
    swapchain_format: c.SDL_GPUTextureFormat,
    initial_values: *const std.StringHashMap(slangp.TypeUnion),
    mutex: *std.Io.Mutex,
    had_error: *bool,
    compile_progress: *std.atomic.Value(u32),
    shader_cache: *ShaderCache,
};

fn compilePassWorker(args: WorkerArgs) void {
    compilePassWorkerInner(args) catch |err| {
        std.log.err("Failed to compile shader pass {}: {s}", .{ args.pass.id, @errorName(err) });
        args.mutex.lockUncancelable(args.io);
        args.had_error.* = true;
        args.mutex.unlock(args.io);
    };
}

fn compilePassWorkerInner(args: WorkerArgs) !void {
    const pass_path = if (std.mem.eql(u8, args.shader_dir, builtin_shaders.border_shader_dir))
        try builtin_shaders.joinBorderShaderPath(args.alloc, args.pass.path)
    else
        try std.fs.path.resolve(args.alloc, &.{ args.shader_dir, args.pass.path });
    defer args.alloc.free(pass_path);

    const target_format: c.SDL_GPUTextureFormat = if (args.pass.params.float_framebuffer orelse false)
        c.SDL_GPU_TEXTUREFORMAT_R16G16B16A16_FLOAT
    else if (args.pass.params.srgb_framebuffer orelse false)
        c.SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM_SRGB
    else
        args.swapchain_format;

    const result = try compileAndCreatePipeline(
        args.alloc,
        args.io,
        args.device,
        args.vk_version,
        pass_path,
        args.pass.id,
        target_format,
        args.shader_cache,
    );
    defer args.alloc.free(result.params);

    const wrap_mode: c_uint = if (args.pass.params.wrap_mode) |wm| switch (wm) {
        .clamp_to_border => c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
        .clamp_to_edge => c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
        .repeat => c.SDL_GPU_SAMPLERADDRESSMODE_REPEAT,
        .mirrored_repeat => c.SDL_GPU_SAMPLERADDRESSMODE_MIRRORED_REPEAT,
    } else c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE;

    const filter: c.SDL_GPUFilter = if (args.pass.params.filter_linear orelse false)
        c.SDL_GPU_FILTER_LINEAR
    else
        c.SDL_GPU_FILTER_NEAREST;

    const sampler = sdlError(c.SDL_CreateGPUSampler(args.device, &.{
        .min_filter = filter,
        .mag_filter = filter,
        .mipmap_mode = if (args.pass.params.mipmap_input orelse false)
            c.SDL_GPU_SAMPLERMIPMAPMODE_LINEAR
        else
            c.SDL_GPU_SAMPLERMIPMAPMODE_NEAREST,
        .address_mode_u = wrap_mode,
        .address_mode_v = wrap_mode,
        .address_mode_w = wrap_mode,
    }));

    const alias = result.alias orelse if (args.pass.params.alias) |a|
        try args.alloc.dupe(u8, a)
    else
        null;
    const feedback_alias = if (alias) |a|
        try std.fmt.allocPrint(args.alloc, "{s}Feedback", .{a})
    else
        null;

    args.mutex.lockUncancelable(args.io);
    defer args.mutex.unlock(args.io);

    for (result.params) |param| {
        const bytes = if (args.initial_values.get(param.name)) |initial|
            try args.alloc.dupe(u8, initial.bytes())
        else
            try args.alloc.dupe(u8, std.mem.asBytes(&param.value));

        if (try args.preset.param_data.fetchPut(param.name, bytes)) |old| {
            // Parameter was already registered by another pass — just update
            // the value bytes and discard the duplicate display_name.
            args.alloc.free(old.value);
            args.alloc.free(param.display_name);
        } else {
            // First time we see this parameter — record its full metadata.
            try args.preset.param_meta.append(args.alloc, .{
                .name = param.name,
                .display_name = param.display_name, // ownership transferred
                .min = param.min,
                .max = param.max,
                .step = param.step,
            });
        }
    }

    args.preset.passes[args.pass.id] = .{
        .initialized = true,
        .id = args.pass.id,
        .pipeline = result.pipeline,
        .sampler = sampler,
        .output_texture = null,
        .fb_feedback = null,
        .vertex_reflection = result.reflection.vertex,
        .fragment_reflection = result.reflection.fragment,
        .scale_params = .{
            .scale_type = args.pass.params.scale_type,
            .scale_type_x = args.pass.params.scale_type_x,
            .scale_type_y = args.pass.params.scale_type_y,
            .scale = args.pass.params.scale,
            .scale_x = args.pass.params.scale_x,
            .scale_y = args.pass.params.scale_y,
        },
        .wrap_mode = wrap_mode,
        .mipmap_input = args.pass.params.mipmap_input orelse false,
        .float_framebuffer = args.pass.params.float_framebuffer orelse false,
        .srgb_framebuffer = args.pass.params.srgb_framebuffer orelse false,
        .frame_count_mod = args.pass.params.frame_count_mod,
        .alias = alias,
        .feedback_alias = feedback_alias,
        .texture_format = result.texture_format,
        .filter = filter,
    };
    _ = args.compile_progress.fetchAdd(1, .monotonic);
}

/// Holds a parsed `.slangp` preset file before GPU compilation begins.
/// `content` must outlive `shader_config` because `ShaderPass.path` values
/// are slices into `content` (not separately allocated).
/// `dir` is an owned copy of the preset's containing directory path.
const ParsedPreset = struct {
    content: []u8,
    shader_config: slangp.ShaderConfig,
    dir: []u8,

    fn deinit(self: *ParsedPreset, alloc: std.mem.Allocator) void {
        self.shader_config.deinit(alloc);
        alloc.free(self.dir);
        alloc.free(self.content);
    }
};

/// Read and parse a `.slangp` preset file.
fn parsePresetFile(alloc: std.mem.Allocator, io: std.Io, path: []const u8) !ParsedPreset {
    if (builtin_shaders.sourceForPath(path)) |source| {
        const content = try alloc.dupe(u8, source);
        errdefer alloc.free(content);

        const dir = try alloc.dupe(u8, std.fs.path.dirname(path) orelse builtin_shaders.border_shader_dir);
        errdefer alloc.free(dir);

        const shader_config = try slangp.parse_slangp(alloc, content);
        return .{ .content = content, .shader_config = shader_config, .dir = dir };
    }

    const full_path = std.Io.Dir.cwd().realPathFileAlloc(io, path, alloc) catch blk: {
        if (std.fs.path.isAbsolute(path)) break :blk try alloc.dupeZ(u8, path);

        const cwd = try std.process.currentPathAlloc(io, alloc);
        defer alloc.free(cwd);
        const resolved = try std.fs.path.resolve(alloc, &.{ cwd, path });
        defer alloc.free(resolved);

        break :blk try alloc.dupeZ(u8, resolved);
    };
    defer alloc.free(full_path);
    const dir = try alloc.dupe(u8, std.fs.path.dirname(full_path).?);
    errdefer alloc.free(dir);

    const content = try std.Io.Dir.cwd().readFileAlloc(io, full_path, alloc, .limited(1024 * 1024));
    errdefer alloc.free(content);

    const shader_config = try slangp.parse_slangp(alloc, content);
    return .{ .content = content, .shader_config = shader_config, .dir = dir };
}

/// Load LUT textures and compile all shader passes from an already-parsed preset.
fn compilePreset(
    alloc: std.mem.Allocator,
    io: std.Io,
    parsed: *ParsedPreset,
    vk_version: c_uint,
    device: ?*c.SDL_GPUDevice,
    swapchain_format: c.SDL_GPUTextureFormat,
    progress: *std.atomic.Value(u32),
    shader_cache: *ShaderCache,
) !LoadedPreset {
    const dir = parsed.dir;
    const shader_config = &parsed.shader_config;

    // Load LUT textures
    var luts: std.StringHashMap(Lut) = .init(alloc);
    errdefer {
        var it = luts.iterator();
        while (it.next()) |entry| {
            alloc.free(entry.key_ptr.*);
            entry.value_ptr.texture.release(device);
            if (entry.value_ptr.sampler) |s| c.SDL_ReleaseGPUSampler(device, s);
        }
        luts.deinit();
    }

    var tex_it = shader_config.textures.iterator();
    while (tex_it.next()) |entry| {
        const tex_path = try std.fs.path.resolve(alloc, &.{ dir, entry.value_ptr.path });
        defer alloc.free(tex_path);
        std.log.info("Loading LUT '{s}': {s}", .{ entry.key_ptr.*, tex_path });

        const tex_path_z = try alloc.dupeZ(u8, tex_path);
        defer alloc.free(tex_path_z);

        var surface = c.SDL_LoadPNG(tex_path_z.ptr) orelse {
            std.log.err("Failed to decode LUT PNG '{s}' ({s}): {s}", .{ entry.key_ptr.*, tex_path, c.SDL_GetError() });
            return error.FailedToLoadLutTexture;
        };

        defer c.SDL_DestroySurface(surface);

        var texture_fmt = c.SDL_GetGPUTextureFormatFromPixelFormat(surface.*.format);
        if (texture_fmt == c.SDL_GPU_TEXTUREFORMAT_INVALID) {
            const new_surface = sdlError(c.SDL_ConvertSurface(surface, c.SDL_PIXELFORMAT_RGBA32));
            c.SDL_DestroySurface(surface);
            surface = new_surface;
            texture_fmt = c.SDL_GetGPUTextureFormatFromPixelFormat(surface.*.format);
        }

        const w: u32 = @intCast(surface.*.w);
        const h: u32 = @intCast(surface.*.h);
        const gpu_texture = sdlError(c.SDL_CreateGPUTexture(device, &.{
            .type = c.SDL_GPU_TEXTURETYPE_2D,
            .format = texture_fmt,
            .width = w,
            .height = h,
            .usage = c.SDL_GPU_TEXTUREUSAGE_SAMPLER | c.SDL_GPU_TEXTUREUSAGE_COLOR_TARGET,
            .layer_count_or_depth = 1,
            .num_levels = 1,
            .sample_count = c.SDL_GPU_SAMPLECOUNT_1,
        }));

        const size: u32 = @intCast(surface.*.h * surface.*.pitch);
        const tb = sdlError(c.SDL_CreateGPUTransferBuffer(
            device,
            &.{ .size = size, .usage = c.SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD },
        ));
        _ = c.SDL_memcpy(c.SDL_MapGPUTransferBuffer(device, tb, false), @ptrCast(surface.*.pixels), size);
        c.SDL_UnmapGPUTransferBuffer(device, tb);
        const cmd = sdlError(c.SDL_AcquireGPUCommandBuffer(device));
        const copy = sdlError(c.SDL_BeginGPUCopyPass(cmd));
        c.SDL_UploadToGPUTexture(
            copy,
            &.{ .transfer_buffer = tb, .pixels_per_row = w, .rows_per_layer = h },
            &.{ .texture = gpu_texture, .w = w, .h = h, .d = 1 },
            false,
        );
        c.SDL_EndGPUCopyPass(copy);
        sdlError(c.SDL_SubmitGPUCommandBuffer(cmd));
        c.SDL_ReleaseGPUTransferBuffer(device, tb);

        const sampler: ?*c.SDL_GPUSampler = if (entry.value_ptr.linear)
            sdlError(c.SDL_CreateGPUSampler(device, &.{
                .min_filter = c.SDL_GPU_FILTER_LINEAR,
                .mag_filter = c.SDL_GPU_FILTER_LINEAR,
                .mipmap_mode = c.SDL_GPU_SAMPLERMIPMAPMODE_NEAREST,
                .address_mode_u = c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
                .address_mode_v = c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
                .address_mode_w = c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
            }))
        else
            null;

        try luts.put(
            try alloc.dupe(u8, entry.key_ptr.*),
            .{ .texture = try Texture.init(alloc, gpu_texture, w, h, texture_fmt, 1), .sampler = sampler },
        );
    }

    var preset = LoadedPreset{
        .passes = try alloc.alloc(ShaderPass, shader_config.total_passes),
        .param_data = .init(alloc),
        .luts = luts.move(),
    };
    errdefer preset.deinit(alloc, device);

    // Zero-initialize passes so deinit is safe even for passes that never run.
    for (preset.passes) |*p| p.* = .{
        .initialized = false,
        .id = 0,
        .pipeline = null,
        .sampler = null,
        .output_texture = null,
        .fb_feedback = null,
        .scale_params = .{},
        .wrap_mode = c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
        .mipmap_input = false,
        .float_framebuffer = false,
        .srgb_framebuffer = false,
        .frame_count_mod = null,
        .alias = null,
        .feedback_alias = null,
        .texture_format = c.SDL_GPU_TEXTUREFORMAT_INVALID,
        .filter = c.SDL_GPU_FILTER_NEAREST,
        .vertex_reflection = undefined,
        .fragment_reflection = undefined,
    };

    var pool: ThreadPool = undefined;
    try pool.init(.{ .allocator = alloc, .io = io, .n_jobs = std.Thread.getCpuCount() catch 4 });
    defer pool.deinit();

    var mutex: std.Io.Mutex = .init;
    var wg = ThreadPool.WaitGroup.init(io);
    var had_error: bool = false;
    for (shader_config.passes) |*pass| {
        pool.spawnWg(&wg, compilePassWorker, .{WorkerArgs{
            .alloc = alloc,
            .io = io,
            .preset = &preset,
            .shader_dir = dir,
            .pass = pass,
            .device = device,
            .vk_version = vk_version,
            .swapchain_format = swapchain_format,
            .initial_values = &shader_config.shader_params_initial_values,
            .mutex = &mutex,
            .had_error = &had_error,
            .compile_progress = progress,
            .shader_cache = shader_cache,
        }});
    }
    wg.wait();

    if (had_error) return error.ShaderCompilationFailed;
    resolveUniformParamRefs(&preset);

    return preset;
}

fn loadPresetFile(
    alloc: std.mem.Allocator,
    io: std.Io,
    path: []const u8,
    vk_version: c_uint,
    device: ?*c.SDL_GPUDevice,
    swapchain_format: c.SDL_GPUTextureFormat,
    progress: *std.atomic.Value(u32),
    shader_cache: *ShaderCache,
) !LoadedPreset {
    var parsed = try parsePresetFile(alloc, io, path);
    defer parsed.deinit(alloc);
    return compilePreset(alloc, io, &parsed, vk_version, device, swapchain_format, progress, shader_cache);
}

fn resolveUniformLayoutParamRefs(preset: *LoadedPreset, layout: *UniformBufferLayout) void {
    for (layout.members.items) |*member| {
        member.param_data = switch (member.field_type) {
            .Other => |name| preset.param_data.get(name),
            else => null,
        };
    }
}

fn resolveReflectionParamRefs(preset: *LoadedPreset, reflection: *ShaderReflection) void {
    for (reflection.descriptor_sets.items) |*set_info| {
        for (set_info.bindings.items) |*binding| {
            switch (binding.binding_type) {
                .push_params, .UBO => |*layout| resolveUniformLayoutParamRefs(preset, layout),
                .sampler2D => {},
            }
        }
    }
}

fn resolveUniformParamRefs(preset: *LoadedPreset) void {
    for (preset.passes) |*pass| {
        if (!pass.initialized) continue;
        resolveReflectionParamRefs(preset, &pass.vertex_reflection);
        resolveReflectionParamRefs(preset, &pass.fragment_reflection);
    }
}

fn prepareUniformPayload(
    payload: []u8,
    preset: *const LoadedPreset,
    layout: *const UniformBufferLayout,
    source_tex: *Texture,
    original_tex: *Texture,
    uniforms: *const BuiltinUniforms,
    aliases: *const std.StringHashMap(*Texture),
    history: []*Texture,
    pass_feedback: []const ?*Texture,
    pass_output: []const ?*Texture,
) []u8 {
    const payload_size: usize = @intCast(layout.size);
    std.debug.assert(payload.len >= payload_size);
    const out = payload[0..payload_size];

    for (layout.members.items) |member| {
        const dest = out[member.offset .. member.offset + member.size];
        const src: []const u8 = switch (member.field_type) {
            .MVP => std.mem.asBytes(&uniforms.MVP),
            .OriginalSize => std.mem.asBytes(&uniforms.OriginalSize),
            .SourceSize => std.mem.asBytes(&uniforms.SourceSize),
            .OutputSize => std.mem.asBytes(&uniforms.OutputSize),
            .FinalViewportSize => std.mem.asBytes(&uniforms.FinalViewportSize),
            .FrameCount => std.mem.asBytes(&uniforms.FrameCount),
            .Other => |name| member.param_data orelse preset.param_data.get(name) orelse &[_]u8{0} ** 4,
            .SizeVariant => |name| blk: {
                const tex = aliases.get(name) orelse source_tex;
                break :blk std.mem.asBytes(&tex.sizeVec4());
            },
            .SizeVariantWithId => |field| blk: {
                if (std.mem.eql(u8, field.name, "PassFeedback")) {
                    const tex = passTextureAt(pass_feedback, field.id) orelse source_tex;
                    break :blk std.mem.asBytes(&tex.sizeVec4());
                } else if (std.mem.eql(u8, field.name, "OriginalHistory")) {
                    const tex = if (field.id == 0) original_tex else history[field.id - 1];
                    break :blk std.mem.asBytes(&tex.sizeVec4());
                } else if (std.mem.eql(u8, field.name, "PassOutput")) {
                    const tex = passTextureAt(pass_output, field.id) orelse source_tex;
                    break :blk std.mem.asBytes(&tex.sizeVec4());
                } else {
                    break :blk &[_]u8{0} ** 16;
                }
            },
            else => &[_]u8{0} ** 16,
        };
        @memcpy(dest, src[0..@min(src.len, dest.len)]);
    }
    return out;
}

fn bindShaderResources(
    rp: ?*c.SDL_GPURenderPass,
    cmd: ?*c.SDL_GPUCommandBuffer,
    pass: *ShaderPass,
    original_tex: *Texture,
    source_tex: *Texture,
    uniforms: *const BuiltinUniforms,
    preset: *const LoadedPreset,
    aliases: *const std.StringHashMap(*Texture),
    history: []*Texture,
    pass_feedback: []const ?*Texture,
    pass_output: []const ?*Texture,
) !void {
    for (pass.vertex_reflection.descriptor_sets.items) |*set_info| {
        for (set_info.bindings.items) |*binding| {
            switch (binding.binding_type) {
                .push_params, .UBO => |*layout| {
                    const payload = prepareUniformPayload(
                        binding.uniform_payload,
                        preset,
                        layout,
                        source_tex,
                        original_tex,
                        uniforms,
                        aliases,
                        history,
                        pass_feedback,
                        pass_output,
                    );
                    c.SDL_PushGPUVertexUniformData(cmd, binding.binding, @ptrCast(payload), @intCast(layout.size));
                },
                .sampler2D => {},
            }
        }
    }

    var lut_sampler: ?*c.SDL_GPUSampler = null;
    for (pass.fragment_reflection.descriptor_sets.items) |*set_info| {
        for (set_info.bindings.items) |*binding| {
            switch (binding.binding_type) {
                .push_params, .UBO => |*layout| {
                    const payload = prepareUniformPayload(
                        binding.uniform_payload,
                        preset,
                        layout,
                        source_tex,
                        original_tex,
                        uniforms,
                        aliases,
                        history,
                        pass_feedback,
                        pass_output,
                    );
                    c.SDL_PushGPUFragmentUniformData(cmd, binding.binding, @ptrCast(payload), @intCast(layout.size));
                },
                .sampler2D => |sampler_type| {
                    const tex: *Texture = switch (sampler_type) {
                        .Original => original_tex,
                        .Source => source_tex,
                        .OriginalHistory => |id| if (id == 0) original_tex else history[id - 1],
                        .PassFeedback => |id| passTextureAt(pass_feedback, id) orelse source_tex,
                        .PassOutput => |id| passTextureAt(pass_output, id) orelse source_tex,
                        .Alias => |alias| blk: {
                            if (aliases.get(alias)) |t| break :blk t;
                            if (preset.luts.get(alias)) |lut| {
                                lut_sampler = lut.sampler;
                                break :blk lut.texture;
                            }
                            std.log.warn("No texture for alias '{s}', using source", .{alias});
                            break :blk source_tex;
                        },
                    };
                    c.SDL_BindGPUFragmentSamplers(
                        rp,
                        binding.binding,
                        &.{ .texture = tex.ptr, .sampler = lut_sampler orelse pass.sampler },
                        1,
                    );
                    lut_sampler = null;
                },
            }
        }
    }
}

fn passTextureAt(slots: []const ?*Texture, id: usize) ?*Texture {
    if (id >= slots.len) return null;
    return slots[id];
}

fn setRetainedPassTextureSlot(slot: *?*Texture, tex: ?*Texture, device: ?*c.SDL_GPUDevice) void {
    if (tex) |new_tex| {
        if (slot.*) |old_tex| {
            if (old_tex == new_tex) return;
            old_tex.release(device);
        }
        slot.* = new_tex.ref();
    } else {
        if (slot.*) |old_tex| old_tex.release(device);
        slot.* = null;
    }
}

fn clearPassTextureSlots(device: ?*c.SDL_GPUDevice, slots: []?*Texture) void {
    for (slots) |*slot| {
        if (slot.*) |tex| {
            tex.release(device);
            slot.* = null;
        }
    }
}

fn calcMipLevels(w: u32, h: u32) u32 {
    var size = @max(w, h);
    var levels: u32 = 0;
    while (size != 0) : (levels += 1) size >>= 1;
    return levels;
}

fn createRenderTarget(
    alloc: std.mem.Allocator,
    device: ?*c.SDL_GPUDevice,
    slot: *?*Texture,
    w: u32,
    h: u32,
    format: c.SDL_GPUTextureFormat,
    num_levels: u32,
) !*Texture {
    if (slot.*) |tex| {
        if (tex.matches(w, h, format, num_levels)) return tex;
        tex.release(device);
        slot.* = null;
    }

    const gpu_tex = sdlError(c.SDL_CreateGPUTexture(device, &.{
        .type = c.SDL_GPU_TEXTURETYPE_2D,
        .format = format,
        .usage = c.SDL_GPU_TEXTUREUSAGE_SAMPLER | c.SDL_GPU_TEXTUREUSAGE_COLOR_TARGET,
        .width = w,
        .height = h,
        .layer_count_or_depth = 1,
        .num_levels = num_levels,
        .sample_count = c.SDL_GPU_SAMPLECOUNT_1,
    }));
    slot.* = try Texture.init(alloc, gpu_tex, w, h, format, num_levels);
    return slot.*.?;
}

// Main render loop for one preset
fn renderPasses(
    alloc: std.mem.Allocator,
    device: ?*c.SDL_GPUDevice,
    cmd: ?*c.SDL_GPUCommandBuffer,
    vertex_buffer: ?*c.SDL_GPUBuffer,
    preset: *LoadedPreset,
    uniforms: *BuiltinUniforms,
    original_tex: *Texture,
    source_tex: **Texture,
    swapchain_format: c.SDL_GPUTextureFormat,
    current_w: *u32,
    current_h: *u32,
    viewport: Viewport,
    frame_count: u32,
    pass_output: []?*Texture,
    aliases: *std.StringHashMap(*Texture),
    pass_feedback: []?*Texture,
    history: []*Texture,
    uses_pass_output: bool,
    uses_pass_feedback: bool,
) !void {
    for (preset.passes, 0..) |*pass, i| {
        const last_pass = (i == preset.passes.len - 1);
        const output_size = pass.calculateOutputSize(
            current_w.*,
            current_h.*,
            viewport.w,
            viewport.h,
        );

        const format: c.SDL_GPUTextureFormat = if (pass.texture_format != c.SDL_GPU_TEXTUREFORMAT_INVALID)
            pass.texture_format
        else if (pass.float_framebuffer)
            c.SDL_GPU_TEXTUREFORMAT_R16G16B16A16_FLOAT
        else if (pass.srgb_framebuffer)
            c.SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM_SRGB
        else
            swapchain_format;
        const num_levels = if (pass.mipmap_input) calcMipLevels(output_size.w, output_size.h) else 1;

        const needs_feedback = uses_pass_feedback or (pass.alias != null);
        if (needs_feedback) {
            const previous_output = pass.output_texture;
            pass.output_texture = pass.fb_feedback;
            pass.fb_feedback = previous_output;

            if (pass.fb_feedback) |feedback| {
                if (!feedback.matches(output_size.w, output_size.h, format, num_levels)) {
                    feedback.release(device);
                    pass.fb_feedback = null;
                }
            }
        }

        const target_tex = try createRenderTarget(
            alloc,
            device,
            &pass.output_texture,
            output_size.w,
            output_size.h,
            format,
            num_levels,
        );

        if (uses_pass_feedback) {
            if (i < pass_feedback.len) setRetainedPassTextureSlot(&pass_feedback[i], pass.fb_feedback, device);
        }

        if (uses_pass_output) {
            if (i < pass_output.len) setRetainedPassTextureSlot(&pass_output[i], target_tex, device);
        }

        // Handle named aliases for feedback loops
        if (pass.alias) |alias| {
            if (try aliases.fetchPut(alias, target_tex.ref())) |old| {
                old.value.release(device);
            }
            if (pass.feedback_alias) |fb_alias| {
                const fb_tex = pass.fb_feedback orelse original_tex;
                if (try aliases.fetchPut(fb_alias, fb_tex.ref())) |old| {
                    old.value.release(device);
                }
            }
        }

        // Begin GPU render pass for this shader pass
        const color_target = c.SDL_GPUColorTargetInfo{
            .texture = target_tex.ptr,
            .load_op = c.SDL_GPU_LOADOP_CLEAR,
            .store_op = c.SDL_GPU_STOREOP_STORE,
            .clear_color = .{ .r = 0, .g = 0, .b = 0, .a = 1 },
        };
        const rp = c.SDL_BeginGPURenderPass(cmd, &color_target, 1, null);
        c.SDL_BindGPUGraphicsPipeline(rp, pass.pipeline);
        c.SDL_BindGPUVertexBuffers(rp, 0, &.{ .buffer = vertex_buffer }, 1);
        c.SDL_SetGPUViewport(rp, &c.SDL_GPUViewport{
            .x = 0,
            .y = 0,
            .w = @floatFromInt(output_size.w),
            .h = @floatFromInt(output_size.h),
            .min_depth = 0.0,
            .max_depth = 1.0,
        });
        c.SDL_SetGPUScissor(rp, &c.SDL_Rect{
            .x = 0,
            .y = 0,
            .w = @intCast(output_size.w),
            .h = @intCast(output_size.h),
        });

        uniforms.FrameCount = if (pass.frame_count_mod) |m| frame_count % m else frame_count;
        uniforms.SourceSize = blk: {
            const sv = [4]f32{
                @floatFromInt(current_w.*),
                @floatFromInt(current_h.*),
                1.0 / @as(f32, @floatFromInt(current_w.*)),
                1.0 / @as(f32, @floatFromInt(current_h.*)),
            };
            break :blk sv;
        };
        uniforms.OutputSize = blk: {
            const ov = [4]f32{
                @floatFromInt(output_size.w),
                @floatFromInt(output_size.h),
                1.0 / @as(f32, @floatFromInt(output_size.w)),
                1.0 / @as(f32, @floatFromInt(output_size.h)),
            };
            break :blk ov;
        };

        try bindShaderResources(
            rp,
            cmd,
            pass,
            original_tex,
            source_tex.*,
            uniforms,
            preset,
            aliases,
            history,
            pass_feedback,
            pass_output,
        );

        c.SDL_DrawGPUPrimitives(rp, 6, 1, 0, 0);
        c.SDL_EndGPURenderPass(rp);

        if (!last_pass) {
            source_tex.* = pass.output_texture.?;
            current_w.* = output_size.w;
            current_h.* = output_size.h;
        }
    }
}

pub const ShaderPipeline = struct {
    alloc: std.mem.Allocator,
    io: std.Io,
    device: ?*c.SDL_GPUDevice,
    vk_version: c_uint,

    vertex_buffer: ?*c.SDL_GPUBuffer,

    preset: ?LoadedPreset = null,
    preset_path: ?[]u8 = null,
    cache: ShaderCache,

    // Async compile state
    compile_progress: std.atomic.Value(u32) = .init(0),
    compile_total: u32 = 0,
    compile_thread: ?std.Thread = null,
    compile_mutex: std.Io.Mutex = .init,
    compile_failed: bool = false,
    compile_error_msg: ?[]u8 = null,
    compile_done: std.atomic.Value(bool) = .init(false),

    // Usage flags derived from preset reflection at load time
    uses_pass_output: bool = false,
    uses_pass_feedback: bool = false,
    max_frame_history: usize = 0,

    // Per-frame accumulation state
    frame_count: u32 = 0,
    pass_output: []?*Texture = &.{},
    pass_feedback: []?*Texture = &.{},
    texture_aliases: std.StringHashMap(*Texture),
    frame_history: std.ArrayList(*Texture),

    const Self = @This();

    pub fn init(
        alloc: std.mem.Allocator,
        io: std.Io,
        device: ?*c.SDL_GPUDevice,
        vk_version: c_uint,
    ) !*Self {
        const self = try alloc.create(Self);

        const vb_size = @sizeOf(Vertex) * QUAD_VERTICES.len;
        const vb_info = c.SDL_GPUBufferCreateInfo{
            .usage = c.SDL_GPU_BUFFERUSAGE_VERTEX,
            .size = vb_size,
        };
        const vertex_buffer = sdlError(c.SDL_CreateGPUBuffer(device, &vb_info));

        // Upload the quad vertices
        {
            const tb = sdlError(c.SDL_CreateGPUTransferBuffer(
                device,
                &.{ .usage = c.SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD, .size = vb_size },
            ));
            const map = c.SDL_MapGPUTransferBuffer(device, tb, false);
            @memcpy(@as([*]Vertex, @ptrCast(@alignCast(map)))[0..QUAD_VERTICES.len], &QUAD_VERTICES);
            c.SDL_UnmapGPUTransferBuffer(device, tb);

            const cmd = sdlError(c.SDL_AcquireGPUCommandBuffer(device));
            const copy = sdlError(c.SDL_BeginGPUCopyPass(cmd));
            c.SDL_UploadToGPUBuffer(
                copy,
                &.{ .transfer_buffer = tb },
                &.{ .buffer = vertex_buffer, .size = vb_size },
                false,
            );
            c.SDL_EndGPUCopyPass(copy);
            sdlError(c.SDL_SubmitGPUCommandBuffer(cmd));
            c.SDL_ReleaseGPUTransferBuffer(device, tb);
        }

        self.* = .{
            .alloc = alloc,
            .io = io,
            .device = device,
            .vk_version = vk_version,
            .vertex_buffer = vertex_buffer,
            .texture_aliases = .init(alloc),
            .frame_history = .empty,
            .cache = try ShaderCache.init(alloc, io),
        };
        return self;
    }

    fn resizePassTextureSlots(self: *Self, pass_count: usize) !void {
        const next_output = try self.alloc.alloc(?*Texture, pass_count);
        @memset(next_output, null);

        const next_feedback = try self.alloc.alloc(?*Texture, pass_count);
        @memset(next_feedback, null);

        clearPassTextureSlots(self.device, self.pass_output);
        clearPassTextureSlots(self.device, self.pass_feedback);
        self.alloc.free(self.pass_output);
        self.alloc.free(self.pass_feedback);
        self.pass_output = next_output;
        self.pass_feedback = next_feedback;
    }

    pub fn deinit(self: *Self) void {
        if (self.compile_thread) |t| {
            t.join();
            self.compile_thread = null;
        }
        {
            self.compile_mutex.lockUncancelable(self.io);
            defer self.compile_mutex.unlock(self.io);
            if (self.compile_error_msg) |m| self.alloc.free(m);
        }
        self.clearAccumulationState();
        self.alloc.free(self.pass_output);
        self.alloc.free(self.pass_feedback);
        self.texture_aliases.deinit();
        self.frame_history.deinit(self.alloc);
        if (self.preset) |*p| p.deinit(self.alloc, self.device);
        if (self.preset_path) |path| self.alloc.free(path);
        c.SDL_ReleaseGPUBuffer(self.device, self.vertex_buffer);
        self.cache.deinit();
        self.alloc.destroy(self);
    }

    /// Unload the current preset and reset all GPU state.
    /// If an async load is in progress it is joined first.
    pub fn unloadPreset(self: *Self) void {
        if (self.compile_thread) |t| {
            t.join();
            self.compile_thread = null;
        }
        {
            self.compile_mutex.lockUncancelable(self.io);
            defer self.compile_mutex.unlock(self.io);
            if (self.compile_error_msg) |m| {
                self.alloc.free(m);
                self.compile_error_msg = null;
            }
            self.compile_failed = false;
        }
        self.compile_progress.store(0, .monotonic);
        self.compile_total = 0;
        self.compile_done.store(false, .monotonic);

        self.clearAccumulationState();
        self.alloc.free(self.pass_output);
        self.alloc.free(self.pass_feedback);
        self.pass_output = &.{};
        self.pass_feedback = &.{};
        if (self.preset) |*p| {
            p.deinit(self.alloc, self.device);
            self.preset = null;
        }
        if (self.preset_path) |path| {
            self.alloc.free(path);
            self.preset_path = null;
        }
        self.frame_count = 0;
        self.uses_pass_output = false;
        self.uses_pass_feedback = false;
        self.max_frame_history = 0;
        self.frame_history.clearRetainingCapacity();
    }

    pub fn isActive(self: *const Self) bool {
        return self.preset != null;
    }

    pub fn getPresetPath(self: *const Self) ?[]const u8 {
        return self.preset_path;
    }

    /// Returns the ordered list of tunable parameters for the active preset.
    pub fn getParamInfos(self: *const Self) []const ParamInfo {
        const p = self.preset orelse return &.{};
        return p.param_meta.items;
    }

    /// Read the current f32 value of a parameter by name.
    pub fn getParam(self: *const Self, name: []const u8) f32 {
        const p = self.preset orelse return 0;
        const bytes = p.param_data.get(name) orelse return 0;
        return std.mem.bytesAsValue(f32, bytes[0..4]).*;
    }

    /// Write a new f32 value for a parameter by name.  No-op if the preset
    /// is not loaded or the parameter does not exist.
    pub fn setParam(self: *Self, name: []const u8, value: f32) void {
        const p = &(self.preset orelse return);
        if (p.param_data.getPtr(name)) |bytes_ptr| {
            @memcpy(bytes_ptr.*[0..4], std.mem.asBytes(&value));
        }
    }

    pub fn isCompiling(self: *const Self) bool {
        return self.compile_thread != null and !self.compile_done.load(.acquire);
    }

    /// Start an asynchronous shader preset load.  Progress can be tracked
    /// via `compile_progress` / `compile_total`, and the result retrieved
    /// by calling `pollLoadResult` each frame.
    pub fn loadPreset(self: *Self, path: []const u8, swapchain_format: c.SDL_GPUTextureFormat) !void {
        // unloadPreset joins any in-progress thread and resets all state.
        self.unloadPreset();

        var parsed = try parsePresetFile(self.alloc, self.io, path);
        errdefer parsed.deinit(self.alloc);
        self.compile_total = @intCast(parsed.shader_config.total_passes);

        const ctx = try self.alloc.create(AsyncLoadCtx);
        errdefer self.alloc.destroy(ctx);
        ctx.* = .{
            .pipeline = self,
            .path = try self.alloc.dupe(u8, path),
            .swapchain_format = swapchain_format,
            .parsed = parsed, // ownership transferred; freed by asyncLoadFn
        };
        errdefer self.alloc.free(ctx.path);

        self.compile_thread = try std.Thread.spawn(.{}, asyncLoadFn, .{ctx});
    }

    /// The result of a `pollLoadResult` call.
    pub const ShaderLoadPoll = union(enum) {
        /// No async load has been started.
        idle,
        /// Compilation is in progress; `completed`/`total` indicate how many
        /// passes have finished.
        compiling: struct { completed: u32, total: u32 },
        /// Compilation succeeded and the preset is now active.
        done,
        /// Compilation failed; the slice is the error message (owned by the
        /// pipeline — do not free it).
        failed: []const u8,
    };

    // TODO: decouple the compilation  from ShaderPipeline?
    /// Poll the status of an async `startLoadPreset` call.
    /// When `.done` or `.failed` is returned the compile thread has been
    /// joined and the pipeline has been updated.
    pub fn pollLoadResult(self: *Self) ShaderLoadPoll {
        if (self.compile_thread == null) return .idle;

        if (!self.compile_done.load(.acquire)) {
            return .{ .compiling = .{
                .completed = self.compile_progress.load(.monotonic),
                .total = self.compile_total,
            } };
        }

        // Compilation finished — join the thread.
        if (self.compile_thread) |t| {
            t.join();
            self.compile_thread = null;
        }

        self.compile_mutex.lockUncancelable(self.io);
        defer self.compile_mutex.unlock(self.io);

        if (self.compile_failed) {
            return .{ .failed = self.compile_error_msg orelse "Unknown error" };
        }

        return .done;
    }

    /// Context passed to the async compilation thread.
    const AsyncLoadCtx = struct {
        pipeline: *ShaderPipeline,
        /// Owned copy of the preset path (used only to store as `preset_path`).
        path: []u8,
        swapchain_format: c.SDL_GPUTextureFormat,
        /// Already-parsed preset data; ownership transferred from `startLoadPreset`.
        parsed: ParsedPreset,
    };

    fn asyncLoadFn(ctx: *AsyncLoadCtx) void {
        const self = ctx.pipeline;
        defer {
            ctx.parsed.deinit(self.alloc);
            self.alloc.free(ctx.path);
            self.alloc.destroy(ctx);
        }

        if (compilePreset(
            self.alloc,
            self.io,
            &ctx.parsed,
            self.vk_version,
            self.device,
            ctx.swapchain_format,
            &self.compile_progress,
            &self.cache,
        )) |preset| {
            self.compile_mutex.lockUncancelable(self.io);
            defer self.compile_mutex.unlock(self.io);

            self.preset = preset;
            self.preset_path = ctx.path;

            for (self.preset.?.passes) |pass| {
                for (pass.fragment_reflection.descriptor_sets.items) |set_info| {
                    for (set_info.bindings.items) |binding| {
                        switch (binding.binding_type) {
                            .sampler2D => |st| switch (st) {
                                .PassOutput => self.uses_pass_output = true,
                                .PassFeedback => self.uses_pass_feedback = true,
                                .OriginalHistory => |id| self.max_frame_history = @max(id, self.max_frame_history),
                                else => {},
                            },
                            else => {},
                        }
                    }
                }
            }

            self.frame_history.resize(self.alloc, self.max_frame_history) catch @panic("OOM");

            std.log.info("Loaded shader preset: {s} ({} pass(es))", .{
                self.preset_path.?, self.preset.?.passes.len,
            });

            // Prevent the defer above from double-freeing `ctx.path` now that ownership has moved to `self.preset_path`.
            ctx.path = &.{};
        } else |err| {
            self.compile_mutex.lockUncancelable(self.io);
            defer self.compile_mutex.unlock(self.io);
            self.compile_failed = true;
            self.compile_error_msg = std.fmt.allocPrint(
                self.alloc,
                "Load failed: {s}",
                .{@errorName(err)},
            ) catch null;
        }

        self.compile_done.store(true, .release);
    }

    pub fn renderFrame(
        self: *Self,
        input_texture: ?*c.SDL_GPUTexture,
        src_w: u32,
        src_h: u32,
        viewport: Viewport,
        cmd: ?*c.SDL_GPUCommandBuffer,
        win_w: u32,
        win_h: u32,
        swapchain_format: c.SDL_GPUTextureFormat,
    ) !?*Texture {
        const preset = &self.preset.?;

        const input = try Texture.init(self.alloc, input_texture, src_w, src_h, c.SDL_GPU_TEXTUREFORMAT_INVALID, 1);
        defer {
            input.ptr = null; // caller owns input_texture — don't SDL-release it
            input.release(self.device);
        }

        var uniforms = BuiltinUniforms{};
        uniforms.OriginalSize = input.sizeVec4();
        uniforms.FinalViewportSize = [4]f32{
            @floatFromInt(win_w),
            @floatFromInt(win_h),
            1.0 / @as(f32, @floatFromInt(win_w)),
            1.0 / @as(f32, @floatFromInt(win_h)),
        };

        var source_tex: *Texture = input;
        var current_w = src_w;
        var current_h = src_h;

        try renderPasses(
            self.alloc,
            self.device,
            cmd,
            self.vertex_buffer,
            preset,
            &uniforms,
            input,
            &source_tex,
            swapchain_format,
            &current_w,
            &current_h,
            viewport,
            self.frame_count,
            self.pass_output,
            &self.texture_aliases,
            self.pass_feedback,
            self.frame_history.items,
            self.uses_pass_output,
            self.uses_pass_feedback,
        );

        // Update frame history for OriginalHistory# uniforms
        if (self.max_frame_history > 0) {
            const history_index = self.frame_count % self.frame_history.items.len;
            if (self.frame_history.items[history_index].ptr != null) {
                self.frame_history.items[history_index].release(self.device);
            }
            self.frame_history.items[history_index] = input.ref();
        }

        // Release pass_output textures if no feedback is needed
        if (self.uses_pass_output and !self.uses_pass_feedback) {
            clearPassTextureSlots(self.device, self.pass_output);
        }

        self.frame_count += 1;
        return preset.passes[preset.passes.len - 1].output_texture;
    }

    fn clearAccumulationState(self: *Self) void {
        clearPassTextureSlots(self.device, self.pass_output);
        clearPassTextureSlots(self.device, self.pass_feedback);

        var alias_it = self.texture_aliases.valueIterator();
        while (alias_it.next()) |t| t.*.release(self.device);
        self.texture_aliases.clearRetainingCapacity();
    }
};
