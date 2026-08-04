const std = @import("std");
const builtin = @import("builtin");

const c = @import("../../root.zig").c;
const vulkan = @import("../../root.zig").vulkan;
const ThreadPool = @import("../../root.zig").ThreadPool;

pub const FRAG =
    \\#version 450
    \\layout(location = 0) in vec4 v_Color;
    \\layout(location = 1) in vec2 v_UV;
    \\layout(location = 2) in vec2 v_Position;
    \\layout(location = 3) in vec4 v_Rect;
    \\layout(location = 4) in vec4 v_CornerRadius;
    \\layout(location = 5) in vec4 v_OverlayColor;
    \\
    \\layout(location = 0) out vec4 Target0;
    \\
    \\layout(set = 2, binding = 0) uniform sampler2D u_Texture;
    \\
    \\float roundedRectDistance(vec2 p, vec2 half_size, vec4 radii) {
    \\    bool right = p.x > 0.0;
    \\    bool bottom = p.y > 0.0;
    \\    float radius = bottom ? (right ? radii.z : radii.w) : (right ? radii.y : radii.x);
    \\    radius = min(radius, min(half_size.x, half_size.y));
    \\    vec2 q = abs(p) - (half_size - vec2(radius));
    \\    return length(max(q, 0.0)) + min(max(q.x, q.y), 0.0) - radius;
    \\}
    \\
    \\void main() {
    \\    vec4 color = texture(u_Texture, v_UV) * v_Color;
    \\    if (dot(v_CornerRadius, vec4(1.0)) > 0.0 && v_Rect.z > 0.0 && v_Rect.w > 0.0) {
    \\        vec2 half_size = v_Rect.zw * 0.5;
    \\        vec2 center = v_Rect.xy + half_size;
    \\        float dist = roundedRectDistance(v_Position - center, half_size, v_CornerRadius);
    \\        float aa = max(fwidth(dist), 0.0001);
    \\        color.a *= clamp(0.5 - dist / aa, 0.0, 1.0);
    \\    }
    \\    color.rgb = mix(color.rgb, v_OverlayColor.rgb, v_OverlayColor.a);
    \\    Target0 = color;
    \\}
;
pub const VERT =
    \\#version 450
    \\layout(location = 0) in vec2 a_Position;
    \\layout(location = 1) in vec4 a_Color;
    \\layout(location = 2) in vec2 a_UV;
    \\layout(location = 3) in vec4 a_Rect;
    \\layout(location = 4) in vec4 a_CornerRadius;
    \\layout(location = 5) in vec4 a_OverlayColor;
    \\
    \\layout(location = 0) out vec4 v_Color;
    \\layout(location = 1) out vec2 v_UV;
    \\layout(location = 2) out vec2 v_Position;
    \\layout(location = 3) out vec4 v_Rect;
    \\layout(location = 4) out vec4 v_CornerRadius;
    \\layout(location = 5) out vec4 v_OverlayColor;
    \\
    \\layout(set = 1, binding = 0) uniform UniformBlock {
    \\    mat4 u_Projection;
    \\};
    \\
    \\void main() {
    \\    v_Color = a_Color;
    \\    v_UV = a_UV;
    \\    v_Position = a_Position;
    \\    v_Rect = a_Rect;
    \\    v_CornerRadius = a_CornerRadius;
    \\    v_OverlayColor = a_OverlayColor;
    \\    gl_Position = u_Projection * vec4(a_Position, 0.0, 1.0);
    \\}
;

pub const TEXT_FRAG =
    \\#version 450
    \\layout(location = 0) in vec4 v_Color;
    \\layout(location = 1) in vec2 v_UV;
    \\layout(location = 0) out vec4 Target0;
    \\layout(set = 2, binding = 0) uniform sampler2D u_Texture;
    \\void main() {
    \\    Target0 = vec4(v_Color.rgb, texture(u_Texture, v_UV).a * v_Color.a);
    \\}
;

pub const TEXT_VERT =
    \\#version 450
    \\layout(location = 0) in vec2 a_Position;
    \\layout(location = 1) in vec4 a_Color;
    \\layout(location = 2) in vec2 a_UV;
    \\layout(location = 0) out vec4 v_Color;
    \\layout(location = 1) out vec2 v_UV;
    \\layout(set = 1, binding = 0) uniform UniformBlock {
    \\    mat4 u_Projection;
    \\};
    \\void main() {
    \\    v_Color = a_Color;
    \\    v_UV = a_UV;
    \\    gl_Position = u_Projection * vec4(a_Position, 0.0, 1.0);
    \\}
;

pub const Shader = struct {
    code: []const u8,
    stage: Stage,
    version: u32 = 450,

    const Self = @This();
    const Stage = enum { Vertex, Fragment };

    pub fn glslang_shader_stage(self: *const Self) c.glslang_stage_t {
        return switch (self.stage) {
            .Vertex => c.GLSLANG_STAGE_VERTEX,
            .Fragment => c.GLSLANG_STAGE_FRAGMENT,
        };
    }

    fn source(self: *const Self) []const u8 {
        return switch (self.stage) {
            .Vertex => self.code,
            .Fragment => self.code,
        };
    }
};

fn get_spirv_version(vk_version: c_uint) c_uint {
    return switch (vk_version) {
        c.GLSLANG_TARGET_VULKAN_1_0 => c.GLSLANG_TARGET_SPV_1_0,
        c.GLSLANG_TARGET_VULKAN_1_1 => c.GLSLANG_TARGET_SPV_1_3,
        c.GLSLANG_TARGET_VULKAN_1_2 => c.GLSLANG_TARGET_SPV_1_5,
        c.GLSLANG_TARGET_VULKAN_1_3, c.GLSLANG_TARGET_VULKAN_1_4 => c.GLSLANG_TARGET_SPV_1_6,
        else => c.GLSLANG_TARGET_SPV_1_0,
    };
}

pub const ShaderCompiler = struct {
    alloc: std.mem.Allocator,
    io: std.Io,
    shaders: std.ArrayList(Shader),

    const Self = @This();
    const SpirV = struct {
        bytes: []const u8,
        stage: Shader.Stage,
    };

    pub fn init(alloc: std.mem.Allocator, io: std.Io) !Self {
        const self: Self = .{
            .alloc = alloc,
            .io = io,
            .shaders = .empty,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.shaders.deinit(self.alloc);
    }

    pub fn addShader(self: *Self, code: []const u8, stage: Shader.Stage) !void {
        try self.shaders.append(self.alloc, .{ .code = code, .stage = stage });
    }

    pub fn clear(self: *Self) void {
        self.shaders.clearRetainingCapacity();
    }

    pub fn compile(self: *Self, vulkan_version: c_uint) ![]SpirV {
        var result: std.ArrayList(SpirV) = .empty;

        var pool: ThreadPool = undefined;
        defer pool.deinit();
        try pool.init(.{ .allocator = self.alloc, .io = self.io, .n_jobs = std.Thread.getCpuCount() catch 4 });

        var wg = ThreadPool.WaitGroup.init(self.io);
        var mutex: std.Io.Mutex = .init;
        for (self.shaders.items) |shader| {
            pool.spawnWg(&wg, worker, .{ self, shader, vulkan_version, &result, &mutex });
        }
        wg.wait();

        return result.toOwnedSlice(self.alloc);
    }

    fn worker(
        self: *const Self,
        shader: Shader,
        vulkan_version: c_uint,
        result: *std.ArrayList(SpirV),
        mutex: *std.Io.Mutex,
    ) !void {
        const source_z = try self.alloc.dupeZ(u8, shader.source());
        defer self.alloc.free(source_z);

        const target_version = vulkan.vk_to_glslang_version(vulkan_version);
        const glslang_input: c.glslang_input_t = .{
            .client = c.GLSLANG_CLIENT_VULKAN,
            .language = c.GLSLANG_SOURCE_GLSL,
            .stage = shader.glslang_shader_stage(),
            .client_version = target_version,
            .target_language = c.GLSLANG_TARGET_SPV,
            .target_language_version = get_spirv_version(target_version),
            .code = source_z.ptr,
            .default_version = @intCast(shader.version),
            .default_profile = c.GLSLANG_NO_PROFILE,
            .messages = c.GLSLANG_MSG_DEFAULT_BIT,
            .resource = c.glslang_default_resource(),
        };
        const glslang_shader = c.glslang_shader_create(&glslang_input);
        defer c.glslang_shader_delete(glslang_shader);

        if (c.glslang_shader_preprocess(glslang_shader, &glslang_input) != 1) {
            std.log.debug("GLSL preprocess failed:\n{s}\n{s}\n", .{
                c.glslang_shader_get_info_log(glslang_shader),
                c.glslang_shader_get_info_debug_log(glslang_shader),
            });
            return error.GLSL_PreprocessFailed;
        }

        if (c.glslang_shader_parse(glslang_shader, &glslang_input) != 1) {
            std.log.debug("GLSL parsing failed:\n{s}", .{c.glslang_shader_get_info_log(glslang_shader)});
            return error.GLSL_ParsingFailed;
        }

        const program = c.glslang_program_create();
        defer c.glslang_program_delete(program);

        c.glslang_program_add_shader(program, glslang_shader);

        if (c.glslang_program_link(program, c.GLSLANG_MSG_SPV_RULES_BIT | c.GLSLANG_MSG_VULKAN_RULES_BIT) != 1) {
            std.log.debug("GLSL linking failed:\n{s}", .{c.glslang_program_get_info_log(program)});
            return error.GLSL_LinkingFailed;
        }

        if (c.glslang_program_map_io(program) != 1) {
            std.log.debug("GLSL IO mapping failed:\n{s}", .{c.glslang_program_get_info_log(program)});
            return error.GLSL_IOMappingFailed;
        }

        var options = [_]c.struct_glslang_spv_options_s{.{
            .generate_debug_info = builtin.mode == .Debug,
            .strip_debug_info = builtin.mode != .Debug,
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

        const messages = c.glslang_program_SPIRV_get_messages(program);
        if (messages != null) {
            std.log.debug("SPIRV generation messages: {s}", .{messages});
        }

        const size = c.glslang_program_SPIRV_get_size(program);
        const ptr = c.glslang_program_SPIRV_get_ptr(program);

        const spirv = try self.alloc.dupe(u8, std.mem.sliceAsBytes(ptr[0..size]));

        mutex.lockUncancelable(self.io);
        defer mutex.unlock(self.io);

        try result.append(self.alloc, .{ .bytes = spirv, .stage = shader.stage });
    }
};
