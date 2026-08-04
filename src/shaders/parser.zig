const std = @import("std");
const builtin_shaders = @import("builtin.zig");

const TokType = enum {
    Id,
    Rparen,
    Lparen,
    Rbrace,
    Lbrace,
    Eq,
    Semi,
    Comma,
    String,
    Int,
    Float,
    Pragma,
    Version,
    Include,
    Newline,
    Eof,
};

const Pos = struct {
    start: u32,
    end: u32,
};

const Token = struct {
    type: TokType,
    value: []const u8,
    pos: Pos,

    fn eof(pos: u32) @This() {
        return .{
            .type = .Eof,
            .value = "",
            .pos = .{ .start = pos, .end = pos },
        };
    }

    pub fn format(self: @This(), writer: *std.Io.Writer) !void {
        const value = if (std.mem.eql(u8, self.value, "\n")) "\\n" else self.value;
        try writer.print(".{{ type: {any}, value: \"{s}\", pos: {}..{} }}", .{ self.type, value, self.pos.start, self.pos.end });
    }
};

fn lex(alloc: std.mem.Allocator, source: []const u8) ![]Token {
    var tokens = try std.ArrayList(Token).initCapacity(alloc, 0);

    var is_lexing_pragma = false;
    var pos: u32 = 0;
    while (pos < source.len) {
        const start = pos;
        switch (source[pos]) {
            '#' => {
                pos += 1;
                while (pos < source.len and std.ascii.isAlphabetic(source[pos])) pos += 1;
                if (std.mem.eql(u8, source[start..pos], "#pragma")) {
                    is_lexing_pragma = true;
                    try tokens.append(alloc, .{
                        .type = .Pragma,
                        .value = source[start..pos],
                        .pos = .{ .start = start, .end = pos },
                    });
                } else if (std.mem.eql(u8, source[start..pos], "#include")) {
                    try tokens.append(alloc, .{
                        .type = .Include,
                        .value = source[start..pos],
                        .pos = .{ .start = start, .end = pos },
                    });
                } else if (std.mem.eql(u8, source[start..pos], "#version")) {
                    try tokens.append(alloc, .{
                        .type = .Version,
                        .value = source[start..pos],
                        .pos = .{ .start = start, .end = pos },
                    });
                }
            },
            '\n', '\r' => {
                is_lexing_pragma = false;
                if (source[pos] == '\r' and pos + 1 < source.len and source[pos + 1] == '\n') {
                    pos += 2; // \r\n
                } else {
                    pos += 1;
                }
                try tokens.append(alloc, .{
                    .type = .Newline,
                    .value = source[start..pos],
                    .pos = .{ .start = start, .end = pos },
                });
            },
            ';' => {
                pos += 1;
                try tokens.append(alloc, .{
                    .type = .Semi,
                    .value = source[start..pos],
                    .pos = .{ .start = start, .end = pos },
                });
            },
            ',' => {
                pos += 1;
                try tokens.append(alloc, .{
                    .type = .Comma,
                    .value = source[start..pos],
                    .pos = .{ .start = start, .end = pos },
                });
            },
            '(' => {
                pos += 1;
                try tokens.append(alloc, .{
                    .type = .Lparen,
                    .value = source[start..pos],
                    .pos = .{ .start = start, .end = pos },
                });
            },
            ')' => {
                pos += 1;
                try tokens.append(alloc, .{
                    .type = .Rparen,
                    .value = source[start..pos],
                    .pos = .{ .start = start, .end = pos },
                });
            },
            '{' => {
                pos += 1;
                try tokens.append(alloc, .{
                    .type = .Lbrace,
                    .value = source[start..pos],
                    .pos = .{ .start = start, .end = pos },
                });
            },
            '}' => {
                pos += 1;
                try tokens.append(alloc, .{
                    .type = .Rbrace,
                    .value = source[start..pos],
                    .pos = .{ .start = start, .end = pos },
                });
            },
            '=' => {
                pos += 1;
                try tokens.append(alloc, .{
                    .type = .Eq,
                    .value = source[start..pos],
                    .pos = .{ .start = start, .end = pos },
                });
            },
            '"' => {
                pos += 1; // openning quote
                while (pos < source.len and source[pos] != '"') pos += 1;
                const value_end = pos;
                if (pos < source.len) pos += 1; // closing quote
                try tokens.append(alloc, .{
                    .type = .String,
                    .value = source[start + 1 .. value_end],
                    .pos = .{ .start = start, .end = pos },
                });
            },
            '0'...'9', '-' => {
                if (source[pos] == '-') pos += 1;
                while (pos < source.len and std.ascii.isDigit(source[pos])) pos += 1;
                if (pos < source.len and source[pos] == '.') {
                    pos += 1; // Dot
                    while (pos < source.len and std.ascii.isDigit(source[pos])) pos += 1;
                    try tokens.append(alloc, .{
                        .type = .Float,
                        .value = source[start..pos],
                        .pos = .{ .start = start, .end = pos },
                    });
                } else {
                    try tokens.append(alloc, .{
                        .type = .Int,
                        .value = source[start..pos],
                        .pos = .{ .start = start, .end = pos },
                    });
                }
            },
            'a'...'z', 'A'...'Z', '_' => {
                while (pos < source.len and (std.ascii.isAlphanumeric(source[pos]) or source[pos] == '_' or
                    (is_lexing_pragma and source[pos] == '-'))) pos += 1;

                try tokens.append(alloc, .{
                    .type = .Id,
                    .value = source[start..pos],
                    .pos = .{ .start = start, .end = pos },
                });
            },
            '/' => if (pos + 1 < source.len and source[pos + 1] == '/') {
                pos += 2; // skip "//"
                while (pos < source.len and source[pos] != '\n') pos += 1;
            } else {
                pos += 1;
            },
            else => pos += 1,
        }
    }

    try tokens.append(alloc, Token.eof(pos));

    return try tokens.toOwnedSlice(alloc);
}

const Field = struct {
    type_name: []const u8,
    name: []const u8,

    pub fn format(self: @This(), writer: *std.Io.Writer) !void {
        try writer.print(".{{.type_name: \"{s}\", .name: \"{s}\"}}", .{ self.type_name, self.name });
    }
};

const LayoutType = union(enum) {
    PushConstant,
    UniformBuffer: struct {
        set: u32,
        binding: u32,
    },
};

const Node = union(enum) {
    PragmaParam: struct {
        id: []const u8,
        description: []const u8,
        initial: f32,
        min: f32,
        max: f32,
        step: ?f32,
        pos: Pos,
    },
    PragmaFormat: struct { value: []const u8, pos: Pos },
    PragmaName: struct { value: []const u8, pos: Pos },
    Version: struct { value: u32, pos: Pos },
    LayoutBlock: struct {
        name: []const u8,
        instance: []const u8,
        fields: []Field,
        type: LayoutType,
        pos: Pos,
    },
    LayoutUniform: struct {
        set: u32,
        binding: u32,
        name: []const u8,
        instance: []const u8,
        pos: Pos,
    },

    fn deinit(self: @This(), alloc: std.mem.Allocator) void {
        switch (self) {
            .LayoutBlock => |l| {
                alloc.free(l.fields);
            },
            else => {},
        }
    }

    pub fn format(self: @This(), writer: *std.Io.Writer) !void {
        switch (self) {
            .PragmaParam => |p| {
                try writer.print(
                    ".PragmaParam{{.id: \"{s}\", .description: \"{s}\", .initial: {any}, .min: {any}, .max: {any}, .step: {any}, pos: {}..{}}}",
                    .{ p.id, p.description, p.initial, p.min, p.max, p.step, p.pos.start, p.pos.end },
                );
            },
            .PragmaFormat => |p| {
                try writer.print(
                    ".PragmaFormat{{.value: \"{s}\", .pos: {}..{}}}",
                    .{ p.value, p.pos.start, p.pos.end },
                );
            },
            .PragmaName => |p| {
                try writer.print(".PragmaName{{.value: \"{s}\", .pos: {}..{}}}", .{ p.value, p.pos.start, p.pos.end });
            },
            .LayoutBlock => |l| {
                try writer.print(
                    ".LayoutBlock {{.name: \"{s}\", .instance: \"{s}\", type: {any}, .fields: ",
                    .{ l.name, l.instance, l.type },
                );
                for (l.fields) |field| {
                    try writer.print("{f}, ", .{field});
                }
                try writer.print(".pos: {}..{}}}", .{ l.pos.start, l.pos.end });
            },
            .LayoutUniform => |l| {
                try writer.print(
                    ".LayoutUniform{{.set: {}, .binding: {}, .name: \"{s}\", .instance: \"{s}\", .pos: {}..{}}}",
                    .{ l.set, l.binding, l.name, l.instance, l.pos.start, l.pos.end },
                );
            },
            .Version => |v| {
                try writer.print(
                    ".LayoutUniform{{.value: {}, .pos: {}..{}}}",
                    .{ v.value, v.pos.start, v.pos.end },
                );
            },
        }
    }

    fn isPushConstant(self: *const @This()) bool {
        return self.* == .LayoutBlock and self.LayoutBlock.type == .PushConstant;
    }

    fn convertPushConstantToUniformBuffer(self: @This()) !Node {
        switch (self) {
            .LayoutBlock => |node| {
                if (node.type == .PushConstant) {
                    var new_node = node;
                    new_node.type = .{ .UniformBuffer = .{
                        .set = 0,
                        .binding = 0,
                    } };
                    return .{ .LayoutBlock = new_node };
                }
                return error.NotPushConstant;
            },
            else => return error.NotPushConstant,
        }
    }

    /// Convert a uniform buffer node to a string, other nodes are ignored.
    fn layoutToStr(self: @This(), alloc: std.mem.Allocator) ![]const u8 {
        var allocating = std.Io.Writer.Allocating.init(alloc);
        defer allocating.deinit();

        const writer = &allocating.writer;
        switch (self) {
            .LayoutBlock => |node| if (node.type == .UniformBuffer) {
                try writer.print(
                    "layout(std140, set = {}, binding = {}) uniform {s} {{\n",
                    .{ node.type.UniformBuffer.set, node.type.UniformBuffer.binding, node.name },
                );
                for (node.fields) |field| {
                    try writer.print("   {s} {s};\n", .{ field.type_name, field.name });
                }
                if (node.instance.len == 0) {
                    try writer.print("}};", .{});
                } else {
                    try writer.print("}} {s};", .{node.instance});
                }
            },
            .LayoutUniform => |node| {
                try writer.print(
                    "layout(set = {}, binding = {}) uniform {s} {s};",
                    .{ node.set, node.binding, node.name, node.instance },
                );
            },
            else => {},
        }

        return try allocating.toOwnedSlice();
    }

    fn pos(self: *const @This()) Pos {
        return switch (self.*) {
            inline else => |node| node.pos,
        };
    }
};

const Parser = struct {
    pos: usize,
    alloc: std.mem.Allocator,
    tokens: []const Token,
    nodes: std.ArrayList(Node),

    const Self = @This();

    fn init(alloc: std.mem.Allocator, tokens: []const Token) !Self {
        return .{
            .pos = 0,
            .alloc = alloc,
            .tokens = tokens,
            .nodes = try .initCapacity(alloc, 0),
        };
    }

    fn atEnd(self: *const Self) bool {
        return self.pos >= self.tokens.len or self.tokens[self.pos].type == .Eof;
    }

    fn peek(self: *const Self) *const Token {
        std.debug.assert(self.tokens.len > 0);
        if (self.pos >= self.tokens.len) {
            return &self.tokens[self.tokens.len - 1];
        }
        return &self.tokens[self.pos];
    }

    fn eat(self: *Self) *const Token {
        std.debug.assert(self.tokens.len > 0);
        if (self.pos >= self.tokens.len) {
            return &self.tokens[self.tokens.len - 1];
        }
        defer self.pos += 1;
        return &self.tokens[self.pos];
    }

    fn eat_if(self: *Self, token: TokType) bool {
        if (self.pos >= self.tokens.len) return false;
        if (self.tokens[self.pos].type == token) {
            self.pos += 1;
            return true;
        }
        return false;
    }

    fn parseBlockFields(self: *Self) ![]Field {
        var fields = try std.ArrayList(Field).initCapacity(self.alloc, 0);
        errdefer fields.deinit(self.alloc);

        field_loop: while (!self.atEnd()) {
            if (self.peek().type == .Rbrace) break;

            if (self.peek().type == .Newline) {
                _ = self.eat();
                continue;
            }

            const type_tok = self.eat();
            if (type_tok.type != .Id) {
                while (!self.atEnd()) {
                    switch (self.peek().type) {
                        .Semi => {
                            _ = self.eat();
                            break;
                        },
                        .Rbrace => break,
                        else => _ = self.eat(),
                    }
                }
                continue;
            }

            var expect_name = true;
            while (!self.atEnd()) {
                switch (self.peek().type) {
                    .Semi => {
                        _ = self.eat();
                        break;
                    },
                    .Rbrace => break :field_loop,
                    .Comma => {
                        _ = self.eat();
                        expect_name = true;
                    },
                    .Newline => _ = self.eat(),
                    else => {
                        const tok = self.eat();
                        if (expect_name and tok.type == .Id) {
                            try fields.append(self.alloc, .{
                                .type_name = type_tok.value,
                                .name = tok.value,
                            });
                            expect_name = false;
                        }
                    },
                }
            }
        }
        return fields.toOwnedSlice(self.alloc);
    }

    fn parse(self: *Self) !void {
        while (!self.atEnd()) {
            const token = self.eat();
            switch (token.type) {
                .Version => {
                    const version = self.eat();
                    try self.nodes.append(self.alloc, .{
                        .Version = .{
                            .value = try std.fmt.parseInt(u32, version.value, 10),
                            .pos = .{ .start = token.pos.start, .end = version.pos.end },
                        },
                    });
                },
                .Pragma => {
                    const next_tok = self.peek();
                    if (next_tok.type == .Id) {
                        if (std.mem.eql(u8, next_tok.value, "parameter")) {
                            _ = self.eat(); // consume "parameter"
                            const id = self.eat();
                            const desc = self.eat();
                            const initial = self.eat();
                            const min = self.eat();
                            const max = self.eat();
                            const step = if (self.peek().type == .Int or self.peek().type == .Float)
                                self.eat()
                            else
                                null;
                            try self.nodes.append(self.alloc, .{
                                .PragmaParam = .{
                                    .id = id.value,
                                    .description = desc.value,
                                    .initial = try std.fmt.parseFloat(f32, initial.value),
                                    .min = try std.fmt.parseFloat(f32, min.value),
                                    .max = try std.fmt.parseFloat(f32, max.value),
                                    .step = if (step) |tok| try std.fmt.parseFloat(f32, tok.value) else null,
                                    .pos = .{
                                        .start = token.pos.start,
                                        .end = if (step) |tok| tok.pos.end else max.pos.end,
                                    },
                                },
                            });
                        } else if (std.mem.eql(u8, next_tok.value, "format")) {
                            _ = self.eat(); // consume "format"
                            const format = self.eat();
                            try self.nodes.append(self.alloc, .{
                                .PragmaFormat = .{
                                    .value = format.value,
                                    .pos = .{ .start = token.pos.start, .end = format.pos.end },
                                },
                            });
                        } else if (std.mem.eql(u8, next_tok.value, "name")) {
                            _ = self.eat(); // consume "name"
                            const format = self.eat();
                            try self.nodes.append(self.alloc, .{
                                .PragmaName = .{
                                    .value = format.value,
                                    .pos = .{ .start = token.pos.start, .end = format.pos.end },
                                },
                            });
                        }
                    }
                },
                .Id => if (std.mem.eql(u8, token.value, "layout")) {
                    _ = self.eat(); // consume (

                    var layout_type: ?LayoutType = null;
                    var set: ?u32 = null;
                    var binding: ?u32 = null;
                    // Parse layout qualifiers. RetroArch shaders commonly use
                    // both layout(set = N, binding = M) and layout(binding = M).
                    while (!self.atEnd() and self.peek().type != .Rparen) {
                        const tok = self.eat();
                        if (tok.type == .Id) {
                            if (std.mem.eql(u8, tok.value, "push_constant")) {
                                layout_type = .PushConstant;
                            } else if (std.mem.eql(u8, tok.value, "set")) {
                                _ = self.eat(); // =
                                const value = self.eat(); // number
                                set = try std.fmt.parseInt(u32, value.value, 10);
                            } else if (std.mem.eql(u8, tok.value, "binding")) {
                                _ = self.eat(); // =
                                const value = self.eat(); // number
                                binding = try std.fmt.parseInt(u32, value.value, 10);
                            }
                        }
                    }
                    if (layout_type == null and binding != null) {
                        layout_type = .{ .UniformBuffer = .{
                            .set = set orelse 0,
                            .binding = binding.?,
                        } };
                    }
                    if (!self.eat_if(.Rparen)) continue;

                    // Check for 'uniform'
                    const next = self.eat();
                    if (next.type == .Id and std.mem.eql(u8, next.value, "uniform")) {

                        // Check for Block Name or Type Name
                        const block_name = self.eat();

                        _ = self.eat_if(.Newline);
                        switch (self.peek().type) {
                            .Id => {
                                const resource_layout = layout_type orelse continue;
                                const name = self.eat();
                                const semi = self.eat();
                                const uniform_set, const uniform_binding = switch (resource_layout) {
                                    .UniformBuffer => |u| .{ u.set, u.binding },
                                    else => continue,
                                };
                                try self.nodes.append(self.alloc, .{ .LayoutUniform = .{
                                    .set = uniform_set,
                                    .binding = uniform_binding,
                                    .instance = name.value,
                                    .name = block_name.value,
                                    .pos = .{
                                        .start = token.pos.start,
                                        .end = semi.pos.end,
                                    },
                                } });
                            },
                            .Lbrace => {
                                const resource_layout = layout_type orelse continue;
                                _ = self.eat(); // consume {
                                const fields = try self.parseBlockFields();
                                if (!self.eat_if(.Rbrace)) {
                                    self.alloc.free(fields);
                                    continue;
                                }

                                var instance_name: []const u8 = "";
                                var semi = self.eat(); // optional instance name or ;
                                if (semi.type == .Id) {
                                    instance_name = semi.value;
                                    semi = self.eat(); // consume ;
                                }
                                if (semi.type != .Semi) {
                                    self.alloc.free(fields);
                                    continue;
                                }

                                try self.nodes.append(self.alloc, .{ .LayoutBlock = .{
                                    .name = block_name.value,
                                    .instance = instance_name,
                                    .fields = fields,
                                    .type = resource_layout,
                                    .pos = .{
                                        .start = token.pos.start,
                                        .end = semi.pos.end,
                                    },
                                } });
                            },
                            else => _ = self.eat(), // Skip other tokens
                        }
                    }
                } else {
                    _ = self.eat(); // Skip other IDs
                },
                else => {},
            }
        }
    }
};

fn parseSource(alloc: std.mem.Allocator, source: []const u8) ![]Node {
    const tokens = try lex(alloc, source);
    defer alloc.free(tokens);
    return parseTokens(alloc, tokens);
}

fn parseTokens(alloc: std.mem.Allocator, tokens: []const Token) ![]Node {
    var parser = try Parser.init(alloc, tokens);
    try parser.parse();
    return parser.nodes.toOwnedSlice(alloc);
}

pub const ShaderStage = enum {
    Vertex,
    Fragment,

    pub fn slice(self: @This()) []const u8 {
        return switch (self) {
            .Vertex => "vert",
            .Fragment => "frag",
        };
    }
};

// For SPIR-V shaders, use the following resource sets:
//
// For vertex shaders:
//
//     0: Sampled textures, followed by storage textures, followed by storage buffers
//     1: Uniform buffers
//
// For fragment shaders:
//
//     2: Sampled textures, followed by storage textures, followed by storage buffers
//     3: Uniform buffers
pub fn patchShaderSource(alloc: std.mem.Allocator, source: []const u8, shader_stage: ShaderStage) ![]const u8 {
    // Always return an owned copy so callers can unconditionally free the result.
    var patched_source: []u8 = try alloc.dupe(u8, source);
    errdefer alloc.free(patched_source);

    const tokens = try lex(alloc, source);
    defer alloc.free(tokens);
    const nodes = try parseTokens(alloc, tokens);
    defer {
        for (nodes) |node| {
            node.deinit(alloc);
        }
        alloc.free(nodes);
    }

    const uniform_set: u8 = if (shader_stage == .Vertex) 1 else 3;
    const texture_set: u8 = if (shader_stage == .Vertex) 0 else 2;

    var uniform_binding: u32 = 0;
    var texture_binding: u32 = 0;

    for (nodes) |*node| {
        switch (node.*) {
            .LayoutBlock => |*layout| switch (layout.type) {
                .UniformBuffer => |*descriptor| {
                    descriptor.set = uniform_set;
                    descriptor.binding = uniform_binding;
                    uniform_binding += 1;
                },
                .PushConstant => {
                    var ub = try node.convertPushConstantToUniformBuffer();
                    ub.LayoutBlock.type.UniformBuffer.set = uniform_set;
                    ub.LayoutBlock.type.UniformBuffer.binding = uniform_binding;
                    uniform_binding += 1;
                    node.* = ub;
                },
            },
            .LayoutUniform => |*layout| {
                if (std.mem.eql(u8, layout.name, "sampler2D")) {
                    layout.set = texture_set;
                    layout.binding = texture_binding;
                    texture_binding += 1;
                }
            },
            else => {},
        }
        if (node.* == .LayoutBlock or node.* == .LayoutUniform) {
            const pos = node.pos();
            const layout_str = try node.layoutToStr(alloc);
            defer alloc.free(layout_str);
            const new_patched = try std.mem.replaceOwned(
                u8,
                alloc,
                patched_source,
                source[pos.start..pos.end],
                layout_str,
            );
            alloc.free(patched_source);
            patched_source = new_patched;
        }
    }
    return patched_source;
}

const ShaderSource = struct {
    /// Code common to both shader stages
    common: []const u8,
    /// Code for the vertex shader stage.
    vertex: []const u8,
    /// Code for the fragment shader stage.
    fragment: []const u8,

    fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
        alloc.free(self.common);
        alloc.free(self.vertex);
        alloc.free(self.fragment);
    }

    /// Return the vertex code with the common code included.
    pub fn vertexCode(self: *const @This(), alloc: std.mem.Allocator) ![]const u8 {
        var allocating = std.Io.Writer.Allocating.init(alloc);
        defer allocating.deinit();
        const writer = &allocating.writer;

        try writer.print("{s}\n{s}", .{ self.common, self.vertex });
        return try allocating.toOwnedSlice();
    }

    /// Return the fragment code with the common code included.
    pub fn fragmentCode(self: *const @This(), alloc: std.mem.Allocator) ![]const u8 {
        var allocating = std.Io.Writer.Allocating.init(alloc);
        defer allocating.deinit();
        const writer = &allocating.writer;

        try writer.print("{s}\n{s}", .{ self.common, self.fragment });
        return try allocating.toOwnedSlice();
    }
};

fn resolveInclude(alloc: std.mem.Allocator, io: std.Io, source: []const u8, dir: []const u8) ![]const u8 {
    var included_files: std.StringHashMap(void) = .init(alloc);
    defer {
        var it = included_files.keyIterator();
        while (it.next()) |key| alloc.free(key.*);
        included_files.deinit();
    }
    return resolveIncludeRecursive(alloc, io, source, dir, &included_files);
}

fn resolveIncludeRecursive(
    alloc: std.mem.Allocator,
    io: std.Io,
    source: []const u8,
    dir: []const u8,
    included_files: *std.StringHashMap(void),
) ![]const u8 {
    const tokens = try lex(alloc, source);
    defer alloc.free(tokens);

    var code: std.ArrayList(u8) = .empty;
    defer code.deinit(alloc);

    var start: usize = 0;
    var i: usize = 0;

    while (i < tokens.len) : (i += 1) {
        const token = tokens[i];

        if (token.type == .Include) {
            try code.appendSlice(alloc, source[start..token.pos.start]);

            i += 1;
            if (i >= tokens.len) break;

            const path_token = tokens[i];

            const include_rel_path = path_token.value;
            const resolved_path = if (builtin_shaders.isBorderShaderPath(dir))
                try std.fs.path.join(alloc, &.{ dir, include_rel_path })
            else
                try std.fs.path.resolve(alloc, &.{ dir, include_rel_path });

            if (included_files.contains(resolved_path)) {
                alloc.free(resolved_path);
                start = path_token.pos.end;
                continue;
            }

            const include_dir = std.fs.path.dirname(resolved_path) orelse ".";
            try included_files.put(resolved_path, {});

            const embedded_include = builtin_shaders.sourceForPath(resolved_path);
            const content = if (embedded_include) |embedded|
                embedded
            else blk: {
                break :blk std.Io.Dir.cwd().readFileAlloc(io, resolved_path, alloc, .unlimited) catch |err| {
                    std.log.err("Failed to open include file: {s}\n", .{resolved_path});
                    return err;
                };
            };
            defer if (embedded_include == null) alloc.free(content);

            const processed_include = try resolveIncludeRecursive(alloc, io, content, include_dir, included_files);
            defer alloc.free(processed_include);

            try code.appendSlice(alloc, processed_include);
            try code.append(alloc, '\n');

            start = path_token.pos.end;
        } else if (token.type == .Eof) {
            break;
        }
    }

    // add the rest of the code after the last #include
    if (start < source.len) {
        try code.appendSlice(alloc, source[start..]);
    }

    return code.toOwnedSlice(alloc);
}

fn isPragmaStage(tokens: []const Token, pos: usize) bool {
    return tokens[pos].type == .Pragma and pos + 1 < tokens.len and
        tokens[pos + 1].type == .Id and std.mem.eql(u8, tokens[pos + 1].value, "stage");
}

pub fn addExtension(alloc: std.mem.Allocator, source: []const u8) ![]const u8 {
    const tokens = try lex(alloc, source);
    defer alloc.free(tokens);

    var code: std.ArrayList(u8) = .empty;

    var pos: usize = 0;
    const version_start = 0;
    pos += 2; // skip version + int tokens
    try code.appendSlice(alloc, source[version_start..tokens[pos].pos.end]);
    try code.append(alloc, '\n');

    try code.appendSlice(alloc, "#extension GL_GOOGLE_include_directive : enable\n");
    try code.appendSlice(alloc, source[pos..]);
    return code.toOwnedSlice(alloc);
}

fn preprocessShaderSource(alloc: std.mem.Allocator, io: std.Io, dir: []const u8, source: []const u8) !ShaderSource {
    const resolved_source = try resolveInclude(alloc, io, source, dir);
    defer alloc.free(resolved_source);

    const tokens = try lex(alloc, resolved_source);
    defer alloc.free(tokens);

    var common_code: std.ArrayList(u8) = .empty;
    var vertex_code: std.ArrayList(u8) = .empty;
    var fragment_code: std.ArrayList(u8) = .empty;

    const Stage = enum { Common, Vertex, Fragment };
    var current_stage: Stage = .Common;
    var text_pos: usize = 0;
    var tok_pos: usize = 0;

    const version_start = 0;
    tok_pos += 2; // skip version + int tokens
    try common_code.appendSlice(alloc, resolved_source[version_start..tokens[tok_pos].pos.start]);
    try common_code.append(alloc, '\n');
    text_pos = tokens[tok_pos].pos.start;

    // TODO
    // try common_code.appendSlice(alloc, "#extension GL_GOOGLE_include_directive : enable\n");

    while (tok_pos < tokens.len) {
        const token = tokens[tok_pos];

        if (isPragmaStage(tokens, tok_pos)) {
            const stage_type = tokens[tok_pos + 2];
            if (std.mem.eql(u8, stage_type.value, "vertex")) {
                current_stage = .Vertex;
            } else if (std.mem.eql(u8, stage_type.value, "fragment")) {
                current_stage = .Fragment;
            }
            tok_pos += 3;
            text_pos = stage_type.pos.end;
            continue;
        }

        const chunk = resolved_source[text_pos..token.pos.end];
        text_pos = token.pos.end;
        switch (current_stage) {
            .Common => try common_code.appendSlice(alloc, chunk),
            .Vertex => try vertex_code.appendSlice(alloc, chunk),
            .Fragment => try fragment_code.appendSlice(alloc, chunk),
        }
        tok_pos += 1;
    }

    if (text_pos < resolved_source.len) {
        const chunk = resolved_source[text_pos..];
        switch (current_stage) {
            .Common => try common_code.appendSlice(alloc, chunk),
            .Vertex => try vertex_code.appendSlice(alloc, chunk),
            .Fragment => try fragment_code.appendSlice(alloc, chunk),
        }
    }

    return .{
        .common = try common_code.toOwnedSlice(alloc),
        .vertex = try vertex_code.toOwnedSlice(alloc),
        .fragment = try fragment_code.toOwnedSlice(alloc),
    };
}

pub const ShaderParam = struct {
    option_name: []const u8,
    initial: f32 = 0,
    min: f32 = 0,
    max: f32 = 0,
    step: ?f32 = null,
};
pub const ShaderParams = std.StringHashMap(ShaderParam);

pub const ParsedShader = struct {
    version: u32,
    vertex: []const u8,
    fragment: []const u8,
    texture_format: ?[]const u8,
    alias: ?[]const u8,
    config: ?ShaderParams, // TODO: rename type, this isn't really the shader params; it's the value for the push contanst fields

    pub fn format(self: @This(), writer: *std.Io.Writer) !void {
        try writer.print(
            ".{{ version: {}\nformat: {s}\nvertex:\n{s}\nfragment:\n{s}\nconfig: ",
            .{ self.version, if (self.texture_format) |f| f else "null", self.vertex, self.fragment },
        );
        if (self.config) |config| {
            try writer.print(".{{ ", .{});
            var iter = config.iterator();
            while (iter.next()) |entry| {
                try writer.print(
                    "\"{s}\": .{{ .option_name: \"{s}\", .initial: {}, .min: {}, .max: {}, step: {any} }}, ",
                    .{
                        entry.key_ptr.*,
                        entry.value_ptr.option_name,
                        entry.value_ptr.initial,
                        entry.value_ptr.min,
                        entry.value_ptr.max,
                        entry.value_ptr.step,
                    },
                );
            }
            try writer.print("}}", .{});
        } else {
            try writer.print("null", .{});
        }
    }

    pub fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
        alloc.free(self.vertex);
        alloc.free(self.fragment);
        if (self.texture_format) |f| alloc.free(f);
        if (self.alias) |a| alloc.free(a);
        if (self.config) |config| {
            var it = @constCast(&config).iterator();
            while (it.next()) |entry| {
                alloc.free(entry.key_ptr.*);
                alloc.free(entry.value_ptr.option_name);
            }
            @constCast(&config).deinit();
        }
    }
};

pub fn parseShader(alloc: std.mem.Allocator, io: std.Io, dir: []const u8, source: []const u8) !ParsedShader {
    var shader_source = try preprocessShaderSource(alloc, io, dir, source);
    defer shader_source.deinit(alloc);

    const raw_vertex = try shader_source.vertexCode(alloc);
    const vertex_code = try patchShaderSource(alloc, raw_vertex, .Vertex);
    alloc.free(raw_vertex);
    errdefer alloc.free(vertex_code);

    const raw_fragment = try shader_source.fragmentCode(alloc);
    const fragment_code = try patchShaderSource(alloc, raw_fragment, .Fragment);
    alloc.free(raw_fragment);
    errdefer alloc.free(fragment_code);

    var shader_params: ShaderParams = .init(alloc);
    const nodes = try parseSource(alloc, shader_source.common);
    defer {
        for (nodes) |node| {
            node.deinit(alloc);
        }
        alloc.free(nodes);
    }

    var version: u32 = 0;
    var texture_format: ?[]const u8 = null;
    var alias: ?[]const u8 = null;
    for (nodes) |node| {
        switch (node) {
            .PragmaParam => |param| try shader_params.put(
                try alloc.dupe(u8, param.id),
                .{
                    .option_name = try alloc.dupe(u8, std.mem.trim(u8, param.description, " \t\r\n")),
                    .initial = param.initial,
                    .min = param.min,
                    .max = param.max,
                    .step = param.step,
                },
            ),
            .PragmaFormat => |format| texture_format = try alloc.dupe(u8, format.value),
            .PragmaName => |name| alias = try alloc.dupe(u8, name.value),
            .Version => |ver| version = ver.value,
            else => {},
        }
    }

    return .{
        .vertex = vertex_code,
        .fragment = fragment_code,
        .texture_format = texture_format,
        .version = version,
        .alias = alias,
        .config = if (shader_params.count() == 0) null else shader_params,
    };
}

test "lexer" {
    const alloc = std.testing.allocator;
    const source =
        \\ #version 450
        \\ #include "blargg_params.inc"
        \\ layout(push_constant) uniform Push
        \\ {
        \\    vec4 SourceSize;
        \\    uint FrameCount;
        \\    float kernel_half, ntsc_sat;
        \\ } params;
        \\ 
        \\ #pragma parameter ntsc_hue "Hue" 0.0 -1.0 6.3 0.05
        \\ 
        \\ #pragma stage vertex
        \\ layout(location = 1) in vec2 TexCoord;
        \\ layout(location = 0) out vec2 vTexCoord;
        \\ 
        \\ void main() {
        \\    gl_Position = global.MVP * Position;
        \\ }
        \\ // Skip comment: This is note lexed -> #pragma parameter foo
    ;
    const tokens = try lex(alloc, source);
    defer alloc.free(tokens);

    const expected = [_][]const u8{
        ".{ type: .Version, value: \"#version\", pos: 1..9 }",
        ".{ type: .Int, value: \"450\", pos: 10..13 }",
        ".{ type: .Newline, value: \"\\n\", pos: 13..14 }",
        ".{ type: .Include, value: \"#include\", pos: 15..23 }",
        ".{ type: .String, value: \"blargg_params.inc\", pos: 24..43 }",
        ".{ type: .Newline, value: \"\\n\", pos: 43..44 }",
        ".{ type: .Id, value: \"layout\", pos: 45..51 }",
        ".{ type: .Lparen, value: \"(\", pos: 51..52 }",
        ".{ type: .Id, value: \"push_constant\", pos: 52..65 }",
        ".{ type: .Rparen, value: \")\", pos: 65..66 }",
        ".{ type: .Id, value: \"uniform\", pos: 67..74 }",
        ".{ type: .Id, value: \"Push\", pos: 75..79 }",
        ".{ type: .Newline, value: \"\\n\", pos: 79..80 }",
        ".{ type: .Lbrace, value: \"{\", pos: 81..82 }",
        ".{ type: .Newline, value: \"\\n\", pos: 82..83 }",
        ".{ type: .Id, value: \"vec4\", pos: 87..91 }",
        ".{ type: .Id, value: \"SourceSize\", pos: 92..102 }",
        ".{ type: .Semi, value: \";\", pos: 102..103 }",
        ".{ type: .Newline, value: \"\\n\", pos: 103..104 }",
        ".{ type: .Id, value: \"uint\", pos: 108..112 }",
        ".{ type: .Id, value: \"FrameCount\", pos: 113..123 }",
        ".{ type: .Semi, value: \";\", pos: 123..124 }",
        ".{ type: .Newline, value: \"\\n\", pos: 124..125 }",
        ".{ type: .Id, value: \"float\", pos: 129..134 }",
        ".{ type: .Id, value: \"kernel_half\", pos: 135..146 }",
        ".{ type: .Comma, value: \",\", pos: 146..147 }",
        ".{ type: .Id, value: \"ntsc_sat\", pos: 148..156 }",
        ".{ type: .Semi, value: \";\", pos: 156..157 }",
        ".{ type: .Newline, value: \"\\n\", pos: 157..158 }",
        ".{ type: .Rbrace, value: \"}\", pos: 159..160 }",
        ".{ type: .Id, value: \"params\", pos: 161..167 }",
        ".{ type: .Semi, value: \";\", pos: 167..168 }",
        ".{ type: .Newline, value: \"\\n\", pos: 168..169 }",
        ".{ type: .Newline, value: \"\\n\", pos: 170..171 }",
        ".{ type: .Pragma, value: \"#pragma\", pos: 172..179 }",
        ".{ type: .Id, value: \"parameter\", pos: 180..189 }",
        ".{ type: .Id, value: \"ntsc_hue\", pos: 190..198 }",
        ".{ type: .String, value: \"Hue\", pos: 199..204 }",
        ".{ type: .Float, value: \"0.0\", pos: 205..208 }",
        ".{ type: .Float, value: \"-1.0\", pos: 209..213 }",
        ".{ type: .Float, value: \"6.3\", pos: 214..217 }",
        ".{ type: .Float, value: \"0.05\", pos: 218..222 }",
        ".{ type: .Newline, value: \"\\n\", pos: 222..223 }",
        ".{ type: .Newline, value: \"\\n\", pos: 224..225 }",
        ".{ type: .Pragma, value: \"#pragma\", pos: 226..233 }",
        ".{ type: .Id, value: \"stage\", pos: 234..239 }",
        ".{ type: .Id, value: \"vertex\", pos: 240..246 }",
        ".{ type: .Newline, value: \"\\n\", pos: 246..247 }",
        ".{ type: .Id, value: \"layout\", pos: 248..254 }",
        ".{ type: .Lparen, value: \"(\", pos: 254..255 }",
        ".{ type: .Id, value: \"location\", pos: 255..263 }",
        ".{ type: .Eq, value: \"=\", pos: 264..265 }",
        ".{ type: .Int, value: \"1\", pos: 266..267 }",
        ".{ type: .Rparen, value: \")\", pos: 267..268 }",
        ".{ type: .Id, value: \"in\", pos: 269..271 }",
        ".{ type: .Id, value: \"vec2\", pos: 272..276 }",
        ".{ type: .Id, value: \"TexCoord\", pos: 277..285 }",
        ".{ type: .Semi, value: \";\", pos: 285..286 }",
        ".{ type: .Newline, value: \"\\n\", pos: 286..287 }",
        ".{ type: .Id, value: \"layout\", pos: 288..294 }",
        ".{ type: .Lparen, value: \"(\", pos: 294..295 }",
        ".{ type: .Id, value: \"location\", pos: 295..303 }",
        ".{ type: .Eq, value: \"=\", pos: 304..305 }",
        ".{ type: .Int, value: \"0\", pos: 306..307 }",
        ".{ type: .Rparen, value: \")\", pos: 307..308 }",
        ".{ type: .Id, value: \"out\", pos: 309..312 }",
        ".{ type: .Id, value: \"vec2\", pos: 313..317 }",
        ".{ type: .Id, value: \"vTexCoord\", pos: 318..327 }",
        ".{ type: .Semi, value: \";\", pos: 327..328 }",
        ".{ type: .Newline, value: \"\\n\", pos: 328..329 }",
        ".{ type: .Newline, value: \"\\n\", pos: 330..331 }",
        ".{ type: .Id, value: \"void\", pos: 332..336 }",
        ".{ type: .Id, value: \"main\", pos: 337..341 }",
        ".{ type: .Lparen, value: \"(\", pos: 341..342 }",
        ".{ type: .Rparen, value: \")\", pos: 342..343 }",
        ".{ type: .Lbrace, value: \"{\", pos: 344..345 }",
        ".{ type: .Newline, value: \"\\n\", pos: 345..346 }",
        ".{ type: .Id, value: \"gl_Position\", pos: 350..361 }",
        ".{ type: .Eq, value: \"=\", pos: 362..363 }",
        ".{ type: .Id, value: \"global\", pos: 364..370 }",
        ".{ type: .Id, value: \"MVP\", pos: 371..374 }",
        ".{ type: .Id, value: \"Position\", pos: 377..385 }",
        ".{ type: .Semi, value: \";\", pos: 385..386 }",
        ".{ type: .Newline, value: \"\\n\", pos: 386..387 }",
        ".{ type: .Rbrace, value: \"}\", pos: 388..389 }",
        ".{ type: .Newline, value: \"\\n\", pos: 389..390 }",
        ".{ type: .Eof, value: \"\", pos: 451..451 }",
    };

    for (tokens, 0..) |token, i| {
        const got = try std.fmt.allocPrint(alloc, "{f}", .{token});
        defer alloc.free(got);
        try std.testing.expect(std.mem.eql(u8, got, expected[i]));
    }
}

test "parser" {
    const alloc = std.testing.allocator;
    const source =
        \\ #include "blargg_params.inc"
        \\ layout(push_constant) uniform Push
        \\ {
        \\    vec4 SourceSize;
        \\    uint FrameCount;
        \\    float kernel_half, ntsc_sat;
        \\ } params;
        \\ 
        \\ #pragma parameter ntsc_hue "Hue" 0.0 -1.0 6.3 0.05
        \\ #pragma format R8G8B8A8_SRGB
        \\ 
        \\ 
        \\ layout(std140, set = 0, binding = 0) uniform UBO
        \\ {
        \\    mat4 MVP;
        \\    vec4 OriginalSize;
        \\    vec4 OutputSize;
        \\ } global;
        \\ 
        \\ #pragma stage vertex
        \\ layout(location = 1) in vec2 TexCoord;
        \\ layout(location = 0) out vec2 vTexCoord;
        \\
        \\ layout(set = 2, binding = 0) uniform sampler2D Source;
        \\ 
        \\ void main() {
        \\    gl_Position = global.MVP * Position;
        \\ }
    ;

    const nodes = try parseSource(alloc, source);
    defer {
        for (nodes) |node| {
            node.deinit(alloc);
        }
        alloc.free(nodes);
    }

    const expected = [_][]const u8{
        ".LayoutBlock {.name: \"Push\", .instance: \"params\", type: .{ .PushConstant = void }," ++
            " .fields: .{.type_name: \"vec4\", .name: \"SourceSize\"}, .{.type_name: \"uint\", .name:" ++
            " \"FrameCount\"}, .{.type_name: \"float\", .name: \"kernel_half\"}, .{.type_name: \"float\"," ++
            " .name: \"ntsc_sat\"}, .pos: 31..154}",
        ".PragmaParam{.id: \"ntsc_hue\", .description: \"Hue\", .initial: 0, .min: -1, .max: 6.3, .step: 0.05, pos: 158..208}",
        ".PragmaFormat{.value: \"R8G8B8A8_SRGB\", .pos: 210..238}",
        ".LayoutBlock {.name: \"UBO\", .instance: \"global\", type: .{ .UniformBuffer = .{ .set = 0," ++
            " .binding = 0 } }, .fields: .{.type_name: \"mat4\", .name: \"MVP\"}, .{.type_name: \"vec4\"," ++
            " .name: \"OriginalSize\"}, .{.type_name: \"vec4\", .name: \"OutputSize\"}, .pos: 244..364}",
        ".LayoutUniform{.set: 2, .binding: 0, .name: \"sampler2D\", .instance: \"Source\", .pos: 473..527}",
    };
    for (nodes, 0..) |node, i| {
        const got = try std.fmt.allocPrint(alloc, "{f}", .{node});
        defer alloc.free(got);
        try std.testing.expect(std.mem.eql(u8, got, expected[i]));
    }
}

test "parser tolerates unterminated layout blocks" {
    const alloc = std.testing.allocator;
    const source =
        \\layout(push_constant) uniform Push
        \\{
        \\   float value
    ;

    const nodes = try parseSource(alloc, source);
    defer {
        for (nodes) |node| {
            node.deinit(alloc);
        }
        alloc.free(nodes);
    }

    try std.testing.expectEqual(@as(usize, 0), nodes.len);
}

test "convert" {
    const alloc = std.testing.allocator;
    const source =
        \\ layout(push_constant) uniform Push
        \\ {
        \\    vec4 SourceSize;
        \\    uint FrameCount;
        \\    float kernel_half, ntsc_sat;
        \\ } params;
    ;

    const nodes = try parseSource(alloc, source);
    defer {
        for (nodes) |node| {
            node.deinit(alloc);
        }
        alloc.free(nodes);
    }

    const expected =
        \\layout(std140, set = 0, binding = 0) uniform Push {
        \\   vec4 SourceSize;
        \\   uint FrameCount;
        \\   float kernel_half;
        \\   float ntsc_sat;
        \\} params;
    ;

    for (nodes) |node| {
        if (node.isPushConstant()) {
            const uniform_buffer = try node.convertPushConstantToUniformBuffer();
            const got = try uniform_buffer.layoutToStr(alloc);
            defer alloc.free(got);
            try std.testing.expect(std.mem.eql(u8, got, expected));
        }
    }
}

test "patch" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const source =
        \\#include "blargg_params.inc"
        \\layout(push_constant) uniform Push
        \\{
        \\   vec4 SourceSize;
        \\   uint FrameCount;
        \\   float kernel_half, ntsc_sat;
        \\} params;
        \\
        \\#pragma parameter ntsc_hue "Hue" 0.0 -1.0 6.3 0.05
        \\#pragma format R8G8B8A8_SRGB
        \\
        \\layout(std140, set = 0, binding = 0) uniform UBO
        \\{
        \\   mat4 MVP;
        \\   vec4 OriginalSize;
        \\   vec4 OutputSize;
        \\} global;
        \\
        \\layout(location = 1) in vec2 TexCoord;
        \\layout(location = 0) out vec2 vTexCoord;
        \\
        \\layout(set = 2, binding = 0) uniform sampler2D Source;
        \\
        \\void main() {
        \\   gl_Position = global.MVP * Position;
        \\}
    ;
    const expected =
        \\#include "blargg_params.inc"
        \\layout(std140, set = 1, binding = 0) uniform Push {
        \\   vec4 SourceSize;
        \\   uint FrameCount;
        \\   float kernel_half;
        \\   float ntsc_sat;
        \\} params;
        \\
        \\#pragma parameter ntsc_hue "Hue" 0.0 -1.0 6.3 0.05
        \\#pragma format R8G8B8A8_SRGB
        \\
        \\layout(std140, set = 1, binding = 1) uniform UBO {
        \\   mat4 MVP;
        \\   vec4 OriginalSize;
        \\   vec4 OutputSize;
        \\} global;
        \\
        \\layout(location = 1) in vec2 TexCoord;
        \\layout(location = 0) out vec2 vTexCoord;
        \\
        \\layout(set = 0, binding = 0) uniform sampler2D Source;
        \\
        \\void main() {
        \\   gl_Position = global.MVP * Position;
        \\}
    ;

    const got = try patchShaderSource(alloc, source, .Vertex);
    defer alloc.free(got);
    try std.testing.expect(std.mem.eql(u8, got, expected));
}

test "patch layout binding without set" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const source =
        \\#version 450
        \\layout(push_constant, std140) uniform UBO
        \\{
        \\   vec4 SourceSize;
        \\   float OUT_GAMMA;
        \\} global;
        \\
        \\layout(binding = 0, std140) uniform UBO1
        \\{
        \\   mat4 MVP;
        \\};
        \\
        \\layout(set = 0, binding = 2) uniform sampler2D Source;
    ;
    const expected =
        \\#version 450
        \\layout(std140, set = 1, binding = 0) uniform UBO {
        \\   vec4 SourceSize;
        \\   float OUT_GAMMA;
        \\} global;
        \\
        \\layout(std140, set = 1, binding = 1) uniform UBO1 {
        \\   mat4 MVP;
        \\};
        \\
        \\layout(set = 0, binding = 0) uniform sampler2D Source;
    ;

    const got = try patchShaderSource(alloc, source, .Vertex);
    defer alloc.free(got);
    try std.testing.expect(std.mem.eql(u8, got, expected));
}

test "preprocess" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const source =
        \\#version 450
        \\#include "blargg_params.inc"
        \\
        \\layout(push_constant) uniform Push
        \\{
        \\    vec4 SourceSize;
        \\    vec4 OriginalSize;
        \\    vec4 OutputSize;
        \\    int FrameCount;
        \\} params;
        \\
        \\layout(std140, set = 0, binding = 0) uniform UBO
        \\{
        \\    mat4 MVP;
        \\} global;
        \\
        \\#pragma stage vertex
        \\layout(location = 0) in vec4 Position;
        \\layout(location = 1) in vec2 TexCoord;
        \\layout(location = 0) out vec2 vTexCoord;
        \\
        \\void main()
        \\{
        \\   gl_Position = global.MVP * Position;
        \\   vTexCoord = TexCoord;
        \\}
        \\
        \\#pragma stage fragment
        \\layout(location = 0) in vec2 vTexCoord;
        \\layout(location = 0) out vec4 FragColor;
        \\layout(set = 0, binding = 2) uniform sampler2D Source;
        \\
        \\void main()
        \\{
        \\   FragColor = vec4(texture(Source, vTexCoord).rgb, 1.0);
        \\}
    ;
    const expected_common =
        \\#version 450
        \\
        \\layout(push_constant) uniform Push
        \\{
        \\   vec4 SourceSize;
        \\   uint FrameCount;
        \\   float kernel_half, ntsc_sat, ntsc_res, ntsc_bri, ntsc_hue, ntsc_sharp, fring, afacts, ntsc_bleed, LUMA_CUTOFF, stat_ph, dummy, pi_mod, vert_scal;
        \\} params;
        \\
        \\#pragma parameter kernel_half "Kernel Half-Size (speed-up)" 16.0 1.0 16.0 1.0
        \\#pragma parameter ntsc_sat "Saturation" 2.0 0.0 6.0 0.05
        \\#pragma parameter ntsc_res "Resolution" 0.0 -1.0 1.0 0.05
        \\#pragma parameter ntsc_sharp "Sharpness" 0.1 -1.0 1.0 0.05
        \\#pragma parameter ntsc_bri "Brightness" 1.0 0.0 2.0 0.01
        \\#pragma parameter ntsc_hue "Hue" 0.0 -1.0 6.3 0.05
        \\#pragma parameter fring "Fringing" 0.0 0.0 1.0 0.05
        \\#pragma parameter afacts "Artifacts" 0.0 0.0 1.0 0.05
        \\#pragma parameter ntsc_bleed "Chroma Bleed" 0.0 -0.75 2.0 0.05
        \\#pragma parameter LUMA_CUTOFF "Luma Cutoff" 0.2 0.0 1.0 0.005
        \\#pragma parameter stat_ph "Dot Crawl On/Off" 0.0 0.0 1.0 1.0
        \\#pragma parameter dummy " [ System Specific Tweaks] " 0.0 0.0 0.0 0.0
        \\#pragma parameter pi_mod "Phase-Horiz. Angle" 96.0 1.0 360.0 1.0
        \\#pragma parameter vert_scal "Phase-Vertical Scale" 0.6667 0.0 2.0 0.05555
        \\
        \\#define kernel_half params.kernel_half
        \\#define ntsc_sat params.ntsc_sat
        \\#define ntsc_res params.ntsc_res
        \\#define ntsc_sharp params.ntsc_sharp
        \\#define fring params.fring
        \\#define afacts params.afacts
        \\#define ntsc_bleed params.ntsc_bleed
        \\#define LUMA_CUTOFF params.LUMA_CUTOFF
        \\#define stat_ph params.stat_ph
        \\#define dummy params.dummy
        \\#define pi_mod params.pi_mod
        \\#define vert_scal params.vert_scal
        \\#define ntsc_bri params.ntsc_bri
        \\#define ntsc_hue params.ntsc_hue
        \\
        \\layout(std140, set = 0, binding = 0) uniform UBO
        \\{
        \\   mat4 MVP;
        \\   vec4 OriginalSize;
        \\   vec4 OutputSize;
        \\} global;
        \\
        \\
        \\layout(push_constant) uniform Push
        \\{
        \\    vec4 SourceSize;
        \\    vec4 OriginalSize;
        \\    vec4 OutputSize;
        \\    int FrameCount;
        \\} params;
        \\
        \\layout(std140, set = 0, binding = 0) uniform UBO
        \\{
        \\    mat4 MVP;
        \\} global;
        \\
        \\
    ;
    const expected_vertex =
        \\
        \\layout(location = 0) in vec4 Position;
        \\layout(location = 1) in vec2 TexCoord;
        \\layout(location = 0) out vec2 vTexCoord;
        \\
        \\void main()
        \\{
        \\   gl_Position = global.MVP * Position;
        \\   vTexCoord = TexCoord;
        \\}
        \\
        \\
    ;
    const expected_fragment =
        \\
        \\layout(location = 0) in vec2 vTexCoord;
        \\layout(location = 0) out vec4 FragColor;
        \\layout(set = 0, binding = 2) uniform sampler2D Source;
        \\
        \\void main()
        \\{
        \\   FragColor = vec4(texture(Source, vTexCoord).rgb, 1.0);
        \\}
    ;

    const shader_source = try preprocessShaderSource(alloc, std.testing.io, "./shaders/blargg/", source);
    const common = try std.mem.replaceOwned(u8, alloc, shader_source.common, "\r", "");
    defer alloc.free(common);

    try std.testing.expect(std.mem.eql(u8, common, expected_common));
    try std.testing.expect(std.mem.eql(u8, shader_source.vertex, expected_vertex));
    try std.testing.expect(std.mem.eql(u8, shader_source.fragment, expected_fragment));
}

test "parse_shader" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const source =
        \\#version 450
        \\#include "blargg_params.inc"
        \\
        \\layout(push_constant) uniform Push
        \\{
        \\    vec4 SourceSize;
        \\    vec4 OriginalSize;
        \\    vec4 OutputSize;
        \\    int FrameCount;
        \\} params;
        \\
        \\layout(std140, set = 0, binding = 0) uniform UBO
        \\{
        \\    mat4 MVP;
        \\} global;
        \\
        \\#pragma stage vertex
        \\layout(location = 0) in vec4 Position;
        \\layout(location = 1) in vec2 TexCoord;
        \\layout(location = 0) out vec2 vTexCoord;
        \\
        \\void main()
        \\{
        \\   gl_Position = global.MVP * Position;
        \\   vTexCoord = TexCoord;
        \\}
        \\
        \\#pragma stage fragment
        \\layout(location = 0) in vec2 vTexCoord;
        \\layout(location = 0) out vec4 FragColor;
        \\layout(set = 0, binding = 2) uniform sampler2D Source;
        \\
        \\void main()
        \\{
        \\   FragColor = vec4(texture(Source, vTexCoord).rgb, 1.0);
        \\}
    ;
    const expected =
        \\.{ version: 450
        \\format: null
        \\vertex:
        \\#version 450
        \\
        \\layout(std140, set = 1, binding = 0) uniform Push {
        \\   vec4 SourceSize;
        \\   uint FrameCount;
        \\   float kernel_half;
        \\   float ntsc_sat;
        \\   float ntsc_res;
        \\   float ntsc_bri;
        \\   float ntsc_hue;
        \\   float ntsc_sharp;
        \\   float fring;
        \\   float afacts;
        \\   float ntsc_bleed;
        \\   float LUMA_CUTOFF;
        \\   float stat_ph;
        \\   float dummy;
        \\   float pi_mod;
        \\   float vert_scal;
        \\} params;
        \\
        \\#pragma parameter kernel_half "Kernel Half-Size (speed-up)" 16.0 1.0 16.0 1.0
        \\#pragma parameter ntsc_sat "Saturation" 2.0 0.0 6.0 0.05
        \\#pragma parameter ntsc_res "Resolution" 0.0 -1.0 1.0 0.05
        \\#pragma parameter ntsc_sharp "Sharpness" 0.1 -1.0 1.0 0.05
        \\#pragma parameter ntsc_bri "Brightness" 1.0 0.0 2.0 0.01
        \\#pragma parameter ntsc_hue "Hue" 0.0 -1.0 6.3 0.05
        \\#pragma parameter fring "Fringing" 0.0 0.0 1.0 0.05
        \\#pragma parameter afacts "Artifacts" 0.0 0.0 1.0 0.05
        \\#pragma parameter ntsc_bleed "Chroma Bleed" 0.0 -0.75 2.0 0.05
        \\#pragma parameter LUMA_CUTOFF "Luma Cutoff" 0.2 0.0 1.0 0.005
        \\#pragma parameter stat_ph "Dot Crawl On/Off" 0.0 0.0 1.0 1.0
        \\#pragma parameter dummy " [ System Specific Tweaks] " 0.0 0.0 0.0 0.0
        \\#pragma parameter pi_mod "Phase-Horiz. Angle" 96.0 1.0 360.0 1.0
        \\#pragma parameter vert_scal "Phase-Vertical Scale" 0.6667 0.0 2.0 0.05555
        \\
        \\#define kernel_half params.kernel_half
        \\#define ntsc_sat params.ntsc_sat
        \\#define ntsc_res params.ntsc_res
        \\#define ntsc_sharp params.ntsc_sharp
        \\#define fring params.fring
        \\#define afacts params.afacts
        \\#define ntsc_bleed params.ntsc_bleed
        \\#define LUMA_CUTOFF params.LUMA_CUTOFF
        \\#define stat_ph params.stat_ph
        \\#define dummy params.dummy
        \\#define pi_mod params.pi_mod
        \\#define vert_scal params.vert_scal
        \\#define ntsc_bri params.ntsc_bri
        \\#define ntsc_hue params.ntsc_hue
        \\
        \\layout(std140, set = 1, binding = 1) uniform UBO {
        \\   mat4 MVP;
        \\   vec4 OriginalSize;
        \\   vec4 OutputSize;
        \\} global;
        \\
        \\
        \\layout(std140, set = 1, binding = 2) uniform Push {
        \\   vec4 SourceSize;
        \\   vec4 OriginalSize;
        \\   vec4 OutputSize;
        \\   int FrameCount;
        \\} params;
        \\
        \\layout(std140, set = 1, binding = 3) uniform UBO {
        \\   mat4 MVP;
        \\} global;
        \\
        \\
        \\
        \\layout(location = 0) in vec4 Position;
        \\layout(location = 1) in vec2 TexCoord;
        \\layout(location = 0) out vec2 vTexCoord;
        \\
        \\void main()
        \\{
        \\   gl_Position = global.MVP * Position;
        \\   vTexCoord = TexCoord;
        \\}
        \\
        \\
        \\fragment:
        \\#version 450
        \\
        \\layout(std140, set = 3, binding = 0) uniform Push {
        \\   vec4 SourceSize;
        \\   uint FrameCount;
        \\   float kernel_half;
        \\   float ntsc_sat;
        \\   float ntsc_res;
        \\   float ntsc_bri;
        \\   float ntsc_hue;
        \\   float ntsc_sharp;
        \\   float fring;
        \\   float afacts;
        \\   float ntsc_bleed;
        \\   float LUMA_CUTOFF;
        \\   float stat_ph;
        \\   float dummy;
        \\   float pi_mod;
        \\   float vert_scal;
        \\} params;
        \\
        \\#pragma parameter kernel_half "Kernel Half-Size (speed-up)" 16.0 1.0 16.0 1.0
        \\#pragma parameter ntsc_sat "Saturation" 2.0 0.0 6.0 0.05
        \\#pragma parameter ntsc_res "Resolution" 0.0 -1.0 1.0 0.05
        \\#pragma parameter ntsc_sharp "Sharpness" 0.1 -1.0 1.0 0.05
        \\#pragma parameter ntsc_bri "Brightness" 1.0 0.0 2.0 0.01
        \\#pragma parameter ntsc_hue "Hue" 0.0 -1.0 6.3 0.05
        \\#pragma parameter fring "Fringing" 0.0 0.0 1.0 0.05
        \\#pragma parameter afacts "Artifacts" 0.0 0.0 1.0 0.05
        \\#pragma parameter ntsc_bleed "Chroma Bleed" 0.0 -0.75 2.0 0.05
        \\#pragma parameter LUMA_CUTOFF "Luma Cutoff" 0.2 0.0 1.0 0.005
        \\#pragma parameter stat_ph "Dot Crawl On/Off" 0.0 0.0 1.0 1.0
        \\#pragma parameter dummy " [ System Specific Tweaks] " 0.0 0.0 0.0 0.0
        \\#pragma parameter pi_mod "Phase-Horiz. Angle" 96.0 1.0 360.0 1.0
        \\#pragma parameter vert_scal "Phase-Vertical Scale" 0.6667 0.0 2.0 0.05555
        \\
        \\#define kernel_half params.kernel_half
        \\#define ntsc_sat params.ntsc_sat
        \\#define ntsc_res params.ntsc_res
        \\#define ntsc_sharp params.ntsc_sharp
        \\#define fring params.fring
        \\#define afacts params.afacts
        \\#define ntsc_bleed params.ntsc_bleed
        \\#define LUMA_CUTOFF params.LUMA_CUTOFF
        \\#define stat_ph params.stat_ph
        \\#define dummy params.dummy
        \\#define pi_mod params.pi_mod
        \\#define vert_scal params.vert_scal
        \\#define ntsc_bri params.ntsc_bri
        \\#define ntsc_hue params.ntsc_hue
        \\
        \\layout(std140, set = 3, binding = 1) uniform UBO {
        \\   mat4 MVP;
        \\   vec4 OriginalSize;
        \\   vec4 OutputSize;
        \\} global;
        \\
        \\
        \\layout(std140, set = 3, binding = 2) uniform Push {
        \\   vec4 SourceSize;
        \\   vec4 OriginalSize;
        \\   vec4 OutputSize;
        \\   int FrameCount;
        \\} params;
        \\
        \\layout(std140, set = 3, binding = 3) uniform UBO {
        \\   mat4 MVP;
        \\} global;
        \\
        \\
        \\
        \\layout(location = 0) in vec2 vTexCoord;
        \\layout(location = 0) out vec4 FragColor;
        \\layout(set = 2, binding = 0) uniform sampler2D Source;
        \\
        \\void main()
        \\{
        \\   FragColor = vec4(texture(Source, vTexCoord).rgb, 1.0);
        \\}
        \\config: .{ "ntsc_hue": .{ .option_name: "Hue", .initial: 0, .min: -1, .max: 6.3, step: 0.05 }, "kernel_half": .{ .option_name: "Kernel Half-Size (speed-up)", .initial: 16, .min: 1, .max: 16, step: 1 }, "pi_mod": .{ .option_name: "Phase-Horiz. Angle", .initial: 96, .min: 1, .max: 360, step: 1 }, "vert_scal": .{ .option_name: "Phase-Vertical Scale", .initial: 0.6667, .min: 0, .max: 2, step: 0.05555 }, "ntsc_sat": .{ .option_name: "Saturation", .initial: 2, .min: 0, .max: 6, step: 0.05 }, "afacts": .{ .option_name: "Artifacts", .initial: 0, .min: 0, .max: 1, step: 0.05 }, "fring": .{ .option_name: "Fringing", .initial: 0, .min: 0, .max: 1, step: 0.05 }, "ntsc_bri": .{ .option_name: "Brightness", .initial: 1, .min: 0, .max: 2, step: 0.01 }, "LUMA_CUTOFF": .{ .option_name: "Luma Cutoff", .initial: 0.2, .min: 0, .max: 1, step: 0.005 }, "ntsc_sharp": .{ .option_name: "Sharpness", .initial: 0.1, .min: -1, .max: 1, step: 0.05 }, "ntsc_res": .{ .option_name: "Resolution", .initial: 0, .min: -1, .max: 1, step: 0.05 }, "stat_ph": .{ .option_name: "Dot Crawl On/Off", .initial: 0, .min: 0, .max: 1, step: 1 }, "ntsc_bleed": .{ .option_name: "Chroma Bleed", .initial: 0, .min: -0.75, .max: 2, step: 0.05 }, "dummy": .{ .option_name: "[ System Specific Tweaks]", .initial: 0, .min: 0, .max: 0, step: 0 }, }
    ;

    const shader = try parseShader(alloc, std.testing.io, "shaders/blargg/", source);
    const got = try std.mem.replaceOwned(
        u8,
        alloc,
        try std.fmt.allocPrint(alloc, "{f}", .{shader}),
        "\r",
        "",
    );
    defer alloc.free(got);
    try std.testing.expectEqualStrings(expected, got);
}

test "parse_shader trims parameter display names" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const source =
        \\#version 450
        \\
        \\layout(push_constant) uniform Push
        \\{
        \\    float value;
        \\} params;
        \\
        \\#pragma parameter value "   Padded Label   " 1.0 0.0 2.0 0.5
        \\#pragma stage vertex
        \\layout(location = 0) in vec4 Position;
        \\layout(location = 1) in vec2 TexCoord;
        \\layout(location = 0) out vec2 vTexCoord;
        \\void main() { gl_Position = Position; vTexCoord = TexCoord; }
        \\#pragma stage fragment
        \\layout(location = 0) in vec2 vTexCoord;
        \\layout(location = 0) out vec4 FragColor;
        \\void main() { FragColor = vec4(vTexCoord, 0.0, 1.0); }
    ;

    var shader = try parseShader(alloc, std.testing.io, ".", source);
    defer shader.deinit(alloc);

    const config = shader.config.?;
    try std.testing.expectEqualStrings("Padded Label", config.get("value").?.option_name);
}
