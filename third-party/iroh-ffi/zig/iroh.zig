const std = @import("std");

const c = @cImport({
    @cInclude("iroh.h");
});

pub const abi_version: u32 = 1;

/// Returns the ABI version exported by the loaded native library.
pub fn runtimeAbiVersion() u32 {
    return c.iroh_abi_version();
}

pub const Error = error{
    InvalidInput,
    Bind,
    Connect,
    Connection,
    Alpn,
    KeyParsing,
    TicketParsing,
    Relay,
    Stream,
    Datagram,
    Callback,
    Closed,
    Timeout,
    Internal,
    OutOfMemory,
};

pub const ErrorKind = enum(u32) {
    invalid_input = 0,
    bind = 1,
    connect = 2,
    connection = 3,
    alpn = 4,
    key_parsing = 5,
    ticket_parsing = 6,
    relay = 7,
    stream = 8,
    datagram = 9,
    callback = 10,
    closed = 11,
    timeout = 12,
    internal = 13,
};

threadlocal var last_error_kind: ErrorKind = .internal;
threadlocal var last_error_buffer: [1024]u8 = undefined;
threadlocal var last_error_len: usize = 0;

/// Details for the last iroh error on this thread. The slice remains valid
/// until another iroh operation fails on the same thread.
pub fn lastErrorMessage() []const u8 {
    return last_error_buffer[0..last_error_len];
}

pub fn lastErrorKind() ErrorKind {
    return last_error_kind;
}

fn bytes(value: []const u8) c.IrohByteSlice {
    return .{
        .ptr = if (value.len == 0) null else value.ptr,
        .len = value.len,
    };
}

fn rememberError(error_handle: ?*c.IrohError) Error {
    const handle = error_handle orelse {
        const fallback = "iroh returned an error without details";
        @memcpy(last_error_buffer[0..fallback.len], fallback);
        last_error_len = fallback.len;
        last_error_kind = .internal;
        return error.Internal;
    };
    defer c.iroh_error_free(handle);

    const kind_value: u32 = @intCast(c.iroh_error_kind(handle));
    last_error_kind = std.meta.intToEnum(ErrorKind, kind_value) catch .internal;

    const message = c.iroh_error_message(handle);
    defer c.iroh_owned_bytes_free(message);
    const len = @min(message.len, last_error_buffer.len);
    if (len != 0 and message.ptr != null) {
        const source: [*]const u8 = @ptrCast(message.ptr);
        @memcpy(last_error_buffer[0..len], source[0..len]);
    }
    last_error_len = len;

    return switch (last_error_kind) {
        .invalid_input => error.InvalidInput,
        .bind => error.Bind,
        .connect => error.Connect,
        .connection => error.Connection,
        .alpn => error.Alpn,
        .key_parsing => error.KeyParsing,
        .ticket_parsing => error.TicketParsing,
        .relay => error.Relay,
        .stream => error.Stream,
        .datagram => error.Datagram,
        .callback => error.Callback,
        .closed => error.Closed,
        .timeout => error.Timeout,
        .internal => error.Internal,
    };
}

fn check(status: c.IrohStatus, error_handle: *?*c.IrohError) Error!void {
    if (status == c.IROH_STATUS_OK) {
        if (error_handle.*) |unexpected| c.iroh_error_free(unexpected);
        error_handle.* = null;
        return;
    }
    return rememberError(error_handle.*);
}

fn copyOwned(allocator: std.mem.Allocator, value: c.IrohOwnedBytes) Error![]u8 {
    defer c.iroh_owned_bytes_free(value);
    const result = try allocator.alloc(u8, value.len);
    if (value.len != 0) {
        const source: [*]const u8 = @ptrCast(value.ptr);
        @memcpy(result, source[0..value.len]);
    }
    return result;
}

pub const Signature = struct {
    bytes: [64]u8,
};

pub const EndpointId = struct {
    bytes: [32]u8,

    pub fn fromString(value: []const u8) Error!EndpointId {
        var result: [32]u8 = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_endpoint_id_from_string(bytes(value), &result, &error_handle), &error_handle);
        return .{ .bytes = result };
    }

    pub fn toString(self: EndpointId, allocator: std.mem.Allocator) Error![]u8 {
        var result: c.IrohOwnedBytes = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_endpoint_id_to_string(bytes(&self.bytes), &result, &error_handle), &error_handle);
        return copyOwned(allocator, result);
    }

    pub fn verify(self: EndpointId, message: []const u8, signature: Signature) Error!void {
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_endpoint_id_verify(
            bytes(&self.bytes),
            bytes(message),
            bytes(&signature.bytes),
            &error_handle,
        ), &error_handle);
    }
};

pub const ConnectionRoute = enum(u32) {
    unknown = 0,
    direct = 1,
    relay = 2,
    custom = 3,
};

pub const ConnectionStats = struct {
    has_selected_path: bool,
    route: ConnectionRoute,
    rtt_ms: u64,
    udp_tx_datagrams: u64,
    udp_tx_bytes: u64,
    udp_rx_datagrams: u64,
    udp_rx_bytes: u64,
    lost_packets: u64,
    lost_bytes: u64,
};

pub const SecretKey = struct {
    bytes: [32]u8,

    pub fn generate() Error!SecretKey {
        var result: [32]u8 = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_secret_key_generate(&result, &error_handle), &error_handle);
        return .{ .bytes = result };
    }

    pub fn public(self: SecretKey) Error!EndpointId {
        var result: [32]u8 = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_secret_key_public(bytes(&self.bytes), &result, &error_handle), &error_handle);
        return .{ .bytes = result };
    }

    pub fn sign(self: SecretKey, message: []const u8) Error!Signature {
        var result: [64]u8 = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_secret_key_sign(bytes(&self.bytes), bytes(message), &result, &error_handle), &error_handle);
        return .{ .bytes = result };
    }
};

pub const Preset = enum(u32) {
    /// Production n0 relays and discovery.
    n0 = 0,
    /// No external dependencies; useful for LANs and tests.
    minimal = 1,
    /// n0 discovery with relays disabled.
    n0_disable_relay = 2,
};

pub const EndpointOptions = struct {
    preset: Preset = .n0,
    secret_key: ?SecretKey = null,
    bind_addr: ?[]const u8 = null,
    alpns: []const []const u8 = &.{},
};

/// An iroh endpoint. Methods that wait for network activity block the calling
/// thread; call them from a dedicated Zig thread when needed.
pub const Endpoint = struct {
    handle: *c.IrohEndpoint,

    pub fn bind(allocator: std.mem.Allocator, options: EndpointOptions) Error!Endpoint {
        const ffi_alpns = try allocator.alloc(c.IrohByteSlice, options.alpns.len);
        defer allocator.free(ffi_alpns);
        for (options.alpns, ffi_alpns) |alpn, *ffi_alpn| ffi_alpn.* = bytes(alpn);

        var ffi_options = c.IrohEndpointOptions{
            .preset = @intCast(@intFromEnum(options.preset)),
            .secret_key = if (options.secret_key) |key| bytes(&key.bytes) else bytes(&.{}),
            .bind_addr = if (options.bind_addr) |addr| bytes(addr) else bytes(&.{}),
            .alpns = if (ffi_alpns.len == 0) null else ffi_alpns.ptr,
            .alpns_len = ffi_alpns.len,
        };
        var handle: ?*c.IrohEndpoint = null;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_endpoint_bind(&ffi_options, &handle, &error_handle), &error_handle);
        return .{ .handle = handle orelse return error.Internal };
    }

    pub fn deinit(self: *Endpoint) void {
        c.iroh_endpoint_free(self.handle);
        self.* = undefined;
    }

    pub fn id(self: Endpoint) Error!EndpointId {
        var result: [32]u8 = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_endpoint_id(self.handle, &result, &error_handle), &error_handle);
        return .{ .bytes = result };
    }

    pub fn secretKey(self: Endpoint) Error!SecretKey {
        var result: [32]u8 = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_endpoint_secret_key(self.handle, &result, &error_handle), &error_handle);
        return .{ .bytes = result };
    }

    /// Returns an alloced, shareable endpoint ticket.
    pub fn ticket(self: Endpoint, allocator: std.mem.Allocator) Error![]u8 {
        var result: c.IrohOwnedBytes = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_endpoint_ticket(self.handle, &result, &error_handle), &error_handle);
        return copyOwned(allocator, result);
    }

    pub fn connect(self: Endpoint, ticket_value: []const u8, alpn: []const u8) Error!Connection {
        var handle: ?*c.IrohConnection = null;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_endpoint_connect_ticket(
            self.handle,
            bytes(ticket_value),
            bytes(alpn),
            &handle,
            &error_handle,
        ), &error_handle);
        return .{ .handle = handle orelse return error.Internal };
    }

    pub fn accept(self: Endpoint) Error!Connection {
        var handle: ?*c.IrohConnection = null;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_endpoint_accept(self.handle, &handle, &error_handle), &error_handle);
        return .{ .handle = handle orelse return error.Internal };
    }

    pub fn close(self: Endpoint) Error!void {
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_endpoint_close(self.handle, &error_handle), &error_handle);
    }

    pub fn isClosed(self: Endpoint) bool {
        return c.iroh_endpoint_is_closed(self.handle);
    }
};

pub const BiStream = struct {
    send: SendStream,
    recv: RecvStream,

    pub fn deinit(self: *BiStream) void {
        self.send.deinit();
        self.recv.deinit();
        self.* = undefined;
    }
};

pub const Connection = struct {
    handle: *c.IrohConnection,

    pub fn deinit(self: *Connection) void {
        c.iroh_connection_free(self.handle);
        self.* = undefined;
    }

    pub fn alpn(self: Connection, allocator: std.mem.Allocator) Error![]u8 {
        var result: c.IrohOwnedBytes = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_connection_alpn(self.handle, &result, &error_handle), &error_handle);
        return copyOwned(allocator, result);
    }

    pub fn remoteId(self: Connection) Error!EndpointId {
        var result: [32]u8 = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_connection_remote_id(self.handle, &result, &error_handle), &error_handle);
        return .{ .bytes = result };
    }

    pub fn stats(self: Connection) Error!ConnectionStats {
        var result: c.IrohConnectionStats = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_connection_stats(self.handle, &result, &error_handle), &error_handle);
        return .{
            .has_selected_path = result.has_selected_path,
            .route = std.meta.intToEnum(ConnectionRoute, @as(u32, @intCast(result.route))) catch .unknown,
            .rtt_ms = result.rtt_ms,
            .udp_tx_datagrams = result.udp_tx_datagrams,
            .udp_tx_bytes = result.udp_tx_bytes,
            .udp_rx_datagrams = result.udp_rx_datagrams,
            .udp_rx_bytes = result.udp_rx_bytes,
            .lost_packets = result.lost_packets,
            .lost_bytes = result.lost_bytes,
        };
    }

    pub fn openUni(self: Connection) Error!SendStream {
        var handle: ?*c.IrohSendStream = null;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_connection_open_uni(self.handle, &handle, &error_handle), &error_handle);
        return .{ .handle = handle orelse return error.Internal };
    }

    pub fn acceptUni(self: Connection) Error!RecvStream {
        var handle: ?*c.IrohRecvStream = null;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_connection_accept_uni(self.handle, &handle, &error_handle), &error_handle);
        return .{ .handle = handle orelse return error.Internal };
    }

    pub fn openBi(self: Connection) Error!BiStream {
        return self.bi(false);
    }

    pub fn acceptBi(self: Connection) Error!BiStream {
        return self.bi(true);
    }

    fn bi(self: Connection, accept_stream: bool) Error!BiStream {
        var send: ?*c.IrohSendStream = null;
        var recv: ?*c.IrohRecvStream = null;
        var error_handle: ?*c.IrohError = null;
        const status = if (accept_stream)
            c.iroh_connection_accept_bi(self.handle, &send, &recv, &error_handle)
        else
            c.iroh_connection_open_bi(self.handle, &send, &recv, &error_handle);
        try check(status, &error_handle);
        return .{
            .send = .{ .handle = send orelse return error.Internal },
            .recv = .{ .handle = recv orelse return error.Internal },
        };
    }

    pub fn sendDatagram(self: Connection, data: []const u8) Error!void {
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_connection_send_datagram(self.handle, bytes(data), &error_handle), &error_handle);
    }

    pub fn readDatagram(self: Connection, allocator: std.mem.Allocator) Error![]u8 {
        var result: c.IrohOwnedBytes = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_connection_read_datagram(self.handle, &result, &error_handle), &error_handle);
        return copyOwned(allocator, result);
    }

    pub fn close(self: Connection, error_code: u64, reason: []const u8) Error!void {
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_connection_close(self.handle, error_code, bytes(reason), &error_handle), &error_handle);
    }
};

pub const SendStream = struct {
    handle: *c.IrohSendStream,

    pub fn deinit(self: *SendStream) void {
        c.iroh_send_stream_free(self.handle);
        self.* = undefined;
    }

    pub fn writeAll(self: SendStream, data: []const u8) Error!void {
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_send_stream_write_all(self.handle, bytes(data), &error_handle), &error_handle);
    }

    pub fn finish(self: SendStream) Error!void {
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_send_stream_finish(self.handle, &error_handle), &error_handle);
    }

    pub fn reset(self: SendStream, error_code: u64) Error!void {
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_send_stream_reset(self.handle, error_code, &error_handle), &error_handle);
    }
};

pub const RecvStream = struct {
    handle: *c.IrohRecvStream,

    pub fn deinit(self: *RecvStream) void {
        c.iroh_recv_stream_free(self.handle);
        self.* = undefined;
    }

    pub fn read(self: RecvStream, allocator: std.mem.Allocator, size_limit: u32) Error![]u8 {
        var result: c.IrohOwnedBytes = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_recv_stream_read(self.handle, size_limit, &result, &error_handle), &error_handle);
        return copyOwned(allocator, result);
    }

    pub fn readExact(self: RecvStream, allocator: std.mem.Allocator, size: u32) Error![]u8 {
        var result: c.IrohOwnedBytes = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_recv_stream_read_exact(self.handle, size, &result, &error_handle), &error_handle);
        return copyOwned(allocator, result);
    }

    pub fn readToEnd(self: RecvStream, allocator: std.mem.Allocator, size_limit: u32) Error![]u8 {
        var result: c.IrohOwnedBytes = undefined;
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_recv_stream_read_to_end(self.handle, size_limit, &result, &error_handle), &error_handle);
        return copyOwned(allocator, result);
    }

    pub fn stop(self: RecvStream, error_code: u64) Error!void {
        var error_handle: ?*c.IrohError = null;
        try check(c.iroh_recv_stream_stop(self.handle, error_code, &error_handle), &error_handle);
    }
};

test "crypto roundtrip" {
    try std.testing.expectEqual(abi_version, runtimeAbiVersion());
    const secret = try SecretKey.generate();
    const id = try secret.public();
    const message = "hello from Zig";
    const signature = try secret.sign(message);
    try id.verify(message, signature);

    const text = try id.toString(std.testing.allocator);
    defer std.testing.allocator.free(text);
    const parsed = try EndpointId.fromString(text);
    try std.testing.expectEqualSlices(u8, &id.bytes, &parsed.bytes);

    try std.testing.expectError(error.KeyParsing, EndpointId.fromString("not-an-endpoint-id"));
    try std.testing.expect(lastErrorMessage().len != 0);
    try std.testing.expectEqual(ErrorKind.key_parsing, lastErrorKind());
}

fn echoServer(endpoint: *Endpoint, server_error: *?anyerror) void {
    var connection = endpoint.accept() catch |err| {
        server_error.* = err;
        return;
    };
    defer connection.deinit();
    var stream = connection.acceptBi() catch |err| {
        server_error.* = err;
        return;
    };
    defer stream.deinit();
    const request = stream.recv.readToEnd(std.heap.page_allocator, 1024) catch |err| {
        server_error.* = err;
        return;
    };
    defer std.heap.page_allocator.free(request);
    stream.send.writeAll(request) catch |err| {
        server_error.* = err;
        return;
    };
    stream.send.finish() catch |err| {
        server_error.* = err;
        return;
    };
}

test "minimal endpoints exchange a bidirectional stream" {
    const alpn = "iroh/zig/test/1";
    var server = Endpoint.bind(std.testing.allocator, .{
        .preset = .minimal,
        .alpns = &.{alpn},
    }) catch |err| {
        std.debug.print("server bind failed: {s}\n", .{lastErrorMessage()});
        return err;
    };
    defer server.deinit();
    const ticket_value = try server.ticket(std.testing.allocator);
    defer std.testing.allocator.free(ticket_value);

    var server_error: ?anyerror = null;
    const thread = try std.Thread.spawn(.{}, echoServer, .{ &server, &server_error });
    var server_joined = false;
    defer if (!server_joined) {
        // Wake a still-blocked accept before releasing the endpoint handle.
        server.close() catch {};
        thread.join();
    };

    var client = Endpoint.bind(std.testing.allocator, .{ .preset = .minimal }) catch |err| {
        std.debug.print("client bind failed: {s}\n", .{lastErrorMessage()});
        return err;
    };
    defer client.deinit();
    var connection = try client.connect(ticket_value, alpn);
    defer connection.deinit();
    var stream = try connection.openBi();
    defer stream.deinit();
    try stream.send.writeAll("ping");
    try stream.send.finish();
    const response = try stream.recv.readToEnd(std.testing.allocator, 1024);
    defer std.testing.allocator.free(response);
    try std.testing.expectEqualStrings("ping", response);

    const stats = try connection.stats();
    try std.testing.expect(stats.has_selected_path);
    try std.testing.expect(stats.route != .unknown);
    try std.testing.expect(stats.udp_tx_bytes != 0);
    try std.testing.expect(stats.udp_rx_bytes != 0);

    thread.join();
    server_joined = true;
    if (server_error) |err| return err;
    try server.close();
    try client.close();
}
