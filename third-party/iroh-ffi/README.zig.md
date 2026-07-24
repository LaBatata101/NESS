# iroh for Zig

The `iroh` module supports Zig 0.15.2 and exposes the core iroh 1.0 networking
surface: secret keys and signatures, endpoint IDs, endpoint tickets,
connections, uni/bi QUIC streams, and datagrams.

The Zig API owns typed handles and byte slices while the Rust library provides
a versioned C ABI in `include/iroh.h`. Network methods are synchronous at this
boundary: operations such as `accept`, `connect`, and stream reads block the
calling thread. Run them on dedicated Zig threads when they must not block your
main loop.

## Build and test this repository

Install Zig 0.15.2 and Rust, then run:

```sh
zig build test
```

The build compiles `libiroh_ffi` with Cargo and runs crypto plus real loopback
endpoint/stream tests.

## Use from another Zig project

Add this repository as a Zig dependency. Its build automatically compiles and
statically links the Rust archive with the matching debug or release profile:

```zig
// build.zig
const iroh_dep = b.dependency("iroh", .{
    .target = target,
    .optimize = optimize,
});
exe.root_module.addImport("iroh", iroh_dep.module("iroh"));
```

Consumers that package a prebuilt library, or that cross-compile, can pass
`.iroh_lib_dir = "/absolute/path/to/libiroh_ffi"`.

For a local dependency, the corresponding `build.zig.zon` entry is:

```zig
.dependencies = .{
    .iroh = .{ .path = "/path/to/iroh-ffi" },
},
```

## Example

```zig
const std = @import("std");
const iroh = @import("iroh");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const alpn = "my-app/1";

    var endpoint = try iroh.Endpoint.bind(allocator, .{
        .preset = .n0,
        .alpns = &.{alpn},
    });
    defer endpoint.deinit();

    const ticket = try endpoint.ticket(allocator);
    defer allocator.free(ticket);
    std.debug.print("share this endpoint ticket: {s}\n", .{ticket});

    // On a client endpoint:
    // var connection = try client.connect(ticket, alpn);
    // defer connection.deinit();
    // var stream = try connection.openBi();
    // defer stream.deinit();
    // try stream.send.writeAll("hello");
    // try stream.send.finish();
}
```

Every handle has a `deinit` method. Any returned allocated slice belongs to the
allocator passed to the method. On failure, inspect `lastErrorKind()` and
`lastErrorMessage()` before the next failing iroh call on the same thread.
