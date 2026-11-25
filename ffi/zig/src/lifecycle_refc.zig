// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
// Native marshaling and serialization only; transition policy lives in Idris.
const std = @import("std");
const c = @cImport({ @cInclude("runtime.h"); });

extern fn Proven_ABI_Lifecycle_lifecycleCommand([*c]c.Idris2_Value) [*c]c.Idris2_Value;

pub const available = true;
var mutex: std.Thread.Mutex = .{};
var ready = false;

// Must be called under mutex. RefC's reference counts are non-atomic.
fn apply(starting: bool) !void {
    const request: u64 = (if (ready) @as(u64, 2) else 0) +
        (if (starting) @as(u64, 1) else 0);
    // Generated RefC functions consume their argument reference. The returned
    // value is owned here and released once after the trampoline finishes.
    const input = c.idris2_mkBits64(request);
    if (input == null) return error.AllocationFailed;
    const output = c.idris2_trampoline(Proven_ABI_Lifecycle_lifecycleCommand(input));
    defer c.idris2_removeReference(output);
    const response = try decodeReply(output);
    const expected: u64 = if (starting) 1 else 0;
    if (response != expected) return error.InvalidLifecycleResponse;
    ready = response == 1;
}

pub fn init() i32 {
    mutex.lock();
    defer mutex.unlock();
    apply(true) catch {
        ready = false;
        return -7; // protocol/backend failure, never a fabricated success
    };
    return 0;
}

pub fn deinit() void {
    mutex.lock();
    defer mutex.unlock();
    apply(false) catch {
        // The C API has no error return. Failure must not leave readiness true.
        ready = false;
    };
}

pub fn isInitialized() bool {
    mutex.lock();
    defer mutex.unlock();
    return ready;
}

// Pointer validity and the compiler/runtime ownership convention remain trusted
// boundaries. Tag checks do not establish safety for arbitrary foreign pointers.
fn decodeReply(value: [*c]c.Idris2_Value) !u64 {
    if (value == null or (@intFromPtr(value) & 3) != 0)
        return error.InvalidLifecycleRepresentation;
    if (value.*.header.tag != c.BITS64_TAG)
        return error.InvalidLifecycleRepresentation;
    const boxed: *const c.Idris2_Bits64 = @ptrCast(@alignCast(value));
    return boxed.ui64;
}

test "lifecycle SDK decoding rejects null, immediate and wrong-tag results" {
    try std.testing.expectError(error.InvalidLifecycleRepresentation, decodeReply(null));
    try std.testing.expectError(error.InvalidLifecycleRepresentation, decodeReply(@ptrFromInt(1)));
    const wrong = c.idris2_mkInt64(1);
    defer c.idris2_removeReference(wrong);
    try std.testing.expectError(error.InvalidLifecycleRepresentation, decodeReply(wrong));
    for ([_]u64{ 0, 1, 2, std.math.maxInt(u64) }) |value| {
        const boxed = c.idris2_mkBits64(value);
        defer c.idris2_removeReference(boxed);
        try std.testing.expectEqual(value, try decodeReply(boxed));
    }
}
