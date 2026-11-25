// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
// Native marshaling and serialization only; transition policy lives in Idris.
const std = @import("std");
const bridge = @import("idris2_zig_ffi");
const c = bridge.c;

extern fn Proven_ABI_Lifecycle_lifecycleCommand([*c]c.Idris2_Value) [*c]c.Idris2_Value;

pub const available = true;
var mutex: std.Thread.Mutex = .{};
var ready = false;

// Must be called under mutex. RefC's reference counts are non-atomic.
fn apply(starting: bool) !void {
    const request: u64 = (if (ready) @as(u64, 2) else 0) +
        (if (starting) @as(u64, 1) else 0);
    var input = try bridge.Value.fromBits64(request);
    defer input.deinit();
    var output = try input.callUnary(Proven_ABI_Lifecycle_lifecycleCommand);
    defer output.deinit();
    const response = try output.bits64();
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
