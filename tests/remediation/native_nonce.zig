// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
const std = @import("std");
const c = @cImport({ @cInclude("runtime.h"); });
extern fn NativeNonceFixture_counterResult([*c]c.Idris2_Value) [*c]c.Idris2_Value;
extern fn NativeNonceFixture_entropyResult([*c]c.Idris2_Value) [*c]c.Idris2_Value;

// Test adapter only. The generated function consumes the argument reference;
// the caller owns its trampoline result. Compiler/runtime pointer validity and
// reference-count conventions remain assumptions, not new proof claims.
fn call(comptime function: anytype, value: u64) !u64 {
    const input = c.idris2_mkBits64(value);
    if (input == null) return error.AllocationFailed;
    const output = c.idris2_trampoline(function(input));
    if (output == null) return error.MissingResult;
    defer c.idris2_removeReference(output);
    if (@intFromPtr(output) % @alignOf(c.Idris2_Bits64) != 0)
        return error.InvalidRepresentation;
    if (output.*.header.tag != c.BITS64_TAG) return error.InvalidRepresentation;
    return c.idris2_vp_to_Bits64(output);
}

test "real Proven counter results across the RefC/Zig boundary" {
    const cases = [_]u64{ 0, 1, 255, 256, 65535, 65536, 0xffffff, 0x1000000, 0xffffffff, 0x100000000, std.math.maxInt(u64) };
    for (cases) |counter| {
        const expected = if (counter <= 0xffffffff) counter else std.math.maxInt(u64);
        try std.testing.expectEqual(expected, try call(NativeNonceFixture_counterResult, counter));
    }
}

test "real Proven entropy failure crosses the RefC/Zig boundary" {
    for ([_]u64{ 0, 1, 12, 32, 1000000, 1000001 }) |size| {
        try std.testing.expectEqual(@as(u64, 0), try call(NativeNonceFixture_entropyResult, size));
    }
}
