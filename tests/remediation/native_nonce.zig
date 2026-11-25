// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
const std = @import("std");
const bridge = @import("idris2_zig_ffi");
const c = bridge.c;
extern fn NativeNonceFixture_counterResult([*c]c.Idris2_Value) [*c]c.Idris2_Value;
extern fn NativeNonceFixture_entropyResult([*c]c.Idris2_Value) [*c]c.Idris2_Value;

test "real Proven counter results across the RefC/Zig boundary" {
    const cases = [_]u64{ 0, 1, 255, 256, 65535, 65536, 0xffffff, 0x1000000, 0xffffffff, 0x100000000, std.math.maxInt(u64) };
    for (cases) |counter| {
        var input = try bridge.Value.fromBits64(counter);
        defer input.deinit();
        var output = try input.callUnary(NativeNonceFixture_counterResult);
        defer output.deinit();
        const expected = if (counter <= 0xffffffff) counter else std.math.maxInt(u64);
        try std.testing.expectEqual(expected, try output.bits64());
    }
}

test "real Proven entropy failure crosses the RefC/Zig boundary" {
    for ([_]u64{ 0, 1, 12, 32, 1000000, 1000001 }) |size| {
        var input = try bridge.Value.fromBits64(size);
        defer input.deinit();
        var output = try input.callUnary(NativeNonceFixture_entropyResult);
        defer output.deinit();
        try std.testing.expectEqual(@as(u64, 0), try output.bits64());
    }
}
