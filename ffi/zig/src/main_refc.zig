// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
const core = @import("core.zig");
pub const Result = core.Result;
pub const Handle = core.Handle;
pub const Callback = core.Callback;
pub const BoolResult = core.BoolResult;
comptime {
    _ = core;
}
pub const proven = @This();
pub export fn proven_header_has_crlf(ptr: ?[*]const u8, len: usize) callconv(.c) core.BoolResult {
    return core.headerHasCrlf(@import("header_refc.zig"), ptr, len);
}

const std = @import("std");
test "header ABI uses Idris for every one-byte and two-byte input" {
    var bytes: [2]u8 = undefined;
    for (0..256) |a| {
        bytes[0] = @intCast(a);
        const single = proven.proven_header_has_crlf(&bytes, 1);
        try std.testing.expectEqual(@as(c_int, 0), single.status);
        try std.testing.expectEqual(a == 10 or a == 13, single.value);
        for (0..256) |b| {
            bytes[1] = @intCast(b);
            const r = proven.proven_header_has_crlf(&bytes, 2);
            try std.testing.expectEqual(@as(c_int, 0), r.status);
            try std.testing.expectEqual(a == 10 or a == 13 or b == 10 or b == 13, r.value);
        }
    }
}
test "header ABI preserves null, length, NUL and non-UTF8 semantics" {
    try std.testing.expectEqual(@as(c_int, -1), proven.proven_header_has_crlf(null, 0).status);
    try std.testing.expectEqual(@as(c_int, -1), proven.proven_header_has_crlf(null, 99).status);
    const bytes = [_]u8{ 0xff, 0, 13, 0x80 };
    for (0..bytes.len + 1) |len| {
        const r = proven.proven_header_has_crlf(&bytes, len);
        try std.testing.expectEqual(@as(c_int, 0), r.status);
        try std.testing.expectEqual(len >= 3, r.value);
    }
    var long = [_]u8{0} ** 8192;
    long[long.len - 1] = 10;
    try std.testing.expect(proven.proven_header_has_crlf(&long, long.len).value);
    try std.testing.expect(!proven.proven_header_has_crlf(&long, long.len - 1).value);
}

test "proven_header_has_crlf detects CRLF" {
    // Classic CRLF injection.
    const r1 = proven_header_has_crlf("Set-Cookie: foo=bar\r\nSet-Cookie: admin=true".ptr, "Set-Cookie: foo=bar\r\nSet-Cookie: admin=true".len);
    try std.testing.expectEqual(@as(c_int, 0), r1.status);
    try std.testing.expect(r1.value);

    const r2 = proven_header_has_crlf("X-Custom\r\n".ptr, "X-Custom\r\n".len);
    try std.testing.expectEqual(@as(c_int, 0), r2.status);
    try std.testing.expect(r2.value);
}

test "proven_header_has_crlf allows safe headers" {
    const r1 = proven_header_has_crlf("Content-Type: application/json".ptr, "Content-Type: application/json".len);
    try std.testing.expectEqual(@as(c_int, 0), r1.status);
    try std.testing.expect(!r1.value);

    const r2 = proven_header_has_crlf("Authorization: Bearer token123".ptr, "Authorization: Bearer token123".len);
    try std.testing.expectEqual(@as(c_int, 0), r2.status);
    try std.testing.expect(!r2.value);
}

test "proven_header_has_crlf handles null and empty" {
    const r_null = proven_header_has_crlf(null, 0);
    try std.testing.expectEqual(@as(c_int, -1), r_null.status);

    const r_empty = proven_header_has_crlf("".ptr, 0);
    try std.testing.expectEqual(@as(c_int, 0), r_empty.status);
    try std.testing.expect(!r_empty.value);

    // Single byte should not crash.
    const r_single = proven_header_has_crlf("X".ptr, 1);
    try std.testing.expectEqual(@as(c_int, 0), r_single.status);
    try std.testing.expect(!r_single.value);
}
