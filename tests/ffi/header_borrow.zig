// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
const std = @import("std");
const BoolResult = extern struct { status: c_int, value: bool };
extern fn proven_header_has_crlf(?[*]const u8, usize) callconv(.c) BoolResult;

test "actual ABI borrows exact spans without modifying caller storage" {
    var bytes = [_]u8{ 255, 0, 128, 13, 10, 7 };
    const original = bytes;
    for (0..bytes.len + 1) |len| {
        const result = proven_header_has_crlf(&bytes, len);
        try std.testing.expectEqual(@as(c_int, 0), result.status);
        try std.testing.expectEqual(len >= 4, result.value);
        try std.testing.expectEqualSlices(u8, &original, &bytes);
    }
    const null_result = proven_header_has_crlf(null, 0);
    try std.testing.expectEqual(@as(c_int, -1), null_result.status);
    // Borrow is synchronous: the owner may modify and reuse storage afterwards.
    bytes[0] = 10;
    const changed = proven_header_has_crlf(&bytes, 1);
    try std.testing.expectEqual(@as(c_int, 0), changed.status);
    try std.testing.expect(changed.value);
}
