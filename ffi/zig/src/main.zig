// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Standalone surface: header checks explicitly require the RefC build.
const std = @import("std");
const core = @import("core.zig");
pub const Result = core.Result;
pub const Handle = core.Handle;
pub const Callback = core.Callback;
pub const BoolResult = core.BoolResult;
comptime {
    _ = core;
}
pub export fn proven_header_has_crlf(ptr: ?[*]const u8, len: usize) callconv(.c) BoolResult {
    return core.headerHasCrlf(@import("header_unavailable.zig"), ptr, len);
}
pub export fn proven_path_has_traversal(ptr: ?[*]const u8, len: usize) callconv(.c) BoolResult {
    return core.pathHasTraversal(@import("path_unavailable.zig"), ptr, len);
}
test "path operation refuses an unavailable Idris backend" {
    for ([_][]const u8{ "", "safe", "..", "../x", "..\\x" }) |bytes| {
        const result = proven_path_has_traversal(bytes.ptr, bytes.len);
        try std.testing.expectEqual(@as(c_int, -99), result.status);
        try std.testing.expect(!result.value);
    }
    try std.testing.expectEqual(@as(c_int, -1), proven_path_has_traversal(null, 0).status);
}
test "header operation explicitly refuses an unavailable Idris backend" {
    for ([_][]const u8{ "", "safe", "\r", "\n", "\r\n" }) |bytes| {
        const result = proven_header_has_crlf(bytes.ptr, bytes.len);
        try std.testing.expectEqual(@as(c_int, -99), result.status);
        try std.testing.expect(!result.value);
    }
    try std.testing.expectEqual(@as(c_int, -1), proven_header_has_crlf(null, 0).status);
}
