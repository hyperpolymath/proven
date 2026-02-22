// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeString - FFI bindings to libproven string operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for string operations.
pub const StringError = error{
    NullPointer,
    EncodingError,
    AllocationFailed,
    ProvenError,
};

/// Managed string from libproven that must be freed.
pub const ProvenString = struct {
    ptr: [*]u8,
    len: usize,

    /// Get the string as a Zig slice.
    pub fn slice(self: ProvenString) []const u8 {
        return self.ptr[0..self.len];
    }

    /// Free the string via libproven.
    pub fn deinit(self: ProvenString) void {
        c.proven_free_string(self.ptr);
    }
};

/// Check if bytes are valid UTF-8 via libproven.
pub fn isValidUtf8(data: []const u8) bool {
    const result = c.proven_string_is_valid_utf8(data.ptr, data.len);
    return result.status == c.PROVEN_OK and result.value;
}

/// Escape string for HTML (prevents XSS) via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn escapeHtml(data: []const u8) StringError!ProvenString {
    const result = c.proven_string_escape_html(data.ptr, data.len);
    if (result.status != c.PROVEN_OK) return error.ProvenError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

/// Escape string for SQL (single quotes) via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn escapeSql(data: []const u8) StringError!ProvenString {
    const result = c.proven_string_escape_sql(data.ptr, data.len);
    if (result.status != c.PROVEN_OK) return error.ProvenError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

/// Escape string for JavaScript string literals via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn escapeJs(data: []const u8) StringError!ProvenString {
    const result = c.proven_string_escape_js(data.ptr, data.len);
    if (result.status != c.PROVEN_OK) return error.ProvenError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

test "isValidUtf8" {
    try std.testing.expect(isValidUtf8("hello"));
    try std.testing.expect(isValidUtf8(""));
}

test "escapeHtml" {
    const result = try escapeHtml("<script>");
    defer result.deinit();
    try std.testing.expectEqualStrings("&lt;script&gt;", result.slice());
}
