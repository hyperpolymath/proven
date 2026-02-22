// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeHeader - FFI bindings to libproven HTTP header operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for header operations.
pub const HeaderError = error{
    InvalidName,
    InvalidValue,
    ProvenError,
};

/// Managed string from libproven.
pub const ProvenString = struct {
    ptr: [*]u8,
    len: usize,

    pub fn slice(self: ProvenString) []const u8 {
        return self.ptr[0..self.len];
    }

    pub fn deinit(self: ProvenString) void {
        c.proven_free_string(self.ptr);
    }
};

/// Check for CRLF injection in header value via libproven.
pub fn hasCrlf(value: []const u8) bool {
    const result = c.proven_header_has_crlf(value.ptr, value.len);
    return result.status == c.PROVEN_OK and result.value;
}

/// Check if header name is valid per RFC 7230 via libproven.
pub fn isValidName(name: []const u8) bool {
    const result = c.proven_header_is_valid_name(name.ptr, name.len);
    return result.status == c.PROVEN_OK and result.value;
}

/// Check if header name is dangerous via libproven.
pub fn isDangerous(name: []const u8) bool {
    const result = c.proven_header_is_dangerous(name.ptr, name.len);
    return result.status == c.PROVEN_OK and result.value;
}

/// Create validated header string "Name: Value" via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn render(name: []const u8, value: []const u8) HeaderError!ProvenString {
    const result = c.proven_header_render(
        name.ptr,
        name.len,
        value.ptr,
        value.len,
    );
    if (result.status != c.PROVEN_OK) return error.ProvenError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

/// Build Content-Security-Policy header value via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn buildCsp(directives_json: []const u8) HeaderError!ProvenString {
    const result = c.proven_header_build_csp(directives_json.ptr, directives_json.len);
    if (result.status != c.PROVEN_OK) return error.ProvenError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

/// Build HSTS header value via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn buildHsts(max_age: i64, include_subdomains: bool, preload: bool) HeaderError!ProvenString {
    const result = c.proven_header_build_hsts(max_age, include_subdomains, preload);
    if (result.status != c.PROVEN_OK) return error.ProvenError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

test "isValidName" {
    try std.testing.expect(isValidName("Content-Type"));
    try std.testing.expect(!isValidName("Invalid Header"));
}

test "hasCrlf" {
    try std.testing.expect(hasCrlf("value\r\nwith\r\nnewlines"));
    try std.testing.expect(!hasCrlf("safe-value"));
}
