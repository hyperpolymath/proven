// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeCookie - FFI bindings to libproven HTTP cookie operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for cookie operations.
pub const CookieError = error{
    InvalidName,
    InvalidValue,
    ProvenError,
};

/// SameSite attribute values.
pub const SameSite = enum(c_int) {
    strict = c.PROVEN_SAMESITE_STRICT,
    lax = c.PROVEN_SAMESITE_LAX,
    none = c.PROVEN_SAMESITE_NONE,
};

/// Cookie prefix type.
pub const CookiePrefix = enum(i64) {
    none = 0,
    secure = 1,
    host = 2,
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

/// Check for cookie injection characters via libproven.
pub fn hasInjection(value: []const u8) bool {
    const result = c.proven_cookie_has_injection(value.ptr, value.len);
    return result.status == c.PROVEN_OK and result.value;
}

/// Validate cookie name via libproven.
pub fn validateName(name: []const u8) bool {
    const result = c.proven_cookie_validate_name(name.ptr, name.len);
    return result.status == c.PROVEN_OK and result.value;
}

/// Validate cookie value via libproven.
pub fn validateValue(value: []const u8) bool {
    const result = c.proven_cookie_validate_value(value.ptr, value.len);
    return result.status == c.PROVEN_OK and result.value;
}

/// Get cookie prefix type via libproven.
pub fn getPrefix(name: []const u8) CookiePrefix {
    const result = c.proven_cookie_get_prefix(name.ptr, name.len);
    if (result.status != c.PROVEN_OK) return .none;
    return @enumFromInt(result.value);
}

/// Build Set-Cookie header value via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn buildSetCookie(
    name: []const u8,
    value: []const u8,
    domain: ?[]const u8,
    cookie_path: ?[]const u8,
    max_age: i64,
    secure: bool,
    http_only: bool,
    same_site: SameSite,
    partitioned: bool,
) CookieError!ProvenString {
    const attrs = c.ProvenCookieAttributes{
        .domain = if (domain) |d| d.ptr else null,
        .domain_len = if (domain) |d| d.len else 0,
        .path = if (cookie_path) |p| p.ptr else null,
        .path_len = if (cookie_path) |p| p.len else 0,
        .max_age = max_age,
        .secure = secure,
        .http_only = http_only,
        .same_site = @intFromEnum(same_site),
        .partitioned = partitioned,
    };
    const result = c.proven_cookie_build_set_cookie(
        name.ptr,
        name.len,
        value.ptr,
        value.len,
        attrs,
    );
    if (result.status != c.PROVEN_OK) return error.ProvenError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

/// Build delete cookie header value via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn buildDelete(name: []const u8) CookieError!ProvenString {
    const result = c.proven_cookie_build_delete(name.ptr, name.len);
    if (result.status != c.PROVEN_OK) return error.ProvenError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

test "validateName" {
    try std.testing.expect(validateName("session_id"));
}

test "hasInjection" {
    try std.testing.expect(hasInjection("value;with;semicolons"));
    try std.testing.expect(!hasInjection("safe_value"));
}
