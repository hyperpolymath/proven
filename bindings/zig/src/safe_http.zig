// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeHttp - FFI bindings to libproven HTTP operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for HTTP operations.
pub const HttpError = error{
    EncodingError,
    ParseFailure,
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

/// URL-encode a string (RFC 3986 percent encoding) via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn urlEncode(input: []const u8) HttpError!ProvenString {
    const result = c.proven_http_url_encode(input.ptr, input.len);
    if (result.status != c.PROVEN_OK) return error.EncodingError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

/// URL-decode a percent-encoded string via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn urlDecode(input: []const u8) HttpError!ProvenString {
    const result = c.proven_http_url_decode(input.ptr, input.len);
    if (result.status != c.PROVEN_OK) return error.EncodingError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

/// Parsed WWW-Authenticate challenge.
pub const AuthChallenge = struct {
    raw: c.ProvenAuthChallengeResult,

    pub fn scheme(self: AuthChallenge) []const u8 {
        if (self.raw.challenge.scheme == null) return "";
        return @as([*]const u8, @ptrCast(self.raw.challenge.scheme))[0..self.raw.challenge.scheme_len];
    }

    pub fn realm(self: AuthChallenge) []const u8 {
        if (self.raw.challenge.realm == null) return "";
        return @as([*]const u8, @ptrCast(self.raw.challenge.realm))[0..self.raw.challenge.realm_len];
    }

    pub fn service(self: AuthChallenge) []const u8 {
        if (self.raw.challenge.service == null) return "";
        return @as([*]const u8, @ptrCast(self.raw.challenge.service))[0..self.raw.challenge.service_len];
    }

    pub fn scope(self: AuthChallenge) []const u8 {
        if (self.raw.challenge.scope == null) return "";
        return @as([*]const u8, @ptrCast(self.raw.challenge.scope))[0..self.raw.challenge.scope_len];
    }
};

/// Parse WWW-Authenticate header via libproven.
pub fn parseWwwAuthenticate(input: []const u8) HttpError!AuthChallenge {
    const result = c.proven_http_parse_www_authenticate(input.ptr, input.len);
    if (result.status != c.PROVEN_OK) return error.ParseFailure;
    return AuthChallenge{ .raw = result };
}

test "urlEncode" {
    const result = try urlEncode("hello world");
    defer result.deinit();
    try std.testing.expectEqualStrings("hello%20world", result.slice());
}

test "urlDecode" {
    const result = try urlDecode("hello%20world");
    defer result.deinit();
    try std.testing.expectEqualStrings("hello world", result.slice());
}
