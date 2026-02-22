// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeUrl - FFI bindings to libproven URL operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for URL operations.
pub const UrlError = error{
    ParseFailure,
    ProvenError,
};

/// Parsed URL components from libproven.
pub const UrlComponents = struct {
    raw: c.ProvenUrlResult,

    pub fn scheme(self: UrlComponents) []const u8 {
        if (self.raw.components.scheme == null) return "";
        return self.raw.components.scheme[0..self.raw.components.scheme_len];
    }

    pub fn host(self: UrlComponents) []const u8 {
        if (self.raw.components.host == null) return "";
        return self.raw.components.host[0..self.raw.components.host_len];
    }

    pub fn port(self: UrlComponents) ?u16 {
        if (self.raw.components.has_port) return self.raw.components.port;
        return null;
    }

    pub fn path(self: UrlComponents) []const u8 {
        if (self.raw.components.path == null) return "";
        return self.raw.components.path[0..self.raw.components.path_len];
    }

    pub fn query(self: UrlComponents) ?[]const u8 {
        if (self.raw.components.query == null) return null;
        if (self.raw.components.query_len == 0) return null;
        return self.raw.components.query[0..self.raw.components.query_len];
    }

    pub fn fragment(self: UrlComponents) ?[]const u8 {
        if (self.raw.components.fragment == null) return null;
        if (self.raw.components.fragment_len == 0) return null;
        return self.raw.components.fragment[0..self.raw.components.fragment_len];
    }

    /// Free URL components allocated by libproven.
    pub fn deinit(self: *UrlComponents) void {
        c.proven_url_free(&self.raw.components);
    }
};

/// Parse a URL into components via libproven.
pub fn parse(input: []const u8) UrlError!UrlComponents {
    const result = c.proven_url_parse(input.ptr, input.len);
    if (result.status != c.PROVEN_OK) return error.ParseFailure;
    return UrlComponents{ .raw = result };
}

test "parse" {
    var url = try parse("https://example.com:8080/path?q=1#frag");
    defer url.deinit();
    try std.testing.expectEqualStrings("https", url.scheme());
    try std.testing.expectEqualStrings("example.com", url.host());
    try std.testing.expectEqual(@as(u16, 8080), url.port().?);
}
