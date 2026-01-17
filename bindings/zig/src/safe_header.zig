// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe HTTP header handling that cannot crash.

const std = @import("std");

/// Error types for header operations.
pub const HeaderError = error{
    InvalidName,
    InvalidValue,
    TooLong,
    OutOfMemory,
};

/// Maximum header name length
pub const MAX_NAME_LENGTH = 256;

/// Maximum header value length
pub const MAX_VALUE_LENGTH = 8192;

/// A single HTTP header
pub const Header = struct {
    name: []const u8,
    value: []const u8,

    /// Check if this is a standard header
    pub fn isStandard(self: Header) bool {
        return isStandardHeader(self.name);
    }

    /// Check if this header is security-sensitive
    pub fn isSensitive(self: Header) bool {
        return isSensitiveHeader(self.name);
    }
};

/// HTTP header collection
pub const Headers = struct {
    items: std.ArrayList(Header),
    allocator: std.mem.Allocator,

    /// Create new headers collection
    pub fn init(allocator: std.mem.Allocator) Headers {
        return Headers{
            .items = std.ArrayList(Header).init(allocator),
            .allocator = allocator,
        };
    }

    /// Free the headers
    pub fn deinit(self: *Headers) void {
        self.items.deinit();
    }

    /// Add a header
    pub fn add(self: *Headers, name: []const u8, value: []const u8) HeaderError!void {
        if (!isValidName(name)) return error.InvalidName;
        if (!isValidValue(value)) return error.InvalidValue;

        self.items.append(Header{ .name = name, .value = value }) catch return error.OutOfMemory;
    }

    /// Get first header value by name (case-insensitive)
    pub fn get(self: Headers, name: []const u8) ?[]const u8 {
        for (self.items.items) |h| {
            if (std.ascii.eqlIgnoreCase(h.name, name)) return h.value;
        }
        return null;
    }

    /// Get all header values by name
    pub fn getAll(self: Headers, allocator: std.mem.Allocator, name: []const u8) ![]const []const u8 {
        var result = std.ArrayList([]const u8).init(allocator);
        errdefer result.deinit();

        for (self.items.items) |h| {
            if (std.ascii.eqlIgnoreCase(h.name, name)) {
                try result.append(h.value);
            }
        }

        return result.toOwnedSlice();
    }

    /// Check if header exists
    pub fn has(self: Headers, name: []const u8) bool {
        return self.get(name) != null;
    }

    /// Remove all headers with name
    pub fn remove(self: *Headers, name: []const u8) void {
        var i: usize = 0;
        while (i < self.items.items.len) {
            if (std.ascii.eqlIgnoreCase(self.items.items[i].name, name)) {
                _ = self.items.orderedRemove(i);
            } else {
                i += 1;
            }
        }
    }

    /// Get number of headers
    pub fn count(self: Headers) usize {
        return self.items.items.len;
    }

    /// Clear all headers
    pub fn clear(self: *Headers) void {
        self.items.clearRetainingCapacity();
    }
};

/// Check if a header name is valid (per RFC 7230)
pub fn isValidName(name: []const u8) bool {
    if (name.len == 0 or name.len > MAX_NAME_LENGTH) return false;

    for (name) |c| {
        if (!isTokenChar(c)) return false;
    }

    return true;
}

/// Check if a header value is valid (per RFC 7230)
pub fn isValidValue(value: []const u8) bool {
    if (value.len > MAX_VALUE_LENGTH) return false;

    for (value) |c| {
        // Allow printable ASCII and horizontal tab
        if (c == '\t') continue;
        if (c < 0x20 or c == 0x7f) return false;
    }

    return true;
}

/// Check if character is valid for token (header name)
fn isTokenChar(c: u8) bool {
    // RFC 7230 token characters
    return switch (c) {
        '!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~' => true,
        'A'...'Z', 'a'...'z', '0'...'9' => true,
        else => false,
    };
}

/// Standard HTTP header names
pub const StandardHeaders = struct {
    pub const ACCEPT = "Accept";
    pub const ACCEPT_CHARSET = "Accept-Charset";
    pub const ACCEPT_ENCODING = "Accept-Encoding";
    pub const ACCEPT_LANGUAGE = "Accept-Language";
    pub const AUTHORIZATION = "Authorization";
    pub const CACHE_CONTROL = "Cache-Control";
    pub const CONNECTION = "Connection";
    pub const CONTENT_ENCODING = "Content-Encoding";
    pub const CONTENT_LENGTH = "Content-Length";
    pub const CONTENT_TYPE = "Content-Type";
    pub const COOKIE = "Cookie";
    pub const DATE = "Date";
    pub const ETAG = "ETag";
    pub const EXPIRES = "Expires";
    pub const HOST = "Host";
    pub const IF_MATCH = "If-Match";
    pub const IF_MODIFIED_SINCE = "If-Modified-Since";
    pub const IF_NONE_MATCH = "If-None-Match";
    pub const LAST_MODIFIED = "Last-Modified";
    pub const LOCATION = "Location";
    pub const ORIGIN = "Origin";
    pub const PRAGMA = "Pragma";
    pub const REFERER = "Referer";
    pub const SET_COOKIE = "Set-Cookie";
    pub const USER_AGENT = "User-Agent";
    pub const WWW_AUTHENTICATE = "WWW-Authenticate";
    pub const X_FORWARDED_FOR = "X-Forwarded-For";
    pub const X_REQUESTED_WITH = "X-Requested-With";
};

/// Check if a header name is standard
pub fn isStandardHeader(name: []const u8) bool {
    const standard = [_][]const u8{
        StandardHeaders.ACCEPT,            StandardHeaders.ACCEPT_CHARSET,
        StandardHeaders.ACCEPT_ENCODING,   StandardHeaders.ACCEPT_LANGUAGE,
        StandardHeaders.AUTHORIZATION,     StandardHeaders.CACHE_CONTROL,
        StandardHeaders.CONNECTION,        StandardHeaders.CONTENT_ENCODING,
        StandardHeaders.CONTENT_LENGTH,    StandardHeaders.CONTENT_TYPE,
        StandardHeaders.COOKIE,            StandardHeaders.DATE,
        StandardHeaders.ETAG,              StandardHeaders.EXPIRES,
        StandardHeaders.HOST,              StandardHeaders.IF_MATCH,
        StandardHeaders.IF_MODIFIED_SINCE, StandardHeaders.IF_NONE_MATCH,
        StandardHeaders.LAST_MODIFIED,     StandardHeaders.LOCATION,
        StandardHeaders.ORIGIN,            StandardHeaders.PRAGMA,
        StandardHeaders.REFERER,           StandardHeaders.SET_COOKIE,
        StandardHeaders.USER_AGENT,        StandardHeaders.WWW_AUTHENTICATE,
    };

    for (standard) |s| {
        if (std.ascii.eqlIgnoreCase(name, s)) return true;
    }
    return false;
}

/// Check if a header is security-sensitive
pub fn isSensitiveHeader(name: []const u8) bool {
    const sensitive = [_][]const u8{
        "Authorization", "Cookie", "Set-Cookie",
        "Proxy-Authorization", "WWW-Authenticate",
        "X-Api-Key", "X-Auth-Token",
    };

    for (sensitive) |s| {
        if (std.ascii.eqlIgnoreCase(name, s)) return true;
    }
    return false;
}

/// Parse a header line "Name: Value"
pub fn parse(line: []const u8) HeaderError!Header {
    const colon_pos = std.mem.indexOfScalar(u8, line, ':') orelse return error.InvalidName;

    const name = std.mem.trim(u8, line[0..colon_pos], " \t");
    const value = std.mem.trim(u8, line[colon_pos + 1 ..], " \t");

    if (!isValidName(name)) return error.InvalidName;
    if (!isValidValue(value)) return error.InvalidValue;

    return Header{ .name = name, .value = value };
}

/// Format a header as "Name: Value"
pub fn format(allocator: std.mem.Allocator, name: []const u8, value: []const u8) ![]u8 {
    if (!isValidName(name)) return error.InvalidName;
    if (!isValidValue(value)) return error.InvalidValue;

    return std.fmt.allocPrint(allocator, "{s}: {s}", .{ name, value });
}

test "isValidName" {
    try std.testing.expect(isValidName("Content-Type"));
    try std.testing.expect(isValidName("X-Custom-Header"));
    try std.testing.expect(!isValidName(""));
    try std.testing.expect(!isValidName("Invalid Header"));
    try std.testing.expect(!isValidName("Invalid:Header"));
}

test "isValidValue" {
    try std.testing.expect(isValidValue("text/html"));
    try std.testing.expect(isValidValue("application/json; charset=utf-8"));
    try std.testing.expect(!isValidValue("value\nwith\nnewlines"));
}

test "parse" {
    const header = try parse("Content-Type: application/json");
    try std.testing.expectEqualStrings("Content-Type", header.name);
    try std.testing.expectEqualStrings("application/json", header.value);
}

test "Headers" {
    const allocator = std.testing.allocator;
    var headers = Headers.init(allocator);
    defer headers.deinit();

    try headers.add("Content-Type", "application/json");
    try headers.add("Accept", "text/html");

    try std.testing.expectEqualStrings("application/json", headers.get("content-type").?);
    try std.testing.expect(headers.has("Accept"));
    try std.testing.expect(!headers.has("Missing"));
}
