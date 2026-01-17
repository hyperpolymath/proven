// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe URL parsing and manipulation that cannot crash.

const std = @import("std");

/// Error types for URL operations.
pub const UrlError = error{
    InvalidFormat,
    InvalidScheme,
    InvalidHost,
    InvalidPort,
    InvalidPath,
    InvalidQuery,
    InvalidFragment,
    OutOfMemory,
};

/// A parsed URL with validated components
pub const Url = struct {
    scheme: []const u8,
    username: ?[]const u8 = null,
    password: ?[]const u8 = null,
    host: []const u8,
    port: ?u16 = null,
    path: []const u8 = "/",
    query: ?[]const u8 = null,
    fragment: ?[]const u8 = null,

    /// Get the authority component (user:pass@host:port)
    pub fn authority(self: Url, buffer: []u8) ![]u8 {
        var pos: usize = 0;

        if (self.username) |user| {
            @memcpy(buffer[pos..][0..user.len], user);
            pos += user.len;
            if (self.password) |pass| {
                buffer[pos] = ':';
                pos += 1;
                @memcpy(buffer[pos..][0..pass.len], pass);
                pos += pass.len;
            }
            buffer[pos] = '@';
            pos += 1;
        }

        @memcpy(buffer[pos..][0..self.host.len], self.host);
        pos += self.host.len;

        if (self.port) |p| {
            buffer[pos] = ':';
            pos += 1;
            const written = std.fmt.bufPrint(buffer[pos..], "{}", .{p}) catch return error.OutOfMemory;
            pos += written.len;
        }

        return buffer[0..pos];
    }

    /// Get the full URL as a string
    pub fn toString(self: Url, buffer: []u8) ![]u8 {
        var pos: usize = 0;

        // Scheme
        @memcpy(buffer[pos..][0..self.scheme.len], self.scheme);
        pos += self.scheme.len;
        @memcpy(buffer[pos..][0..3], "://");
        pos += 3;

        // Authority
        const auth = try self.authority(buffer[pos..]);
        pos += auth.len;

        // Path
        @memcpy(buffer[pos..][0..self.path.len], self.path);
        pos += self.path.len;

        // Query
        if (self.query) |q| {
            buffer[pos] = '?';
            pos += 1;
            @memcpy(buffer[pos..][0..q.len], q);
            pos += q.len;
        }

        // Fragment
        if (self.fragment) |f| {
            buffer[pos] = '#';
            pos += 1;
            @memcpy(buffer[pos..][0..f.len], f);
            pos += f.len;
        }

        return buffer[0..pos];
    }

    /// Check if URL uses a secure scheme (https, wss, etc.)
    pub fn isSecure(self: Url) bool {
        return std.mem.eql(u8, self.scheme, "https") or
               std.mem.eql(u8, self.scheme, "wss") or
               std.mem.eql(u8, self.scheme, "ftps") or
               std.mem.eql(u8, self.scheme, "sftp");
    }

    /// Get the default port for the scheme
    pub fn defaultPort(self: Url) ?u16 {
        return getDefaultPort(self.scheme);
    }

    /// Get the effective port (explicit or default)
    pub fn effectivePort(self: Url) ?u16 {
        return self.port orelse self.defaultPort();
    }
};

/// Get the default port for a scheme
pub fn getDefaultPort(scheme: []const u8) ?u16 {
    const ports = std.ComptimeStringMap(u16, .{
        .{ "http", 80 },
        .{ "https", 443 },
        .{ "ftp", 21 },
        .{ "ftps", 990 },
        .{ "ssh", 22 },
        .{ "sftp", 22 },
        .{ "ws", 80 },
        .{ "wss", 443 },
        .{ "smtp", 25 },
        .{ "smtps", 465 },
        .{ "imap", 143 },
        .{ "imaps", 993 },
        .{ "pop3", 110 },
        .{ "pop3s", 995 },
    });
    return ports.get(scheme);
}

/// Parse a URL string
pub fn parse(input: []const u8) UrlError!Url {
    if (input.len == 0) return error.InvalidFormat;

    // Find scheme
    const scheme_end = std.mem.indexOf(u8, input, "://") orelse return error.InvalidScheme;
    const scheme = input[0..scheme_end];

    if (!isValidScheme(scheme)) return error.InvalidScheme;

    var rest = input[scheme_end + 3..];

    // Find path start
    const path_start = std.mem.indexOfScalar(u8, rest, '/') orelse rest.len;
    const authority_part = rest[0..path_start];
    rest = if (path_start < rest.len) rest[path_start..] else "/";

    // Parse authority (userinfo@host:port)
    var userinfo: ?[]const u8 = null;
    var host_part = authority_part;

    if (std.mem.indexOfScalar(u8, authority_part, '@')) |at_pos| {
        userinfo = authority_part[0..at_pos];
        host_part = authority_part[at_pos + 1..];
    }

    // Parse host and port
    var host: []const u8 = undefined;
    var port: ?u16 = null;

    if (std.mem.lastIndexOfScalar(u8, host_part, ':')) |colon_pos| {
        host = host_part[0..colon_pos];
        const port_str = host_part[colon_pos + 1..];
        port = std.fmt.parseInt(u16, port_str, 10) catch return error.InvalidPort;
    } else {
        host = host_part;
    }

    if (host.len == 0) return error.InvalidHost;

    // Parse path, query, fragment
    var path: []const u8 = "/";
    var query: ?[]const u8 = null;
    var fragment: ?[]const u8 = null;

    if (rest.len > 0) {
        // Find fragment
        if (std.mem.indexOfScalar(u8, rest, '#')) |hash_pos| {
            fragment = rest[hash_pos + 1..];
            rest = rest[0..hash_pos];
        }

        // Find query
        if (std.mem.indexOfScalar(u8, rest, '?')) |q_pos| {
            query = rest[q_pos + 1..];
            path = rest[0..q_pos];
        } else {
            path = rest;
        }
    }

    // Parse userinfo
    var username: ?[]const u8 = null;
    var password: ?[]const u8 = null;

    if (userinfo) |ui| {
        if (std.mem.indexOfScalar(u8, ui, ':')) |colon_pos| {
            username = ui[0..colon_pos];
            password = ui[colon_pos + 1..];
        } else {
            username = ui;
        }
    }

    return Url{
        .scheme = scheme,
        .username = username,
        .password = password,
        .host = host,
        .port = port,
        .path = path,
        .query = query,
        .fragment = fragment,
    };
}

/// Check if a scheme is valid
pub fn isValidScheme(scheme: []const u8) bool {
    if (scheme.len == 0) return false;

    // First char must be letter
    if (!std.ascii.isAlphabetic(scheme[0])) return false;

    // Rest must be alphanumeric, +, -, or .
    for (scheme[1..]) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '+' and c != '-' and c != '.') {
            return false;
        }
    }

    return true;
}

/// Check if a string is a valid URL
pub fn isValid(input: []const u8) bool {
    _ = parse(input) catch return false;
    return true;
}

/// URL-encode a string
pub fn encode(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    errdefer list.deinit();

    for (input) |c| {
        if (std.ascii.isAlphanumeric(c) or c == '-' or c == '_' or c == '.' or c == '~') {
            try list.append(c);
        } else {
            try list.writer().print("%{X:0>2}", .{c});
        }
    }

    return list.toOwnedSlice();
}

/// URL-decode a string
pub fn decode(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    errdefer list.deinit();

    var i: usize = 0;
    while (i < input.len) {
        if (input[i] == '%' and i + 2 < input.len) {
            const hex = input[i + 1 .. i + 3];
            const byte = std.fmt.parseInt(u8, hex, 16) catch {
                try list.append('%');
                i += 1;
                continue;
            };
            try list.append(byte);
            i += 3;
        } else if (input[i] == '+') {
            try list.append(' ');
            i += 1;
        } else {
            try list.append(input[i]);
            i += 1;
        }
    }

    return list.toOwnedSlice();
}

test "parse" {
    const url = try parse("https://user:pass@example.com:8080/path?query=value#fragment");
    try std.testing.expectEqualStrings("https", url.scheme);
    try std.testing.expectEqualStrings("user", url.username.?);
    try std.testing.expectEqualStrings("pass", url.password.?);
    try std.testing.expectEqualStrings("example.com", url.host);
    try std.testing.expectEqual(@as(u16, 8080), url.port.?);
    try std.testing.expectEqualStrings("/path", url.path);
    try std.testing.expectEqualStrings("query=value", url.query.?);
    try std.testing.expectEqualStrings("fragment", url.fragment.?);
}

test "isValid" {
    try std.testing.expect(isValid("https://example.com"));
    try std.testing.expect(isValid("http://localhost:8080/path"));
    try std.testing.expect(!isValid("not a url"));
    try std.testing.expect(!isValid("://missing-scheme"));
}

test "encode" {
    const allocator = std.testing.allocator;
    const encoded = try encode(allocator, "hello world");
    defer allocator.free(encoded);
    try std.testing.expectEqualStrings("hello%20world", encoded);
}

test "decode" {
    const allocator = std.testing.allocator;
    const decoded = try decode(allocator, "hello%20world");
    defer allocator.free(decoded);
    try std.testing.expectEqualStrings("hello world", decoded);
}
