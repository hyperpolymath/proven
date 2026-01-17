// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe HTTP cookie handling that cannot crash.

const std = @import("std");

/// Error types for cookie operations.
pub const CookieError = error{
    InvalidName,
    InvalidValue,
    InvalidDomain,
    InvalidPath,
    InvalidDate,
    TooLong,
    OutOfMemory,
};

/// Maximum cookie size (4KB per RFC 6265)
pub const MAX_COOKIE_SIZE = 4096;

/// SameSite attribute values
pub const SameSite = enum {
    strict,
    lax,
    none,

    pub fn toString(self: SameSite) []const u8 {
        return switch (self) {
            .strict => "Strict",
            .lax => "Lax",
            .none => "None",
        };
    }
};

/// A parsed HTTP cookie
pub const Cookie = struct {
    name: []const u8,
    value: []const u8,
    domain: ?[]const u8 = null,
    path: ?[]const u8 = null,
    expires: ?i64 = null,        // Unix timestamp
    max_age: ?i64 = null,        // Seconds
    secure: bool = false,
    http_only: bool = false,
    same_site: ?SameSite = null,

    /// Check if cookie is expired
    pub fn isExpired(self: Cookie) bool {
        if (self.max_age) |ma| {
            if (ma <= 0) return true;
        }
        if (self.expires) |exp| {
            const now = std.time.timestamp();
            if (exp <= now) return true;
        }
        return false;
    }

    /// Check if cookie is session cookie (no expiry)
    pub fn isSession(self: Cookie) bool {
        return self.expires == null and self.max_age == null;
    }

    /// Check if cookie should be sent to domain
    pub fn matchesDomain(self: Cookie, domain: []const u8) bool {
        if (self.domain) |d| {
            // Domain matching per RFC 6265
            if (std.mem.eql(u8, d, domain)) return true;
            if (d.len > 0 and d[0] == '.') {
                // Subdomain matching
                return std.mem.endsWith(u8, domain, d);
            }
        }
        return true; // No domain restriction
    }

    /// Check if cookie should be sent to path
    pub fn matchesPath(self: Cookie, path: []const u8) bool {
        if (self.path) |p| {
            return std.mem.startsWith(u8, path, p);
        }
        return true; // No path restriction
    }

    /// Format as Set-Cookie header value
    pub fn toSetCookie(self: Cookie, allocator: std.mem.Allocator) ![]u8 {
        var list = std.ArrayList(u8).init(allocator);
        errdefer list.deinit();

        // Name=Value
        try list.appendSlice(self.name);
        try list.append('=');
        try list.appendSlice(self.value);

        // Domain
        if (self.domain) |d| {
            try list.appendSlice("; Domain=");
            try list.appendSlice(d);
        }

        // Path
        if (self.path) |p| {
            try list.appendSlice("; Path=");
            try list.appendSlice(p);
        }

        // Max-Age
        if (self.max_age) |ma| {
            try list.writer().print("; Max-Age={}", .{ma});
        }

        // Secure
        if (self.secure) {
            try list.appendSlice("; Secure");
        }

        // HttpOnly
        if (self.http_only) {
            try list.appendSlice("; HttpOnly");
        }

        // SameSite
        if (self.same_site) |ss| {
            try list.appendSlice("; SameSite=");
            try list.appendSlice(ss.toString());
        }

        return list.toOwnedSlice();
    }

    /// Format as Cookie header value (just Name=Value)
    pub fn toCookie(self: Cookie, allocator: std.mem.Allocator) ![]u8 {
        return std.fmt.allocPrint(allocator, "{s}={s}", .{ self.name, self.value });
    }
};

/// Check if a cookie name is valid
pub fn isValidName(name: []const u8) bool {
    if (name.len == 0) return false;

    for (name) |c| {
        // RFC 6265: token characters
        if (c <= 0x20 or c >= 0x7f) return false;
        if (std.mem.indexOfScalar(u8, "()<>@,;:\\\"/[]?={} \t", c) != null) return false;
    }

    return true;
}

/// Check if a cookie value is valid
pub fn isValidValue(value: []const u8) bool {
    for (value) |c| {
        // RFC 6265: cookie-octet
        if (c < 0x21 or c > 0x7e) return false;
        if (c == '"' or c == ',' or c == ';' or c == '\\') return false;
    }
    return true;
}

/// Parse a Cookie header (client-side format: name=value; name2=value2)
pub fn parseCookieHeader(allocator: std.mem.Allocator, header: []const u8) ![]Cookie {
    var cookies = std.ArrayList(Cookie).init(allocator);
    errdefer cookies.deinit();

    var it = std.mem.splitSequence(u8, header, "; ");
    while (it.next()) |pair| {
        if (pair.len == 0) continue;

        if (std.mem.indexOfScalar(u8, pair, '=')) |eq_pos| {
            const name = std.mem.trim(u8, pair[0..eq_pos], " ");
            const value = std.mem.trim(u8, pair[eq_pos + 1 ..], " ");

            if (isValidName(name)) {
                try cookies.append(Cookie{ .name = name, .value = value });
            }
        }
    }

    return cookies.toOwnedSlice();
}

/// Parse a Set-Cookie header (server-side format with attributes)
pub fn parseSetCookie(header: []const u8) CookieError!Cookie {
    var it = std.mem.splitSequence(u8, header, "; ");

    // First part is name=value
    const first = it.next() orelse return error.InvalidName;
    const eq_pos = std.mem.indexOfScalar(u8, first, '=') orelse return error.InvalidName;

    const name = std.mem.trim(u8, first[0..eq_pos], " ");
    const value = std.mem.trim(u8, first[eq_pos + 1 ..], " ");

    if (!isValidName(name)) return error.InvalidName;

    var cookie = Cookie{ .name = name, .value = value };

    // Parse attributes
    while (it.next()) |attr| {
        if (attr.len == 0) continue;

        const lower_attr = attr; // Would need to lowercase
        if (std.mem.indexOfScalar(u8, attr, '=')) |eq| {
            const attr_name = std.mem.trim(u8, attr[0..eq], " ");
            const attr_value = std.mem.trim(u8, attr[eq + 1 ..], " ");

            if (std.ascii.eqlIgnoreCase(attr_name, "Domain")) {
                cookie.domain = attr_value;
            } else if (std.ascii.eqlIgnoreCase(attr_name, "Path")) {
                cookie.path = attr_value;
            } else if (std.ascii.eqlIgnoreCase(attr_name, "Max-Age")) {
                cookie.max_age = std.fmt.parseInt(i64, attr_value, 10) catch null;
            } else if (std.ascii.eqlIgnoreCase(attr_name, "SameSite")) {
                if (std.ascii.eqlIgnoreCase(attr_value, "Strict")) {
                    cookie.same_site = .strict;
                } else if (std.ascii.eqlIgnoreCase(attr_value, "Lax")) {
                    cookie.same_site = .lax;
                } else if (std.ascii.eqlIgnoreCase(attr_value, "None")) {
                    cookie.same_site = .none;
                }
            }
        } else {
            // Flag attributes
            if (std.ascii.eqlIgnoreCase(lower_attr, "Secure")) {
                cookie.secure = true;
            } else if (std.ascii.eqlIgnoreCase(lower_attr, "HttpOnly")) {
                cookie.http_only = true;
            }
        }
    }

    return cookie;
}

/// Create a new cookie
pub fn create(name: []const u8, value: []const u8) CookieError!Cookie {
    if (!isValidName(name)) return error.InvalidName;
    if (!isValidValue(value)) return error.InvalidValue;

    return Cookie{ .name = name, .value = value };
}

/// Create a secure cookie (HttpOnly, Secure, SameSite=Strict)
pub fn createSecure(name: []const u8, value: []const u8) CookieError!Cookie {
    var cookie = try create(name, value);
    cookie.http_only = true;
    cookie.secure = true;
    cookie.same_site = .strict;
    return cookie;
}

/// Create a session cookie
pub fn createSession(name: []const u8, value: []const u8) CookieError!Cookie {
    return create(name, value);
}

/// Create a persistent cookie with max-age
pub fn createPersistent(name: []const u8, value: []const u8, max_age_seconds: i64) CookieError!Cookie {
    var cookie = try create(name, value);
    cookie.max_age = max_age_seconds;
    return cookie;
}

test "isValidName" {
    try std.testing.expect(isValidName("session_id"));
    try std.testing.expect(isValidName("_token"));
    try std.testing.expect(!isValidName(""));
    try std.testing.expect(!isValidName("invalid name"));
    try std.testing.expect(!isValidName("invalid;name"));
}

test "isValidValue" {
    try std.testing.expect(isValidValue("abc123"));
    try std.testing.expect(isValidValue("base64value"));
    try std.testing.expect(!isValidValue("value with spaces"));
}

test "parseSetCookie" {
    const cookie = try parseSetCookie("session=abc123; Path=/; HttpOnly; Secure; SameSite=Strict");
    try std.testing.expectEqualStrings("session", cookie.name);
    try std.testing.expectEqualStrings("abc123", cookie.value);
    try std.testing.expectEqualStrings("/", cookie.path.?);
    try std.testing.expect(cookie.http_only);
    try std.testing.expect(cookie.secure);
    try std.testing.expectEqual(SameSite.strict, cookie.same_site.?);
}

test "Cookie.toSetCookie" {
    const allocator = std.testing.allocator;
    var cookie = Cookie{
        .name = "test",
        .value = "value",
        .path = "/",
        .http_only = true,
        .secure = true,
    };
    const header = try cookie.toSetCookie(allocator);
    defer allocator.free(header);

    try std.testing.expect(std.mem.indexOf(u8, header, "test=value") != null);
    try std.testing.expect(std.mem.indexOf(u8, header, "HttpOnly") != null);
    try std.testing.expect(std.mem.indexOf(u8, header, "Secure") != null);
}
