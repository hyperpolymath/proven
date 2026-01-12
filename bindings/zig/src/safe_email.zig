// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe email validation and parsing operations.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Represents the parts of an email address.
pub const EmailParts = struct {
    local_part: []const u8,
    domain: []const u8,
};

/// Error types for email operations.
pub const EmailError = error{
    InvalidEmail,
    OutOfMemory,
};

/// Check if an email address is valid (basic check).
pub fn isValid(email: []const u8) bool {
    // Find @ symbol
    const at_index = std.mem.indexOf(u8, email, "@") orelse return false;

    // Must have exactly one @
    if (std.mem.lastIndexOf(u8, email, "@") != at_index) return false;

    const local_part = email[0..at_index];
    const domain = email[at_index + 1 ..];

    // Local part must not be empty
    if (local_part.len == 0) return false;

    // Domain must be at least 3 chars (a.b)
    if (domain.len < 3) return false;

    // Domain must contain a dot
    if (std.mem.indexOf(u8, domain, ".") == null) return false;

    // Domain must not start or end with a dot
    if (domain[0] == '.' or domain[domain.len - 1] == '.') return false;

    return true;
}

/// Split an email into local part and domain.
pub fn split(email: []const u8) EmailError!EmailParts {
    if (!isValid(email)) return error.InvalidEmail;

    const at_index = std.mem.indexOf(u8, email, "@").?;
    return EmailParts{
        .local_part = email[0..at_index],
        .domain = email[at_index + 1 ..],
    };
}

/// Extract the domain from an email address.
pub fn getDomain(email: []const u8) EmailError![]const u8 {
    const parts = try split(email);
    return parts.domain;
}

/// Extract the local part from an email address.
pub fn getLocalPart(email: []const u8) EmailError![]const u8 {
    const parts = try split(email);
    return parts.local_part;
}

/// Normalize an email address (lowercase domain).
pub fn normalize(allocator: Allocator, email: []const u8) EmailError![]u8 {
    const parts = try split(email);

    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    result.appendSlice(parts.local_part) catch return error.OutOfMemory;
    result.append('@') catch return error.OutOfMemory;

    // Lowercase the domain
    for (parts.domain) |c| {
        result.append(std.ascii.toLower(c)) catch return error.OutOfMemory;
    }

    return result.toOwnedSlice() catch error.OutOfMemory;
}

test "isValid" {
    try std.testing.expect(isValid("user@example.com"));
    try std.testing.expect(!isValid("not-an-email"));
    try std.testing.expect(!isValid("@invalid.com"));
    try std.testing.expect(!isValid("user@.com"));
}

test "split" {
    const parts = try split("user@example.com");
    try std.testing.expectEqualStrings("user", parts.local_part);
    try std.testing.expectEqualStrings("example.com", parts.domain);
}

test "normalize" {
    const allocator = std.testing.allocator;
    const result = try normalize(allocator, "User@EXAMPLE.COM");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("User@example.com", result);
}
