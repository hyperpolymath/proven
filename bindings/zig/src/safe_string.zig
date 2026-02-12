// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe string operations for escaping and sanitization.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Escape a string for safe HTML insertion.
pub fn escapeHtml(allocator: Allocator, value: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (value) |c| {
        switch (c) {
            '&' => try result.appendSlice("&amp;"),
            '<' => try result.appendSlice("&lt;"),
            '>' => try result.appendSlice("&gt;"),
            '"' => try result.appendSlice("&quot;"),
            '\'' => try result.appendSlice("&#x27;"),
            else => try result.append(c),
        }
    }

    return result.toOwnedSlice();
}

/// Escape a string for safe SQL interpolation.
/// Note: Prefer parameterized queries over string interpolation.
pub fn escapeSql(allocator: Allocator, value: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (value) |c| {
        if (c == '\'') {
            try result.appendSlice("''");
        } else {
            try result.append(c);
        }
    }

    return result.toOwnedSlice();
}

/// Escape a string for safe JavaScript string literal insertion.
pub fn escapeJs(allocator: Allocator, value: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (value) |c| {
        switch (c) {
            '\\' => try result.appendSlice("\\\\"),
            '"' => try result.appendSlice("\\\""),
            '\'' => try result.appendSlice("\\'"),
            '\n' => try result.appendSlice("\\n"),
            '\r' => try result.appendSlice("\\r"),
            '\t' => try result.appendSlice("\\t"),
            else => try result.append(c),
        }
    }

    return result.toOwnedSlice();
}

/// Safely truncate a string to a maximum length.
pub fn truncateSafe(allocator: Allocator, value: []const u8, max_length: usize, suffix: []const u8) ![]u8 {
    if (value.len <= max_length) {
        return try allocator.dupe(u8, value);
    }

    if (max_length <= suffix.len) {
        return try allocator.dupe(u8, value[0..max_length]);
    }

    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    try result.appendSlice(value[0 .. max_length - suffix.len]);
    try result.appendSlice(suffix);

    return result.toOwnedSlice();
}

test "escapeHtml" {
    const allocator = std.testing.allocator;
    const result = try escapeHtml(allocator, "<script>");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("&lt;script&gt;", result);
}

test "escapeSql" {
    const allocator = std.testing.allocator;
    const result = try escapeSql(allocator, "it's");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("it''s", result);
}

test "escapeJs" {
    const allocator = std.testing.allocator;
    const result = try escapeJs(allocator, "line\nbreak");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("line\\nbreak", result);
}

test "truncateSafe" {
    const allocator = std.testing.allocator;
    const result = try truncateSafe(allocator, "hello world", 5, "...");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("he...", result);
}
