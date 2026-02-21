// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe filesystem path operations with traversal attack prevention.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for path operations.
pub const PathError = error{
    TraversalDetected,
    OutOfMemory,
};

/// Check if a path contains directory traversal sequences.
pub fn hasTraversal(path: []const u8) bool {
    return std.mem.indexOf(u8, path, "..") != null or
        std.mem.indexOf(u8, path, "~") != null;
}

/// Check if a path is safe (no traversal attacks).
pub fn isSafe(path: []const u8) bool {
    return !hasTraversal(path);
}

/// Sanitize a filename by removing dangerous characters.
pub fn sanitizeFilename(allocator: Allocator, filename: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    var i: usize = 0;
    while (i < filename.len) {
        const c = filename[i];
        // Check for ".." sequence
        if (c == '.' and i + 1 < filename.len and filename[i + 1] == '.') {
            try result.append('_');
            i += 2;
            continue;
        }
        switch (c) {
            '/', '\\', '<', '>', ':', '"', '|', '?', '*', 0 => try result.append('_'),
            else => try result.append(c),
        }
        i += 1;
    }

    return result.toOwnedSlice();
}

/// Safely join path components, rejecting traversal attempts.
pub fn safeJoin(allocator: Allocator, base: []const u8, parts: []const []const u8) PathError![]u8 {
    // Check for traversal in all parts
    for (parts) |part| {
        if (hasTraversal(part)) {
            return error.TraversalDetected;
        }
    }

    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    result.appendSlice(base) catch return error.OutOfMemory;

    for (parts) |part| {
        const sanitized = sanitizeFilename(allocator, part) catch return error.OutOfMemory;
        defer allocator.free(sanitized);

        if (result.items.len > 0 and result.items[result.items.len - 1] != '/') {
            result.append('/') catch return error.OutOfMemory;
        }
        result.appendSlice(sanitized) catch return error.OutOfMemory;
    }

    return result.toOwnedSlice() catch error.OutOfMemory;
}

test "hasTraversal" {
    try std.testing.expect(hasTraversal("../etc/passwd"));
    try std.testing.expect(hasTraversal("~/file"));
    try std.testing.expect(!hasTraversal("normal/path"));
}

test "isSafe" {
    try std.testing.expect(isSafe("safe/path"));
    try std.testing.expect(!isSafe("../unsafe"));
}

test "sanitizeFilename" {
    const allocator = std.testing.allocator;
    const result = try sanitizeFilename(allocator, "file<>name");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("file__name", result);
}
