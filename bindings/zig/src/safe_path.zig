// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafePath - FFI bindings to libproven path operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for path operations.
pub const PathError = error{
    ProvenError,
};

/// Managed string from libproven that must be freed.
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

/// Check if a path contains directory traversal sequences ("..") via libproven.
/// Returns true if traversal detected.
pub fn hasTraversal(path: []const u8) bool {
    const result = c.proven_path_has_traversal(path.ptr, path.len);
    return result.status == c.PROVEN_OK and result.value;
}

/// Check if a path is safe (no traversal attacks).
pub fn isSafe(path: []const u8) bool {
    return !hasTraversal(path);
}

/// Sanitize a filename by removing dangerous characters via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn sanitizeFilename(path: []const u8) PathError!ProvenString {
    const result = c.proven_path_sanitize_filename(path.ptr, path.len);
    if (result.status != c.PROVEN_OK) return error.ProvenError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

test "hasTraversal" {
    try std.testing.expect(hasTraversal("../etc/passwd"));
    try std.testing.expect(!hasTraversal("normal/path"));
}

test "sanitizeFilename" {
    const result = try sanitizeFilename("file<>name");
    defer result.deinit();
    _ = result.slice(); // Just verify it doesn't crash
}
