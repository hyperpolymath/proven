// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeUUID - FFI bindings to libproven UUID operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for UUID operations.
pub const UUIDError = error{
    GenerationFailed,
    ParseFailure,
    FormatError,
};

/// A validated UUID (128 bits / 16 bytes).
pub const UUID = struct {
    bytes: [16]u8,

    /// The nil UUID (all zeros).
    pub const nil: UUID = .{ .bytes = [_]u8{0} ** 16 };

    /// Check if this is the nil UUID via libproven.
    pub fn isNil(self: UUID) bool {
        const c_uuid = c.ProvenUUID{ .bytes = self.bytes };
        return c.proven_uuid_is_nil(c_uuid);
    }

    /// Get UUID version via libproven.
    pub fn version(self: UUID) u8 {
        const c_uuid = c.ProvenUUID{ .bytes = self.bytes };
        return c.proven_uuid_version(c_uuid);
    }

    /// Format as canonical string via libproven.
    /// Caller must call deinit() on the returned ProvenString.
    pub fn toString(self: UUID) UUIDError!ProvenString {
        const c_uuid = c.ProvenUUID{ .bytes = self.bytes };
        const result = c.proven_uuid_to_string(c_uuid);
        if (result.status != c.PROVEN_OK) return error.FormatError;
        return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
    }

    /// Compare two UUIDs for equality.
    pub fn eql(self: UUID, other: UUID) bool {
        return std.mem.eql(u8, &self.bytes, &other.bytes);
    }
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

/// Generate a UUID v4 (random) via libproven.
pub fn v4() UUIDError!UUID {
    const result = c.proven_uuid_v4();
    if (result.status != c.PROVEN_OK) return error.GenerationFailed;
    return UUID{ .bytes = result.uuid.bytes };
}

/// Parse UUID from canonical string via libproven.
pub fn parse(str: []const u8) UUIDError!UUID {
    const result = c.proven_uuid_parse(str.ptr, str.len);
    if (result.status != c.PROVEN_OK) return error.ParseFailure;
    return UUID{ .bytes = result.uuid.bytes };
}

test "v4 generation" {
    const uuid = try v4();
    try std.testing.expect(!uuid.isNil());
    try std.testing.expectEqual(@as(u8, 4), uuid.version());
}

test "parse and format" {
    const uuid = try parse("550e8400-e29b-41d4-a716-446655440000");
    try std.testing.expect(!uuid.isNil());
    const str = try uuid.toString();
    defer str.deinit();
    try std.testing.expectEqualStrings("550e8400-e29b-41d4-a716-446655440000", str.slice());
}

test "nil UUID" {
    try std.testing.expect(UUID.nil.isNil());
}
