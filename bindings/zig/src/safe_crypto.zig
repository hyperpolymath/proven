// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Cryptographic safety operations with constant-time guarantees.

const std = @import("std");

/// Compare two byte slices in constant time to prevent timing attacks.
/// Uses Zig's standard library constant-time comparison.
pub fn constantTimeCompare(a: []const u8, b: []const u8) bool {
    return std.crypto.utils.timingSafeEql(a, b);
}

/// Compare two slices where lengths may differ.
/// Returns false immediately if lengths differ (this is safe as length is public info).
pub fn constantTimeCompareVariable(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    return constantTimeCompare(a, b);
}

/// Securely zero out a byte slice to prevent data leakage.
/// Uses Zig's standard library secure zeroing.
pub fn secureZero(data: []u8) void {
    std.crypto.utils.secureZero(u8, data);
}

/// Securely zero out a typed slice.
pub fn secureZeroTyped(comptime T: type, data: []T) void {
    std.crypto.utils.secureZero(T, data);
}

/// Constant-time conditional select.
/// Returns `a` if `choice` is true, `b` otherwise.
pub fn constantTimeSelect(comptime T: type, choice: bool, a: T, b: T) T {
    return std.crypto.utils.select(T, choice, a, b);
}

/// Generate cryptographically secure random bytes.
pub fn randomBytes(buffer: []u8) void {
    std.crypto.random.bytes(buffer);
}

/// Generate a single random value of type T.
pub fn randomValue(comptime T: type) T {
    return std.crypto.random.int(T);
}

test "constantTimeCompare" {
    try std.testing.expect(constantTimeCompare("secret", "secret"));
    try std.testing.expect(!constantTimeCompare("secret", "other!"));
    try std.testing.expect(constantTimeCompare("", ""));
}

test "constantTimeCompareVariable" {
    try std.testing.expect(constantTimeCompareVariable("secret", "secret"));
    try std.testing.expect(!constantTimeCompareVariable("secret", "other"));
    try std.testing.expect(!constantTimeCompareVariable("short", "longer"));
}

test "secureZero" {
    var data = [_]u8{ 1, 2, 3, 4 };
    secureZero(&data);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0, 0, 0, 0 }, &data);
}

test "constantTimeSelect" {
    try std.testing.expectEqual(@as(u8, 10), constantTimeSelect(u8, true, 10, 20));
    try std.testing.expectEqual(@as(u8, 20), constantTimeSelect(u8, false, 10, 20));
}
