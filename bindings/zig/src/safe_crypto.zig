// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Cryptographic safety operations with constant-time guarantees.

const std = @import("std");

/// Compare two byte slices in constant time to prevent timing attacks.
/// Uses Zig's standard library constant-time comparison.
pub fn constantTimeCompare(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    var acc: u8 = 0;
    for (a, b) |x, y| {
        acc |= x ^ y;
    }
    return acc == 0;
}

/// Compare two slices where lengths may differ.
/// Returns false immediately if lengths differ (this is safe as length is public info).
pub fn constantTimeCompareVariable(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    return constantTimeCompare(a, b);
}

/// Securely zero out a byte slice to prevent data leakage.
/// Uses volatile write to prevent compiler from optimizing away the zeroing.
pub fn secureZero(data: []u8) void {
    const volatile_ptr: [*]volatile u8 = @ptrCast(data.ptr);
    for (0..data.len) |i| {
        volatile_ptr[i] = 0;
    }
}

/// Securely zero out a typed slice.
pub fn secureZeroTyped(comptime T: type, data: []T) void {
    const bytes: []u8 = @as([*]u8, @ptrCast(data.ptr))[0 .. data.len * @sizeOf(T)];
    secureZero(bytes);
}

/// Constant-time conditional select.
/// Returns `a` if `choice` is true, `b` otherwise.
pub fn constantTimeSelect(comptime T: type, choice: bool, a: T, b: T) T {
    // Use bitwise operations for constant-time selection
    const mask = @as(T, 0) -% @as(T, @intFromBool(choice));
    return (a & mask) | (b & ~mask);
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
