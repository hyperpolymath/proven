// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe arithmetic operations that cannot crash or overflow unexpectedly.

const std = @import("std");

/// Error types for math operations.
pub const MathError = error{
    DivisionByZero,
    Overflow,
};

/// Safely divide two integers, returning error on division by zero.
pub fn safeDiv(comptime T: type, numerator: T, denominator: T) MathError!T {
    if (denominator == 0) return error.DivisionByZero;
    return @divTrunc(numerator, denominator);
}

/// Safely compute modulo, returning error on division by zero.
pub fn safeMod(comptime T: type, numerator: T, denominator: T) MathError!T {
    if (denominator == 0) return error.DivisionByZero;
    return @mod(numerator, denominator);
}

/// Safely add two integers, returning error on overflow.
pub fn safeAdd(comptime T: type, a: T, b: T) MathError!T {
    return std.math.add(T, a, b) catch error.Overflow;
}

/// Safely subtract two integers, returning error on overflow.
pub fn safeSub(comptime T: type, a: T, b: T) MathError!T {
    return std.math.sub(T, a, b) catch error.Overflow;
}

/// Safely multiply two integers, returning error on overflow.
pub fn safeMul(comptime T: type, a: T, b: T) MathError!T {
    return std.math.mul(T, a, b) catch error.Overflow;
}

/// Safe division that returns null instead of error.
pub fn safeDivOrNull(comptime T: type, numerator: T, denominator: T) ?T {
    return safeDiv(T, numerator, denominator) catch null;
}

/// Safe addition that returns null instead of error.
pub fn safeAddOrNull(comptime T: type, a: T, b: T) ?T {
    return safeAdd(T, a, b) catch null;
}

/// Safe subtraction that returns null instead of error.
pub fn safeSubOrNull(comptime T: type, a: T, b: T) ?T {
    return safeSub(T, a, b) catch null;
}

/// Safe multiplication that returns null instead of error.
pub fn safeMulOrNull(comptime T: type, a: T, b: T) ?T {
    return safeMul(T, a, b) catch null;
}

test "safeDiv" {
    try std.testing.expectEqual(@as(i64, 5), try safeDiv(i64, 10, 2));
    try std.testing.expectError(error.DivisionByZero, safeDiv(i64, 10, 0));
}

test "safeMod" {
    try std.testing.expectEqual(@as(i64, 1), try safeMod(i64, 10, 3));
    try std.testing.expectError(error.DivisionByZero, safeMod(i64, 10, 0));
}

test "safeAdd" {
    try std.testing.expectEqual(@as(i64, 3), try safeAdd(i64, 1, 2));
    try std.testing.expectError(error.Overflow, safeAdd(i64, std.math.maxInt(i64), 1));
}

test "safeSub" {
    try std.testing.expectEqual(@as(i64, 2), try safeSub(i64, 5, 3));
    try std.testing.expectError(error.Overflow, safeSub(i64, std.math.minInt(i64), 1));
}

test "safeMul" {
    try std.testing.expectEqual(@as(i64, 12), try safeMul(i64, 3, 4));
    try std.testing.expectError(error.Overflow, safeMul(i64, std.math.maxInt(i64), 2));
}
