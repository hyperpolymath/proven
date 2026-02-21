// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe floating-point operations with NaN and infinity handling.

const std = @import("std");

/// Error types for float operations.
pub const FloatError = error{
    NaN,
    Infinity,
    DivisionByZero,
};

/// Check if a float is NaN.
pub fn isNaN(value: f64) bool {
    return std.math.isNan(value);
}

/// Check if a float is infinite.
pub fn isInfinite(value: f64) bool {
    return std.math.isInf(value);
}

/// Check if a float is finite (not NaN or infinite).
pub fn isFinite(value: f64) bool {
    return !isNaN(value) and !isInfinite(value);
}

/// Safe division that returns error on NaN, infinity, or division by zero.
pub fn safeDiv(numerator: f64, denominator: f64) FloatError!f64 {
    if (isNaN(numerator) or isNaN(denominator)) return error.NaN;
    if (denominator == 0.0) return error.DivisionByZero;
    const result = numerator / denominator;
    if (isInfinite(result)) return error.Infinity;
    if (isNaN(result)) return error.NaN;
    return result;
}

/// Approximate equality within epsilon.
pub fn approxEqual(a: f64, b: f64, epsilon: f64) bool {
    if (isNaN(a) or isNaN(b)) return false;
    return @abs(a - b) <= epsilon;
}

/// Clamp a value between min and max.
pub fn clamp(value: f64, min_val: f64, max_val: f64) f64 {
    return @max(min_val, @min(max_val, value));
}

/// Linear interpolation between two values.
pub fn lerp(a: f64, b: f64, t: f64) f64 {
    return a + (b - a) * clamp(t, 0.0, 1.0);
}

/// Safe square root (returns error for negative values).
pub fn safeSqrt(value: f64) FloatError!f64 {
    if (isNaN(value)) return error.NaN;
    if (value < 0.0) return error.NaN;
    return @sqrt(value);
}

/// Safe natural logarithm (returns error for non-positive values).
pub fn safeLn(value: f64) FloatError!f64 {
    if (isNaN(value)) return error.NaN;
    if (value <= 0.0) return error.NaN;
    return @log(value);
}

test "isNaN" {
    try std.testing.expect(isNaN(std.math.nan(f64)));
    try std.testing.expect(!isNaN(1.0));
}

test "safeDiv" {
    try std.testing.expectApproxEqAbs(@as(f64, 5.0), try safeDiv(10.0, 2.0), 0.001);
    try std.testing.expectError(error.DivisionByZero, safeDiv(10.0, 0.0));
}

test "approxEqual" {
    try std.testing.expect(approxEqual(1.0, 1.0000001, 0.001));
    try std.testing.expect(!approxEqual(1.0, 2.0, 0.001));
}
