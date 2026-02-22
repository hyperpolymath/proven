// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeMath - FFI bindings to libproven arithmetic operations.
// All computation is performed in verified Idris 2 code via libproven.

const c = @cImport(@cInclude("proven.h"));

/// Error types for math operations, mapped from ProvenStatus codes.
pub const MathError = error{
    DivisionByZero,
    Overflow,
    Underflow,
    InvalidArgument,
};

/// Map ProvenStatus to MathError.
fn mapStatus(status: i32) MathError!void {
    return switch (status) {
        c.PROVEN_OK => {},
        c.PROVEN_ERR_DIVISION_BY_ZERO => error.DivisionByZero,
        c.PROVEN_ERR_OVERFLOW => error.Overflow,
        c.PROVEN_ERR_UNDERFLOW => error.Underflow,
        else => error.InvalidArgument,
    };
}

/// Safe integer division via libproven.
/// Returns PROVEN_ERR_DIVISION_BY_ZERO if denominator is 0,
/// PROVEN_ERR_OVERFLOW for INT64_MIN / -1.
pub fn div(numerator: i64, denominator: i64) MathError!i64 {
    const result = c.proven_math_div(numerator, denominator);
    try mapStatus(result.status);
    return result.value;
}

/// Safe modulo operation via libproven.
/// Returns error if denominator is 0.
pub fn mod(numerator: i64, denominator: i64) MathError!i64 {
    const result = c.proven_math_mod(numerator, denominator);
    try mapStatus(result.status);
    return result.value;
}

/// Checked addition with overflow detection via libproven.
pub fn addChecked(a: i64, b: i64) MathError!i64 {
    const result = c.proven_math_add_checked(a, b);
    try mapStatus(result.status);
    return result.value;
}

/// Checked subtraction with underflow detection via libproven.
pub fn subChecked(a: i64, b: i64) MathError!i64 {
    const result = c.proven_math_sub_checked(a, b);
    try mapStatus(result.status);
    return result.value;
}

/// Checked multiplication with overflow detection via libproven.
pub fn mulChecked(a: i64, b: i64) MathError!i64 {
    const result = c.proven_math_mul_checked(a, b);
    try mapStatus(result.status);
    return result.value;
}

/// Safe absolute value via libproven.
/// Returns error for INT64_MIN (cannot be represented as positive).
pub fn absSafe(n: i64) MathError!i64 {
    const result = c.proven_math_abs_safe(n);
    try mapStatus(result.status);
    return result.value;
}

/// Clamp value to [lo, hi] range via libproven.
pub fn clamp(lo: i64, hi: i64, value: i64) i64 {
    return c.proven_math_clamp(lo, hi, value);
}

/// Integer exponentiation with overflow checking via libproven.
pub fn powChecked(base: i64, exp: u32) MathError!i64 {
    const result = c.proven_math_pow_checked(base, exp);
    try mapStatus(result.status);
    return result.value;
}

test "div" {
    const std = @import("std");
    try std.testing.expectEqual(@as(i64, 5), try div(10, 2));
    try std.testing.expectError(error.DivisionByZero, div(10, 0));
}

test "addChecked" {
    const std = @import("std");
    try std.testing.expectEqual(@as(i64, 3), try addChecked(1, 2));
}

test "clamp" {
    const std = @import("std");
    try std.testing.expectEqual(@as(i64, 5), clamp(0, 10, 5));
    try std.testing.expectEqual(@as(i64, 0), clamp(0, 10, -5));
    try std.testing.expectEqual(@as(i64, 10), clamp(0, 10, 15));
}
