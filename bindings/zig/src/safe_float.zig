// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeFloat - FFI bindings to libproven floating-point operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for float operations.
pub const FloatError = error{
    DivisionByZero,
    InvalidArgument,
    ProvenError,
};

/// Map ProvenStatus to FloatError.
fn mapStatus(status: i32) FloatError!void {
    return switch (status) {
        c.PROVEN_OK => {},
        c.PROVEN_ERR_DIVISION_BY_ZERO => error.DivisionByZero,
        c.PROVEN_ERR_INVALID_ARGUMENT => error.InvalidArgument,
        else => error.ProvenError,
    };
}

/// Safe floating-point division via libproven.
pub fn safeDiv(a: f64, b: f64) FloatError!f64 {
    const result = c.proven_float_div(a, b);
    try mapStatus(result.status);
    return result.value;
}

/// Check if float is finite (not NaN or Inf) via libproven.
pub fn isFinite(x: f64) bool {
    return c.proven_float_is_finite(x);
}

/// Check if float is NaN via libproven.
pub fn isNaN(x: f64) bool {
    return c.proven_float_is_nan(x);
}

/// Safe square root via libproven.
/// Returns error if x is negative or NaN.
pub fn safeSqrt(x: f64) FloatError!f64 {
    const result = c.proven_float_sqrt(x);
    try mapStatus(result.status);
    return result.value;
}

/// Safe natural logarithm via libproven.
/// Returns error if x <= 0 or NaN.
pub fn safeLn(x: f64) FloatError!f64 {
    const result = c.proven_float_ln(x);
    try mapStatus(result.status);
    return result.value;
}

test "safeDiv" {
    try std.testing.expectApproxEqAbs(@as(f64, 5.0), try safeDiv(10.0, 2.0), 0.001);
    try std.testing.expectError(error.DivisionByZero, safeDiv(10.0, 0.0));
}

test "isFinite" {
    try std.testing.expect(isFinite(1.0));
    try std.testing.expect(!isFinite(std.math.inf(f64)));
}

test "isNaN" {
    try std.testing.expect(isNaN(std.math.nan(f64)));
    try std.testing.expect(!isNaN(1.0));
}

test "safeSqrt" {
    try std.testing.expectApproxEqAbs(@as(f64, 3.0), try safeSqrt(9.0), 0.001);
    try std.testing.expectError(error.InvalidArgument, safeSqrt(-1.0));
}
