// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeAngle - FFI bindings to libproven angle operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Convert degrees to radians via libproven.
pub fn degToRad(degrees: f64) f64 {
    return c.proven_angle_deg_to_rad(degrees);
}

/// Convert radians to degrees via libproven.
pub fn radToDeg(radians: f64) f64 {
    return c.proven_angle_rad_to_deg(radians);
}

/// Normalize angle to [0, 360) degrees via libproven.
/// Returns 0 for NaN.
pub fn normalizeDegrees(degrees: f64) f64 {
    return c.proven_angle_normalize_degrees(degrees);
}

/// Normalize angle to [0, 2*pi) radians via libproven.
/// Returns 0 for NaN.
pub fn normalizeRadians(radians: f64) f64 {
    return c.proven_angle_normalize_radians(radians);
}

test "degToRad" {
    try std.testing.expectApproxEqAbs(std.math.pi, degToRad(180.0), 0.0001);
}

test "radToDeg" {
    try std.testing.expectApproxEqAbs(@as(f64, 180.0), radToDeg(std.math.pi), 0.0001);
}

test "normalizeDegrees" {
    try std.testing.expectApproxEqAbs(@as(f64, 90.0), normalizeDegrees(450.0), 0.0001);
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), normalizeDegrees(360.0), 0.0001);
}
