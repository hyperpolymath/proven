// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeProbability - FFI bindings to libproven probability operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Create probability value (clamped to [0, 1]) via libproven.
pub fn create(value: f64) f64 {
    return c.proven_probability_create(value);
}

/// Multiply probabilities (P(A and B) = P(A) * P(B)) via libproven.
pub fn andProb(a: f64, b: f64) f64 {
    return c.proven_probability_and(a, b);
}

/// Add probabilities for mutually exclusive events via libproven.
/// P(A or B) = P(A) + P(B), clamped to 1.0.
pub fn orExclusive(a: f64, b: f64) f64 {
    return c.proven_probability_or_exclusive(a, b);
}

/// Complement probability (P(not A) = 1 - P(A)) via libproven.
pub fn not(p: f64) f64 {
    return c.proven_probability_not(p);
}

test "create" {
    try std.testing.expectApproxEqAbs(@as(f64, 0.5), create(0.5), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), create(-0.5), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 1.0), create(1.5), 0.001);
}

test "andProb" {
    try std.testing.expectApproxEqAbs(@as(f64, 0.25), andProb(0.5, 0.5), 0.001);
}

test "not" {
    try std.testing.expectApproxEqAbs(@as(f64, 0.7), not(0.3), 0.001);
}
