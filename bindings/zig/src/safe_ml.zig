// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeMl - FFI bindings to libproven machine learning operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for ML operations.
pub const MlError = error{
    ProvenError,
};

/// Softmax normalization over an array via libproven.
pub fn softmax(input: []const f64, output: []f64) MlError!void {
    if (input.len != output.len) return error.ProvenError;
    const status = c.proven_ml_softmax(input.ptr, output.ptr, input.len);
    if (status != c.PROVEN_OK) return error.ProvenError;
}

/// Sigmoid function via libproven: 1 / (1 + exp(-x)).
pub fn sigmoid(x: f64) f64 {
    return c.proven_ml_sigmoid(x);
}

/// ReLU function via libproven: max(0, x).
pub fn relu(x: f64) f64 {
    return c.proven_ml_relu(x);
}

/// Leaky ReLU via libproven: x >= 0 ? x : alpha * x.
pub fn leakyRelu(x: f64, alpha: f64) f64 {
    return c.proven_ml_leaky_relu(x, alpha);
}

/// Clamp value to [min_val, max_val] via libproven.
pub fn mlClamp(x: f64, min_val: f64, max_val: f64) f64 {
    return c.proven_ml_clamp(x, min_val, max_val);
}

test "sigmoid" {
    try std.testing.expectApproxEqAbs(@as(f64, 0.5), sigmoid(0.0), 0.001);
    try std.testing.expect(sigmoid(10.0) > 0.99);
    try std.testing.expect(sigmoid(-10.0) < 0.01);
}

test "relu" {
    try std.testing.expectApproxEqAbs(@as(f64, 5.0), relu(5.0), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), relu(-5.0), 0.001);
}

test "leakyRelu" {
    try std.testing.expectApproxEqAbs(@as(f64, 5.0), leakyRelu(5.0, 0.01), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, -0.05), leakyRelu(-5.0, 0.01), 0.001);
}
