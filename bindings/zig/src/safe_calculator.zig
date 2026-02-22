// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeCalculator - FFI bindings to libproven expression evaluation.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for calculator operations.
pub const CalcError = error{
    ParseFailure,
    DivisionByZero,
    ProvenError,
};

/// Evaluate an arithmetic expression safely via libproven.
/// Supports +, -, *, /, parentheses, negative and decimal numbers.
pub fn eval(expression: []const u8) CalcError!f64 {
    const result = c.proven_calculator_eval(expression.ptr, expression.len);
    return switch (result.status) {
        c.PROVEN_OK => result.value,
        c.PROVEN_ERR_PARSE_FAILURE => error.ParseFailure,
        c.PROVEN_ERR_DIVISION_BY_ZERO => error.DivisionByZero,
        else => error.ProvenError,
    };
}

test "eval" {
    try std.testing.expectApproxEqAbs(@as(f64, 7.0), try eval("3 + 4"), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 14.0), try eval("2 * (3 + 4)"), 0.001);
    try std.testing.expectError(error.DivisionByZero, eval("1 / 0"));
    try std.testing.expectError(error.ParseFailure, eval("not math"));
}
