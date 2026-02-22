// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeDecimal - Placeholder for libproven decimal arithmetic.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const DecimalError = error{ NotImplemented };

/// Placeholder: Parse decimal string (awaits libproven C ABI).
pub fn parse(_: []const u8) DecimalError!f64 {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, parse("123.45"));
}
