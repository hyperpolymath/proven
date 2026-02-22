// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafePolicy - Placeholder for libproven policy evaluation operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const PolicyError = error{ NotImplemented };

/// Placeholder: Evaluate policy expression (awaits libproven C ABI).
pub fn evaluate(_: []const u8) PolicyError!bool {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, evaluate("allow"));
}
