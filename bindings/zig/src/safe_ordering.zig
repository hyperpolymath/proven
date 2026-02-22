// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeOrdering - Placeholder for libproven ordering operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const OrderingError = error{ NotImplemented };

/// Placeholder: Total order compare (awaits libproven C ABI).
pub fn totalCompare(_: i64, _: i64) OrderingError!i32 {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, totalCompare(1, 2));
}
