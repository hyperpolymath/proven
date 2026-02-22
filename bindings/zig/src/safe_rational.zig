// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeRational - Placeholder for libproven rational number operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const RationalError = error{ NotImplemented };

/// Placeholder: Create rational number (awaits libproven C ABI).
pub fn create(_: i64, _: i64) RationalError![2]i64 {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, create(1, 3));
}
