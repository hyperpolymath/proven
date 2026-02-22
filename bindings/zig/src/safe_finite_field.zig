// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeFiniteField - Placeholder for libproven finite field arithmetic.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const FiniteFieldError = error{ NotImplemented };

/// Placeholder: Modular addition (awaits libproven C ABI).
pub fn modAdd(_: u64, _: u64, _: u64) FiniteFieldError!u64 {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, modAdd(3, 4, 7));
}
