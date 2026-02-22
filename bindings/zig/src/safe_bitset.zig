// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeBitset - Placeholder for libproven bitset operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const BitsetError = error{ NotImplemented };

/// Placeholder: Create bitset (awaits libproven C ABI).
pub fn create(_: usize) BitsetError!void {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, create(64));
}
