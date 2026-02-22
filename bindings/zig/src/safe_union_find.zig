// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeUnionFind - Placeholder for libproven union-find operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const UnionFindError = error{ NotImplemented };

/// Placeholder: Create union-find (awaits libproven C ABI).
pub fn create(_: usize) UnionFindError!void {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, create(10));
}
