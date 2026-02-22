// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeJwt - Placeholder for libproven JWT operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const JwtError = error{ NotImplemented };

/// Placeholder: Validate JWT structure (awaits libproven C ABI).
pub fn isValidStructure(_: []const u8) JwtError!bool {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, isValidStructure("eyJ..."));
}
