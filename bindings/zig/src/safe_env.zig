// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeEnv - Placeholder for libproven environment variable operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const EnvError = error{ NotImplemented };

/// Placeholder: Get validated environment variable (awaits libproven C ABI).
pub fn getValidated(_: []const u8) EnvError![]const u8 {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, getValidated("PATH"));
}
