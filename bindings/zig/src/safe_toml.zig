// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeToml - Placeholder for libproven TOML parsing operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const TomlError = error{ NotImplemented };

/// Placeholder: Validate TOML string (awaits libproven C ABI).
pub fn isValid(_: []const u8) TomlError!bool {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, isValid("[section]"));
}
