// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeFile - Placeholder for libproven file operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const FileError = error{ NotImplemented };

/// Placeholder: Safe file read (awaits libproven C ABI).
pub fn safeRead(_: []const u8) FileError![]const u8 {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, safeRead("/tmp/test"));
}
