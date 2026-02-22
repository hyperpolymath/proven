// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeResource - Placeholder for libproven resource management operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const ResourceError = error{ NotImplemented };

/// Placeholder: Acquire resource handle (awaits libproven C ABI).
pub fn acquire(_: []const u8) ResourceError!void {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, acquire("resource-name"));
}
