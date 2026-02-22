// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeSsh - Placeholder for libproven SSH validation operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const SshError = error{ NotImplemented };

/// Placeholder: Validate SSH public key (awaits libproven C ABI).
pub fn isValidPublicKey(_: []const u8) SshError!bool {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, isValidPublicKey("ssh-ed25519 ..."));
}
