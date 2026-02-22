// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeConsensus - Placeholder for libproven consensus operations.
// This module awaits C ABI coverage in proven.h. All functions return NotImplemented.
// All computation will be performed in verified Idris 2 code via libproven once available.

const std = @import("std");

pub const ConsensusError = error{ NotImplemented };

/// Placeholder: Calculate quorum size (awaits libproven C ABI).
pub fn quorumSize(_: u32) ConsensusError!u32 {
    return error.NotImplemented;
}

test "stub" {
    try std.testing.expectError(error.NotImplemented, quorumSize(5));
}
