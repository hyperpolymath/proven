// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeCrypto - FFI bindings to libproven cryptographic operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for crypto operations.
pub const CryptoError = error{
    ProvenError,
};

/// Constant-time byte comparison (timing-attack safe) via libproven.
/// Returns false if lengths differ.
pub fn constantTimeCompare(a: []const u8, b: []const u8) bool {
    const result = c.proven_crypto_constant_time_eq(
        a.ptr,
        a.len,
        b.ptr,
        b.len,
    );
    return result.status == c.PROVEN_OK and result.value;
}

/// Fill buffer with cryptographically secure random bytes via libproven.
pub fn randomBytes(buffer: []u8) CryptoError!void {
    const status = c.proven_crypto_random_bytes(buffer.ptr, buffer.len);
    if (status != c.PROVEN_OK) return error.ProvenError;
}

test "constantTimeCompare" {
    try std.testing.expect(constantTimeCompare("secret", "secret"));
    try std.testing.expect(!constantTimeCompare("secret", "other!"));
    try std.testing.expect(!constantTimeCompare("short", "longer"));
}

test "randomBytes" {
    var buf: [16]u8 = undefined;
    try randomBytes(&buf);
}
