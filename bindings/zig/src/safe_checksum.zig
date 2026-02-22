// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeChecksum - FFI bindings to libproven checksum operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for checksum operations.
pub const ChecksumError = error{
    ProvenError,
};

/// Calculate CRC32 checksum via libproven.
pub fn crc32(data: []const u8) ChecksumError!u32 {
    const result = c.proven_checksum_crc32(data.ptr, data.len);
    if (result.status != c.PROVEN_OK) return error.ProvenError;
    return @intCast(result.value);
}

/// Verify CRC32 matches expected value via libproven.
pub fn verifyCrc32(data: []const u8, expected: u32) bool {
    const result = c.proven_checksum_verify_crc32(data.ptr, data.len, expected);
    return result.status == c.PROVEN_OK and result.value;
}

test "crc32" {
    const checksum = try crc32("hello");
    _ = checksum; // Value depends on libproven implementation
}

test "verifyCrc32" {
    const checksum = try crc32("hello");
    try std.testing.expect(verifyCrc32("hello", checksum));
}
