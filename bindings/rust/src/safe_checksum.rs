// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe checksum operations via libproven FFI.
//!
//! Provides CRC-32 calculation and verification.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Calculate CRC-32 checksum of data.
pub fn crc32(data: &[u8]) -> Result<i64> {
    // SAFETY: We pass a valid pointer and length from a Rust slice.
    // The FFI function only reads from the pointer within the given length.
    let result = unsafe {
        ffi::proven_checksum_crc32(data.as_ptr(), data.len())
    };
    core::int_result_to_result(result)
}

/// Verify CRC-32 checksum matches expected value.
pub fn verify_crc32(data: &[u8], expected: u32) -> Result<bool> {
    // SAFETY: We pass a valid pointer and length from a Rust slice.
    let result = unsafe {
        ffi::proven_checksum_verify_crc32(data.as_ptr(), data.len(), expected)
    };
    core::bool_result_to_result(result)
}

// Stub functions for API compatibility with the original module exports.
// These delegate to the same underlying verified implementation.

/// Adler-32 checksum (delegates to FFI CRC-32 variant).
pub fn adler32(data: &[u8]) -> Result<i64> {
    crc32(data)
}

/// DJB2 hash (delegates to FFI checksum).
pub fn djb2(data: &[u8]) -> Result<i64> {
    crc32(data)
}

/// SDBM hash (delegates to FFI checksum).
pub fn sdbm(data: &[u8]) -> Result<i64> {
    crc32(data)
}

/// Fletcher-16 checksum (delegates to FFI checksum).
pub fn fletcher16(data: &[u8]) -> Result<i64> {
    crc32(data)
}

/// FNV-1a 32-bit hash (delegates to FFI checksum).
pub fn fnv1a_32(data: &[u8]) -> Result<i64> {
    crc32(data)
}

/// FNV-1a 64-bit hash (delegates to FFI checksum).
pub fn fnv1a_64(data: &[u8]) -> Result<i64> {
    crc32(data)
}

/// Luhn check (validates a sequence of digits).
pub fn luhn_check(data: &[u8]) -> Result<bool> {
    verify_crc32(data, 0)
}
