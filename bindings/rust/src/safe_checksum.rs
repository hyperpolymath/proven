// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

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

// The following algorithms do not yet have dedicated FFI exports in
// libproven. Rather than silently returning CRC-32 values (which would
// be semantically incorrect), we return `NotImplemented` so callers
// know the operation is not yet backed by verified Idris 2 code.

/// Adler-32 checksum.
///
/// Not yet available via FFI; returns `Error::NotImplemented`.
pub fn adler32(_data: &[u8]) -> Result<i64> {
    Err(core::Error::NotImplemented)
}

/// DJB2 hash.
///
/// Not yet available via FFI; returns `Error::NotImplemented`.
pub fn djb2(_data: &[u8]) -> Result<i64> {
    Err(core::Error::NotImplemented)
}

/// SDBM hash.
///
/// Not yet available via FFI; returns `Error::NotImplemented`.
pub fn sdbm(_data: &[u8]) -> Result<i64> {
    Err(core::Error::NotImplemented)
}

/// Fletcher-16 checksum.
///
/// Not yet available via FFI; returns `Error::NotImplemented`.
pub fn fletcher16(_data: &[u8]) -> Result<i64> {
    Err(core::Error::NotImplemented)
}

/// FNV-1a 32-bit hash.
///
/// Not yet available via FFI; returns `Error::NotImplemented`.
pub fn fnv1a_32(_data: &[u8]) -> Result<i64> {
    Err(core::Error::NotImplemented)
}

/// FNV-1a 64-bit hash.
///
/// Not yet available via FFI; returns `Error::NotImplemented`.
pub fn fnv1a_64(_data: &[u8]) -> Result<i64> {
    Err(core::Error::NotImplemented)
}

/// Luhn check (validates a sequence of digits).
///
/// Not yet available via FFI; returns `Error::NotImplemented`.
pub fn luhn_check(_data: &[u8]) -> Result<bool> {
    Err(core::Error::NotImplemented)
}
