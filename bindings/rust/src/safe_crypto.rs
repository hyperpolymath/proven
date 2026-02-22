// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe cryptographic primitives via libproven FFI.
//!
//! Provides constant-time comparison and cryptographic random byte generation.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// Safe cryptographic operations.
pub struct SafeCrypto;

impl SafeCrypto {
    /// Constant-time byte comparison (timing-attack resistant).
    ///
    /// Returns `true` if both byte slices are identical. The comparison
    /// takes the same amount of time regardless of where a mismatch occurs.
    pub fn constant_time_eq(a: &[u8], b: &[u8]) -> Result<bool> {
        // SAFETY: We pass valid pointers and lengths from Rust slices.
        // The FFI function only reads from both pointers within their lengths.
        let result = unsafe {
            ffi::proven_crypto_constant_time_eq(
                a.as_ptr(),
                a.len(),
                b.as_ptr(),
                b.len(),
            )
        };
        core::bool_result_to_result(result)
    }

    /// Fill a buffer with cryptographically secure random bytes.
    ///
    /// Uses the Idris 2 verified CSPRNG implementation.
    pub fn random_bytes(buf: &mut [u8]) -> Result<()> {
        // SAFETY: We pass a valid mutable pointer and length from a Rust slice.
        // The FFI function writes to the pointer within the given length.
        let status = unsafe {
            ffi::proven_crypto_random_bytes(buf.as_mut_ptr(), buf.len())
        };
        core::status_to_result(status)
    }

    /// Generate a vector of cryptographically secure random bytes.
    ///
    /// Convenience wrapper around `random_bytes` that allocates the buffer.
    pub fn random_vec(len: usize) -> Result<Vec<u8>> {
        let mut buf = vec![0u8; len];
        Self::random_bytes(&mut buf)?;
        Ok(buf)
    }
}
