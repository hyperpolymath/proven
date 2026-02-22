// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Cryptographic operations with affine type semantics.
//!
//! All cryptographic computation is performed by libproven's formally
//! verified Idris 2 core. This module provides affine-typed wrappers
//! for constant-time comparison, random bytes, hex encoding, and hashing.
//!
//! ## Affine Type Semantics
//!
//! Random bytes are wrapped in `AffineBytes` which can be consumed at
//! most once. This prevents accidental reuse of nonces or keys.

use crate::ffi;
use crate::{bool_result_to_proven, string_result_to_proven, ProvenResult};
use crate::safe_string::AffineString;

/// Affine wrapper for a byte buffer.
///
/// `#[affine]` -- this value can be consumed at most once.
/// Once the bytes are extracted, the buffer is consumed.
/// This is critical for cryptographic nonces and keys.
#[derive(Debug)]
pub struct AffineBytes {
    data: Vec<u8>,
}

impl AffineBytes {
    /// Consume this affine byte buffer, returning the inner `Vec<u8>`.
    ///
    /// After calling this, the `AffineBytes` is consumed and cannot be reused.
    pub fn consume(self) -> Vec<u8> {
        self.data
    }

    /// Get the length of the contained bytes without consuming.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Check if the contained bytes are empty without consuming.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

/// Constant-time byte comparison (timing-attack resistant).
///
/// Returns `Ok(true)` if the two byte sequences are equal, `Ok(false)` otherwise.
/// Delegates to `proven_crypto_constant_time_eq` via FFI.
pub fn constant_time_eq(a: &[u8], b: &[u8]) -> ProvenResult<bool> {
    // SAFETY: pointers are valid for their respective lengths.
    let result = unsafe {
        ffi::proven_crypto_constant_time_eq(
            a.as_ptr(),
            a.len(),
            b.as_ptr(),
            b.len(),
        )
    };
    bool_result_to_proven(result)
}

/// Generate cryptographically secure random bytes.
///
/// Returns affine bytes that can be consumed at most once.
/// This prevents accidental reuse of nonces.
/// Delegates to `proven_crypto_random_bytes` via FFI.
pub fn random_bytes(len: usize) -> ProvenResult<AffineBytes> {
    let mut buf = vec![0u8; len];
    // SAFETY: buf is a valid mutable buffer of len bytes.
    let status = unsafe {
        ffi::proven_crypto_random_bytes(buf.as_mut_ptr(), len)
    };
    if status == crate::STATUS_OK {
        Ok(AffineBytes { data: buf })
    } else {
        Err(crate::ProvenError {
            code: status,
            message: "Failed to generate random bytes",
        })
    }
}

/// SHA-256 hash of input bytes.
///
/// Returns the hash as an affine string (hex-encoded).
/// Delegates to `proven_hash_sha256` via FFI.
pub fn hash_sha256(input: &[u8]) -> ProvenResult<AffineString> {
    // SAFETY: input pointer is valid for input.len() bytes.
    let result = unsafe {
        ffi::proven_hash_sha256(input.as_ptr(), input.len())
    };
    string_result_to_proven(result).map(|s| {
        // We reconstruct AffineString manually since its field is private.
        // The string_result_to_proven already freed native memory.
        // We use the consume/create pattern here.
        AffineString::from_proven_string(s)
    })
}

/// Hex-encode a byte slice.
///
/// Returns the hex-encoded result as an affine string.
/// Delegates to `proven_hex_encode` via FFI.
pub fn hex_encode(input: &[u8]) -> ProvenResult<AffineString> {
    // SAFETY: input pointer is valid for input.len() bytes.
    let result = unsafe {
        ffi::proven_hex_encode(input.as_ptr(), input.len())
    };
    string_result_to_proven(result).map(|s| {
        AffineString::from_proven_string(s)
    })
}

/// Hex-decode a hex string into raw bytes.
///
/// Returns the decoded bytes as affine bytes.
/// Delegates to `proven_hex_decode` via FFI.
pub fn hex_decode(input: &str) -> ProvenResult<AffineBytes> {
    let bytes = input.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_hex_decode(bytes.as_ptr(), bytes.len())
    };
    string_result_to_proven(result).map(|s| {
        AffineBytes { data: s.into_bytes() }
    })
}

// Extension to AffineString for internal construction.
impl AffineString {
    /// Create an AffineString from an already-proven String.
    ///
    /// This is internal to the crate and used only by FFI result extractors.
    pub(crate) fn from_proven_string(s: String) -> Self {
        AffineString { value: s }
    }
}
