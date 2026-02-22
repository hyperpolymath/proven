// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe cryptographic digest operations via libproven FFI.
//!
//! Parses, verifies, and formats content-addressable digests.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// Re-export HashAlgorithm from FFI for public use.
pub use crate::ffi::HashAlgorithm;

/// Parsed cryptographic digest (algorithm + hex value).
#[derive(Debug, Clone)]
pub struct Digest {
    /// Hash algorithm used.
    pub algorithm: HashAlgorithm,
    /// Hex-encoded hash value.
    pub value: String,
}

impl std::fmt::Display for Digest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let algo = match self.algorithm {
            HashAlgorithm::SHA256 => "sha256",
            HashAlgorithm::SHA384 => "sha384",
            HashAlgorithm::SHA512 => "sha512",
            HashAlgorithm::Blake3 => "blake3",
        };
        write!(f, "{}:{}", algo, self.value)
    }
}

/// Safe digest operations.
pub struct SafeDigest;

impl SafeDigest {
    /// Parse a digest string (e.g., "sha256:abc123...").
    ///
    /// Validates the algorithm prefix and hex encoding.
    pub fn parse(input: &str) -> Result<Digest> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_digest_parse(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;

        // SAFETY: The FFI call succeeded. The value pointer is valid.
        let value = unsafe {
            if result.digest.value.is_null() || result.digest.value_len == 0 {
                String::new()
            } else {
                let slice = std::slice::from_raw_parts(
                    result.digest.value,
                    result.digest.value_len,
                );
                let s = String::from_utf8_lossy(slice).into_owned();
                ffi::proven_free_string(result.digest.value);
                s
            }
        };

        Ok(Digest {
            algorithm: result.digest.algorithm,
            value,
        })
    }

    /// Format a digest as a string (e.g., "sha256:abc123...").
    pub fn to_string(digest: &Digest) -> Result<String> {
        // We can format directly since we have the data in Rust
        Ok(format!("{}", digest))
    }
}
