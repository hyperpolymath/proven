// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe hexadecimal encoding/decoding via libproven FFI.
//!
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// Safe hexadecimal operations.
pub struct SafeHex;

impl SafeHex {
    /// Hex-encode a byte slice to a string.
    ///
    /// If `uppercase` is true, uses A-F; otherwise uses a-f.
    pub fn encode(data: &[u8], uppercase: bool) -> Result<String> {
        // SAFETY: We pass a valid pointer and length from a Rust slice.
        // The FFI function only reads from the pointer and allocates a
        // new string for the result.
        let result = unsafe {
            ffi::proven_hex_encode(data.as_ptr(), data.len(), uppercase)
        };
        core::string_result_to_result(result)
    }

    /// Hex-decode a string to bytes.
    ///
    /// Returns `Err` if the input contains non-hexadecimal characters
    /// or has an odd length.
    pub fn decode(input: &str) -> Result<Vec<u8>> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function allocates memory for the decoded bytes.
        let mut result = unsafe {
            ffi::proven_hex_decode(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;

        if result.data.is_null() {
            return Err(Error::NullPointer);
        }

        // SAFETY: The FFI call succeeded and data is non-null. We copy the
        // bytes before freeing via proven_hex_free.
        let decoded = unsafe {
            let slice = std::slice::from_raw_parts(result.data, result.length);
            let vec = slice.to_vec();
            ffi::proven_hex_free(&mut result);
            vec
        };

        Ok(decoded)
    }
}
