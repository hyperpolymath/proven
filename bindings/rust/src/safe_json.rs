// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe JSON validation via libproven FFI.
//!
//! Validates JSON strings and determines root value types without
//! full parsing. All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Re-export JsonType from FFI for public use.
pub use crate::ffi::JsonType;

/// Safe JSON operations.
pub struct SafeJson;

impl SafeJson {
    /// Check if a string is valid JSON.
    ///
    /// Returns `true` if the input is syntactically valid JSON.
    pub fn is_valid(input: &str) -> Result<bool> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_json_is_valid(bytes.as_ptr(), bytes.len())
        };
        core::bool_result_to_result(result)
    }

    /// Get the JSON value type at the root level.
    ///
    /// Returns `JsonType::Invalid` if the input is not valid JSON.
    pub fn get_type(input: &str) -> JsonType {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        unsafe { ffi::proven_json_get_type(bytes.as_ptr(), bytes.len()) }
    }
}
