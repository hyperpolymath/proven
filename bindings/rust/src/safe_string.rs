// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe string operations via libproven FFI.
//!
//! Provides UTF-8 validation and injection-safe escaping for SQL, HTML,
//! and JavaScript contexts. All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Safe string operations that prevent injection attacks.
pub struct SafeString;

impl SafeString {
    /// Check if a byte slice is valid UTF-8.
    pub fn is_valid_utf8(input: &[u8]) -> Result<bool> {
        // SAFETY: We pass a valid pointer and length from a Rust slice.
        // The FFI function only reads from the pointer within the given length.
        let result = unsafe {
            ffi::proven_string_is_valid_utf8(input.as_ptr(), input.len())
        };
        core::bool_result_to_result(result)
    }

    /// Escape a string for safe use in SQL queries.
    ///
    /// Escapes single quotes by doubling them.
    pub fn escape_sql(input: &str) -> Result<String> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer and allocates a new
        // string that we free via string_result_to_result.
        let result = unsafe {
            ffi::proven_string_escape_sql(bytes.as_ptr(), bytes.len())
        };
        core::string_result_to_result(result)
    }

    /// Escape a string for safe use in HTML.
    ///
    /// Escapes `<`, `>`, `&`, `"`, and `'` characters.
    pub fn escape_html(input: &str) -> Result<String> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer and allocates a new
        // string that we free via string_result_to_result.
        let result = unsafe {
            ffi::proven_string_escape_html(bytes.as_ptr(), bytes.len())
        };
        core::string_result_to_result(result)
    }

    /// Escape a string for safe use in JavaScript string literals.
    pub fn escape_js(input: &str) -> Result<String> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer and allocates a new
        // string that we free via string_result_to_result.
        let result = unsafe {
            ffi::proven_string_escape_js(bytes.as_ptr(), bytes.len())
        };
        core::string_result_to_result(result)
    }
}
