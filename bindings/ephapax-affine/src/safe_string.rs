// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe string operations with affine type semantics.
//!
//! All computation is delegated to libproven's formally verified Idris 2
//! core via the FFI bridge. No string processing logic is reimplemented.
//!
//! ## Affine Type Semantics
//!
//! Escaped strings are returned as `AffineString`, which can be consumed
//! at most once. This prevents double-use of sanitized output which could
//! lead to re-escaping bugs.

use crate::ffi;
use crate::{bool_result_to_proven, string_result_to_proven, ProvenResult};

/// Affine wrapper for an owned string value.
///
/// `#[affine]` -- this value can be consumed at most once.
/// Once unwrapped, the inner `String` is moved out and the wrapper
/// is consumed. This prevents accidental reuse of sanitized strings.
#[derive(Debug)]
pub struct AffineString {
    value: String,
}

impl AffineString {
    /// Consume this affine string, returning the inner `String`.
    ///
    /// After calling this, the `AffineString` is consumed and cannot be reused.
    pub fn consume(self) -> String {
        self.value
    }

    /// Get the length of the contained string without consuming it.
    pub fn len(&self) -> usize {
        self.value.len()
    }

    /// Check if the contained string is empty without consuming it.
    pub fn is_empty(&self) -> bool {
        self.value.is_empty()
    }
}

/// Validate that a byte slice is valid UTF-8.
///
/// Returns `Ok(true)` if valid, `Ok(false)` if invalid.
/// Delegates to `proven_string_is_valid_utf8` via FFI.
pub fn is_valid_utf8(data: &[u8]) -> ProvenResult<bool> {
    // SAFETY: data pointer is valid for data.len() bytes.
    let result = unsafe {
        ffi::proven_string_is_valid_utf8(data.as_ptr(), data.len())
    };
    bool_result_to_proven(result)
}

/// Escape a string for safe SQL interpolation.
///
/// Returns an affine escaped string, or error on failure.
/// Delegates to `proven_string_escape_sql` via FFI.
pub fn escape_sql(input: &str) -> ProvenResult<AffineString> {
    let bytes = input.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_string_escape_sql(bytes.as_ptr(), bytes.len())
    };
    string_result_to_proven(result).map(|s| AffineString { value: s })
}

/// Escape a string for safe HTML rendering (XSS prevention).
///
/// Returns an affine escaped string, or error on failure.
/// Delegates to `proven_string_escape_html` via FFI.
pub fn escape_html(input: &str) -> ProvenResult<AffineString> {
    let bytes = input.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_string_escape_html(bytes.as_ptr(), bytes.len())
    };
    string_result_to_proven(result).map(|s| AffineString { value: s })
}

/// Escape a string for safe JavaScript string literals.
///
/// Returns an affine escaped string, or error on failure.
/// Delegates to `proven_string_escape_js` via FFI.
pub fn escape_js(input: &str) -> ProvenResult<AffineString> {
    let bytes = input.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_string_escape_js(bytes.as_ptr(), bytes.len())
    };
    string_result_to_proven(result).map(|s| AffineString { value: s })
}

/// Sanitize a string (strip control characters, normalize whitespace).
///
/// Returns an affine sanitized string, or error on failure.
/// Delegates to `proven_sanitize_string` via FFI.
pub fn sanitize(input: &str) -> ProvenResult<AffineString> {
    let bytes = input.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_sanitize_string(bytes.as_ptr(), bytes.len())
    };
    string_result_to_proven(result).map(|s| AffineString { value: s })
}
