// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Linear safe string operations via libproven FFI.
//!
//! All operations delegate to the Idris 2 verified implementation through
//! the Zig FFI layer. No string logic is implemented in Rust.
//!
//! ## Linear Type Semantics
//!
//! [`LinearString`] wraps a `String` with linear semantics: it has no `Drop`
//! implementation and no `Clone`/`Copy`. The caller must explicitly consume
//! the value via `.into_inner()`, `.consume()`, or a transforming method.
//!
//! Note: Because strings are heap-allocated, failing to consume a
//! `LinearString` will **leak memory**. The `#[must_use]` attribute ensures
//! a compiler warning if the value is not consumed, but in production code
//! the caller is responsible for handling all code paths.

use crate::ffi;
use crate::{bool_result_to_result, string_result_to_result, Result};

/// A linear string value that must be consumed exactly once.
///
/// Wraps a `String` with linear semantics. Has no `Drop` implementation,
/// so the caller must explicitly consume the value to avoid memory leaks.
///
/// ## Example
///
/// ```ignore
/// let input = LinearString::from_str("Hello <world>");
/// let escaped = input.escape_html().unwrap();
/// let result: String = escaped.into_inner();
/// assert_eq!(result, "Hello &lt;world&gt;");
/// ```
#[must_use = "linear values must be consumed; call .into_inner() or .consume() to avoid memory leaks"]
pub struct LinearString {
    inner: std::mem::ManuallyDrop<String>,
}

// No Drop impl: linear semantics. The caller MUST consume the value.
// No Clone/Copy: each value is unique and must be consumed exactly once.

impl LinearString {
    /// Create a new linear string from a `String`.
    pub fn new(s: String) -> Self {
        LinearString {
            inner: std::mem::ManuallyDrop::new(s),
        }
    }

    /// Create a new linear string from a string slice.
    pub fn from_str(s: &str) -> Self {
        Self::new(s.to_owned())
    }

    /// Extract the inner `String`, consuming this linear wrapper.
    ///
    /// This is the primary way to "finish" a linear string computation.
    /// The returned `String` is a normal Rust `String` with standard
    /// Drop semantics.
    pub fn into_inner(self) -> String {
        // SAFETY: We take ownership of the ManuallyDrop contents.
        // Since LinearString has no Drop impl, the ManuallyDrop wrapper
        // is never dropped automatically; we extract the value here.
        let md = self.inner;
        std::mem::ManuallyDrop::into_inner(md)
    }

    /// Explicitly discard this linear value, freeing the string memory.
    ///
    /// Use this when you need to satisfy the linear consumption requirement
    /// but do not need the string.
    pub fn consume(self) {
        // Extract and immediately drop the inner String.
        let _ = self.into_inner();
    }

    /// Peek at the inner string without consuming.
    ///
    /// Returns a reference to the underlying string data. See
    /// [`crate::safe_math::LinearInt::peek`] for caveats about linear type
    /// approximation in Rust.
    pub fn peek(&self) -> &str {
        &self.inner
    }

    /// Get the byte length of the string without consuming.
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if the string is empty without consuming.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Escape for safe SQL interpolation: consumes self, returns a new linear result.
    ///
    /// Delegates to `proven_string_escape_sql` in libproven.
    pub fn escape_sql(self) -> Result<LinearString> {
        let bytes = self.inner.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer and allocates a new string.
        let result = unsafe { ffi::proven_string_escape_sql(bytes.as_ptr(), bytes.len()) };
        // Consume self (free the original string memory).
        let _ = std::mem::ManuallyDrop::into_inner(self.inner);
        string_result_to_result(result).map(LinearString::new)
    }

    /// Escape for safe HTML rendering: consumes self, returns a new linear result.
    ///
    /// Delegates to `proven_string_escape_html` in libproven.
    pub fn escape_html(self) -> Result<LinearString> {
        let bytes = self.inner.as_bytes();
        // SAFETY: Valid pointer and length from Rust string slice.
        let result = unsafe { ffi::proven_string_escape_html(bytes.as_ptr(), bytes.len()) };
        let _ = std::mem::ManuallyDrop::into_inner(self.inner);
        string_result_to_result(result).map(LinearString::new)
    }

    /// Escape for safe JavaScript string literals: consumes self, returns a new linear result.
    ///
    /// Delegates to `proven_string_escape_js` in libproven.
    pub fn escape_js(self) -> Result<LinearString> {
        let bytes = self.inner.as_bytes();
        // SAFETY: Valid pointer and length from Rust string slice.
        let result = unsafe { ffi::proven_string_escape_js(bytes.as_ptr(), bytes.len()) };
        let _ = std::mem::ManuallyDrop::into_inner(self.inner);
        string_result_to_result(result).map(LinearString::new)
    }

    /// Sanitize the string (strip control characters, normalize whitespace):
    /// consumes self, returns a new linear result.
    ///
    /// Delegates to `proven_sanitize_string` in libproven.
    pub fn sanitize(self) -> Result<LinearString> {
        let bytes = self.inner.as_bytes();
        // SAFETY: Valid pointer and length from Rust string slice.
        let result = unsafe { ffi::proven_sanitize_string(bytes.as_ptr(), bytes.len()) };
        let _ = std::mem::ManuallyDrop::into_inner(self.inner);
        string_result_to_result(result).map(LinearString::new)
    }

    /// Validate as an email address (RFC 5321/5322): consumes self, returns bool.
    ///
    /// Delegates to `proven_validate_email` in libproven.
    pub fn validate_email(self) -> Result<bool> {
        let bytes = self.inner.as_bytes();
        // SAFETY: Valid pointer and length from Rust string slice.
        let result = unsafe { ffi::proven_validate_email(bytes.as_ptr(), bytes.len()) };
        let _ = std::mem::ManuallyDrop::into_inner(self.inner);
        bool_result_to_result(result)
    }

    /// Validate as a URL (RFC 3986): consumes self, returns bool.
    ///
    /// Delegates to `proven_validate_url` in libproven.
    pub fn validate_url(self) -> Result<bool> {
        let bytes = self.inner.as_bytes();
        // SAFETY: Valid pointer and length from Rust string slice.
        let result = unsafe { ffi::proven_validate_url(bytes.as_ptr(), bytes.len()) };
        let _ = std::mem::ManuallyDrop::into_inner(self.inner);
        bool_result_to_result(result)
    }

    /// Validate as an IPv4 address: consumes self, returns bool.
    ///
    /// Delegates to `proven_validate_ipv4` in libproven.
    pub fn validate_ipv4(self) -> Result<bool> {
        let bytes = self.inner.as_bytes();
        // SAFETY: Valid pointer and length from Rust string slice.
        let result = unsafe { ffi::proven_validate_ipv4(bytes.as_ptr(), bytes.len()) };
        let _ = std::mem::ManuallyDrop::into_inner(self.inner);
        bool_result_to_result(result)
    }

    /// Validate as a filesystem path (no traversal): consumes self, returns bool.
    ///
    /// Delegates to `proven_validate_path` in libproven.
    pub fn validate_path(self) -> Result<bool> {
        let bytes = self.inner.as_bytes();
        // SAFETY: Valid pointer and length from Rust string slice.
        let result = unsafe { ffi::proven_validate_path(bytes.as_ptr(), bytes.len()) };
        let _ = std::mem::ManuallyDrop::into_inner(self.inner);
        bool_result_to_result(result)
    }

    /// Validate as JSON: consumes self, returns bool.
    ///
    /// Delegates to `proven_json_validate` in libproven.
    pub fn validate_json(self) -> Result<bool> {
        let bytes = self.inner.as_bytes();
        // SAFETY: Valid pointer and length from Rust string slice.
        let result = unsafe { ffi::proven_json_validate(bytes.as_ptr(), bytes.len()) };
        let _ = std::mem::ManuallyDrop::into_inner(self.inner);
        bool_result_to_result(result)
    }
}

/// Non-consuming string validation functions.
///
/// These are stateless utility functions that do not wrap a linear value.
/// They accept borrowed data and return results without ownership transfer.
pub struct LinearStringValidator;

impl LinearStringValidator {
    /// Check if a byte slice is valid UTF-8.
    ///
    /// Delegates to `proven_string_is_valid_utf8` in libproven.
    pub fn is_valid_utf8(input: &[u8]) -> Result<bool> {
        // SAFETY: Valid pointer and length from Rust slice.
        let result = unsafe { ffi::proven_string_is_valid_utf8(input.as_ptr(), input.len()) };
        bool_result_to_result(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_linear_string_create_and_consume() {
        let s = LinearString::from_str("hello");
        assert_eq!(s.into_inner(), "hello");
    }

    #[test]
    fn test_linear_string_explicit_consume() {
        let s = LinearString::from_str("discard me");
        s.consume();
    }

    #[test]
    fn test_linear_string_peek() {
        let s = LinearString::from_str("peeking");
        assert_eq!(s.peek(), "peeking");
        assert_eq!(s.len(), 7);
        assert!(!s.is_empty());
        s.consume();
    }

    #[test]
    fn test_linear_string_empty() {
        let s = LinearString::from_str("");
        assert!(s.is_empty());
        s.consume();
    }
}
