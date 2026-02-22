// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe HTTP header operations via libproven FFI.
//!
//! Validates header names and values, prevents CRLF injection, and builds
//! security-critical headers (CSP, HSTS).
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Validated HTTP header (name-value pair).
#[derive(Debug, Clone)]
pub struct Header {
    /// Header name.
    pub name: String,
    /// Header value.
    pub value: String,
}

/// Safe HTTP header operations that prevent CRLF injection.
pub struct SafeHeader;

impl SafeHeader {
    /// Check for CRLF injection characters in a header value.
    ///
    /// Returns `true` if the value contains CR or LF characters.
    pub fn has_crlf(value: &str) -> Result<bool> {
        let bytes = value.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_header_has_crlf(bytes.as_ptr(), bytes.len())
        };
        core::bool_result_to_result(result)
    }

    /// Check if a header name is valid per RFC 7230.
    ///
    /// Valid header names consist of token characters only.
    pub fn is_valid_name(name: &str) -> Result<bool> {
        let bytes = name.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_header_is_valid_name(bytes.as_ptr(), bytes.len())
        };
        core::bool_result_to_result(result)
    }

    /// Check if a header name is in the dangerous headers list.
    ///
    /// Dangerous headers can be exploited for various attacks if set
    /// by untrusted code.
    pub fn is_dangerous(name: &str) -> Result<bool> {
        let bytes = name.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_header_is_dangerous(bytes.as_ptr(), bytes.len())
        };
        core::bool_result_to_result(result)
    }

    /// Create a validated header string "Name: Value".
    ///
    /// Returns `Err` if the name or value contains invalid characters.
    pub fn render(name: &str, value: &str) -> Result<String> {
        let name_bytes = name.as_bytes();
        let value_bytes = value.as_bytes();
        // SAFETY: We pass valid pointers and lengths from string bytes.
        // The FFI function allocates a new string for the result.
        let result = unsafe {
            ffi::proven_header_render(
                name_bytes.as_ptr(),
                name_bytes.len(),
                value_bytes.as_ptr(),
                value_bytes.len(),
            )
        };
        core::string_result_to_result(result)
    }

    /// Build a Content-Security-Policy header value from directives.
    ///
    /// Takes a JSON string of directive key-value pairs.
    pub fn build_csp(directives_json: &str) -> Result<String> {
        let bytes = directives_json.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_header_build_csp(bytes.as_ptr(), bytes.len())
        };
        core::string_result_to_result(result)
    }

    /// Build an HSTS (HTTP Strict Transport Security) header value.
    pub fn build_hsts(max_age: i64, include_subdomains: bool, preload: bool) -> Result<String> {
        // SAFETY: proven_header_build_hsts takes value-type arguments;
        // always safe to call.
        let result = unsafe {
            ffi::proven_header_build_hsts(max_age, include_subdomains, preload)
        };
        core::string_result_to_result(result)
    }
}
