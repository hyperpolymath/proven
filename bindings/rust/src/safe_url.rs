// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe URL parsing and validation via libproven FFI.
//!
//! Parses URLs into components following RFC 3986.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// Parsed URL components.
#[derive(Debug, Clone)]
pub struct ParsedUrl {
    /// URL scheme (e.g., "https").
    pub scheme: Option<String>,
    /// Host (e.g., "example.com").
    pub host: Option<String>,
    /// Port number if specified.
    pub port: Option<u16>,
    /// Path component.
    pub path: Option<String>,
    /// Query string (without leading `?`).
    pub query: Option<String>,
    /// Fragment (without leading `#`).
    pub fragment: Option<String>,
}

/// Safe URL operations.
pub struct SafeUrl;

/// Extract an owned String from a raw FFI pointer+length, or None if null.
///
/// # Safety
///
/// The caller must ensure `ptr` is either null or points to `len` valid bytes
/// allocated by libproven's allocator.
unsafe fn extract_ffi_string(ptr: *mut u8, len: usize) -> Option<String> {
    if ptr.is_null() || len == 0 {
        return None;
    }
    // SAFETY: ptr is non-null, len describes valid readable bytes.
    let slice = std::slice::from_raw_parts(ptr, len);
    Some(String::from_utf8_lossy(slice).into_owned())
}

impl SafeUrl {
    /// Parse a URL string into its components.
    ///
    /// Returns a `ParsedUrl` with each component extracted. Components
    /// not present in the input URL are `None`.
    pub fn parse(input: &str) -> Result<ParsedUrl> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function allocates new strings for each component that
        // must be freed. We free the entire UrlComponents via proven_url_free.
        let mut result = unsafe {
            ffi::proven_url_parse(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;

        // SAFETY: The FFI call succeeded, so all string pointers in components
        // are either null or valid allocations from libproven. We copy the data
        // before freeing.
        let parsed = unsafe {
            let scheme = extract_ffi_string(
                result.components.scheme,
                result.components.scheme_len,
            );
            let host = extract_ffi_string(
                result.components.host,
                result.components.host_len,
            );
            let port = if result.components.has_port {
                Some(result.components.port)
            } else {
                None
            };
            let path = extract_ffi_string(
                result.components.path,
                result.components.path_len,
            );
            let query = extract_ffi_string(
                result.components.query,
                result.components.query_len,
            );
            let fragment = extract_ffi_string(
                result.components.fragment,
                result.components.fragment_len,
            );

            // Free all allocated component strings
            ffi::proven_url_free(&mut result.components);

            ParsedUrl {
                scheme,
                host,
                port,
                path,
                query,
                fragment,
            }
        };

        Ok(parsed)
    }
}
