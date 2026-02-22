// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe Content-Type operations via libproven FFI.
//!
//! Parses and validates MIME content types, prevents MIME sniffing attacks.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Re-export enums from FFI for public use.
pub use crate::ffi::{Charset, MediaCategory};

/// Parsed Content-Type.
#[derive(Debug, Clone)]
pub struct ContentType {
    /// Media type (e.g., "text", "application").
    pub media_type: String,
    /// Subtype (e.g., "html", "json").
    pub subtype: String,
    /// Suffix if present (e.g., "xml" in "application/atom+xml").
    pub suffix: Option<String>,
    /// Category of the media type.
    pub category: MediaCategory,
    /// Character set if specified.
    pub charset: Option<Charset>,
}

/// Safe Content-Type operations.
pub struct SafeContentType;

impl SafeContentType {
    /// Parse a Content-Type header string.
    ///
    /// Returns the parsed components including media type, subtype, suffix,
    /// category, and charset.
    pub fn parse(input: &str) -> Result<ContentType> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function allocates strings for media_type, subtype, suffix.
        let mut result = unsafe {
            ffi::proven_content_type_parse(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;

        // SAFETY: The FFI call succeeded, so allocated string pointers are valid.
        // We copy the data before freeing via proven_content_type_free.
        let ct = unsafe {
            let media_type = if result.media_type.is_null() {
                String::new()
            } else {
                let slice = std::slice::from_raw_parts(
                    result.media_type,
                    result.media_type_len,
                );
                String::from_utf8_lossy(slice).into_owned()
            };

            let subtype = if result.subtype.is_null() {
                String::new()
            } else {
                let slice = std::slice::from_raw_parts(
                    result.subtype,
                    result.subtype_len,
                );
                String::from_utf8_lossy(slice).into_owned()
            };

            let suffix = if result.suffix.is_null() || result.suffix_len == 0 {
                None
            } else {
                let slice = std::slice::from_raw_parts(
                    result.suffix,
                    result.suffix_len,
                );
                Some(String::from_utf8_lossy(slice).into_owned())
            };

            let charset = if result.has_charset {
                Some(result.charset)
            } else {
                None
            };

            // Free all allocated strings in the result
            ffi::proven_content_type_free(&mut result);

            ContentType {
                media_type,
                subtype,
                suffix,
                category: result.category,
                charset,
            }
        };

        Ok(ct)
    }

    /// Check if a content type can be sniffed to something dangerous.
    ///
    /// Some content types can be interpreted differently by browsers
    /// through MIME sniffing, potentially leading to XSS attacks.
    pub fn can_sniff_dangerous(content_type: &str) -> Result<bool> {
        let bytes = content_type.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_content_type_can_sniff_dangerous(bytes.as_ptr(), bytes.len())
        };
        core::bool_result_to_result(result)
    }

    /// Render a content type to a string.
    ///
    /// Produces a properly formatted Content-Type header value.
    pub fn render(
        media_type: &str,
        subtype: &str,
        suffix: Option<&str>,
        charset: Option<Charset>,
    ) -> Result<String> {
        let type_bytes = media_type.as_bytes();
        let subtype_bytes = subtype.as_bytes();
        let suffix_str = suffix.unwrap_or("");
        let suffix_bytes = suffix_str.as_bytes();
        let (cs, has_cs) = match charset {
            Some(c) => (c, true),
            None => (Charset::Utf8, false),
        };

        // SAFETY: We pass valid pointers and lengths from string bytes.
        let result = unsafe {
            ffi::proven_content_type_render(
                type_bytes.as_ptr(),
                type_bytes.len(),
                subtype_bytes.as_ptr(),
                subtype_bytes.len(),
                suffix_bytes.as_ptr(),
                suffix_bytes.len(),
                cs,
                has_cs,
            )
        };
        core::string_result_to_result(result)
    }

    /// Check if content type is JSON.
    pub fn is_json(subtype: &str, suffix: Option<&str>) -> Result<bool> {
        let subtype_bytes = subtype.as_bytes();
        let suffix_str = suffix.unwrap_or("");
        let suffix_bytes = suffix_str.as_bytes();
        // SAFETY: We pass valid pointers and lengths from string bytes.
        let result = unsafe {
            ffi::proven_content_type_is_json(
                subtype_bytes.as_ptr(),
                subtype_bytes.len(),
                suffix_bytes.as_ptr(),
                suffix_bytes.len(),
            )
        };
        core::bool_result_to_result(result)
    }

    /// Check if content type is XML.
    pub fn is_xml(subtype: &str, suffix: Option<&str>) -> Result<bool> {
        let subtype_bytes = subtype.as_bytes();
        let suffix_str = suffix.unwrap_or("");
        let suffix_bytes = suffix_str.as_bytes();
        // SAFETY: We pass valid pointers and lengths from string bytes.
        let result = unsafe {
            ffi::proven_content_type_is_xml(
                subtype_bytes.as_ptr(),
                subtype_bytes.len(),
                suffix_bytes.as_ptr(),
                suffix_bytes.len(),
            )
        };
        core::bool_result_to_result(result)
    }
}
