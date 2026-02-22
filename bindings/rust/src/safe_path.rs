// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe filesystem path operations via libproven FFI.
//!
//! Prevents directory traversal attacks and sanitizes filenames.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Safe path operations that prevent traversal attacks.
pub struct SafePath;

impl SafePath {
    /// Check if a path contains directory traversal sequences (e.g., `../`).
    ///
    /// Returns `true` if traversal is detected.
    pub fn has_traversal(path: &str) -> Result<bool> {
        let bytes = path.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer within the given length.
        let result = unsafe {
            ffi::proven_path_has_traversal(bytes.as_ptr(), bytes.len())
        };
        core::bool_result_to_result(result)
    }

    /// Sanitize a filename by removing dangerous characters.
    ///
    /// Returns the sanitized filename as a `String`.
    pub fn sanitize_filename(filename: &str) -> Result<String> {
        let bytes = filename.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer and allocates a new
        // string that we free via string_result_to_result.
        let result = unsafe {
            ffi::proven_path_sanitize_filename(bytes.as_ptr(), bytes.len())
        };
        core::string_result_to_result(result)
    }
}
