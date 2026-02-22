// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe email validation via libproven FFI.
//!
//! Validates email addresses according to RFC 5321/5322 simplified rules.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Safe email validation.
pub struct SafeEmail;

impl SafeEmail {
    /// Validate an email address.
    ///
    /// Returns `true` if the email address is valid per RFC 5321 simplified rules.
    pub fn is_valid(email: &str) -> Result<bool> {
        let bytes = email.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer within the given length.
        let result = unsafe {
            ffi::proven_email_is_valid(bytes.as_ptr(), bytes.len())
        };
        core::bool_result_to_result(result)
    }
}
