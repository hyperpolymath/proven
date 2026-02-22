// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Email validation with affine type semantics.
//!
//! All validation is performed by libproven's formally verified Idris 2
//! core. This module provides affine-typed wrappers that ensure validation
//! results are consumed at most once.

use crate::ffi;
use crate::{bool_result_to_proven, ProvenResult};

/// Affine wrapper for a validated email address.
///
/// `#[affine]` -- this value can be consumed at most once.
/// Once the email is extracted, the validation proof is consumed.
/// This ensures each validated email is used exactly once in
/// security-critical contexts.
#[derive(Debug)]
pub struct ValidatedEmail {
    address: String,
}

impl ValidatedEmail {
    /// Consume this validated email, returning the address string.
    ///
    /// After calling this, the `ValidatedEmail` is consumed and cannot be reused.
    pub fn consume(self) -> String {
        self.address
    }

    /// Borrow the email address without consuming the validation proof.
    pub fn as_str(&self) -> &str {
        &self.address
    }
}

/// Validate an email address (RFC 5321/5322).
///
/// Returns `Ok(true)` if the email is valid, `Ok(false)` if invalid.
/// Delegates to `proven_validate_email` via FFI.
pub fn is_valid_email(email: &str) -> ProvenResult<bool> {
    let bytes = email.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_validate_email(bytes.as_ptr(), bytes.len())
    };
    bool_result_to_proven(result)
}

/// Validate an email address and return a `ValidatedEmail` on success.
///
/// Returns `Ok(ValidatedEmail)` if the address is valid, or
/// `Err(ProvenError)` if the address is invalid or FFI fails.
/// Delegates to `proven_validate_email` via FFI.
///
/// The `ValidatedEmail` wrapper proves that validation occurred and
/// can be consumed at most once (affine semantics).
pub fn validate_email(email: &str) -> ProvenResult<ValidatedEmail> {
    let bytes = email.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_validate_email(bytes.as_ptr(), bytes.len())
    };
    let valid = bool_result_to_proven(result)?;
    if valid {
        Ok(ValidatedEmail {
            address: email.to_owned(),
        })
    } else {
        Err(crate::ProvenError {
            code: crate::STATUS_ERR_VALIDATION_FAILED,
            message: "Email validation failed",
        })
    }
}
