// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! URL parsing and validation with affine type semantics.
//!
//! All URL parsing is performed by libproven's formally verified Idris 2
//! core. This module provides affine-typed wrappers that ensure parsed
//! URL components are consumed at most once.

use crate::ffi;
use crate::{bool_result_to_proven, ProvenResult, ProvenError};

/// Affine wrapper for a validated URL.
///
/// `#[affine]` -- this value can be consumed at most once.
/// Once the URL string is extracted, the validation proof is consumed.
#[derive(Debug)]
pub struct ValidatedUrl {
    url: String,
}

impl ValidatedUrl {
    /// Consume this validated URL, returning the URL string.
    ///
    /// After calling this, the `ValidatedUrl` is consumed and cannot be reused.
    pub fn consume(self) -> String {
        self.url
    }

    /// Borrow the URL string without consuming the validation proof.
    pub fn as_str(&self) -> &str {
        &self.url
    }
}

/// Validate a URL by attempting to parse it (RFC 3986).
///
/// Returns `Ok(true)` if the URL is valid, `Ok(false)` if invalid.
/// Delegates to `proven_validate_url` via FFI.
pub fn is_valid_url(url: &str) -> ProvenResult<bool> {
    let bytes = url.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_validate_url(bytes.as_ptr(), bytes.len())
    };
    bool_result_to_proven(result)
}

/// Validate a URL and return a `ValidatedUrl` on success.
///
/// Returns `Ok(ValidatedUrl)` if the URL is valid, or
/// `Err(ProvenError)` if the URL is invalid or FFI fails.
/// Delegates to `proven_validate_url` via FFI.
///
/// The `ValidatedUrl` wrapper proves that validation occurred and
/// can be consumed at most once (affine semantics).
pub fn validate_url(url: &str) -> ProvenResult<ValidatedUrl> {
    let bytes = url.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_validate_url(bytes.as_ptr(), bytes.len())
    };
    let valid = bool_result_to_proven(result)?;
    if valid {
        Ok(ValidatedUrl {
            url: url.to_owned(),
        })
    } else {
        Err(ProvenError {
            code: crate::STATUS_ERR_VALIDATION_FAILED,
            message: "URL validation failed",
        })
    }
}

/// Validate a filesystem path (no traversal attacks).
///
/// Returns `Ok(true)` if the path is safe, `Ok(false)` if it contains traversal.
/// Delegates to `proven_validate_path` via FFI.
pub fn is_safe_path(path: &str) -> ProvenResult<bool> {
    let bytes = path.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_validate_path(bytes.as_ptr(), bytes.len())
    };
    bool_result_to_proven(result)
}

/// Validate an IPv4 address.
///
/// Returns `Ok(true)` if the address is valid, `Ok(false)` if invalid.
/// Delegates to `proven_validate_ipv4` via FFI.
pub fn is_valid_ipv4(addr: &str) -> ProvenResult<bool> {
    let bytes = addr.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_validate_ipv4(bytes.as_ptr(), bytes.len())
    };
    bool_result_to_proven(result)
}
