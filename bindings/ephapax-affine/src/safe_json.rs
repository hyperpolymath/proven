// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! JSON validation with affine type semantics.
//!
//! All JSON validation is performed by libproven's formally verified
//! Idris 2 core. This module provides affine-typed wrappers for JSON
//! validation and type detection.

use crate::ffi;
use crate::{bool_result_to_proven, ProvenResult, ProvenError};

/// Affine wrapper for a validated JSON document.
///
/// `#[affine]` -- this value can be consumed at most once.
/// Once the JSON string is extracted, the validation proof is consumed.
/// This prevents using unvalidated JSON in security-critical contexts.
#[derive(Debug)]
pub struct ValidatedJson {
    document: String,
    json_type: JsonType,
}

impl ValidatedJson {
    /// Consume this validated JSON document, returning the raw string.
    ///
    /// After calling this, the `ValidatedJson` is consumed and cannot be reused.
    pub fn consume(self) -> String {
        self.document
    }

    /// Borrow the JSON document string without consuming the validation proof.
    pub fn as_str(&self) -> &str {
        &self.document
    }

    /// Get the top-level JSON type without consuming the validation proof.
    pub fn json_type(&self) -> JsonType {
        self.json_type
    }
}

/// JSON type enumeration matching the libproven C ABI constants.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JsonType {
    /// JSON null literal.
    Null,
    /// JSON boolean (true/false).
    Bool,
    /// JSON number (integer or floating-point).
    Number,
    /// JSON string.
    String,
    /// JSON array.
    Array,
    /// JSON object.
    Object,
    /// Not valid JSON.
    Invalid,
}

impl JsonType {
    /// Convert from the libproven integer constant to `JsonType`.
    fn from_i32(value: i32) -> Self {
        match value {
            0 => JsonType::Null,
            1 => JsonType::Bool,
            2 => JsonType::Number,
            3 => JsonType::String,
            4 => JsonType::Array,
            5 => JsonType::Object,
            _ => JsonType::Invalid,
        }
    }
}

/// Validate a JSON document.
///
/// Returns `Ok(true)` if valid JSON, `Ok(false)` if invalid.
/// Delegates to `proven_json_validate` via FFI.
pub fn is_valid_json(json_str: &str) -> ProvenResult<bool> {
    let bytes = json_str.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_json_validate(bytes.as_ptr(), bytes.len())
    };
    bool_result_to_proven(result)
}

/// Get the top-level JSON type of a document.
///
/// Returns the `JsonType` enum value.
/// Delegates to `proven_json_get_type` via FFI (from ffi.err reference).
pub fn json_type(json_str: &str) -> JsonType {
    let bytes = json_str.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let type_code = unsafe {
        ffi::proven_json_get_type(bytes.as_ptr(), bytes.len())
    };
    JsonType::from_i32(type_code)
}

/// Validate a JSON document and return a `ValidatedJson` on success.
///
/// Returns `Ok(ValidatedJson)` if the document is valid, or
/// `Err(ProvenError)` if the document is invalid.
/// Delegates to `proven_json_validate` via FFI.
///
/// The `ValidatedJson` wrapper proves that validation occurred and
/// can be consumed at most once (affine semantics).
pub fn validate_json(json_str: &str) -> ProvenResult<ValidatedJson> {
    let bytes = json_str.as_bytes();
    // SAFETY: bytes pointer is valid for bytes.len() bytes.
    let result = unsafe {
        ffi::proven_json_validate(bytes.as_ptr(), bytes.len())
    };
    let valid = bool_result_to_proven(result)?;
    if valid {
        let jtype = json_type(json_str);
        Ok(ValidatedJson {
            document: json_str.to_owned(),
            json_type: jtype,
        })
    } else {
        Err(ProvenError {
            code: crate::STATUS_ERR_VALIDATION_FAILED,
            message: "JSON validation failed",
        })
    }
}
