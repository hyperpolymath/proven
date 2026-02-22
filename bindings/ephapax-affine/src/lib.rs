// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Ephapax-Affine bindings for the proven library.
//!
//! This crate provides affine-typed wrappers around libproven's formally
//! verified safety operations. In the affine variant, each value can be
//! consumed at most once, enforced at compile time by Rust's ownership
//! system serving as the affine type checker.
//!
//! ## Architecture
//!
//! ```text
//! Ephapax-Affine (Rust, affine types via ownership)
//!   | extern "C" FFI calls
//!   v
//! libproven C ABI (proven.h)
//!   | C ABI
//!   v
//! Zig FFI bridge
//!   | Zig -> Idris2 RefC calls
//!   v
//! Verified Idris 2 core (dependent types, totality checking)
//! ```
//!
//! ALL computation is performed in the verified Idris 2 core.
//! This crate contains ONLY FFI marshaling and affine type wrappers.
//! No algorithms are reimplemented here.

#![deny(unsafe_op_in_unsafe_fn)]

pub mod ffi;
pub mod safe_math;
pub mod safe_string;
pub mod safe_email;
pub mod safe_url;
pub mod safe_crypto;
pub mod safe_json;

// Re-export commonly used types.
pub use ffi::{IntResult, BoolResult, StringResult, FloatResult};
pub use ffi::{STATUS_OK, STATUS_ERR_OVERFLOW, STATUS_ERR_UNDERFLOW,
              STATUS_ERR_DIVISION_BY_ZERO, STATUS_ERR_VALIDATION_FAILED};

/// Error type for proven operations.
///
/// Wraps the status code from the libproven C ABI.
/// This type is `#[affine]` -- once matched against, it is consumed.
#[derive(Debug, Clone)]
pub struct ProvenError {
    /// The status code from libproven (negative values indicate errors).
    pub code: i32,
    /// Human-readable error message.
    pub message: &'static str,
}

impl core::fmt::Display for ProvenError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "ProvenError({}): {}", self.code, self.message)
    }
}

/// Result type for proven operations.
///
/// Affine semantics: this result can be matched/consumed at most once.
/// The Rust ownership system enforces this at compile time.
pub type ProvenResult<T> = Result<T, ProvenError>;

/// Convert a status code to a human-readable error message.
fn status_to_message(code: i32) -> &'static str {
    match code {
        0 => "Ok",
        -1 => "Null pointer",
        -2 => "Invalid argument",
        -3 => "Integer overflow",
        -4 => "Integer underflow",
        -5 => "Division by zero",
        -6 => "Parse failure",
        -7 => "Validation failed",
        -8 => "Out of bounds",
        -9 => "Encoding error",
        -10 => "Allocation failed",
        -99 => "Not implemented",
        _ => "Unknown error",
    }
}

/// Convert an [`IntResult`] to a [`ProvenResult<i64>`].
///
/// Consumes the `IntResult` (affine: used at most once).
fn int_result_to_proven(result: IntResult) -> ProvenResult<i64> {
    if result.status == STATUS_OK {
        Ok(result.value)
    } else {
        Err(ProvenError {
            code: result.status,
            message: status_to_message(result.status),
        })
    }
}

/// Convert a [`BoolResult`] to a [`ProvenResult<bool>`].
///
/// Consumes the `BoolResult` (affine: used at most once).
fn bool_result_to_proven(result: BoolResult) -> ProvenResult<bool> {
    if result.status == STATUS_OK {
        Ok(result.value != 0)
    } else {
        Err(ProvenError {
            code: result.status,
            message: status_to_message(result.status),
        })
    }
}

/// Convert a [`FloatResult`] to a [`ProvenResult<f64>`].
///
/// Consumes the `FloatResult` (affine: used at most once).
fn float_result_to_proven(result: FloatResult) -> ProvenResult<f64> {
    if result.status == STATUS_OK {
        Ok(result.value)
    } else {
        Err(ProvenError {
            code: result.status,
            message: status_to_message(result.status),
        })
    }
}

/// Convert a [`StringResult`] to a [`ProvenResult<String>`].
///
/// Consumes the `StringResult` and frees native memory (affine: used at most once).
///
/// # Safety
///
/// The StringResult must contain a valid pointer from libproven if status == 0.
fn string_result_to_proven(result: StringResult) -> ProvenResult<String> {
    if result.status == STATUS_OK && !result.ptr.is_null() {
        // SAFETY: ptr is valid, len is the correct length, allocated by libproven.
        let slice = unsafe { core::slice::from_raw_parts(result.ptr, result.len) };
        let s = String::from_utf8_lossy(slice).into_owned();
        // SAFETY: ptr was allocated by libproven and is non-null.
        unsafe { ffi::proven_free_string(result.ptr) };
        Ok(s)
    } else if !result.ptr.is_null() {
        // SAFETY: ptr was allocated by libproven and is non-null.
        unsafe { ffi::proven_free_string(result.ptr) };
        Err(ProvenError {
            code: result.status,
            message: status_to_message(result.status),
        })
    } else {
        Err(ProvenError {
            code: result.status,
            message: status_to_message(result.status),
        })
    }
}

// ============================================================================
// Lifecycle
// ============================================================================

/// Initialize the proven runtime.
///
/// Must be called before any safe operation. Idempotent.
/// Returns `Ok(())` on success.
pub fn init() -> ProvenResult<()> {
    // SAFETY: proven_init is safe to call; it is idempotent.
    let status = unsafe { ffi::proven_init() };
    if status == STATUS_OK {
        Ok(())
    } else {
        Err(ProvenError {
            code: status,
            message: "Failed to initialize proven runtime",
        })
    }
}

/// Deinitialize the proven runtime.
///
/// Releases all resources. Idempotent.
pub fn deinit() {
    // SAFETY: proven_deinit is safe to call; it is idempotent.
    unsafe { ffi::proven_deinit() };
}

/// Check whether the proven runtime is initialized.
pub fn is_initialized() -> bool {
    // SAFETY: proven_is_initialized is a pure query.
    unsafe { ffi::proven_is_initialized() }
}
