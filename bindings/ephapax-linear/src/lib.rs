// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! # Ephapax Proven Linear Bindings
//!
//! Linear type variant of the Ephapax bindings for the `libproven` library.
//! All computation is delegated to the formally verified Idris 2
//! implementation via the Zig FFI bridge. This crate provides safe Rust
//! wrappers that enforce **linear type semantics**: every value must be
//! consumed exactly once.
//!
//! ## Linear vs Affine
//!
//! In substructural type theory:
//!
//! - **Linear types** (this crate): Values must be used *exactly once*.
//!   No implicit Drop allowed. The caller must explicitly consume each
//!   value via `.consume()`, `.into_inner()`, or a consuming method.
//!   Failing to consume a linear value is a compile-time error (via
//!   `#[must_use]` and the absence of a `Drop` impl).
//!
//! - **Affine types** (see `ephapax-proven-affine`): Values may be used
//!   *at most once*. Implicit Drop is allowed (cleanup happens
//!   automatically). Still no Clone/Copy.
//!
//! ## Architecture
//!
//! ```text
//! Rust (this crate)  -->  libproven.so (Zig FFI)  -->  Idris 2 RefC
//!   linear wrappers         C ABI bridge               formal proofs
//! ```
//!
//! ## Modules
//!
//! - [`ffi`]: Raw `extern "C"` FFI declarations (unsafe).
//! - [`safe_math`]: Linear safe arithmetic (values consumed on use).
//! - [`safe_string`]: Linear safe string operations.
//! - [`safe_crypto`]: Linear safe cryptographic primitives.
//! - [`safe_buffer`]: Linear safe bounded buffer (must be consumed exactly once).
//!
//! ## Example
//!
//! ```ignore
//! use ephapax_proven_linear::safe_math::LinearInt;
//!
//! // Initialize the runtime
//! ephapax_proven_linear::init().unwrap();
//!
//! // Create a linear integer - it MUST be consumed
//! let a = LinearInt::new(42);
//! let b = LinearInt::new(8);
//!
//! // Addition consumes both operands and produces a new linear value
//! let sum = a.add(b).unwrap();
//!
//! // Extract the final value, consuming the linear wrapper
//! let result: i64 = sum.into_inner();
//! assert_eq!(result, 50);
//!
//! ephapax_proven_linear::deinit();
//! ```

#![warn(missing_docs)]
#![deny(unsafe_op_in_unsafe_fn)]

/// Raw FFI declarations for libproven.
pub mod ffi;

/// Linear safe math operations (values consumed on use).
pub mod safe_math;

/// Linear safe string operations.
pub mod safe_string;

/// Linear safe cryptographic primitives.
pub mod safe_crypto;

/// Linear safe bounded buffer (must be consumed exactly once).
pub mod safe_buffer;

// ============================================================================
// Error types
// ============================================================================

/// Error type for proven operations.
///
/// Each variant maps to a `ProvenStatus` error code from the C ABI.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// Null pointer passed to FFI function (status -1).
    NullPointer,
    /// Invalid argument (status -2).
    InvalidArgument,
    /// Integer overflow detected (status -3).
    Overflow,
    /// Integer underflow detected (status -4).
    Underflow,
    /// Division by zero attempted (status -5).
    DivisionByZero,
    /// Parse failure (status -6).
    ParseError,
    /// Validation failed (status -7).
    ValidationError,
    /// Value out of bounds (status -8).
    OutOfBounds,
    /// Encoding error (status -9).
    EncodingError,
    /// Memory allocation failed (status -10).
    AllocationFailed,
    /// Function not implemented (status -99).
    NotImplemented,
    /// Unknown error code from FFI.
    Unknown(i32),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NullPointer => write!(f, "Null pointer"),
            Error::InvalidArgument => write!(f, "Invalid argument"),
            Error::Overflow => write!(f, "Integer overflow"),
            Error::Underflow => write!(f, "Integer underflow"),
            Error::DivisionByZero => write!(f, "Division by zero"),
            Error::ParseError => write!(f, "Parse error"),
            Error::ValidationError => write!(f, "Validation error"),
            Error::OutOfBounds => write!(f, "Out of bounds"),
            Error::EncodingError => write!(f, "Encoding error"),
            Error::AllocationFailed => write!(f, "Allocation failed"),
            Error::NotImplemented => write!(f, "Not implemented"),
            Error::Unknown(code) => write!(f, "Unknown error (code {})", code),
        }
    }
}

impl std::error::Error for Error {}

/// Result type alias for this crate.
pub type Result<T> = std::result::Result<T, Error>;

// ============================================================================
// Status code conversion (internal)
// ============================================================================

/// Convert an FFI status code to a `Result<()>`.
pub(crate) fn status_to_result(status: i32) -> Result<()> {
    match status {
        ffi::STATUS_OK => Ok(()),
        ffi::STATUS_ERR_NULL_POINTER => Err(Error::NullPointer),
        ffi::STATUS_ERR_INVALID_ARGUMENT => Err(Error::InvalidArgument),
        ffi::STATUS_ERR_OVERFLOW => Err(Error::Overflow),
        ffi::STATUS_ERR_UNDERFLOW => Err(Error::Underflow),
        ffi::STATUS_ERR_DIVISION_BY_ZERO => Err(Error::DivisionByZero),
        ffi::STATUS_ERR_PARSE_FAILURE => Err(Error::ParseError),
        ffi::STATUS_ERR_VALIDATION_FAILED => Err(Error::ValidationError),
        ffi::STATUS_ERR_OUT_OF_BOUNDS => Err(Error::OutOfBounds),
        ffi::STATUS_ERR_ENCODING_ERROR => Err(Error::EncodingError),
        ffi::STATUS_ERR_ALLOCATION_FAILED => Err(Error::AllocationFailed),
        ffi::STATUS_ERR_NOT_IMPLEMENTED => Err(Error::NotImplemented),
        other => Err(Error::Unknown(other)),
    }
}

/// Convert an [`ffi::IntResult`] to a `Result<i64>`.
pub(crate) fn int_result_to_result(result: ffi::IntResult) -> Result<i64> {
    status_to_result(result.status)?;
    Ok(result.value)
}

/// Convert an [`ffi::BoolResult`] to a `Result<bool>`.
pub(crate) fn bool_result_to_result(result: ffi::BoolResult) -> Result<bool> {
    status_to_result(result.status)?;
    Ok(result.value != 0)
}

/// Convert a [`ffi::FloatResult`] to a `Result<f64>`.
pub(crate) fn float_result_to_result(result: ffi::FloatResult) -> Result<f64> {
    status_to_result(result.status)?;
    Ok(result.value)
}

/// Convert a [`ffi::StringResult`] to a `Result<String>`.
///
/// Takes ownership of the allocated string pointer and frees it after
/// copying the data into a Rust `String`.
pub(crate) fn string_result_to_result(result: ffi::StringResult) -> Result<String> {
    status_to_result(result.status)?;

    if result.ptr.is_null() {
        return Err(Error::NullPointer);
    }

    // SAFETY: The pointer is non-null, was allocated by libproven, and the
    // length field correctly describes the valid byte range. We copy the bytes
    // before freeing the original allocation.
    let owned = unsafe {
        let slice = std::slice::from_raw_parts(result.ptr, result.len);
        let s = String::from_utf8_lossy(slice).into_owned();
        ffi::proven_free_string(result.ptr);
        s
    };

    Ok(owned)
}

// ============================================================================
// Lifecycle
// ============================================================================

/// Initialize the Proven runtime.
///
/// Must be called before using any other proven functions. Idempotent.
pub fn init() -> Result<()> {
    // SAFETY: proven_init takes no arguments; always safe to call.
    let status = unsafe { ffi::proven_init() };
    status_to_result(status)
}

/// Deinitialize the Proven runtime. Idempotent.
pub fn deinit() {
    // SAFETY: proven_deinit takes no arguments; always safe to call.
    unsafe { ffi::proven_deinit() }
}

/// Check if the Proven runtime has been initialized.
pub fn is_initialized() -> bool {
    // SAFETY: proven_is_initialized takes no arguments; always safe to call.
    unsafe { ffi::proven_is_initialized() }
}
