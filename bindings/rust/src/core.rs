// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Core types for the proven library.
//!
//! Provides error types, result conversions, and helper functions for
//! mapping libproven C ABI status codes to idiomatic Rust `Result` types.

use crate::ffi;

/// Error type for proven operations.
///
/// Each variant maps to a `ProvenStatus` error code from the C ABI.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// Null pointer passed to FFI function (status -1).
    NullPointer,

    /// Invalid argument (status -2).
    InvalidArgument(String),

    /// Integer overflow detected (status -3).
    Overflow,

    /// Integer underflow detected (status -4).
    Underflow,

    /// Division by zero attempted (status -5).
    DivisionByZero,

    /// Parse failure (status -6).
    ParseError(String),

    /// Validation failed (status -7).
    ValidationError(String),

    /// Value out of bounds (status -8).
    OutOfBounds,

    /// Encoding error (status -9).
    EncodingError(String),

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
            Error::InvalidArgument(msg) => write!(f, "Invalid argument: {}", msg),
            Error::Overflow => write!(f, "Integer overflow"),
            Error::Underflow => write!(f, "Integer underflow"),
            Error::DivisionByZero => write!(f, "Division by zero"),
            Error::ParseError(msg) => write!(f, "Parse error: {}", msg),
            Error::ValidationError(msg) => write!(f, "Validation error: {}", msg),
            Error::OutOfBounds => write!(f, "Out of bounds"),
            Error::EncodingError(msg) => write!(f, "Encoding error: {}", msg),
            Error::AllocationFailed => write!(f, "Allocation failed"),
            Error::NotImplemented => write!(f, "Not implemented"),
            Error::Unknown(code) => write!(f, "Unknown error (code {})", code),
        }
    }
}

impl std::error::Error for Error {}

/// Result type for proven operations.
pub type Result<T> = std::result::Result<T, Error>;

/// Convert an FFI status code to a `Result<()>`.
///
/// Returns `Ok(())` for status 0, or the corresponding `Error` variant.
pub fn status_to_result(status: i32) -> Result<()> {
    match status {
        ffi::STATUS_OK => Ok(()),
        ffi::STATUS_ERR_NULL_POINTER => Err(Error::NullPointer),
        ffi::STATUS_ERR_INVALID_ARGUMENT => Err(Error::InvalidArgument(String::new())),
        ffi::STATUS_ERR_OVERFLOW => Err(Error::Overflow),
        ffi::STATUS_ERR_UNDERFLOW => Err(Error::Underflow),
        ffi::STATUS_ERR_DIVISION_BY_ZERO => Err(Error::DivisionByZero),
        ffi::STATUS_ERR_PARSE_FAILURE => Err(Error::ParseError(String::new())),
        ffi::STATUS_ERR_VALIDATION_FAILED => Err(Error::ValidationError(String::new())),
        ffi::STATUS_ERR_OUT_OF_BOUNDS => Err(Error::OutOfBounds),
        ffi::STATUS_ERR_ENCODING_ERROR => Err(Error::EncodingError(String::new())),
        ffi::STATUS_ERR_ALLOCATION_FAILED => Err(Error::AllocationFailed),
        ffi::STATUS_ERR_NOT_IMPLEMENTED => Err(Error::NotImplemented),
        other => Err(Error::Unknown(other)),
    }
}

/// Convert an `IntResult` from FFI to a Rust `Result<i64>`.
pub fn int_result_to_result(result: ffi::IntResult) -> Result<i64> {
    status_to_result(result.status)?;
    Ok(result.value)
}

/// Convert a `BoolResult` from FFI to a Rust `Result<bool>`.
pub fn bool_result_to_result(result: ffi::BoolResult) -> Result<bool> {
    status_to_result(result.status)?;
    Ok(result.value)
}

/// Convert a `FloatResult` from FFI to a Rust `Result<f64>`.
pub fn float_result_to_result(result: ffi::FloatResult) -> Result<f64> {
    status_to_result(result.status)?;
    Ok(result.value)
}

/// Convert a `StringResult` from FFI to a Rust `Result<String>`.
///
/// This function takes ownership of the string pointer and frees it via
/// `proven_free_string` after copying the data into a Rust `String`.
///
/// # Safety
///
/// The caller must ensure that the `StringResult` was produced by a
/// libproven function and has not already been freed.
pub fn string_result_to_result(result: ffi::StringResult) -> Result<String> {
    status_to_result(result.status)?;

    if result.value.is_null() {
        return Err(Error::NullPointer);
    }

    // SAFETY: The pointer is non-null, was allocated by libproven's allocator,
    // and the length field correctly describes the valid byte range. We copy
    // the bytes before freeing the original allocation.
    let owned = unsafe {
        let slice = std::slice::from_raw_parts(result.value, result.length);
        let s = String::from_utf8_lossy(slice).into_owned();
        ffi::proven_free_string(result.value);
        s
    };

    Ok(owned)
}

/// A non-empty collection that guarantees at least one element.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NonEmpty<T> {
    head: T,
    tail: Vec<T>,
}

impl<T> NonEmpty<T> {
    /// Create a new NonEmpty with a single element.
    pub fn singleton(value: T) -> Self {
        NonEmpty {
            head: value,
            tail: Vec::new(),
        }
    }

    /// Create a NonEmpty from a head and tail.
    pub fn new(head: T, tail: Vec<T>) -> Self {
        NonEmpty { head, tail }
    }

    /// Try to create a NonEmpty from a Vec.
    pub fn from_vec(mut vec: Vec<T>) -> Option<Self> {
        if vec.is_empty() {
            None
        } else {
            let head = vec.remove(0);
            Some(NonEmpty { head, tail: vec })
        }
    }

    /// Get the first element.
    pub fn head(&self) -> &T {
        &self.head
    }

    /// Get the tail elements.
    pub fn tail(&self) -> &[T] {
        &self.tail
    }

    /// Get the total length.
    pub fn len(&self) -> usize {
        1 + self.tail.len()
    }

    /// Check if this contains exactly one element.
    pub fn is_singleton(&self) -> bool {
        self.tail.is_empty()
    }

    /// Convert to a Vec.
    pub fn to_vec(self) -> Vec<T> {
        let mut v = vec![self.head];
        v.extend(self.tail);
        v
    }
}

/// A bounded integer that is guaranteed to be within [MIN, MAX].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bounded<const MIN: i64, const MAX: i64> {
    value: i64,
}

impl<const MIN: i64, const MAX: i64> Bounded<MIN, MAX> {
    /// Try to create a bounded value.
    pub fn new(value: i64) -> Result<Self> {
        if value >= MIN && value <= MAX {
            Ok(Bounded { value })
        } else {
            Err(Error::OutOfBounds)
        }
    }

    /// Get the underlying value.
    pub fn get(&self) -> i64 {
        self.value
    }

    /// Get the minimum bound.
    pub const fn min() -> i64 {
        MIN
    }

    /// Get the maximum bound.
    pub const fn max() -> i64 {
        MAX
    }
}

/// Type alias for percentage values [0, 100].
pub type Percentage = Bounded<0, 100>;

/// Type alias for port numbers [0, 65535].
pub type Port = Bounded<0, 65535>;

/// Type alias for byte values [0, 255].
pub type Byte = Bounded<0, 255>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_non_empty() {
        let ne = NonEmpty::singleton(42);
        assert_eq!(ne.len(), 1);
        assert_eq!(*ne.head(), 42);
        assert!(ne.is_singleton());
    }

    #[test]
    fn test_non_empty_from_vec() {
        let ne = NonEmpty::from_vec(vec![1, 2, 3]);
        assert!(ne.is_some());
        let ne = ne.unwrap();
        assert_eq!(ne.len(), 3);
        assert_eq!(*ne.head(), 1);
    }

    #[test]
    fn test_non_empty_from_empty_vec() {
        let ne: Option<NonEmpty<i32>> = NonEmpty::from_vec(vec![]);
        assert!(ne.is_none());
    }

    #[test]
    fn test_bounded_valid() {
        let pct: Result<Percentage> = Bounded::new(50);
        assert!(pct.is_ok());
        assert_eq!(pct.unwrap().get(), 50);
    }

    #[test]
    fn test_bounded_invalid() {
        let invalid: Result<Percentage> = Bounded::new(101);
        assert!(invalid.is_err());
    }

    #[test]
    fn test_status_to_result_ok() {
        assert!(status_to_result(0).is_ok());
    }

    #[test]
    fn test_status_to_result_error() {
        assert_eq!(
            status_to_result(-5),
            Err(Error::DivisionByZero)
        );
    }

    #[test]
    fn test_error_display() {
        let err = Error::DivisionByZero;
        assert_eq!(format!("{}", err), "Division by zero");
    }
}
