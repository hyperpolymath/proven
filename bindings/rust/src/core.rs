// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Core types for the proven library.

use thiserror::Error;

/// Error type for proven operations
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum Error {
    /// Integer overflow detected
    #[error("Integer overflow: {0}")]
    Overflow(String),

    /// Division by zero attempted
    #[error("Division by zero")]
    DivisionByZero,

    /// Value out of bounds
    #[error("Value {value} out of bounds [{min}, {max}]")]
    OutOfBounds {
        /// The value that was out of bounds
        value: i64,
        /// Minimum allowed value
        min: i64,
        /// Maximum allowed value
        max: i64,
    },

    /// Invalid UTF-8 encoding
    #[error("Invalid UTF-8 encoding: {0}")]
    InvalidUtf8(String),

    /// Invalid format
    #[error("Invalid format: {0}")]
    InvalidFormat(String),

    /// Parsing error
    #[error("Parse error: {0}")]
    ParseError(String),

    /// Validation error
    #[error("Validation error: {0}")]
    ValidationError(String),

    /// Empty input not allowed
    #[error("Empty input not allowed")]
    EmptyInput,

    /// Path traversal detected
    #[error("Path traversal detected: {0}")]
    PathTraversal(String),

    /// Injection attempt detected
    #[error("Injection attempt detected: {0}")]
    InjectionDetected(String),

    /// Input too long
    #[error("Input too long: {0}")]
    TooLong(String),
}

/// Result type for proven operations
pub type Result<T> = std::result::Result<T, Error>;

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

/// A bounded integer that is guaranteed to be within [min, max].
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
            Err(Error::OutOfBounds {
                value,
                min: MIN,
                max: MAX,
            })
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

/// Type alias for common bounded types
pub type Percentage = Bounded<0, 100>;
pub type Port = Bounded<0, 65535>;
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
    fn test_bounded() {
        let pct: Result<Percentage> = Bounded::new(50);
        assert!(pct.is_ok());
        assert_eq!(pct.unwrap().get(), 50);

        let invalid: Result<Percentage> = Bounded::new(101);
        assert!(invalid.is_err());
    }
}
