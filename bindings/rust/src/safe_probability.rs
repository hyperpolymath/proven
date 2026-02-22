// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe probability operations via libproven FFI.
//!
//! Probability values are clamped to [0, 1].
//! All operations delegate to Idris 2 verified code.

use crate::ffi;

/// A probability value guaranteed to be in [0, 1].
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Probability(f64);

impl Probability {
    /// Create a probability value, clamping to [0, 1].
    pub fn new(value: f64) -> Self {
        // SAFETY: proven_probability_create takes a value-type f64;
        // always safe to call.
        let clamped = unsafe { ffi::proven_probability_create(value) };
        Probability(clamped)
    }

    /// Get the underlying f64 value.
    pub fn value(&self) -> f64 {
        self.0
    }

    /// Multiply two independent probabilities (P(A and B) = P(A) * P(B)).
    pub fn and(self, other: Probability) -> Probability {
        // SAFETY: proven_probability_and takes value-type f64 arguments;
        // always safe to call.
        let result = unsafe { ffi::proven_probability_and(self.0, other.0) };
        Probability(result)
    }

    /// Add mutually exclusive probabilities (P(A or B) = P(A) + P(B)).
    pub fn or_exclusive(self, other: Probability) -> Probability {
        // SAFETY: proven_probability_or_exclusive takes value-type f64 arguments;
        // always safe to call.
        let result = unsafe {
            ffi::proven_probability_or_exclusive(self.0, other.0)
        };
        Probability(result)
    }

    /// Complement (P(not A) = 1 - P(A)).
    pub fn not(self) -> Probability {
        // SAFETY: proven_probability_not takes a value-type f64;
        // always safe to call.
        let result = unsafe { ffi::proven_probability_not(self.0) };
        Probability(result)
    }
}

impl std::fmt::Display for Probability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
