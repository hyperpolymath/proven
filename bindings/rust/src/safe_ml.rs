// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe machine learning utilities via libproven FFI.
//!
//! Provides numerically stable activation functions and normalization.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Safe machine learning operations.
pub struct SafeML;

impl SafeML {
    /// Softmax normalization.
    ///
    /// Converts a vector of logits into a probability distribution
    /// that sums to 1.0. Uses a numerically stable implementation
    /// that subtracts the max before exponentiating.
    pub fn softmax(input: &[f64]) -> Result<Vec<f64>> {
        let mut output = vec![0.0f64; input.len()];
        // SAFETY: We pass valid pointers and lengths from Rust slices.
        // The FFI function reads from input and writes to output within
        // the given length.
        let status = unsafe {
            ffi::proven_ml_softmax(
                input.as_ptr(),
                output.as_mut_ptr(),
                input.len(),
            )
        };
        core::status_to_result(status)?;
        Ok(output)
    }

    /// Sigmoid activation function: 1 / (1 + exp(-x)).
    pub fn sigmoid(x: f64) -> f64 {
        // SAFETY: proven_ml_sigmoid takes a value-type f64;
        // always safe to call.
        unsafe { ffi::proven_ml_sigmoid(x) }
    }

    /// ReLU activation function: max(0, x).
    pub fn relu(x: f64) -> f64 {
        // SAFETY: proven_ml_relu takes a value-type f64;
        // always safe to call.
        unsafe { ffi::proven_ml_relu(x) }
    }

    /// Leaky ReLU activation function: x if x > 0, alpha * x otherwise.
    pub fn leaky_relu(x: f64, alpha: f64) -> f64 {
        // SAFETY: proven_ml_leaky_relu takes value-type f64 arguments;
        // always safe to call.
        unsafe { ffi::proven_ml_leaky_relu(x, alpha) }
    }

    /// Clamp a value to a range [min_val, max_val].
    pub fn clamp(x: f64, min_val: f64, max_val: f64) -> f64 {
        // SAFETY: proven_ml_clamp takes value-type f64 arguments;
        // always safe to call.
        unsafe { ffi::proven_ml_clamp(x, min_val, max_val) }
    }
}
