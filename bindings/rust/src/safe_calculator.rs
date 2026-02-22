// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe expression evaluator via libproven FFI.
//!
//! Evaluates mathematical expressions with overflow and division-by-zero
//! protection. All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Safe calculator for mathematical expressions.
pub struct SafeCalculator;

impl SafeCalculator {
    /// Evaluate a mathematical expression string.
    ///
    /// Supports basic arithmetic operators (+, -, *, /) with proper
    /// precedence. Returns `Err` for division by zero, overflow, or
    /// invalid syntax.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let result = SafeCalculator::eval("2 + 3 * 4");
    /// // result == Ok(14.0)
    /// ```
    pub fn eval(expression: &str) -> Result<f64> {
        let bytes = expression.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer within the given length.
        let result = unsafe {
            ffi::proven_calculator_eval(bytes.as_ptr(), bytes.len())
        };
        core::float_result_to_result(result)
    }
}
