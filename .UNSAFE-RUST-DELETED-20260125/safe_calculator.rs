// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe calculator using Proven's SafeMath primitives.
//!
//! This module provides a simple expression evaluator that demonstrates
//! and uses Proven's verified safe math operations. All arithmetic is
//! protected against overflow, underflow, and division by zero.
//!
//! # Example
//!
//! ```rust
//! use proven::SafeCalculator;
//!
//! let mut calc = SafeCalculator::new();
//!
//! // Basic arithmetic with overflow protection
//! assert!(calc.evaluate("40 + 2").contains("42"));
//! assert!(calc.evaluate("100 / 5").contains("20"));
//!
//! // Overflow is safely caught
//! let result = calc.evaluate("9223372036854775807 + 1");
//! assert!(result.contains("Overflow"));
//!
//! // Division by zero is safely caught
//! let result = calc.evaluate("42 / 0");
//! assert!(result.contains("error"));
//! ```

use crate::SafeMath;

/// A calculator that uses Proven's verified safe math operations.
///
/// The calculator supports basic arithmetic operations (+, -, *, /, %)
/// and provides protection against common arithmetic errors:
/// - Integer overflow
/// - Integer underflow
/// - Division by zero
///
/// Special commands:
/// - `ans` in expressions uses the last result
/// - `max i64` shows the maximum i64 value
/// - `min i64` shows the minimum i64 value
/// - `overflow demo` demonstrates overflow protection
/// - `divzero demo` demonstrates division by zero protection
#[derive(Debug, Default, Clone)]
pub struct SafeCalculator {
    /// Last result for chaining operations
    last_result: Option<i64>,
    /// History of evaluations (bounded to prevent memory issues)
    history: Vec<String>,
    /// Maximum history entries
    max_history: usize,
}

impl SafeCalculator {
    /// Create a new safe calculator.
    pub fn new() -> Self {
        Self {
            last_result: None,
            history: Vec::new(),
            max_history: 100,
        }
    }

    /// Create a calculator with a custom history limit.
    pub fn with_history_limit(max_history: usize) -> Self {
        Self {
            last_result: None,
            history: Vec::new(),
            max_history,
        }
    }

    /// Get the last result, if any.
    pub fn last_result(&self) -> Option<i64> {
        self.last_result
    }

    /// Get the calculation history.
    pub fn history(&self) -> &[String] {
        &self.history
    }

    /// Clear the history.
    pub fn clear_history(&mut self) {
        self.history.clear();
    }

    /// Evaluate a simple expression like "42 + 7" or "1000 * 1000000000".
    ///
    /// Supported operations:
    /// - Addition: `a + b`
    /// - Subtraction: `a - b`
    /// - Multiplication: `a * b`
    /// - Division: `a / b`
    /// - Modulo: `a % b`
    ///
    /// Special values:
    /// - `ans` refers to the previous result
    /// - `max i64` returns the maximum i64 value
    /// - `min i64` returns the minimum i64 value
    ///
    /// # Returns
    ///
    /// A string describing the result or error message.
    pub fn evaluate(&mut self, expr: &str) -> String {
        let parts: Vec<&str> = expr.split_whitespace().collect();

        let result = match parts.as_slice() {
            // Single number
            [num] => match num.parse::<i64>() {
                Ok(n) => {
                    self.last_result = Some(n);
                    format!("{}", n)
                }
                Err(_) => "Invalid number".into(),
            },

            // Binary operation: num op num
            [left, op, right] => {
                let left_val = if *left == "ans" {
                    self.last_result.unwrap_or(0)
                } else {
                    match left.parse::<i64>() {
                        Ok(n) => n,
                        Err(_) => return "Invalid left operand".into(),
                    }
                };

                let right_val = if *right == "ans" {
                    self.last_result.unwrap_or(0)
                } else {
                    match right.parse::<i64>() {
                        Ok(n) => n,
                        Err(_) => return "Invalid right operand".into(),
                    }
                };

                match *op {
                    "+" => self.safe_add(left_val, right_val),
                    "-" => self.safe_sub(left_val, right_val),
                    "*" => self.safe_mul(left_val, right_val),
                    "/" => self.safe_div(left_val, right_val),
                    "%" => self.safe_mod(left_val, right_val),
                    _ => format!("Unknown operator: {}", op),
                }
            }

            // Special commands
            ["max", "i64"] => format!("{}", i64::MAX),
            ["min", "i64"] => format!("{}", i64::MIN),
            ["overflow", "demo"] => self.demonstrate_overflow_protection(),
            ["divzero", "demo"] => self.demonstrate_division_by_zero_protection(),
            ["help"] => self.help_message(),

            _ => "Usage: <num> <op> <num> (ops: + - * / %), or 'help'".into(),
        };

        // Add to history (safely bounded)
        let entry = format!("{} = {}", expr.trim(), result);
        self.history.push(entry);
        while self.history.len() > self.max_history {
            self.history.remove(0);
        }

        result
    }

    fn safe_add(&mut self, a: i64, b: i64) -> String {
        match SafeMath::add(a, b) {
            Ok(result) => {
                self.last_result = Some(result);
                format!("{} (safe)", result)
            }
            Err(e) => format!("Overflow protected: {:?}", e),
        }
    }

    fn safe_sub(&mut self, a: i64, b: i64) -> String {
        match SafeMath::sub(a, b) {
            Ok(result) => {
                self.last_result = Some(result);
                format!("{} (safe)", result)
            }
            Err(e) => format!("Underflow protected: {:?}", e),
        }
    }

    fn safe_mul(&mut self, a: i64, b: i64) -> String {
        match SafeMath::mul(a, b) {
            Ok(result) => {
                self.last_result = Some(result);
                format!("{} (safe)", result)
            }
            Err(e) => format!("Overflow protected: {:?}", e),
        }
    }

    fn safe_div(&mut self, a: i64, b: i64) -> String {
        match SafeMath::div(a, b) {
            Ok(result) => {
                self.last_result = Some(result);
                format!("{} (safe)", result)
            }
            Err(e) => format!("Division error protected: {:?}", e),
        }
    }

    fn safe_mod(&mut self, a: i64, b: i64) -> String {
        match SafeMath::modulo(a, b) {
            Ok(result) => {
                self.last_result = Some(result);
                format!("{} (safe)", result)
            }
            Err(e) => format!("Modulo error protected: {:?}", e),
        }
    }

    fn demonstrate_overflow_protection(&mut self) -> String {
        let max = i64::MAX;
        match SafeMath::add(max, 1) {
            Ok(_) => "Unexpected: overflow not caught!".into(),
            Err(_) => format!(
                "SAFE! Adding 1 to {} would overflow, but Proven caught it",
                max
            ),
        }
    }

    fn demonstrate_division_by_zero_protection(&mut self) -> String {
        match SafeMath::div(42, 0) {
            Ok(_) => "Unexpected: division by zero not caught!".into(),
            Err(_) => "SAFE! Division by zero was caught by Proven".into(),
        }
    }

    fn help_message(&self) -> String {
        r#"SafeCalculator - Verified Safe Arithmetic

Operations:
  <num> + <num>   Addition with overflow protection
  <num> - <num>   Subtraction with underflow protection
  <num> * <num>   Multiplication with overflow protection
  <num> / <num>   Division with zero-check protection
  <num> % <num>   Modulo with zero-check protection

Special:
  ans             Use previous result
  max i64         Show maximum i64 value
  min i64         Show minimum i64 value
  overflow demo   Demonstrate overflow protection
  divzero demo    Demonstrate division by zero protection"#
            .into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe_addition() {
        let mut calc = SafeCalculator::new();
        let result = calc.evaluate("40 + 2");
        assert!(result.contains("42"));
    }

    #[test]
    fn test_safe_subtraction() {
        let mut calc = SafeCalculator::new();
        let result = calc.evaluate("50 - 8");
        assert!(result.contains("42"));
    }

    #[test]
    fn test_safe_multiplication() {
        let mut calc = SafeCalculator::new();
        let result = calc.evaluate("6 * 7");
        assert!(result.contains("42"));
    }

    #[test]
    fn test_safe_division() {
        let mut calc = SafeCalculator::new();
        let result = calc.evaluate("84 / 2");
        assert!(result.contains("42"));
    }

    #[test]
    fn test_overflow_protection() {
        let mut calc = SafeCalculator::new();
        let result = calc.evaluate("overflow demo");
        assert!(result.contains("SAFE"));
    }

    #[test]
    fn test_division_by_zero_protection() {
        let mut calc = SafeCalculator::new();
        let result = calc.evaluate("divzero demo");
        assert!(result.contains("SAFE"));
    }

    #[test]
    fn test_ans_feature() {
        let mut calc = SafeCalculator::new();
        calc.evaluate("10 + 5");
        let result = calc.evaluate("ans * 2");
        assert!(result.contains("30"));
    }

    #[test]
    fn test_history_bounded() {
        let mut calc = SafeCalculator::with_history_limit(3);
        calc.evaluate("1 + 1");
        calc.evaluate("2 + 2");
        calc.evaluate("3 + 3");
        calc.evaluate("4 + 4");
        assert_eq!(calc.history().len(), 3);
    }
}
