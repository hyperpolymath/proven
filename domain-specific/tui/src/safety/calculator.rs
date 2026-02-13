// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe calculator using Proven's SafeMath primitives

use proven::SafeMath;

/// A calculator that uses Proven's verified safe math operations
#[derive(Debug, Default)]
pub struct SafeCalculator {
    /// Last result for chaining operations
    last_result: Option<i64>,
}

impl SafeCalculator {
    pub fn new() -> Self {
        Self { last_result: None }
    }

    /// Evaluate a simple expression like "42 + 7" or "1000 * 1000000000"
    pub fn evaluate(&mut self, expr: &str) -> String {
        let parts: Vec<&str> = expr.split_whitespace().collect();

        match parts.as_slice() {
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

                let right_val = match right.parse::<i64>() {
                    Ok(n) => n,
                    Err(_) => return "Invalid right operand".into(),
                };

                let result = match *op {
                    "+" => self.safe_add(left_val, right_val),
                    "-" => self.safe_sub(left_val, right_val),
                    "*" => self.safe_mul(left_val, right_val),
                    "/" => self.safe_div(left_val, right_val),
                    "%" => self.safe_mod(left_val, right_val),
                    _ => return format!("Unknown operator: {}", op),
                };

                result
            }

            // Special commands
            ["max", "i64"] => format!("{}", i64::MAX),
            ["min", "i64"] => format!("{}", i64::MIN),
            ["overflow", "demo"] => self.demonstrate_overflow_protection(),
            ["divzero", "demo"] => self.demonstrate_division_by_zero_protection(),

            _ => "Usage: <num> <op> <num> (ops: + - * / %)".into(),
        }
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
            Err(e) => format!("Overflow protected: {:?}", e),
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
}
