// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe mathematical operations with overflow detection.

use crate::core::{Error, Result};

/// Safe mathematical operations that never panic.
pub struct SafeMath;

impl SafeMath {
    /// Safe addition with overflow detection.
    ///
    /// # Example
    /// ```
    /// use proven::SafeMath;
    ///
    /// assert_eq!(SafeMath::add(1, 2).unwrap(), 3);
    /// assert!(SafeMath::add(i64::MAX, 1).is_err());
    /// ```
    pub fn add(a: i64, b: i64) -> Result<i64> {
        a.checked_add(b)
            .ok_or_else(|| Error::Overflow(format!("{} + {} overflows", a, b)))
    }

    /// Safe subtraction with underflow detection.
    pub fn sub(a: i64, b: i64) -> Result<i64> {
        a.checked_sub(b)
            .ok_or_else(|| Error::Overflow(format!("{} - {} underflows", a, b)))
    }

    /// Safe multiplication with overflow detection.
    pub fn mul(a: i64, b: i64) -> Result<i64> {
        a.checked_mul(b)
            .ok_or_else(|| Error::Overflow(format!("{} * {} overflows", a, b)))
    }

    /// Safe division with zero check.
    ///
    /// # Example
    /// ```
    /// use proven::SafeMath;
    ///
    /// assert_eq!(SafeMath::div(10, 2).unwrap(), 5);
    /// assert!(SafeMath::div(10, 0).is_err());
    /// ```
    pub fn div(a: i64, b: i64) -> Result<i64> {
        if b == 0 {
            return Err(Error::DivisionByZero);
        }
        // Check for MIN / -1 overflow
        if a == i64::MIN && b == -1 {
            return Err(Error::Overflow(format!("{} / {} overflows", a, b)));
        }
        Ok(a / b)
    }

    /// Safe modulo with zero check.
    pub fn modulo(a: i64, b: i64) -> Result<i64> {
        if b == 0 {
            return Err(Error::DivisionByZero);
        }
        Ok(a % b)
    }

    /// Safe negation with overflow check.
    pub fn negate(a: i64) -> Result<i64> {
        a.checked_neg()
            .ok_or_else(|| Error::Overflow(format!("-{} overflows", a)))
    }

    /// Safe absolute value with overflow check.
    pub fn abs(a: i64) -> Result<i64> {
        if a == i64::MIN {
            Err(Error::Overflow(format!("abs({}) overflows", a)))
        } else {
            Ok(a.abs())
        }
    }

    /// Safe power with overflow detection.
    pub fn pow(base: i64, exp: u32) -> Result<i64> {
        base.checked_pow(exp)
            .ok_or_else(|| Error::Overflow(format!("{}^{} overflows", base, exp)))
    }

    /// Check if addition would overflow.
    pub fn would_overflow_add(a: i64, b: i64) -> bool {
        a.checked_add(b).is_none()
    }

    /// Check if multiplication would overflow.
    pub fn would_overflow_mul(a: i64, b: i64) -> bool {
        a.checked_mul(b).is_none()
    }

    /// Clamp a value to a range.
    pub fn clamp(value: i64, min: i64, max: i64) -> i64 {
        value.clamp(min, max)
    }

    /// Safe increment.
    pub fn inc(a: i64) -> Result<i64> {
        Self::add(a, 1)
    }

    /// Safe decrement.
    pub fn dec(a: i64) -> Result<i64> {
        Self::sub(a, 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_normal() {
        assert_eq!(SafeMath::add(1, 2).unwrap(), 3);
        assert_eq!(SafeMath::add(-1, 1).unwrap(), 0);
        assert_eq!(SafeMath::add(0, 0).unwrap(), 0);
    }

    #[test]
    fn test_add_overflow() {
        assert!(SafeMath::add(i64::MAX, 1).is_err());
        assert!(SafeMath::add(i64::MIN, -1).is_err());
    }

    #[test]
    fn test_sub_normal() {
        assert_eq!(SafeMath::sub(3, 2).unwrap(), 1);
        assert_eq!(SafeMath::sub(0, 0).unwrap(), 0);
    }

    #[test]
    fn test_sub_underflow() {
        assert!(SafeMath::sub(i64::MIN, 1).is_err());
    }

    #[test]
    fn test_mul_overflow() {
        assert!(SafeMath::mul(i64::MAX, 2).is_err());
    }

    #[test]
    fn test_div_zero() {
        assert!(SafeMath::div(1, 0).is_err());
        assert!(SafeMath::modulo(1, 0).is_err());
    }

    #[test]
    fn test_div_overflow() {
        assert!(SafeMath::div(i64::MIN, -1).is_err());
    }

    #[test]
    fn test_negate_overflow() {
        assert!(SafeMath::negate(i64::MIN).is_err());
        assert_eq!(SafeMath::negate(5).unwrap(), -5);
    }

    #[test]
    fn test_abs_overflow() {
        assert!(SafeMath::abs(i64::MIN).is_err());
        assert_eq!(SafeMath::abs(-5).unwrap(), 5);
    }

    #[test]
    fn test_pow() {
        assert_eq!(SafeMath::pow(2, 10).unwrap(), 1024);
        assert!(SafeMath::pow(i64::MAX, 2).is_err());
    }
}
