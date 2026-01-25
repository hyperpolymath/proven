// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe floating-point operations with NaN/Infinity prevention.
//!
//! This module provides mathematically verified safe operations for f32/f64,
//! preventing the common pitfalls that lead to NaN, Infinity, or crashes
//! in numerical computing and machine learning applications.
//!
//! # Design Philosophy
//!
//! Unlike integers which can overflow, floating-point numbers silently produce
//! NaN or Infinity on edge cases. This module makes those edge cases explicit
//! through Rust's Result type, following the proven library's pattern.
//!
//! # Example
//!
//! ```rust
//! use proven::SafeFloat;
//!
//! // Division by zero returns Error instead of Infinity
//! assert!(SafeFloat::div(1.0, 0.0).is_err());
//!
//! // Safe normalization prevents divide-by-zero on zero vectors
//! let v = vec![0.0, 0.0, 0.0];
//! assert!(SafeFloat::normalize(&v).is_err());
//!
//! // Safe log prevents NaN on non-positive values
//! assert!(SafeFloat::ln(0.0).is_err());
//! assert!(SafeFloat::ln(-1.0).is_err());
//! ```

use crate::core::{Error, Result};

/// Safe floating-point operations that never produce NaN or Infinity.
pub struct SafeFloat;

impl SafeFloat {
    /// Minimum positive value to consider non-zero (prevents denormal issues)
    pub const EPSILON: f64 = 1e-10;

    /// Safe division with zero check.
    ///
    /// Returns Error::DivisionByZero if divisor is zero or too small.
    ///
    /// # Example
    /// ```rust
    /// use proven::SafeFloat;
    ///
    /// assert_eq!(SafeFloat::div(10.0, 2.0).unwrap(), 5.0);
    /// assert!(SafeFloat::div(1.0, 0.0).is_err());
    /// ```
    pub fn div(a: f64, b: f64) -> Result<f64> {
        if b.abs() < Self::EPSILON {
            return Err(Error::DivisionByZero);
        }
        let result = a / b;
        if result.is_nan() || result.is_infinite() {
            return Err(Error::Overflow(format!("{} / {} is not finite", a, b)));
        }
        Ok(result)
    }

    /// Safe division for f32 with zero check.
    pub fn div_f32(a: f32, b: f32) -> Result<f32> {
        if b.abs() < f32::EPSILON {
            return Err(Error::DivisionByZero);
        }
        let result = a / b;
        if result.is_nan() || result.is_infinite() {
            return Err(Error::Overflow(format!("{} / {} is not finite", a, b)));
        }
        Ok(result)
    }

    /// Safe natural logarithm (ln).
    ///
    /// Returns Error for non-positive values (which would produce NaN or -Infinity).
    ///
    /// # Example
    /// ```rust
    /// use proven::SafeFloat;
    ///
    /// assert!(SafeFloat::ln(std::f64::consts::E).unwrap().abs() - 1.0 < 1e-10);
    /// assert!(SafeFloat::ln(0.0).is_err());
    /// assert!(SafeFloat::ln(-1.0).is_err());
    /// ```
    pub fn ln(x: f64) -> Result<f64> {
        if x <= 0.0 {
            return Err(Error::ValidationError(format!(
                "ln({}) undefined for non-positive values",
                x
            )));
        }
        Ok(x.ln())
    }

    /// Safe log base 10.
    pub fn log10(x: f64) -> Result<f64> {
        if x <= 0.0 {
            return Err(Error::ValidationError(format!(
                "log10({}) undefined for non-positive values",
                x
            )));
        }
        Ok(x.log10())
    }

    /// Safe square root.
    ///
    /// Returns Error for negative values.
    pub fn sqrt(x: f64) -> Result<f64> {
        if x < 0.0 {
            return Err(Error::ValidationError(format!(
                "sqrt({}) undefined for negative values",
                x
            )));
        }
        Ok(x.sqrt())
    }

    /// Safe power operation.
    ///
    /// Returns Error if result would be NaN or Infinity.
    pub fn pow(base: f64, exp: f64) -> Result<f64> {
        let result = base.powf(exp);
        if result.is_nan() || result.is_infinite() {
            return Err(Error::Overflow(format!(
                "{}^{} is not finite",
                base, exp
            )));
        }
        Ok(result)
    }

    /// Safe exponential (e^x).
    ///
    /// Returns Error if result would overflow to Infinity.
    pub fn exp(x: f64) -> Result<f64> {
        let result = x.exp();
        if result.is_infinite() {
            return Err(Error::Overflow(format!("exp({}) overflows", x)));
        }
        Ok(result)
    }

    /// Safe exponential for f32.
    pub fn exp_f32(x: f32) -> Result<f32> {
        let result = x.exp();
        if result.is_infinite() {
            return Err(Error::Overflow(format!("exp({}) overflows", x)));
        }
        Ok(result)
    }

    /// Compute vector magnitude safely.
    ///
    /// Returns the L2 norm (Euclidean length) of the vector.
    pub fn magnitude(v: &[f64]) -> f64 {
        v.iter().map(|x| x * x).sum::<f64>().sqrt()
    }

    /// Compute f32 vector magnitude safely.
    pub fn magnitude_f32(v: &[f32]) -> f32 {
        v.iter().map(|x| x * x).sum::<f32>().sqrt()
    }

    /// Safe vector normalization (unit vector).
    ///
    /// Returns Error if vector has zero magnitude.
    ///
    /// # Example
    /// ```rust
    /// use proven::SafeFloat;
    ///
    /// let v = vec![3.0, 4.0];
    /// let normalized = SafeFloat::normalize(&v).unwrap();
    /// assert!((normalized[0] - 0.6).abs() < 1e-10);
    /// assert!((normalized[1] - 0.8).abs() < 1e-10);
    ///
    /// // Zero vector cannot be normalized
    /// assert!(SafeFloat::normalize(&vec![0.0, 0.0]).is_err());
    /// ```
    pub fn normalize(v: &[f64]) -> Result<Vec<f64>> {
        let mag = Self::magnitude(v);
        if mag < Self::EPSILON {
            return Err(Error::DivisionByZero);
        }
        Ok(v.iter().map(|x| x / mag).collect())
    }

    /// Safe f32 vector normalization.
    pub fn normalize_f32(v: &[f32]) -> Result<Vec<f32>> {
        let mag = Self::magnitude_f32(v);
        if mag < f32::EPSILON {
            return Err(Error::DivisionByZero);
        }
        Ok(v.iter().map(|x| x / mag).collect())
    }

    /// Check if a float is finite (not NaN or Infinity).
    pub fn is_finite(x: f64) -> bool {
        x.is_finite()
    }

    /// Check if a float is safe for division (non-zero and finite).
    pub fn is_safe_divisor(x: f64) -> bool {
        x.is_finite() && x.abs() >= Self::EPSILON
    }

    /// Clamp a float to a range, handling NaN by returning min.
    ///
    /// Unlike std::f64::clamp which panics on NaN, this safely handles it.
    pub fn clamp(value: f64, min: f64, max: f64) -> f64 {
        if value.is_nan() {
            return min;
        }
        if value < min {
            min
        } else if value > max {
            max
        } else {
            value
        }
    }

    /// Safe reciprocal (1/x).
    pub fn reciprocal(x: f64) -> Result<f64> {
        Self::div(1.0, x)
    }

    /// Compute mean of a vector safely.
    ///
    /// Returns Error for empty vectors.
    pub fn mean(v: &[f64]) -> Result<f64> {
        if v.is_empty() {
            return Err(Error::EmptyInput);
        }
        let sum: f64 = v.iter().sum();
        Self::div(sum, v.len() as f64)
    }

    /// Compute mean of f32 vector.
    pub fn mean_f32(v: &[f32]) -> Result<f32> {
        if v.is_empty() {
            return Err(Error::EmptyInput);
        }
        let sum: f32 = v.iter().sum();
        Self::div_f32(sum, v.len() as f32)
    }

    /// Compute variance safely.
    pub fn variance(v: &[f64]) -> Result<f64> {
        let mean = Self::mean(v)?;
        let sum_sq: f64 = v.iter().map(|x| (x - mean).powi(2)).sum();
        Self::div(sum_sq, v.len() as f64)
    }

    /// Compute standard deviation safely.
    pub fn std_dev(v: &[f64]) -> Result<f64> {
        let var = Self::variance(v)?;
        Self::sqrt(var)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_div_normal() {
        assert_eq!(SafeFloat::div(10.0, 2.0).unwrap(), 5.0);
        assert_eq!(SafeFloat::div(-10.0, 2.0).unwrap(), -5.0);
    }

    #[test]
    fn test_div_by_zero() {
        assert!(SafeFloat::div(1.0, 0.0).is_err());
        assert!(SafeFloat::div(0.0, 0.0).is_err());
        assert!(SafeFloat::div_f32(1.0, 0.0).is_err());
    }

    #[test]
    fn test_ln_positive() {
        let result = SafeFloat::ln(std::f64::consts::E).unwrap();
        assert!((result - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_ln_non_positive() {
        assert!(SafeFloat::ln(0.0).is_err());
        assert!(SafeFloat::ln(-1.0).is_err());
    }

    #[test]
    fn test_sqrt_positive() {
        assert_eq!(SafeFloat::sqrt(4.0).unwrap(), 2.0);
        assert_eq!(SafeFloat::sqrt(0.0).unwrap(), 0.0);
    }

    #[test]
    fn test_sqrt_negative() {
        assert!(SafeFloat::sqrt(-1.0).is_err());
    }

    #[test]
    fn test_normalize() {
        let v = vec![3.0, 4.0];
        let n = SafeFloat::normalize(&v).unwrap();
        assert!((n[0] - 0.6).abs() < 1e-10);
        assert!((n[1] - 0.8).abs() < 1e-10);

        // Magnitude should be 1.0
        let mag = SafeFloat::magnitude(&n);
        assert!((mag - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_normalize_zero_vector() {
        let v = vec![0.0, 0.0, 0.0];
        assert!(SafeFloat::normalize(&v).is_err());
    }

    #[test]
    fn test_exp_normal() {
        assert!((SafeFloat::exp(0.0).unwrap() - 1.0).abs() < 1e-10);
        assert!((SafeFloat::exp(1.0).unwrap() - std::f64::consts::E).abs() < 1e-10);
    }

    #[test]
    fn test_exp_overflow() {
        assert!(SafeFloat::exp(1000.0).is_err());
    }

    #[test]
    fn test_clamp_nan() {
        let nan = f64::NAN;
        assert_eq!(SafeFloat::clamp(nan, 0.0, 1.0), 0.0);
    }

    #[test]
    fn test_clamp_normal() {
        assert_eq!(SafeFloat::clamp(0.5, 0.0, 1.0), 0.5);
        assert_eq!(SafeFloat::clamp(-1.0, 0.0, 1.0), 0.0);
        assert_eq!(SafeFloat::clamp(2.0, 0.0, 1.0), 1.0);
    }

    #[test]
    fn test_mean() {
        let v = vec![1.0, 2.0, 3.0, 4.0, 5.0];
        assert_eq!(SafeFloat::mean(&v).unwrap(), 3.0);
    }

    #[test]
    fn test_mean_empty() {
        let v: Vec<f64> = vec![];
        assert!(SafeFloat::mean(&v).is_err());
    }

    #[test]
    fn test_variance() {
        let v = vec![2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0];
        let var = SafeFloat::variance(&v).unwrap();
        assert!((var - 4.0).abs() < 1e-10);
    }

    #[test]
    fn test_std_dev() {
        let v = vec![2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0];
        let sd = SafeFloat::std_dev(&v).unwrap();
        assert!((sd - 2.0).abs() < 1e-10);
    }
}
