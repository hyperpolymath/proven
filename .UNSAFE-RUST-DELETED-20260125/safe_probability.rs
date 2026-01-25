// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe probability values clamped to [0, 1].
//!
//! Provides type-safe probability operations with automatic
//! clamping and validation.

use crate::{Error, Result};

/// A probability value guaranteed to be in [0, 1].
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Probability(f64);

impl Probability {
    /// Create a probability, clamping to [0, 1].
    pub fn new(value: f64) -> Self {
        Self(value.clamp(0.0, 1.0))
    }

    /// Create a probability with validation (fails if out of range).
    pub fn try_new(value: f64) -> Result<Self> {
        if value.is_nan() {
            return Err(Error::InvalidInput("Probability cannot be NaN".into()));
        }
        if !(0.0..=1.0).contains(&value) {
            return Err(Error::OutOfRange(
                "Probability must be between 0 and 1".into(),
            ));
        }
        Ok(Self(value))
    }

    /// Get the value.
    pub fn value(&self) -> f64 {
        self.0
    }

    /// Probability of 0 (impossible).
    pub const ZERO: Probability = Probability(0.0);

    /// Probability of 1 (certain).
    pub const ONE: Probability = Probability(1.0);

    /// Probability of 0.5.
    pub const HALF: Probability = Probability(0.5);

    /// Get the complement (1 - p).
    pub fn complement(&self) -> Self {
        Self(1.0 - self.0)
    }

    /// Combine independent probabilities (AND).
    pub fn and(&self, other: &Probability) -> Self {
        Self(self.0 * other.0)
    }

    /// Combine probabilities (OR for independent events).
    pub fn or(&self, other: &Probability) -> Self {
        Self::new(self.0 + other.0 - self.0 * other.0)
    }

    /// Conditional probability (given another probability).
    pub fn given(&self, condition: &Probability) -> Option<Self> {
        if condition.0 == 0.0 {
            None
        } else {
            Some(Self::new(self.0 / condition.0))
        }
    }

    /// Convert to percentage.
    pub fn to_percent(&self) -> f64 {
        self.0 * 100.0
    }

    /// Create from percentage.
    pub fn from_percent(percent: f64) -> Self {
        Self::new(percent / 100.0)
    }

    /// Convert to odds ratio.
    pub fn to_odds(&self) -> Option<f64> {
        if self.0 >= 1.0 {
            None
        } else {
            Some(self.0 / (1.0 - self.0))
        }
    }

    /// Create from odds ratio.
    pub fn from_odds(odds: f64) -> Self {
        if odds < 0.0 {
            Self::ZERO
        } else {
            Self::new(odds / (1.0 + odds))
        }
    }
}

impl Default for Probability {
    fn default() -> Self {
        Self::HALF
    }
}

/// Calculate expected value given outcomes and probabilities.
pub fn expected_value(outcomes: &[(f64, Probability)]) -> f64 {
    outcomes.iter().map(|(value, prob)| value * prob.value()).sum()
}

/// Normalize a list of weights to probabilities.
pub fn normalize_weights(weights: &[f64]) -> Vec<Probability> {
    let sum: f64 = weights.iter().filter(|w| **w >= 0.0).sum();
    if sum == 0.0 {
        return vec![Probability::ZERO; weights.len()];
    }
    weights
        .iter()
        .map(|w| Probability::new(w.max(0.0) / sum))
        .collect()
}

/// Calculate entropy of a probability distribution.
pub fn entropy(probs: &[Probability]) -> f64 {
    probs
        .iter()
        .filter(|p| p.value() > 0.0)
        .map(|p| -p.value() * p.value().log2())
        .sum()
}

/// Binomial probability: P(X = k) for n trials with probability p.
pub fn binomial(n: u32, k: u32, p: &Probability) -> Probability {
    if k > n {
        return Probability::ZERO;
    }

    let coeff = binomial_coefficient(n, k);
    let prob = coeff as f64 * p.value().powi(k as i32) * (1.0 - p.value()).powi((n - k) as i32);
    Probability::new(prob)
}

fn binomial_coefficient(n: u32, k: u32) -> u64 {
    if k > n {
        return 0;
    }
    if k == 0 || k == n {
        return 1;
    }

    let k = k.min(n - k) as u64;
    let mut result: u64 = 1;
    for i in 0..k {
        result = result.saturating_mul(n as u64 - i) / (i + 1);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_probability_clamping() {
        assert_eq!(Probability::new(1.5).value(), 1.0);
        assert_eq!(Probability::new(-0.5).value(), 0.0);
        assert_eq!(Probability::new(0.5).value(), 0.5);
    }

    #[test]
    fn test_complement() {
        let p = Probability::new(0.3);
        assert!((p.complement().value() - 0.7).abs() < 0.0001);
    }

    #[test]
    fn test_and_or() {
        let a = Probability::new(0.5);
        let b = Probability::new(0.5);

        assert!((a.and(&b).value() - 0.25).abs() < 0.0001);
        assert!((a.or(&b).value() - 0.75).abs() < 0.0001);
    }

    #[test]
    fn test_odds() {
        let p = Probability::new(0.75);
        let odds = p.to_odds().unwrap();
        assert!((odds - 3.0).abs() < 0.0001);

        let back = Probability::from_odds(3.0);
        assert!((back.value() - 0.75).abs() < 0.0001);
    }

    #[test]
    fn test_normalize() {
        let weights = vec![1.0, 2.0, 3.0];
        let probs = normalize_weights(&weights);
        assert!((probs[0].value() - 1.0 / 6.0).abs() < 0.0001);
        assert!((probs[1].value() - 2.0 / 6.0).abs() < 0.0001);
        assert!((probs[2].value() - 3.0 / 6.0).abs() < 0.0001);
    }
}
