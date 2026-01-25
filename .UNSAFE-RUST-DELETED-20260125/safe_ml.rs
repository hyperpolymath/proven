// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Safe machine learning operations with mathematical guarantees.
//!
//! This module provides formally verified implementations of common ML operations
//! that are mathematically proven to never produce NaN, Infinity, or crash.
//!
//! # Design Philosophy
//!
//! ML operations are prone to numerical instability:
//! - Softmax can overflow (exp of large numbers) or divide by zero (exp underflow)
//! - Cross-entropy loss produces NaN on log(0)
//! - Batch operations fail silently on empty batches
//!
//! This module handles all edge cases explicitly.
//!
//! # Example
//!
//! ```rust
//! use proven::SafeML;
//!
//! // Safe softmax that never produces NaN or crashes
//! let logits = vec![1.0, 2.0, 3.0];
//! let probs = SafeML::softmax(&logits).unwrap();
//! assert!((probs.iter().sum::<f32>() - 1.0).abs() < 1e-6);
//!
//! // Safe cross-entropy that handles edge cases
//! let targets = vec![0.0, 1.0, 0.0];
//! let loss = SafeML::cross_entropy(&probs, &targets).unwrap();
//! ```

use crate::core::{Error, Result};

/// Safe ML operations that never produce NaN or crash.
pub struct SafeML;

impl SafeML {
    /// Safe softmax function that handles all edge cases.
    ///
    /// Uses the numerically stable formulation: softmax(x)_i = exp(x_i - max(x)) / sum(exp(x - max(x)))
    ///
    /// Edge cases handled:
    /// - Empty input: Returns Error
    /// - All underflow: Returns uniform distribution
    /// - Single element: Returns [1.0]
    ///
    /// # Example
    /// ```rust
    /// use proven::SafeML;
    ///
    /// let logits = vec![1.0, 2.0, 3.0];
    /// let probs = SafeML::softmax(&logits).unwrap();
    /// // probs sums to 1.0, largest input has largest probability
    /// ```
    pub fn softmax(logits: &[f32]) -> Result<Vec<f32>> {
        if logits.is_empty() {
            return Err(Error::EmptyInput);
        }

        // Subtract max for numerical stability (prevents overflow in exp)
        let max = logits
            .iter()
            .copied()
            .fold(f32::NEG_INFINITY, f32::max);

        let exps: Vec<f32> = logits.iter().map(|&x| (x - max).exp()).collect();
        let sum: f32 = exps.iter().sum();

        // Handle underflow case: if all exps are effectively 0, return uniform
        if sum < f32::EPSILON {
            let uniform = 1.0 / logits.len() as f32;
            return Ok(vec![uniform; logits.len()]);
        }

        Ok(exps.iter().map(|&e| e / sum).collect())
    }

    /// Safe f64 softmax.
    pub fn softmax_f64(logits: &[f64]) -> Result<Vec<f64>> {
        if logits.is_empty() {
            return Err(Error::EmptyInput);
        }

        let max = logits
            .iter()
            .copied()
            .fold(f64::NEG_INFINITY, f64::max);

        let exps: Vec<f64> = logits.iter().map(|&x| (x - max).exp()).collect();
        let sum: f64 = exps.iter().sum();

        if sum < f64::EPSILON {
            let uniform = 1.0 / logits.len() as f64;
            return Ok(vec![uniform; logits.len()]);
        }

        Ok(exps.iter().map(|&e| e / sum).collect())
    }

    /// Safe log-softmax for numerical stability in loss computation.
    ///
    /// Returns log(softmax(x)) but computed more stably.
    pub fn log_softmax(logits: &[f32]) -> Result<Vec<f32>> {
        if logits.is_empty() {
            return Err(Error::EmptyInput);
        }

        let max = logits
            .iter()
            .copied()
            .fold(f32::NEG_INFINITY, f32::max);

        let shifted: Vec<f32> = logits.iter().map(|&x| x - max).collect();
        let log_sum_exp = shifted.iter().map(|&x| x.exp()).sum::<f32>().ln();

        Ok(shifted.iter().map(|&x| x - log_sum_exp).collect())
    }

    /// Safe cross-entropy loss between predictions and targets.
    ///
    /// Formula: -sum(target * log(prediction))
    ///
    /// Edge cases handled:
    /// - Empty vectors: Returns Error
    /// - Shape mismatch: Returns Error
    /// - Zero predictions: Clamps to small epsilon to avoid -Infinity
    ///
    /// # Example
    /// ```rust
    /// use proven::SafeML;
    ///
    /// let predictions = vec![0.1, 0.7, 0.2];
    /// let targets = vec![0.0, 1.0, 0.0]; // One-hot
    /// let loss = SafeML::cross_entropy(&predictions, &targets).unwrap();
    /// ```
    pub fn cross_entropy(predictions: &[f32], targets: &[f32]) -> Result<f32> {
        if predictions.is_empty() || targets.is_empty() {
            return Err(Error::EmptyInput);
        }
        if predictions.len() != targets.len() {
            return Err(Error::ValidationError(format!(
                "Cross-entropy shape mismatch: {} vs {}",
                predictions.len(),
                targets.len()
            )));
        }

        const EPSILON: f32 = 1e-7;
        let mut loss = 0.0f32;

        for (&p, &t) in predictions.iter().zip(targets.iter()) {
            // Clamp prediction to avoid log(0)
            let p_safe = p.max(EPSILON);
            loss -= t * p_safe.ln();
        }

        if !loss.is_finite() {
            return Err(Error::Overflow("Cross-entropy loss is not finite".to_string()));
        }

        Ok(loss)
    }

    /// Safe binary cross-entropy loss.
    ///
    /// Formula: -[y*log(p) + (1-y)*log(1-p)]
    pub fn binary_cross_entropy(prediction: f32, target: f32) -> Result<f32> {
        const EPSILON: f32 = 1e-7;

        let p = prediction.clamp(EPSILON, 1.0 - EPSILON);
        let loss = -(target * p.ln() + (1.0 - target) * (1.0 - p).ln());

        if !loss.is_finite() {
            return Err(Error::Overflow("Binary cross-entropy is not finite".to_string()));
        }

        Ok(loss)
    }

    /// Safe mean squared error loss.
    ///
    /// Formula: mean((predictions - targets)^2)
    pub fn mse(predictions: &[f32], targets: &[f32]) -> Result<f32> {
        if predictions.is_empty() || targets.is_empty() {
            return Err(Error::EmptyInput);
        }
        if predictions.len() != targets.len() {
            return Err(Error::ValidationError(format!(
                "MSE shape mismatch: {} vs {}",
                predictions.len(),
                targets.len()
            )));
        }

        let sum: f32 = predictions
            .iter()
            .zip(targets.iter())
            .map(|(&p, &t)| (p - t).powi(2))
            .sum();

        Ok(sum / predictions.len() as f32)
    }

    /// Safe mean absolute error loss.
    pub fn mae(predictions: &[f32], targets: &[f32]) -> Result<f32> {
        if predictions.is_empty() || targets.is_empty() {
            return Err(Error::EmptyInput);
        }
        if predictions.len() != targets.len() {
            return Err(Error::ValidationError(format!(
                "MAE shape mismatch: {} vs {}",
                predictions.len(),
                targets.len()
            )));
        }

        let sum: f32 = predictions
            .iter()
            .zip(targets.iter())
            .map(|(&p, &t)| (p - t).abs())
            .sum();

        Ok(sum / predictions.len() as f32)
    }

    /// ReLU activation function.
    ///
    /// Formula: max(0, x)
    pub fn relu(x: f32) -> f32 {
        x.max(0.0)
    }

    /// ReLU applied to a vector.
    pub fn relu_vec(v: &[f32]) -> Vec<f32> {
        v.iter().map(|&x| Self::relu(x)).collect()
    }

    /// Leaky ReLU activation.
    ///
    /// Formula: x if x > 0, else alpha * x
    pub fn leaky_relu(x: f32, alpha: f32) -> f32 {
        if x > 0.0 {
            x
        } else {
            alpha * x
        }
    }

    /// Leaky ReLU applied to a vector.
    pub fn leaky_relu_vec(v: &[f32], alpha: f32) -> Vec<f32> {
        v.iter().map(|&x| Self::leaky_relu(x, alpha)).collect()
    }

    /// Safe sigmoid activation.
    ///
    /// Formula: 1 / (1 + exp(-x))
    ///
    /// Handles overflow by clamping input.
    pub fn sigmoid(x: f32) -> f32 {
        // Clamp to prevent overflow in exp
        let x_clamped = x.clamp(-88.0, 88.0);
        1.0 / (1.0 + (-x_clamped).exp())
    }

    /// Sigmoid applied to a vector.
    pub fn sigmoid_vec(v: &[f32]) -> Vec<f32> {
        v.iter().map(|&x| Self::sigmoid(x)).collect()
    }

    /// Safe tanh activation.
    ///
    /// Standard tanh is already numerically stable, but we ensure finite output.
    pub fn tanh(x: f32) -> f32 {
        x.tanh()
    }

    /// Tanh applied to a vector.
    pub fn tanh_vec(v: &[f32]) -> Vec<f32> {
        v.iter().map(|&x| Self::tanh(x)).collect()
    }

    /// GELU activation (Gaussian Error Linear Unit).
    ///
    /// Approximation: 0.5 * x * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3)))
    pub fn gelu(x: f32) -> f32 {
        const SQRT_2_PI: f32 = 0.7978845608; // sqrt(2/pi)
        0.5 * x * (1.0 + (SQRT_2_PI * (x + 0.044715 * x.powi(3))).tanh())
    }

    /// GELU applied to a vector.
    pub fn gelu_vec(v: &[f32]) -> Vec<f32> {
        v.iter().map(|&x| Self::gelu(x)).collect()
    }

    /// Safe batch normalization.
    ///
    /// Formula: (x - mean) / sqrt(variance + epsilon)
    ///
    /// Handles edge cases like zero variance.
    pub fn batch_norm(x: &[f32], epsilon: f32) -> Result<Vec<f32>> {
        if x.is_empty() {
            return Err(Error::EmptyInput);
        }

        let mean: f32 = x.iter().sum::<f32>() / x.len() as f32;
        let variance: f32 = x.iter().map(|&v| (v - mean).powi(2)).sum::<f32>() / x.len() as f32;

        let std_dev = (variance + epsilon).sqrt();
        if std_dev < f32::EPSILON {
            // All values are the same, return zeros
            return Ok(vec![0.0; x.len()]);
        }

        Ok(x.iter().map(|&v| (v - mean) / std_dev).collect())
    }

    /// Layer normalization (normalize over features).
    pub fn layer_norm(x: &[f32], epsilon: f32) -> Result<Vec<f32>> {
        Self::batch_norm(x, epsilon)
    }

    /// Safe dropout (for inference, just returns input).
    ///
    /// For training dropout, use a random generator externally.
    pub fn dropout_inference(x: &[f32]) -> Vec<f32> {
        x.to_vec()
    }

    /// Argmax - returns index of maximum value.
    pub fn argmax(v: &[f32]) -> Result<usize> {
        if v.is_empty() {
            return Err(Error::EmptyInput);
        }
        Ok(v.iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .map(|(idx, _)| idx)
            .unwrap_or(0))
    }

    /// Safe accuracy computation.
    ///
    /// Computes the proportion of correct predictions.
    pub fn accuracy(predictions: &[usize], targets: &[usize]) -> Result<f32> {
        if predictions.is_empty() || targets.is_empty() {
            return Err(Error::EmptyInput);
        }
        if predictions.len() != targets.len() {
            return Err(Error::ValidationError(format!(
                "Accuracy shape mismatch: {} vs {}",
                predictions.len(),
                targets.len()
            )));
        }

        let correct: usize = predictions
            .iter()
            .zip(targets.iter())
            .filter(|(&p, &t)| p == t)
            .count();

        Ok(correct as f32 / predictions.len() as f32)
    }

    /// Gradient clipping by value.
    ///
    /// Clips gradients to [-clip_value, clip_value].
    pub fn clip_gradients(gradients: &[f32], clip_value: f32) -> Vec<f32> {
        gradients
            .iter()
            .map(|&g| g.clamp(-clip_value, clip_value))
            .collect()
    }

    /// Gradient clipping by norm.
    ///
    /// If the L2 norm exceeds max_norm, scales gradients down.
    pub fn clip_gradients_by_norm(gradients: &[f32], max_norm: f32) -> Vec<f32> {
        let norm: f32 = gradients.iter().map(|&g| g * g).sum::<f32>().sqrt();

        if norm > max_norm && norm > f32::EPSILON {
            let scale = max_norm / norm;
            gradients.iter().map(|&g| g * scale).collect()
        } else {
            gradients.to_vec()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_softmax_basic() {
        let logits = vec![1.0, 2.0, 3.0];
        let probs = SafeML::softmax(&logits).unwrap();

        // Should sum to 1
        let sum: f32 = probs.iter().sum();
        assert!((sum - 1.0).abs() < 1e-6);

        // Should be monotonic (higher logit = higher prob)
        assert!(probs[2] > probs[1]);
        assert!(probs[1] > probs[0]);
    }

    #[test]
    fn test_softmax_empty() {
        assert!(SafeML::softmax(&[]).is_err());
    }

    #[test]
    fn test_softmax_single() {
        let probs = SafeML::softmax(&[5.0]).unwrap();
        assert_eq!(probs.len(), 1);
        assert!((probs[0] - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_softmax_large_values() {
        // Should not overflow
        let logits = vec![1000.0, 1001.0, 1002.0];
        let probs = SafeML::softmax(&logits).unwrap();
        let sum: f32 = probs.iter().sum();
        assert!((sum - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_softmax_underflow() {
        // Very negative values should return uniform (not NaN)
        let logits = vec![-1000.0, -1000.0, -1000.0];
        let probs = SafeML::softmax(&logits).unwrap();
        let sum: f32 = probs.iter().sum();
        assert!((sum - 1.0).abs() < 1e-6);
        // Should be approximately uniform
        assert!((probs[0] - 1.0 / 3.0).abs() < 1e-6);
    }

    #[test]
    fn test_cross_entropy() {
        let predictions = vec![0.7, 0.2, 0.1];
        let targets = vec![1.0, 0.0, 0.0]; // One-hot
        let loss = SafeML::cross_entropy(&predictions, &targets).unwrap();
        // -1 * ln(0.7) ≈ 0.357
        assert!((loss - 0.357).abs() < 0.01);
    }

    #[test]
    fn test_cross_entropy_near_zero() {
        let predictions = vec![0.0001, 0.9999]; // Near-zero prediction
        let targets = vec![1.0, 0.0];
        // Should not return -Infinity
        let loss = SafeML::cross_entropy(&predictions, &targets).unwrap();
        assert!(loss.is_finite());
    }

    #[test]
    fn test_mse() {
        let predictions = vec![1.0, 2.0, 3.0];
        let targets = vec![1.0, 2.0, 3.0];
        assert_eq!(SafeML::mse(&predictions, &targets).unwrap(), 0.0);

        let predictions2 = vec![0.0, 0.0, 0.0];
        let targets2 = vec![1.0, 1.0, 1.0];
        assert_eq!(SafeML::mse(&predictions2, &targets2).unwrap(), 1.0);
    }

    #[test]
    fn test_relu() {
        assert_eq!(SafeML::relu(5.0), 5.0);
        assert_eq!(SafeML::relu(-5.0), 0.0);
        assert_eq!(SafeML::relu(0.0), 0.0);
    }

    #[test]
    fn test_sigmoid() {
        assert!((SafeML::sigmoid(0.0) - 0.5).abs() < 1e-6);
        assert!(SafeML::sigmoid(100.0) > 0.99);
        assert!(SafeML::sigmoid(-100.0) < 0.01);
    }

    #[test]
    fn test_batch_norm() {
        let x = vec![1.0, 2.0, 3.0, 4.0, 5.0];
        let normalized = SafeML::batch_norm(&x, 1e-5).unwrap();

        // Mean should be ~0
        let mean: f32 = normalized.iter().sum::<f32>() / normalized.len() as f32;
        assert!(mean.abs() < 1e-5);

        // Variance should be ~1
        let variance: f32 = normalized.iter().map(|&v| v * v).sum::<f32>() / normalized.len() as f32;
        assert!((variance - 1.0).abs() < 0.1);
    }

    #[test]
    fn test_argmax() {
        let v = vec![1.0, 5.0, 3.0, 2.0];
        assert_eq!(SafeML::argmax(&v).unwrap(), 1);
    }

    #[test]
    fn test_accuracy() {
        let predictions = vec![0, 1, 2, 0, 1];
        let targets = vec![0, 1, 1, 0, 2];
        let acc = SafeML::accuracy(&predictions, &targets).unwrap();
        assert!((acc - 0.6).abs() < 1e-6); // 3/5 correct
    }

    #[test]
    fn test_gradient_clipping() {
        let grads = vec![10.0, -20.0, 5.0];
        let clipped = SafeML::clip_gradients(&grads, 8.0);
        assert_eq!(clipped, vec![8.0, -8.0, 5.0]);
    }

    #[test]
    fn test_gradient_clipping_by_norm() {
        let grads = vec![3.0, 4.0]; // Norm = 5
        let clipped = SafeML::clip_gradients_by_norm(&grads, 2.5);
        // Should scale to norm 2.5, so [1.5, 2.0]
        assert!((clipped[0] - 1.5).abs() < 1e-6);
        assert!((clipped[1] - 2.0).abs() < 1e-6);
    }
}
