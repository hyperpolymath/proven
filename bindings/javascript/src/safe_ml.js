// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeML - Numerically stable machine learning primitives.
 *
 * Provides safe implementations of softmax, loss functions, and activations.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Numerically stable ML operations.
 */
export class SafeML {
  /**
   * Numerically stable softmax.
   *
   * Uses the log-sum-exp trick to prevent overflow.
   *
   * @param {number[]} logits - Input logits
   * @returns {number[]}
   *
   * @example
   * SafeML.softmax([1.0, 2.0, 3.0])  // [0.09..., 0.24..., 0.66...]
   */
  static softmax(logits) {
    if (logits.length === 0) {
      return [];
    }

    // Subtract max for numerical stability
    const maxVal = Math.max(...logits);
    const expVals = logits.map((x) => Math.exp(x - maxVal));
    const sumExp = expVals.reduce((sum, val) => sum + val, 0);

    if (sumExp === 0) {
      return logits.map(() => 1.0 / logits.length);
    }

    return expVals.map((e) => e / sumExp);
  }

  /**
   * Numerically stable log-softmax.
   *
   * @param {number[]} logits - Input logits
   * @returns {number[]}
   */
  static logSoftmax(logits) {
    if (logits.length === 0) {
      return [];
    }

    const maxVal = Math.max(...logits);
    const shifted = logits.map((x) => x - maxVal);
    const logSumExp = Math.log(shifted.reduce((sum, x) => sum + Math.exp(x), 0));

    return shifted.map((x) => x - logSumExp);
  }

  /**
   * Numerically stable sigmoid.
   *
   * @param {number} x - Input value
   * @returns {number}
   */
  static sigmoid(x) {
    if (x >= 0) {
      const z = Math.exp(-x);
      return 1.0 / (1.0 + z);
    } else {
      const z = Math.exp(x);
      return z / (1.0 + z);
    }
  }

  /**
   * ReLU activation.
   *
   * @param {number} x - Input value
   * @returns {number}
   */
  static relu(x) {
    return Math.max(0, x);
  }

  /**
   * Leaky ReLU activation.
   *
   * @param {number} x - Input value
   * @param {number} [alpha=0.01] - Leak coefficient
   * @returns {number}
   */
  static leakyRelu(x, alpha = 0.01) {
    return x > 0 ? x : alpha * x;
  }

  /**
   * Hyperbolic tangent.
   *
   * @param {number} x - Input value
   * @returns {number}
   */
  static tanh(x) {
    return Math.tanh(x);
  }

  /**
   * Gaussian Error Linear Unit (GELU).
   *
   * Approximate GELU using tanh.
   *
   * @param {number} x - Input value
   * @returns {number}
   */
  static gelu(x) {
    return 0.5 * x * (1 + Math.tanh(Math.sqrt(2 / Math.PI) * (x + 0.044715 * x * x * x)));
  }

  /**
   * Cross-entropy loss for single sample.
   *
   * @param {number[]} predictions - Predicted probabilities (should sum to 1)
   * @param {number} target - True class index
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static crossEntropyLoss(predictions, target) {
    if (predictions.length === 0 || target < 0 || target >= predictions.length) {
      return err('Invalid predictions or target');
    }

    const prob = predictions[target];
    if (prob <= 0) {
      return ok(Infinity);
    }

    return ok(-Math.log(prob));
  }

  /**
   * Binary cross-entropy loss.
   *
   * @param {number} prediction - Predicted probability (0-1)
   * @param {number} target - True label (0 or 1)
   * @returns {number}
   */
  static binaryCrossEntropyLoss(prediction, target) {
    const eps = 1e-15;
    const clampedPred = Math.max(eps, Math.min(1 - eps, prediction));
    return -(target * Math.log(clampedPred) + (1 - target) * Math.log(1 - clampedPred));
  }

  /**
   * Mean squared error loss.
   *
   * @param {number[]} predictions - Predicted values
   * @param {number[]} targets - True values
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static mseLoss(predictions, targets) {
    if (predictions.length !== targets.length || predictions.length === 0) {
      return err('Predictions and targets must have same non-zero length');
    }

    let sum = 0;
    for (let sampleIndex = 0; sampleIndex < predictions.length; sampleIndex++) {
      const diff = predictions[sampleIndex] - targets[sampleIndex];
      sum += diff * diff;
    }
    return ok(sum / predictions.length);
  }

  /**
   * Mean absolute error loss.
   *
   * @param {number[]} predictions - Predicted values
   * @param {number[]} targets - True values
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static maeLoss(predictions, targets) {
    if (predictions.length !== targets.length || predictions.length === 0) {
      return err('Predictions and targets must have same non-zero length');
    }

    let sum = 0;
    for (let sampleIndex = 0; sampleIndex < predictions.length; sampleIndex++) {
      sum += Math.abs(predictions[sampleIndex] - targets[sampleIndex]);
    }
    return ok(sum / predictions.length);
  }

  /**
   * Huber loss (smooth L1).
   *
   * @param {number[]} predictions - Predicted values
   * @param {number[]} targets - True values
   * @param {number} [delta=1.0] - Threshold for quadratic vs linear
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static huberLoss(predictions, targets, delta = 1.0) {
    if (predictions.length !== targets.length || predictions.length === 0) {
      return err('Predictions and targets must have same non-zero length');
    }

    let total = 0;
    for (let sampleIndex = 0; sampleIndex < predictions.length; sampleIndex++) {
      const diff = Math.abs(predictions[sampleIndex] - targets[sampleIndex]);
      if (diff <= delta) {
        total += 0.5 * diff * diff;
      } else {
        total += delta * (diff - 0.5 * delta);
      }
    }
    return ok(total / predictions.length);
  }

  /**
   * L2 (Ridge) regularization term.
   *
   * @param {number[]} weights - Model weights
   * @param {number} [lambda=0.01] - Regularization strength
   * @returns {number}
   */
  static l2Regularization(weights, lambda = 0.01) {
    const sumSquared = weights.reduce((sum, w) => sum + w * w, 0);
    return 0.5 * lambda * sumSquared;
  }

  /**
   * L1 (Lasso) regularization term.
   *
   * @param {number[]} weights - Model weights
   * @param {number} [lambda=0.01] - Regularization strength
   * @returns {number}
   */
  static l1Regularization(weights, lambda = 0.01) {
    const sumAbs = weights.reduce((sum, w) => sum + Math.abs(w), 0);
    return lambda * sumAbs;
  }

  /**
   * Classification accuracy.
   *
   * @param {number[]} predictions - Predicted class indices
   * @param {number[]} targets - True class indices
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static accuracy(predictions, targets) {
    if (predictions.length !== targets.length || predictions.length === 0) {
      return err('Predictions and targets must have same non-zero length');
    }

    let correct = 0;
    for (let sampleIndex = 0; sampleIndex < predictions.length; sampleIndex++) {
      if (predictions[sampleIndex] === targets[sampleIndex]) {
        correct++;
      }
    }
    return ok(correct / predictions.length);
  }

  /**
   * Index of maximum value.
   *
   * @param {number[]} values - List of values
   * @returns {number} Index of max, or -1 if empty
   */
  static argmax(values) {
    if (values.length === 0) {
      return -1;
    }
    let maxIndex = 0;
    let maxVal = values[0];
    for (let valueIndex = 1; valueIndex < values.length; valueIndex++) {
      if (values[valueIndex] > maxVal) {
        maxVal = values[valueIndex];
        maxIndex = valueIndex;
      }
    }
    return maxIndex;
  }

  /**
   * Batch normalization (normalize to mean=0, std=1).
   *
   * @param {number[]} values - Input values
   * @param {number} [epsilon=1e-5] - Small constant for numerical stability
   * @returns {number[]}
   */
  static batchNorm(values, epsilon = 1e-5) {
    if (values.length < 2) {
      return [...values];
    }

    const mean = values.reduce((sum, x) => sum + x, 0) / values.length;
    const variance = values.reduce((sum, x) => sum + (x - mean) * (x - mean), 0) / values.length;

    return values.map((x) => (x - mean) / Math.sqrt(variance + epsilon));
  }

  /**
   * Gradient clipping by norm.
   *
   * @param {number[]} gradients - Gradient values
   * @param {number} maxNorm - Maximum L2 norm
   * @returns {number[]}
   */
  static clipGradients(gradients, maxNorm) {
    if (gradients.length === 0) {
      return [];
    }

    const norm = Math.sqrt(gradients.reduce((sum, g) => sum + g * g, 0));
    if (norm <= maxNorm) {
      return [...gradients];
    }

    const scale = maxNorm / norm;
    return gradients.map((g) => g * scale);
  }
}
