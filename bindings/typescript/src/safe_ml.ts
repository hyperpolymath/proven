// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * Softmax activation function.
 */
export function softmax(values: number[]): Result<number[]> {
  if (values.length === 0) {
    return { ok: false, error: 'Cannot compute softmax of empty array' };
  }

  // Find max for numerical stability
  const max = Math.max(...values);
  const exps = values.map((v) => Math.exp(v - max));
  const sum = exps.reduce((a, b) => a + b, 0);

  if (sum === 0 || !Number.isFinite(sum)) {
    return { ok: false, error: 'Softmax computation overflow' };
  }

  return { ok: true, value: exps.map((e) => e / sum) };
}

/**
 * Sigmoid activation function.
 */
export function sigmoid(x: number): number {
  if (x >= 0) {
    return 1 / (1 + Math.exp(-x));
  } else {
    const exp = Math.exp(x);
    return exp / (1 + exp);
  }
}

/**
 * Sigmoid derivative.
 */
export function sigmoidDerivative(x: number): number {
  const s = sigmoid(x);
  return s * (1 - s);
}

/**
 * ReLU activation function.
 */
export function relu(x: number): number {
  return Math.max(0, x);
}

/**
 * ReLU derivative.
 */
export function reluDerivative(x: number): number {
  return x > 0 ? 1 : 0;
}

/**
 * Leaky ReLU activation.
 */
export function leakyRelu(x: number, alpha: number = 0.01): number {
  return x >= 0 ? x : alpha * x;
}

/**
 * Leaky ReLU derivative.
 */
export function leakyReluDerivative(x: number, alpha: number = 0.01): number {
  return x >= 0 ? 1 : alpha;
}

/**
 * Tanh activation (hyperbolic tangent).
 */
export function tanh(x: number): number {
  return Math.tanh(x);
}

/**
 * Tanh derivative.
 */
export function tanhDerivative(x: number): number {
  const t = Math.tanh(x);
  return 1 - t * t;
}

/**
 * GELU activation (Gaussian Error Linear Unit).
 */
export function gelu(x: number): number {
  return 0.5 * x * (1 + Math.tanh(Math.sqrt(2 / Math.PI) * (x + 0.044715 * Math.pow(x, 3))));
}

/**
 * Swish activation (x * sigmoid(x)).
 */
export function swish(x: number): number {
  return x * sigmoid(x);
}

/**
 * ELU activation (Exponential Linear Unit).
 */
export function elu(x: number, alpha: number = 1.0): number {
  return x >= 0 ? x : alpha * (Math.exp(x) - 1);
}

/**
 * Cross-entropy loss for classification.
 */
export function crossEntropyLoss(predicted: number[], target: number[]): Result<number> {
  if (predicted.length !== target.length) {
    return { ok: false, error: 'Array lengths must match' };
  }
  if (predicted.length === 0) {
    return { ok: false, error: 'Arrays cannot be empty' };
  }

  let loss = 0;
  const epsilon = 1e-15;

  for (let i = 0; i < predicted.length; i++) {
    const p = Math.max(epsilon, Math.min(1 - epsilon, predicted[i]));
    const t = target[i];
    loss -= t * Math.log(p) + (1 - t) * Math.log(1 - p);
  }

  return { ok: true, value: loss / predicted.length };
}

/**
 * Mean squared error loss.
 */
export function mseLoss(predicted: number[], target: number[]): Result<number> {
  if (predicted.length !== target.length) {
    return { ok: false, error: 'Array lengths must match' };
  }
  if (predicted.length === 0) {
    return { ok: false, error: 'Arrays cannot be empty' };
  }

  let sum = 0;
  for (let i = 0; i < predicted.length; i++) {
    const diff = predicted[i] - target[i];
    sum += diff * diff;
  }

  return { ok: true, value: sum / predicted.length };
}

/**
 * Mean absolute error loss.
 */
export function maeLoss(predicted: number[], target: number[]): Result<number> {
  if (predicted.length !== target.length) {
    return { ok: false, error: 'Array lengths must match' };
  }
  if (predicted.length === 0) {
    return { ok: false, error: 'Arrays cannot be empty' };
  }

  let sum = 0;
  for (let i = 0; i < predicted.length; i++) {
    sum += Math.abs(predicted[i] - target[i]);
  }

  return { ok: true, value: sum / predicted.length };
}

/**
 * Huber loss (smooth L1).
 */
export function huberLoss(predicted: number[], target: number[], delta: number = 1.0): Result<number> {
  if (predicted.length !== target.length) {
    return { ok: false, error: 'Array lengths must match' };
  }
  if (predicted.length === 0) {
    return { ok: false, error: 'Arrays cannot be empty' };
  }

  let sum = 0;
  for (let i = 0; i < predicted.length; i++) {
    const error = Math.abs(predicted[i] - target[i]);
    if (error <= delta) {
      sum += 0.5 * error * error;
    } else {
      sum += delta * (error - 0.5 * delta);
    }
  }

  return { ok: true, value: sum / predicted.length };
}

/**
 * Accuracy metric for classification.
 */
export function accuracy(predicted: number[], target: number[]): Result<number> {
  if (predicted.length !== target.length) {
    return { ok: false, error: 'Array lengths must match' };
  }
  if (predicted.length === 0) {
    return { ok: false, error: 'Arrays cannot be empty' };
  }

  let correct = 0;
  for (let i = 0; i < predicted.length; i++) {
    if (Math.round(predicted[i]) === Math.round(target[i])) {
      correct++;
    }
  }

  return { ok: true, value: correct / predicted.length };
}

/**
 * Batch normalization.
 */
export function batchNorm(
  values: number[],
  gamma: number = 1,
  beta: number = 0,
  epsilon: number = 1e-5
): Result<number[]> {
  if (values.length === 0) {
    return { ok: false, error: 'Cannot normalize empty array' };
  }

  const mean = values.reduce((a, b) => a + b, 0) / values.length;
  const variance = values.reduce((sum, v) => sum + (v - mean) ** 2, 0) / values.length;
  const std = Math.sqrt(variance + epsilon);

  const normalized = values.map((v) => gamma * ((v - mean) / std) + beta);

  return { ok: true, value: normalized };
}

/**
 * Layer normalization.
 */
export function layerNorm(
  values: number[],
  gamma: number = 1,
  beta: number = 0,
  epsilon: number = 1e-5
): Result<number[]> {
  return batchNorm(values, gamma, beta, epsilon);
}

/**
 * Dropout (training mode).
 */
export function dropout(values: number[], rate: number = 0.5): number[] {
  if (rate <= 0) return values;
  if (rate >= 1) return new Array(values.length).fill(0);

  const scale = 1 / (1 - rate);
  return values.map((v) => (Math.random() > rate ? v * scale : 0));
}

/**
 * L2 regularization penalty.
 */
export function l2Regularization(weights: number[], lambda: number = 0.01): number {
  return lambda * weights.reduce((sum, w) => sum + w * w, 0);
}

/**
 * L1 regularization penalty.
 */
export function l1Regularization(weights: number[], lambda: number = 0.01): number {
  return lambda * weights.reduce((sum, w) => sum + Math.abs(w), 0);
}

/**
 * Clip gradients to prevent exploding gradients.
 */
export function clipGradients(gradients: number[], maxNorm: number): number[] {
  const norm = Math.sqrt(gradients.reduce((sum, g) => sum + g * g, 0));
  if (norm <= maxNorm) return gradients;
  const scale = maxNorm / norm;
  return gradients.map((g) => g * scale);
}

export const SafeML = {
  // Activations
  softmax,
  sigmoid,
  sigmoidDerivative,
  relu,
  reluDerivative,
  leakyRelu,
  leakyReluDerivative,
  tanh,
  tanhDerivative,
  gelu,
  swish,
  elu,
  // Loss functions
  crossEntropyLoss,
  mseLoss,
  maeLoss,
  huberLoss,
  // Metrics
  accuracy,
  // Normalization
  batchNorm,
  layerNorm,
  dropout,
  // Regularization
  l1Regularization,
  l2Regularization,
  clipGradients,
};
