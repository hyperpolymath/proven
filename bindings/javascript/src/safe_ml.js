// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeML - Machine learning activation functions and utilities.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Safe ML operations backed by formally verified Idris 2 code.
 */
export class SafeML {
  /**
   * Sigmoid activation function: 1 / (1 + exp(-x)).
   *
   * @param {number} x - Input value.
   * @returns {number}
   */
  static sigmoid(x) {
    const symbols = getLib();
    return symbols.proven_ml_sigmoid(x);
  }

  /**
   * ReLU activation function: max(0, x).
   *
   * @param {number} x - Input value.
   * @returns {number}
   */
  static relu(x) {
    const symbols = getLib();
    return symbols.proven_ml_relu(x);
  }

  /**
   * Leaky ReLU activation function: x > 0 ? x : alpha * x.
   *
   * @param {number} x - Input value.
   * @param {number} [alpha=0.01] - Leak coefficient.
   * @returns {number}
   */
  static leakyRelu(x, alpha = 0.01) {
    const symbols = getLib();
    return symbols.proven_ml_leaky_relu(x, alpha);
  }

  /**
   * Clamp value to range [min, max].
   *
   * @param {number} x - Input value.
   * @param {number} min - Minimum.
   * @param {number} max - Maximum.
   * @returns {number}
   */
  static clamp(x, min, max) {
    const symbols = getLib();
    return symbols.proven_ml_clamp(x, min, max);
  }

  /**
   * Softmax normalization.
   *
   * @param {Float64Array} input - Input values.
   * @returns {{ ok: true, value: Float64Array } | { ok: false, error: string }}
   */
  static softmax(input) {
    const symbols = getLib();
    const output = new Float64Array(input.length);
    const status = symbols.proven_ml_softmax(input, output, input.length);
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    return ok(output);
  }
}
