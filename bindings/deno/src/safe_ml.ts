// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeML - Machine learning activation functions and utilities.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Safe ML operations backed by formally verified Idris 2 code.
 */
export class SafeML {
  /**
   * Sigmoid activation function: 1 / (1 + exp(-x)).
   *
   * @param x - Input value.
   * @returns Sigmoid output.
   */
  static sigmoid(x: number): number {
    const symbols = getLib();
    return symbols.proven_ml_sigmoid(x);
  }

  /**
   * ReLU activation function: max(0, x).
   *
   * @param x - Input value.
   * @returns ReLU output.
   */
  static relu(x: number): number {
    const symbols = getLib();
    return symbols.proven_ml_relu(x);
  }

  /**
   * Leaky ReLU activation function: x > 0 ? x : alpha * x.
   *
   * @param x - Input value.
   * @param alpha - Leak coefficient (default 0.01).
   * @returns Leaky ReLU output.
   */
  static leakyRelu(x: number, alpha = 0.01): number {
    const symbols = getLib();
    return symbols.proven_ml_leaky_relu(x, alpha);
  }

  /**
   * Clamp value to range [min, max].
   *
   * @param x - Input value.
   * @param min - Minimum.
   * @param max - Maximum.
   * @returns Clamped value.
   */
  static clamp(x: number, min: number, max: number): number {
    const symbols = getLib();
    return symbols.proven_ml_clamp(x, min, max);
  }

  /**
   * Softmax normalization.
   *
   * @param input - Input values.
   * @returns Result containing normalized output or an error string.
   */
  static softmax(input: Float64Array): Result<Float64Array> {
    const symbols = getLib();
    const output = new Float64Array(input.length);
    const status = symbols.proven_ml_softmax(input, output, input.length);
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    return ok(output);
  }
}
