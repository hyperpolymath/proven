// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeTensor - Basic 2D tensor (matrix) operations.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * 2D tensor (matrix) backed by the libproven FFI.
 */
export class Matrix {
  /** @type {Deno.PointerObject|null} */
  #ptr = null;
  /** @type {number} */
  #rows;
  /** @type {number} */
  #cols;

  /**
   * @param {Deno.PointerObject} ptr - Native pointer.
   * @param {number} rows - Row count.
   * @param {number} cols - Column count.
   */
  constructor(ptr, rows, cols) {
    this.#ptr = ptr;
    this.#rows = rows;
    this.#cols = cols;
  }

  /** @returns {number} */
  get rows() { return this.#rows; }

  /** @returns {number} */
  get cols() { return this.#cols; }

  /**
   * Create a 2D tensor.
   *
   * @param {number} rows - Number of rows.
   * @param {number} cols - Number of columns.
   * @returns {{ ok: true, value: Matrix } | { ok: false, error: string }}
   */
  static create(rows, cols) {
    const symbols = getLib();
    const ptr = symbols.proven_tensor_create(rows, cols);
    if (ptr === null) return err('Failed to create tensor');
    return ok(new Matrix(ptr, rows, cols));
  }

  /**
   * Set a value in the tensor.
   *
   * @param {number} row - Row index.
   * @param {number} col - Column index.
   * @param {number} value - The value to set.
   * @returns {{ ok: true, value: undefined } | { ok: false, error: string }}
   */
  set(row, col, value) {
    if (this.#ptr === null) return err('Tensor is closed');
    const symbols = getLib();
    const status = symbols.proven_tensor_set(this.#ptr, row, col, value);
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    return ok(undefined);
  }

  /**
   * Get a value from the tensor.
   *
   * @param {number} row - Row index.
   * @param {number} col - Column index.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  get(row, col) {
    if (this.#ptr === null) return err('Tensor is closed');
    const symbols = getLib();
    const result = symbols.proven_tensor_get(this.#ptr, row, col);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Matrix multiplication (this * other).
   *
   * @param {Matrix} other - The other matrix.
   * @returns {{ ok: true, value: Matrix } | { ok: false, error: string }}
   */
  matmul(other) {
    if (this.#ptr === null || other.#ptr === null) return err('Tensor is closed');
    if (this.#cols !== other.#rows) return err('Incompatible dimensions for matrix multiplication');
    const symbols = getLib();
    const resultPtr = symbols.proven_tensor_matmul(this.#ptr, other.#ptr);
    if (resultPtr === null) return err('Matrix multiplication failed');
    return ok(new Matrix(resultPtr, this.#rows, other.#cols));
  }

  /**
   * Get the native pointer (for FFI interop).
   *
   * @returns {Deno.PointerObject|null}
   */
  get nativePtr() { return this.#ptr; }

  /**
   * Close and free the native tensor.
   */
  close() {
    if (this.#ptr !== null) {
      const symbols = getLib();
      symbols.proven_tensor_free(this.#ptr);
      this.#ptr = null;
    }
  }
}

/**
 * Vector is a 1-column matrix.
 */
export class Vector extends Matrix {
  /**
   * Create a vector.
   *
   * @param {number} length - Number of elements.
   * @returns {{ ok: true, value: Vector } | { ok: false, error: string }}
   */
  static create(length) {
    return Matrix.create(length, 1);
  }
}
