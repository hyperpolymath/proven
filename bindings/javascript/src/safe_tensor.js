// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeTensor - Bounds-checked vector/matrix operations.
 *
 * Provides safe tensor operations with shape validation.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Shape error for incompatible operations.
 */
export class ShapeError extends Error {
  constructor(message) {
    super(message);
    this.name = 'ShapeError';
  }
}

/**
 * Bounds-checked vector operations.
 */
export class Vector {
  /** @type {Float64Array} */
  #data;

  /**
   * Create a vector.
   *
   * @param {number[] | Float64Array} data - Vector elements
   */
  constructor(data) {
    this.#data = data instanceof Float64Array ? new Float64Array(data) : Float64Array.from(data);
  }

  /**
   * Get vector size.
   *
   * @returns {number}
   */
  get size() {
    return this.#data.length;
  }

  /**
   * Get element at index (safe).
   *
   * @param {number} index - Element index
   * @returns {number | null}
   */
  get(index) {
    if (index < 0 || index >= this.#data.length) {
      return null;
    }
    return this.#data[index];
  }

  /**
   * Set element at index (safe).
   *
   * @param {number} index - Element index
   * @param {number} value - New value
   * @returns {boolean}
   */
  set(index, value) {
    if (index < 0 || index >= this.#data.length) {
      return false;
    }
    this.#data[index] = value;
    return true;
  }

  /**
   * Get data as array.
   *
   * @returns {number[]}
   */
  toArray() {
    return Array.from(this.#data);
  }

  /**
   * Dot product.
   *
   * @param {Vector} other - Other vector
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  dot(other) {
    if (this.#data.length !== other.#data.length) {
      return err('Vector sizes do not match');
    }
    let sum = 0;
    for (let elementIndex = 0; elementIndex < this.#data.length; elementIndex++) {
      sum += this.#data[elementIndex] * other.#data[elementIndex];
    }
    return ok(sum);
  }

  /**
   * Calculate magnitude (L2 norm).
   *
   * @returns {number}
   */
  magnitude() {
    let sum = 0;
    for (let elementIndex = 0; elementIndex < this.#data.length; elementIndex++) {
      sum += this.#data[elementIndex] * this.#data[elementIndex];
    }
    return Math.sqrt(sum);
  }

  /**
   * Normalize to unit vector.
   *
   * @returns {{ ok: true, value: Vector } | { ok: false, error: string }}
   */
  normalize() {
    const mag = this.magnitude();
    if (mag === 0) {
      return err('Cannot normalize zero vector');
    }
    const result = new Float64Array(this.#data.length);
    for (let elementIndex = 0; elementIndex < this.#data.length; elementIndex++) {
      result[elementIndex] = this.#data[elementIndex] / mag;
    }
    return ok(new Vector(result));
  }

  /**
   * Element-wise addition.
   *
   * @param {Vector} other - Other vector
   * @returns {{ ok: true, value: Vector } | { ok: false, error: string }}
   */
  add(other) {
    if (this.#data.length !== other.#data.length) {
      return err('Vector sizes do not match');
    }
    const result = new Float64Array(this.#data.length);
    for (let elementIndex = 0; elementIndex < this.#data.length; elementIndex++) {
      result[elementIndex] = this.#data[elementIndex] + other.#data[elementIndex];
    }
    return ok(new Vector(result));
  }

  /**
   * Element-wise subtraction.
   *
   * @param {Vector} other - Other vector
   * @returns {{ ok: true, value: Vector } | { ok: false, error: string }}
   */
  sub(other) {
    if (this.#data.length !== other.#data.length) {
      return err('Vector sizes do not match');
    }
    const result = new Float64Array(this.#data.length);
    for (let elementIndex = 0; elementIndex < this.#data.length; elementIndex++) {
      result[elementIndex] = this.#data[elementIndex] - other.#data[elementIndex];
    }
    return ok(new Vector(result));
  }

  /**
   * Scalar multiplication.
   *
   * @param {number} scalar - Scalar value
   * @returns {Vector}
   */
  mul(scalar) {
    const result = new Float64Array(this.#data.length);
    for (let elementIndex = 0; elementIndex < this.#data.length; elementIndex++) {
      result[elementIndex] = this.#data[elementIndex] * scalar;
    }
    return new Vector(result);
  }

  /**
   * Create zero vector.
   *
   * @param {number} size - Vector size
   * @returns {Vector}
   */
  static zeros(size) {
    return new Vector(new Float64Array(size));
  }

  /**
   * Create ones vector.
   *
   * @param {number} size - Vector size
   * @returns {Vector}
   */
  static ones(size) {
    return new Vector(new Float64Array(size).fill(1));
  }
}

/**
 * Bounds-checked matrix operations.
 */
export class Matrix {
  /** @type {Float64Array} */
  #data;
  /** @type {number} */
  #rows;
  /** @type {number} */
  #cols;

  /**
   * Create a matrix.
   *
   * @param {number[][]} data - 2D array (row-major)
   */
  constructor(data) {
    if (!data || data.length === 0 || !data[0]) {
      this.#data = new Float64Array(0);
      this.#rows = 0;
      this.#cols = 0;
      return;
    }

    this.#rows = data.length;
    this.#cols = data[0].length;

    // Verify all rows have same length
    for (const row of data) {
      if (row.length !== this.#cols) {
        throw new Error('All rows must have same length');
      }
    }

    this.#data = new Float64Array(this.#rows * this.#cols);
    for (let rowIndex = 0; rowIndex < this.#rows; rowIndex++) {
      for (let colIndex = 0; colIndex < this.#cols; colIndex++) {
        this.#data[rowIndex * this.#cols + colIndex] = data[rowIndex][colIndex];
      }
    }
  }

  /**
   * Get matrix shape.
   *
   * @returns {[number, number]}
   */
  get shape() {
    return [this.#rows, this.#cols];
  }

  /**
   * Get number of rows.
   *
   * @returns {number}
   */
  get rows() {
    return this.#rows;
  }

  /**
   * Get number of columns.
   *
   * @returns {number}
   */
  get cols() {
    return this.#cols;
  }

  /**
   * Get element at (row, col).
   *
   * @param {number} row - Row index
   * @param {number} col - Column index
   * @returns {number | null}
   */
  get(row, col) {
    if (row < 0 || row >= this.#rows || col < 0 || col >= this.#cols) {
      return null;
    }
    return this.#data[row * this.#cols + col];
  }

  /**
   * Set element at (row, col).
   *
   * @param {number} row - Row index
   * @param {number} col - Column index
   * @param {number} value - New value
   * @returns {boolean}
   */
  set(row, col, value) {
    if (row < 0 || row >= this.#rows || col < 0 || col >= this.#cols) {
      return false;
    }
    this.#data[row * this.#cols + col] = value;
    return true;
  }

  /**
   * Get row as vector.
   *
   * @param {number} row - Row index
   * @returns {Vector | null}
   */
  getRow(row) {
    if (row < 0 || row >= this.#rows) {
      return null;
    }
    const result = new Float64Array(this.#cols);
    for (let colIndex = 0; colIndex < this.#cols; colIndex++) {
      result[colIndex] = this.#data[row * this.#cols + colIndex];
    }
    return new Vector(result);
  }

  /**
   * Get column as vector.
   *
   * @param {number} col - Column index
   * @returns {Vector | null}
   */
  getCol(col) {
    if (col < 0 || col >= this.#cols) {
      return null;
    }
    const result = new Float64Array(this.#rows);
    for (let rowIndex = 0; rowIndex < this.#rows; rowIndex++) {
      result[rowIndex] = this.#data[rowIndex * this.#cols + col];
    }
    return new Vector(result);
  }

  /**
   * Get data as 2D array.
   *
   * @returns {number[][]}
   */
  toArray() {
    const result = [];
    for (let rowIndex = 0; rowIndex < this.#rows; rowIndex++) {
      const row = [];
      for (let colIndex = 0; colIndex < this.#cols; colIndex++) {
        row.push(this.#data[rowIndex * this.#cols + colIndex]);
      }
      result.push(row);
    }
    return result;
  }

  /**
   * Transpose matrix.
   *
   * @returns {Matrix}
   */
  transpose() {
    const result = [];
    for (let colIndex = 0; colIndex < this.#cols; colIndex++) {
      const row = [];
      for (let rowIndex = 0; rowIndex < this.#rows; rowIndex++) {
        row.push(this.#data[rowIndex * this.#cols + colIndex]);
      }
      result.push(row);
    }
    return new Matrix(result);
  }

  /**
   * Matrix multiplication.
   *
   * @param {Matrix} other - Right-hand matrix
   * @returns {{ ok: true, value: Matrix } | { ok: false, error: string }}
   */
  matmul(other) {
    if (this.#cols !== other.#rows) {
      return err(`Shape mismatch: (${this.#rows},${this.#cols}) x (${other.#rows},${other.#cols})`);
    }

    const result = [];
    for (let rowIndex = 0; rowIndex < this.#rows; rowIndex++) {
      const row = [];
      for (let colIndex = 0; colIndex < other.#cols; colIndex++) {
        let sum = 0;
        for (let innerIndex = 0; innerIndex < this.#cols; innerIndex++) {
          sum += this.#data[rowIndex * this.#cols + innerIndex] * other.#data[innerIndex * other.#cols + colIndex];
        }
        row.push(sum);
      }
      result.push(row);
    }
    return ok(new Matrix(result));
  }

  /**
   * Matrix-vector multiplication.
   *
   * @param {Vector} v - Vector to multiply
   * @returns {{ ok: true, value: Vector } | { ok: false, error: string }}
   */
  mulVector(v) {
    if (this.#cols !== v.size) {
      return err(`Shape mismatch: matrix cols (${this.#cols}) != vector size (${v.size})`);
    }

    const result = new Float64Array(this.#rows);
    for (let rowIndex = 0; rowIndex < this.#rows; rowIndex++) {
      let sum = 0;
      for (let colIndex = 0; colIndex < this.#cols; colIndex++) {
        sum += this.#data[rowIndex * this.#cols + colIndex] * v.get(colIndex);
      }
      result[rowIndex] = sum;
    }
    return ok(new Vector(result));
  }

  /**
   * Create identity matrix.
   *
   * @param {number} n - Size
   * @returns {Matrix}
   */
  static identity(n) {
    const data = [];
    for (let rowIndex = 0; rowIndex < n; rowIndex++) {
      const row = [];
      for (let colIndex = 0; colIndex < n; colIndex++) {
        row.push(rowIndex === colIndex ? 1 : 0);
      }
      data.push(row);
    }
    return new Matrix(data);
  }

  /**
   * Create zero matrix.
   *
   * @param {number} rows - Number of rows
   * @param {number} cols - Number of columns
   * @returns {Matrix}
   */
  static zeros(rows, cols) {
    const data = [];
    for (let rowIndex = 0; rowIndex < rows; rowIndex++) {
      data.push(new Array(cols).fill(0));
    }
    return new Matrix(data);
  }
}

/**
 * Safe tensor utilities.
 */
export class SafeTensor {
  /**
   * Create a vector.
   *
   * @param {number[]} data - Vector data
   * @returns {Vector}
   */
  static vector(data) {
    return new Vector(data);
  }

  /**
   * Create a matrix.
   *
   * @param {number[][]} data - Matrix data
   * @returns {Matrix}
   */
  static matrix(data) {
    return new Matrix(data);
  }

  /**
   * Dot product of two vectors.
   *
   * @param {Vector} a - First vector
   * @param {Vector} b - Second vector
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static dot(a, b) {
    return a.dot(b);
  }

  /**
   * Matrix multiplication.
   *
   * @param {Matrix} a - First matrix
   * @param {Matrix} b - Second matrix
   * @returns {{ ok: true, value: Matrix } | { ok: false, error: string }}
   */
  static matmul(a, b) {
    return a.matmul(b);
  }
}
