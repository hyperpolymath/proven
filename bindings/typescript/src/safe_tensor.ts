// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * Vector represents a 1D array with bounds checking.
 */
export class Vector {
  private readonly data: Float64Array;

  private constructor(data: Float64Array) {
    this.data = data;
  }

  /**
   * Create a vector from an array.
   */
  static from(values: number[]): Result<Vector> {
    for (const v of values) {
      if (!Number.isFinite(v)) {
        return { ok: false, error: 'Vector values must be finite' };
      }
    }
    return { ok: true, value: new Vector(Float64Array.from(values)) };
  }

  /**
   * Create a zero vector.
   */
  static zeros(length: number): Vector {
    return new Vector(new Float64Array(length));
  }

  /**
   * Create a vector of ones.
   */
  static ones(length: number): Vector {
    const data = new Float64Array(length);
    data.fill(1);
    return new Vector(data);
  }

  /**
   * Create a vector filled with a value.
   */
  static fill(length: number, value: number): Result<Vector> {
    if (!Number.isFinite(value)) {
      return { ok: false, error: 'Fill value must be finite' };
    }
    const data = new Float64Array(length);
    data.fill(value);
    return { ok: true, value: new Vector(data) };
  }

  /**
   * Get vector length.
   */
  get length(): number {
    return this.data.length;
  }

  /**
   * Get value at index with bounds checking.
   */
  get(index: number): Result<number> {
    if (index < 0 || index >= this.data.length) {
      return { ok: false, error: `Index ${index} out of bounds [0, ${this.data.length})` };
    }
    return { ok: true, value: this.data[index] };
  }

  /**
   * Set value at index with bounds checking.
   */
  set(index: number, value: number): Result<void> {
    if (index < 0 || index >= this.data.length) {
      return { ok: false, error: `Index ${index} out of bounds [0, ${this.data.length})` };
    }
    if (!Number.isFinite(value)) {
      return { ok: false, error: 'Value must be finite' };
    }
    this.data[index] = value;
    return { ok: true };
  }

  /**
   * Convert to array.
   */
  toArray(): number[] {
    return Array.from(this.data);
  }

  /**
   * Add vectors element-wise.
   */
  add(other: Vector): Result<Vector> {
    if (this.data.length !== other.data.length) {
      return { ok: false, error: 'Vector dimensions must match' };
    }
    const result = new Float64Array(this.data.length);
    for (let i = 0; i < this.data.length; i++) {
      result[i] = this.data[i] + other.data[i];
    }
    return { ok: true, value: new Vector(result) };
  }

  /**
   * Subtract vectors element-wise.
   */
  sub(other: Vector): Result<Vector> {
    if (this.data.length !== other.data.length) {
      return { ok: false, error: 'Vector dimensions must match' };
    }
    const result = new Float64Array(this.data.length);
    for (let i = 0; i < this.data.length; i++) {
      result[i] = this.data[i] - other.data[i];
    }
    return { ok: true, value: new Vector(result) };
  }

  /**
   * Multiply by scalar.
   */
  scale(scalar: number): Result<Vector> {
    if (!Number.isFinite(scalar)) {
      return { ok: false, error: 'Scalar must be finite' };
    }
    const result = new Float64Array(this.data.length);
    for (let i = 0; i < this.data.length; i++) {
      result[i] = this.data[i] * scalar;
    }
    return { ok: true, value: new Vector(result) };
  }

  /**
   * Element-wise multiplication.
   */
  mul(other: Vector): Result<Vector> {
    if (this.data.length !== other.data.length) {
      return { ok: false, error: 'Vector dimensions must match' };
    }
    const result = new Float64Array(this.data.length);
    for (let i = 0; i < this.data.length; i++) {
      result[i] = this.data[i] * other.data[i];
    }
    return { ok: true, value: new Vector(result) };
  }

  /**
   * Dot product.
   */
  dot(other: Vector): Result<number> {
    if (this.data.length !== other.data.length) {
      return { ok: false, error: 'Vector dimensions must match' };
    }
    let sum = 0;
    for (let i = 0; i < this.data.length; i++) {
      sum += this.data[i] * other.data[i];
    }
    return { ok: true, value: sum };
  }

  /**
   * Euclidean norm (L2).
   */
  norm(): number {
    let sum = 0;
    for (let i = 0; i < this.data.length; i++) {
      sum += this.data[i] * this.data[i];
    }
    return Math.sqrt(sum);
  }

  /**
   * L1 norm (Manhattan distance).
   */
  normL1(): number {
    let sum = 0;
    for (let i = 0; i < this.data.length; i++) {
      sum += Math.abs(this.data[i]);
    }
    return sum;
  }

  /**
   * Normalize to unit vector.
   */
  normalize(): Result<Vector> {
    const n = this.norm();
    if (n === 0) {
      return { ok: false, error: 'Cannot normalize zero vector' };
    }
    return this.scale(1 / n);
  }

  /**
   * Sum of elements.
   */
  sum(): number {
    return this.data.reduce((a, b) => a + b, 0);
  }

  /**
   * Mean of elements.
   */
  mean(): number {
    return this.sum() / this.data.length;
  }

  /**
   * Maximum element.
   */
  max(): number {
    return Math.max(...this.data);
  }

  /**
   * Minimum element.
   */
  min(): number {
    return Math.min(...this.data);
  }

  /**
   * Index of maximum element.
   */
  argmax(): number {
    let maxIdx = 0;
    for (let i = 1; i < this.data.length; i++) {
      if (this.data[i] > this.data[maxIdx]) {
        maxIdx = i;
      }
    }
    return maxIdx;
  }

  /**
   * Index of minimum element.
   */
  argmin(): number {
    let minIdx = 0;
    for (let i = 1; i < this.data.length; i++) {
      if (this.data[i] < this.data[minIdx]) {
        minIdx = i;
      }
    }
    return minIdx;
  }
}

/**
 * Matrix represents a 2D array with bounds checking.
 */
export class Matrix {
  private readonly data: Float64Array;
  readonly rows: number;
  readonly cols: number;

  private constructor(data: Float64Array, rows: number, cols: number) {
    this.data = data;
    this.rows = rows;
    this.cols = cols;
  }

  /**
   * Create a matrix from a 2D array.
   */
  static from(values: number[][]): Result<Matrix> {
    if (values.length === 0) {
      return { ok: false, error: 'Matrix cannot be empty' };
    }
    const rows = values.length;
    const cols = values[0].length;

    for (const row of values) {
      if (row.length !== cols) {
        return { ok: false, error: 'All rows must have same length' };
      }
      for (const v of row) {
        if (!Number.isFinite(v)) {
          return { ok: false, error: 'Matrix values must be finite' };
        }
      }
    }

    const data = new Float64Array(rows * cols);
    for (let i = 0; i < rows; i++) {
      for (let j = 0; j < cols; j++) {
        data[i * cols + j] = values[i][j];
      }
    }

    return { ok: true, value: new Matrix(data, rows, cols) };
  }

  /**
   * Create a zero matrix.
   */
  static zeros(rows: number, cols: number): Matrix {
    return new Matrix(new Float64Array(rows * cols), rows, cols);
  }

  /**
   * Create an identity matrix.
   */
  static identity(size: number): Matrix {
    const data = new Float64Array(size * size);
    for (let i = 0; i < size; i++) {
      data[i * size + i] = 1;
    }
    return new Matrix(data, size, size);
  }

  /**
   * Get value at position.
   */
  get(row: number, col: number): Result<number> {
    if (row < 0 || row >= this.rows || col < 0 || col >= this.cols) {
      return { ok: false, error: `Index (${row}, ${col}) out of bounds` };
    }
    return { ok: true, value: this.data[row * this.cols + col] };
  }

  /**
   * Set value at position.
   */
  set(row: number, col: number, value: number): Result<void> {
    if (row < 0 || row >= this.rows || col < 0 || col >= this.cols) {
      return { ok: false, error: `Index (${row}, ${col}) out of bounds` };
    }
    if (!Number.isFinite(value)) {
      return { ok: false, error: 'Value must be finite' };
    }
    this.data[row * this.cols + col] = value;
    return { ok: true };
  }

  /**
   * Get a row as a vector.
   */
  getRow(row: number): Result<Vector> {
    if (row < 0 || row >= this.rows) {
      return { ok: false, error: `Row ${row} out of bounds` };
    }
    const values: number[] = [];
    for (let j = 0; j < this.cols; j++) {
      values.push(this.data[row * this.cols + j]);
    }
    return Vector.from(values);
  }

  /**
   * Get a column as a vector.
   */
  getCol(col: number): Result<Vector> {
    if (col < 0 || col >= this.cols) {
      return { ok: false, error: `Column ${col} out of bounds` };
    }
    const values: number[] = [];
    for (let i = 0; i < this.rows; i++) {
      values.push(this.data[i * this.cols + col]);
    }
    return Vector.from(values);
  }

  /**
   * Convert to 2D array.
   */
  toArray(): number[][] {
    const result: number[][] = [];
    for (let i = 0; i < this.rows; i++) {
      const row: number[] = [];
      for (let j = 0; j < this.cols; j++) {
        row.push(this.data[i * this.cols + j]);
      }
      result.push(row);
    }
    return result;
  }

  /**
   * Add matrices.
   */
  add(other: Matrix): Result<Matrix> {
    if (this.rows !== other.rows || this.cols !== other.cols) {
      return { ok: false, error: 'Matrix dimensions must match' };
    }
    const result = new Float64Array(this.data.length);
    for (let i = 0; i < this.data.length; i++) {
      result[i] = this.data[i] + other.data[i];
    }
    return { ok: true, value: new Matrix(result, this.rows, this.cols) };
  }

  /**
   * Subtract matrices.
   */
  sub(other: Matrix): Result<Matrix> {
    if (this.rows !== other.rows || this.cols !== other.cols) {
      return { ok: false, error: 'Matrix dimensions must match' };
    }
    const result = new Float64Array(this.data.length);
    for (let i = 0; i < this.data.length; i++) {
      result[i] = this.data[i] - other.data[i];
    }
    return { ok: true, value: new Matrix(result, this.rows, this.cols) };
  }

  /**
   * Scale by scalar.
   */
  scale(scalar: number): Result<Matrix> {
    if (!Number.isFinite(scalar)) {
      return { ok: false, error: 'Scalar must be finite' };
    }
    const result = new Float64Array(this.data.length);
    for (let i = 0; i < this.data.length; i++) {
      result[i] = this.data[i] * scalar;
    }
    return { ok: true, value: new Matrix(result, this.rows, this.cols) };
  }

  /**
   * Matrix multiplication.
   */
  mul(other: Matrix): Result<Matrix> {
    if (this.cols !== other.rows) {
      return { ok: false, error: `Cannot multiply ${this.rows}x${this.cols} by ${other.rows}x${other.cols}` };
    }
    const result = new Float64Array(this.rows * other.cols);
    for (let i = 0; i < this.rows; i++) {
      for (let j = 0; j < other.cols; j++) {
        let sum = 0;
        for (let k = 0; k < this.cols; k++) {
          sum += this.data[i * this.cols + k] * other.data[k * other.cols + j];
        }
        result[i * other.cols + j] = sum;
      }
    }
    return { ok: true, value: new Matrix(result, this.rows, other.cols) };
  }

  /**
   * Transpose.
   */
  transpose(): Matrix {
    const result = new Float64Array(this.cols * this.rows);
    for (let i = 0; i < this.rows; i++) {
      for (let j = 0; j < this.cols; j++) {
        result[j * this.rows + i] = this.data[i * this.cols + j];
      }
    }
    return new Matrix(result, this.cols, this.rows);
  }

  /**
   * Matrix-vector multiplication.
   */
  mulVector(v: Vector): Result<Vector> {
    if (this.cols !== v.length) {
      return { ok: false, error: 'Matrix columns must match vector length' };
    }
    const result: number[] = [];
    for (let i = 0; i < this.rows; i++) {
      let sum = 0;
      for (let j = 0; j < this.cols; j++) {
        const vj = v.get(j);
        if (!vj.ok) return { ok: false, error: vj.error };
        sum += this.data[i * this.cols + j] * vj.value!;
      }
      result.push(sum);
    }
    return Vector.from(result);
  }
}

export const SafeTensor = {
  Vector,
  Matrix,
};
