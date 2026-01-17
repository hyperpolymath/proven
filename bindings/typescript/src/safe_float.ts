// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * SafeFloat wraps a number ensuring it's never NaN or Infinity.
 */
export class SafeFloat {
  private readonly value: number;

  private constructor(value: number) {
    this.value = value;
  }

  /**
   * Create a SafeFloat, returning error if NaN or Infinity.
   */
  static create(value: number): Result<SafeFloat> {
    if (!Number.isFinite(value)) {
      return { ok: false, error: 'Value must be finite' };
    }
    return { ok: true, value: new SafeFloat(value) };
  }

  /**
   * Create a SafeFloat, clamping to bounds if needed.
   */
  static clamp(value: number, min: number = Number.MIN_SAFE_INTEGER, max: number = Number.MAX_SAFE_INTEGER): SafeFloat {
    if (Number.isNaN(value)) {
      return new SafeFloat(0);
    }
    if (value === Infinity) {
      return new SafeFloat(max);
    }
    if (value === -Infinity) {
      return new SafeFloat(min);
    }
    return new SafeFloat(Math.max(min, Math.min(max, value)));
  }

  /**
   * Create a SafeFloat from zero.
   */
  static zero(): SafeFloat {
    return new SafeFloat(0);
  }

  /**
   * Create a SafeFloat from one.
   */
  static one(): SafeFloat {
    return new SafeFloat(1);
  }

  /**
   * Get the underlying value.
   */
  unwrap(): number {
    return this.value;
  }

  /**
   * Add another SafeFloat.
   */
  add(other: SafeFloat): Result<SafeFloat> {
    return SafeFloat.create(this.value + other.value);
  }

  /**
   * Subtract another SafeFloat.
   */
  sub(other: SafeFloat): Result<SafeFloat> {
    return SafeFloat.create(this.value - other.value);
  }

  /**
   * Multiply by another SafeFloat.
   */
  mul(other: SafeFloat): Result<SafeFloat> {
    return SafeFloat.create(this.value * other.value);
  }

  /**
   * Divide by another SafeFloat.
   */
  div(other: SafeFloat): Result<SafeFloat> {
    if (other.value === 0) {
      return { ok: false, error: 'Division by zero' };
    }
    return SafeFloat.create(this.value / other.value);
  }

  /**
   * Modulo operation.
   */
  mod(other: SafeFloat): Result<SafeFloat> {
    if (other.value === 0) {
      return { ok: false, error: 'Division by zero' };
    }
    return SafeFloat.create(this.value % other.value);
  }

  /**
   * Power operation.
   */
  pow(exponent: SafeFloat): Result<SafeFloat> {
    return SafeFloat.create(Math.pow(this.value, exponent.value));
  }

  /**
   * Square root.
   */
  sqrt(): Result<SafeFloat> {
    if (this.value < 0) {
      return { ok: false, error: 'Cannot take square root of negative number' };
    }
    return SafeFloat.create(Math.sqrt(this.value));
  }

  /**
   * Absolute value.
   */
  abs(): SafeFloat {
    return new SafeFloat(Math.abs(this.value));
  }

  /**
   * Negate.
   */
  neg(): SafeFloat {
    return new SafeFloat(-this.value);
  }

  /**
   * Floor.
   */
  floor(): SafeFloat {
    return new SafeFloat(Math.floor(this.value));
  }

  /**
   * Ceiling.
   */
  ceil(): SafeFloat {
    return new SafeFloat(Math.ceil(this.value));
  }

  /**
   * Round.
   */
  round(): SafeFloat {
    return new SafeFloat(Math.round(this.value));
  }

  /**
   * Truncate (round towards zero).
   */
  trunc(): SafeFloat {
    return new SafeFloat(Math.trunc(this.value));
  }

  /**
   * Natural logarithm.
   */
  ln(): Result<SafeFloat> {
    if (this.value <= 0) {
      return { ok: false, error: 'Logarithm of non-positive number' };
    }
    return SafeFloat.create(Math.log(this.value));
  }

  /**
   * Base-10 logarithm.
   */
  log10(): Result<SafeFloat> {
    if (this.value <= 0) {
      return { ok: false, error: 'Logarithm of non-positive number' };
    }
    return SafeFloat.create(Math.log10(this.value));
  }

  /**
   * Exponential (e^x).
   */
  exp(): Result<SafeFloat> {
    return SafeFloat.create(Math.exp(this.value));
  }

  /**
   * Sine.
   */
  sin(): SafeFloat {
    return new SafeFloat(Math.sin(this.value));
  }

  /**
   * Cosine.
   */
  cos(): SafeFloat {
    return new SafeFloat(Math.cos(this.value));
  }

  /**
   * Tangent.
   */
  tan(): Result<SafeFloat> {
    return SafeFloat.create(Math.tan(this.value));
  }

  /**
   * Check if approximately equal within epsilon.
   */
  approxEquals(other: SafeFloat, epsilon: number = 1e-10): boolean {
    return Math.abs(this.value - other.value) < epsilon;
  }

  /**
   * Compare with another SafeFloat.
   */
  compareTo(other: SafeFloat): number {
    if (this.value < other.value) return -1;
    if (this.value > other.value) return 1;
    return 0;
  }

  /**
   * Check if zero.
   */
  isZero(): boolean {
    return this.value === 0;
  }

  /**
   * Check if positive.
   */
  isPositive(): boolean {
    return this.value > 0;
  }

  /**
   * Check if negative.
   */
  isNegative(): boolean {
    return this.value < 0;
  }

  /**
   * Check if integer.
   */
  isInteger(): boolean {
    return Number.isInteger(this.value);
  }

  /**
   * Format with fixed decimal places.
   */
  toFixed(digits: number): string {
    return this.value.toFixed(digits);
  }

  /**
   * Format with precision.
   */
  toPrecision(precision: number): string {
    return this.value.toPrecision(precision);
  }

  toString(): string {
    return this.value.toString();
  }
}

/**
 * Minimum of SafeFloat values.
 */
export function min(...values: SafeFloat[]): SafeFloat | undefined {
  if (values.length === 0) return undefined;
  return values.reduce((a, b) => (a.unwrap() < b.unwrap() ? a : b));
}

/**
 * Maximum of SafeFloat values.
 */
export function max(...values: SafeFloat[]): SafeFloat | undefined {
  if (values.length === 0) return undefined;
  return values.reduce((a, b) => (a.unwrap() > b.unwrap() ? a : b));
}

/**
 * Sum of SafeFloat values.
 */
export function sum(values: SafeFloat[]): Result<SafeFloat> {
  let total = SafeFloat.zero();
  for (const v of values) {
    const result = total.add(v);
    if (!result.ok) return result;
    total = result.value!;
  }
  return { ok: true, value: total };
}

/**
 * Average of SafeFloat values.
 */
export function average(values: SafeFloat[]): Result<SafeFloat> {
  if (values.length === 0) {
    return { ok: false, error: 'Cannot average empty list' };
  }
  const sumResult = sum(values);
  if (!sumResult.ok) return sumResult;
  const countResult = SafeFloat.create(values.length);
  if (!countResult.ok) return countResult;
  return sumResult.value!.div(countResult.value!);
}

export { SafeFloat as default };
