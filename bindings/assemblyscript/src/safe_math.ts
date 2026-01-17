// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeMath - Overflow-safe arithmetic operations
 *
 * Provides checked arithmetic that detects overflow/underflow
 * and division by zero conditions.
 */

import { Result, ErrorCode } from "./common";

/**
 * SafeMath provides overflow-checked arithmetic operations.
 */
export class SafeMath {
  private value: i64;

  constructor(value: i64 = 0) {
    this.value = value;
  }

  /**
   * Get the current value.
   */
  get(): i64 {
    return this.value;
  }

  /**
   * Set the value.
   */
  set(value: i64): void {
    this.value = value;
  }

  /**
   * Safe addition with overflow detection.
   */
  static checkedAdd(a: i64, b: i64): Result<i64> {
    const result = a + b;
    const aSign = a >> 63;
    const bSign = b >> 63;
    const resultSign = result >> 63;

    if (aSign == bSign && aSign != resultSign) {
      return Result.err<i64>(ErrorCode.Overflow);
    }
    return Result.ok<i64>(result);
  }

  /**
   * Safe addition for i32.
   */
  static checkedAdd32(a: i32, b: i32): Result<i32> {
    const result = <i64>a + <i64>b;
    if (result < <i64>i32.MIN_VALUE || result > <i64>i32.MAX_VALUE) {
      return Result.err<i32>(ErrorCode.Overflow);
    }
    return Result.ok<i32>(<i32>result);
  }

  /**
   * Safe subtraction with overflow detection.
   */
  static checkedSub(a: i64, b: i64): Result<i64> {
    const result = a - b;
    const aSign = a >> 63;
    const bSign = b >> 63;
    const resultSign = result >> 63;

    if (aSign != bSign && aSign != resultSign) {
      return Result.err<i64>(ErrorCode.Overflow);
    }
    return Result.ok<i64>(result);
  }

  /**
   * Safe subtraction for i32.
   */
  static checkedSub32(a: i32, b: i32): Result<i32> {
    const result = <i64>a - <i64>b;
    if (result < <i64>i32.MIN_VALUE || result > <i64>i32.MAX_VALUE) {
      return Result.err<i32>(ErrorCode.Overflow);
    }
    return Result.ok<i32>(<i32>result);
  }

  /**
   * Safe multiplication with overflow detection.
   */
  static checkedMul(a: i64, b: i64): Result<i64> {
    if (a == 0 || b == 0) {
      return Result.ok<i64>(0);
    }
    if (a == i64.MIN_VALUE && b == -1) {
      return Result.err<i64>(ErrorCode.Overflow);
    }
    if (b == i64.MIN_VALUE && a == -1) {
      return Result.err<i64>(ErrorCode.Overflow);
    }
    const result = a * b;
    if (result / a != b) {
      return Result.err<i64>(ErrorCode.Overflow);
    }
    return Result.ok<i64>(result);
  }

  /**
   * Safe multiplication for i32.
   */
  static checkedMul32(a: i32, b: i32): Result<i32> {
    const result = <i64>a * <i64>b;
    if (result < <i64>i32.MIN_VALUE || result > <i64>i32.MAX_VALUE) {
      return Result.err<i32>(ErrorCode.Overflow);
    }
    return Result.ok<i32>(<i32>result);
  }

  /**
   * Safe division (no divide by zero).
   */
  static checkedDiv(a: i64, b: i64): Result<i64> {
    if (b == 0) {
      return Result.err<i64>(ErrorCode.DivisionByZero);
    }
    if (a == i64.MIN_VALUE && b == -1) {
      return Result.err<i64>(ErrorCode.Overflow);
    }
    return Result.ok<i64>(a / b);
  }

  /**
   * Safe division for i32.
   */
  static checkedDiv32(a: i32, b: i32): Result<i32> {
    if (b == 0) {
      return Result.err<i32>(ErrorCode.DivisionByZero);
    }
    if (a == i32.MIN_VALUE && b == -1) {
      return Result.err<i32>(ErrorCode.Overflow);
    }
    return Result.ok<i32>(a / b);
  }

  /**
   * Safe modulo (no divide by zero).
   */
  static checkedMod(a: i64, b: i64): Result<i64> {
    if (b == 0) {
      return Result.err<i64>(ErrorCode.DivisionByZero);
    }
    return Result.ok<i64>(a % b);
  }

  /**
   * Safe negation with overflow detection.
   */
  static checkedNeg(a: i64): Result<i64> {
    if (a == i64.MIN_VALUE) {
      return Result.err<i64>(ErrorCode.Overflow);
    }
    return Result.ok<i64>(-a);
  }

  /**
   * Safe absolute value.
   */
  static checkedAbs(a: i64): Result<i64> {
    if (a == i64.MIN_VALUE) {
      return Result.err<i64>(ErrorCode.Overflow);
    }
    return Result.ok<i64>(a < 0 ? -a : a);
  }

  /**
   * Safe power operation.
   */
  static checkedPow(base: i64, exp: u32): Result<i64> {
    if (exp == 0) {
      return Result.ok<i64>(1);
    }
    if (base == 0) {
      return Result.ok<i64>(0);
    }
    if (base == 1) {
      return Result.ok<i64>(1);
    }
    if (base == -1) {
      return Result.ok<i64>((exp & 1) == 0 ? 1 : -1);
    }

    let result: i64 = 1;
    let b = base;
    let e = exp;

    while (e > 0) {
      if ((e & 1) == 1) {
        const mulResult = SafeMath.checkedMul(result, b);
        if (mulResult.isErr()) {
          return mulResult;
        }
        result = mulResult.unwrap();
      }
      e >>= 1;
      if (e > 0) {
        const sqResult = SafeMath.checkedMul(b, b);
        if (sqResult.isErr()) {
          return sqResult;
        }
        b = sqResult.unwrap();
      }
    }
    return Result.ok<i64>(result);
  }

  /**
   * Clamp value to range [min, max].
   */
  static clamp(value: i64, minVal: i64, maxVal: i64): i64 {
    if (value < minVal) return minVal;
    if (value > maxVal) return maxVal;
    return value;
  }

  /**
   * Clamp f64 to range [min, max].
   */
  static clampF64(value: f64, minVal: f64, maxVal: f64): f64 {
    if (value < minVal) return minVal;
    if (value > maxVal) return maxVal;
    return value;
  }

  /**
   * Check if value is in range [min, max].
   */
  static inRange(value: i64, minVal: i64, maxVal: i64): bool {
    return value >= minVal && value <= maxVal;
  }

  /**
   * Safe u64 addition.
   */
  static checkedAddU64(a: u64, b: u64): Result<u64> {
    const result = a + b;
    if (result < a) {
      return Result.err<u64>(ErrorCode.Overflow);
    }
    return Result.ok<u64>(result);
  }

  /**
   * Safe u64 subtraction.
   */
  static checkedSubU64(a: u64, b: u64): Result<u64> {
    if (b > a) {
      return Result.err<u64>(ErrorCode.Overflow);
    }
    return Result.ok<u64>(a - b);
  }

  /**
   * Safe u64 multiplication.
   */
  static checkedMulU64(a: u64, b: u64): Result<u64> {
    if (a == 0 || b == 0) {
      return Result.ok<u64>(0);
    }
    const result = a * b;
    if (result / a != b) {
      return Result.err<u64>(ErrorCode.Overflow);
    }
    return Result.ok<u64>(result);
  }

  // Instance methods for chaining
  add(other: i64): Result<SafeMath> {
    const result = SafeMath.checkedAdd(this.value, other);
    if (result.isErr()) {
      return Result.err<SafeMath>(result.error);
    }
    return Result.ok<SafeMath>(new SafeMath(result.unwrap()));
  }

  sub(other: i64): Result<SafeMath> {
    const result = SafeMath.checkedSub(this.value, other);
    if (result.isErr()) {
      return Result.err<SafeMath>(result.error);
    }
    return Result.ok<SafeMath>(new SafeMath(result.unwrap()));
  }

  mul(other: i64): Result<SafeMath> {
    const result = SafeMath.checkedMul(this.value, other);
    if (result.isErr()) {
      return Result.err<SafeMath>(result.error);
    }
    return Result.ok<SafeMath>(new SafeMath(result.unwrap()));
  }

  div(other: i64): Result<SafeMath> {
    const result = SafeMath.checkedDiv(this.value, other);
    if (result.isErr()) {
      return Result.err<SafeMath>(result.error);
    }
    return Result.ok<SafeMath>(new SafeMath(result.unwrap()));
  }
}
