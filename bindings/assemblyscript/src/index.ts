// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Proven Safety Primitives for AssemblyScript
 *
 * Type-safe operations that compile to efficient WebAssembly.
 * AssemblyScript's TypeScript-like syntax with strict typing
 * makes it ideal for expressing Proven's safety guarantees.
 */

// ============================================================================
// RESULT TYPE
// ============================================================================

@unmanaged
export class Result<T, E> {
  constructor(
    public readonly ok: bool,
    public readonly value: T,
    public readonly error: E
  ) {}

  static Ok<T, E>(value: T): Result<T, E> {
    // @ts-ignore: null for error in Ok case
    return new Result<T, E>(true, value, changetype<E>(0));
  }

  static Err<T, E>(error: E): Result<T, E> {
    // @ts-ignore: null for value in Err case
    return new Result<T, E>(false, changetype<T>(0), error);
  }

  isOk(): bool {
    return this.ok;
  }

  isErr(): bool {
    return !this.ok;
  }

  unwrap(): T {
    assert(this.ok, "Called unwrap on Err");
    return this.value;
  }

  unwrapOr(defaultValue: T): T {
    return this.ok ? this.value : defaultValue;
  }
}

// ============================================================================
// ERROR CODES
// ============================================================================

export const enum ErrorCode {
  None = 0,
  Overflow = 1,
  DivisionByZero = 2,
  OutOfBounds = 3,
  InvalidInput = 4,
  ParseError = 5,
}

// ============================================================================
// SAFE MATH
// ============================================================================

export namespace SafeMath {
  /**
   * Safe addition with overflow detection.
   */
  export function add(a: i64, b: i64): Result<i64, ErrorCode> {
    const result = a + b;

    // Overflow if: same signs for a,b but different sign for result
    const aSign = a >> 63;
    const bSign = b >> 63;
    const resultSign = result >> 63;

    if (aSign == bSign && aSign != resultSign) {
      return Result.Err<i64, ErrorCode>(ErrorCode.Overflow);
    }

    return Result.Ok<i64, ErrorCode>(result);
  }

  /**
   * Safe addition for i32.
   */
  export function add32(a: i32, b: i32): Result<i32, ErrorCode> {
    const result = <i64>a + <i64>b;

    if (result < <i64>i32.MIN_VALUE || result > <i64>i32.MAX_VALUE) {
      return Result.Err<i32, ErrorCode>(ErrorCode.Overflow);
    }

    return Result.Ok<i32, ErrorCode>(<i32>result);
  }

  /**
   * Safe subtraction with overflow detection.
   */
  export function sub(a: i64, b: i64): Result<i64, ErrorCode> {
    const result = a - b;

    // Overflow if: different signs for a,b and result sign differs from a
    const aSign = a >> 63;
    const bSign = b >> 63;
    const resultSign = result >> 63;

    if (aSign != bSign && aSign != resultSign) {
      return Result.Err<i64, ErrorCode>(ErrorCode.Overflow);
    }

    return Result.Ok<i64, ErrorCode>(result);
  }

  /**
   * Safe multiplication with overflow detection.
   */
  export function mul(a: i64, b: i64): Result<i64, ErrorCode> {
    if (a == 0 || b == 0) {
      return Result.Ok<i64, ErrorCode>(0);
    }

    const result = a * b;

    // Check for overflow by division
    if (result / a != b) {
      return Result.Err<i64, ErrorCode>(ErrorCode.Overflow);
    }

    // Special case: MIN_VALUE * -1
    if (a == i64.MIN_VALUE && b == -1) {
      return Result.Err<i64, ErrorCode>(ErrorCode.Overflow);
    }

    return Result.Ok<i64, ErrorCode>(result);
  }

  /**
   * Safe division (no divide by zero).
   */
  export function div(a: i64, b: i64): Result<i64, ErrorCode> {
    if (b == 0) {
      return Result.Err<i64, ErrorCode>(ErrorCode.DivisionByZero);
    }

    // MIN_VALUE / -1 overflows
    if (a == i64.MIN_VALUE && b == -1) {
      return Result.Err<i64, ErrorCode>(ErrorCode.Overflow);
    }

    return Result.Ok<i64, ErrorCode>(a / b);
  }

  /**
   * Safe modulo (no divide by zero).
   */
  export function mod(a: i64, b: i64): Result<i64, ErrorCode> {
    if (b == 0) {
      return Result.Err<i64, ErrorCode>(ErrorCode.DivisionByZero);
    }

    return Result.Ok<i64, ErrorCode>(a % b);
  }

  /**
   * Safe absolute value.
   */
  export function abs(a: i64): Result<i64, ErrorCode> {
    if (a == i64.MIN_VALUE) {
      return Result.Err<i64, ErrorCode>(ErrorCode.Overflow);
    }

    return Result.Ok<i64, ErrorCode>(a < 0 ? -a : a);
  }

  /**
   * Clamp value to range [min, max].
   */
  export function clamp(value: i64, min: i64, max: i64): i64 {
    if (value < min) return min;
    if (value > max) return max;
    return value;
  }

  /**
   * Check if value is in range [min, max].
   */
  export function inRange(value: i64, min: i64, max: i64): bool {
    return value >= min && value <= max;
  }
}

// ============================================================================
// SAFE VALIDATION
// ============================================================================

export namespace SafeValidation {
  /**
   * Validate port number (1-65535).
   */
  export function isValidPort(port: i32): bool {
    return port >= 1 && port <= 65535;
  }

  /**
   * Validate port with Result.
   */
  export function validatePort(port: i32): Result<i32, ErrorCode> {
    if (port < 1 || port > 65535) {
      return Result.Err<i32, ErrorCode>(ErrorCode.InvalidInput);
    }
    return Result.Ok<i32, ErrorCode>(port);
  }

  /**
   * Validate percentage (0-100).
   */
  export function isValidPercentage(value: i32): bool {
    return value >= 0 && value <= 100;
  }

  /**
   * Validate positive number.
   */
  export function isPositive(value: i64): bool {
    return value > 0;
  }

  /**
   * Validate non-negative number.
   */
  export function isNonNegative(value: i64): bool {
    return value >= 0;
  }
}

// ============================================================================
// SAFE MEMORY
// ============================================================================

export namespace SafeMemory {
  /**
   * Bounds-checked array access.
   */
  export function getAt<T>(arr: T[], index: i32): Result<T, ErrorCode> {
    if (index < 0 || index >= arr.length) {
      return Result.Err<T, ErrorCode>(ErrorCode.OutOfBounds);
    }
    return Result.Ok<T, ErrorCode>(arr[index]);
  }

  /**
   * Bounds-checked array write.
   */
  export function setAt<T>(arr: T[], index: i32, value: T): Result<bool, ErrorCode> {
    if (index < 0 || index >= arr.length) {
      return Result.Err<bool, ErrorCode>(ErrorCode.OutOfBounds);
    }
    arr[index] = value;
    return Result.Ok<bool, ErrorCode>(true);
  }

  /**
   * Safe slice with bounds checking.
   */
  export function slice<T>(arr: T[], start: i32, end: i32): Result<T[], ErrorCode> {
    const safeStart = max(0, min(start, arr.length));
    const safeEnd = max(safeStart, min(end, arr.length));
    return Result.Ok<T[], ErrorCode>(arr.slice(safeStart, safeEnd));
  }
}

// ============================================================================
// COMMON SAFE VALUES
// ============================================================================

export namespace CommonPorts {
  export const HTTP: i32 = 80;
  export const HTTPS: i32 = 443;
  export const SSH: i32 = 22;
  export const DNS: i32 = 53;
  export const MYSQL: i32 = 3306;
  export const POSTGRES: i32 = 5432;
  export const REDIS: i32 = 6379;
}

// ============================================================================
// VERSION
// ============================================================================

export function version(): string {
  return "0.9.0";
}

export function versionPacked(): i32 {
  // major << 16 | minor << 8 | patch
  return 0x000900;
}
