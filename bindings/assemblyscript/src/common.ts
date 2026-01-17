// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Common types and utilities for Proven AssemblyScript bindings.
 */

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
  NotFound = 6,
  InvalidState = 7,
  NetworkError = 8,
  CapacityExceeded = 9,
  InvalidFormat = 10,
  InvalidPath = 11,
  InvalidEmail = 12,
  InvalidUrl = 13,
  InvalidIp = 14,
  RateLimited = 15,
  CircuitOpen = 16,
  Timeout = 17,
  InvalidTransition = 18,
  CycleDetected = 19,
}

// ============================================================================
// RESULT TYPE
// ============================================================================

/**
 * Result type for operations that can fail.
 * Similar to Rust's Result<T, E>.
 */
export class Result<T> {
  private _ok: bool;
  private _value: T;
  private _error: ErrorCode;

  private constructor(ok: bool, value: T, error: ErrorCode) {
    this._ok = ok;
    this._value = value;
    this._error = error;
  }

  static ok<T>(value: T): Result<T> {
    return new Result<T>(true, value, ErrorCode.None);
  }

  static err<T>(error: ErrorCode): Result<T> {
    return new Result<T>(false, changetype<T>(0), error);
  }

  get isOk(): bool {
    return this._ok;
  }

  isErr(): bool {
    return !this._ok;
  }

  get value(): T {
    return this._value;
  }

  get error(): ErrorCode {
    return this._error;
  }

  unwrap(): T {
    assert(this._ok, "Called unwrap on Err");
    return this._value;
  }

  unwrapOr(defaultValue: T): T {
    return this._ok ? this._value : defaultValue;
  }

  unwrapOrElse(fn: () => T): T {
    return this._ok ? this._value : fn();
  }

  map<U>(fn: (value: T) => U): Result<U> {
    if (this._ok) {
      return Result.ok<U>(fn(this._value));
    }
    return Result.err<U>(this._error);
  }

  andThen<U>(fn: (value: T) => Result<U>): Result<U> {
    if (this._ok) {
      return fn(this._value);
    }
    return Result.err<U>(this._error);
  }
}

// ============================================================================
// OPTION TYPE
// ============================================================================

/**
 * Option type for nullable values.
 * Similar to Rust's Option<T>.
 */
export class Option<T> {
  private _some: bool;
  private _value: T;

  private constructor(some: bool, value: T) {
    this._some = some;
    this._value = value;
  }

  static some<T>(value: T): Option<T> {
    return new Option<T>(true, value);
  }

  static none<T>(): Option<T> {
    return new Option<T>(false, changetype<T>(0));
  }

  get isSome(): bool {
    return this._some;
  }

  get isNone(): bool {
    return !this._some;
  }

  get value(): T {
    return this._value;
  }

  unwrap(): T {
    assert(this._some, "Called unwrap on None");
    return this._value;
  }

  unwrapOr(defaultValue: T): T {
    return this._some ? this._value : defaultValue;
  }

  map<U>(fn: (value: T) => U): Option<U> {
    if (this._some) {
      return Option.some<U>(fn(this._value));
    }
    return Option.none<U>();
  }

  andThen<U>(fn: (value: T) => Option<U>): Option<U> {
    if (this._some) {
      return fn(this._value);
    }
    return Option.none<U>();
  }
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Check if a character is ASCII alphanumeric.
 */
export function isAlphanumeric(code: i32): bool {
  return (code >= 48 && code <= 57) ||  // 0-9
         (code >= 65 && code <= 90) ||  // A-Z
         (code >= 97 && code <= 122);   // a-z
}

/**
 * Check if a character is ASCII digit.
 */
export function isDigit(code: i32): bool {
  return code >= 48 && code <= 57;  // 0-9
}

/**
 * Check if a character is ASCII letter.
 */
export function isLetter(code: i32): bool {
  return (code >= 65 && code <= 90) ||  // A-Z
         (code >= 97 && code <= 122);   // a-z
}

/**
 * Check if a character is ASCII lowercase.
 */
export function isLowercase(code: i32): bool {
  return code >= 97 && code <= 122;  // a-z
}

/**
 * Check if a character is ASCII uppercase.
 */
export function isUppercase(code: i32): bool {
  return code >= 65 && code <= 90;  // A-Z
}

/**
 * Check if a character is hex digit.
 */
export function isHexDigit(code: i32): bool {
  return (code >= 48 && code <= 57) ||  // 0-9
         (code >= 65 && code <= 70) ||  // A-F
         (code >= 97 && code <= 102);   // a-f
}

/**
 * Check if a character is whitespace.
 */
export function isWhitespace(code: i32): bool {
  return code == 32 || code == 9 || code == 10 || code == 13;  // space, tab, LF, CR
}

/**
 * Convert hex character to value.
 */
export function hexToValue(code: i32): i32 {
  if (code >= 48 && code <= 57) return code - 48;       // 0-9
  if (code >= 65 && code <= 70) return code - 55;       // A-F
  if (code >= 97 && code <= 102) return code - 87;      // a-f
  return -1;
}

/**
 * Convert value to hex character.
 */
export function valueToHex(value: i32, uppercase: bool = false): i32 {
  if (value < 10) return 48 + value;  // 0-9
  return (uppercase ? 55 : 87) + value;  // A-F or a-f
}
