// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Common types and data marshaling utilities for Proven AssemblyScript bindings.
 *
 * This file provides:
 *   - ProvenStatus enum matching libproven's C status codes
 *   - Result<T> and Option<T> types for safe error handling
 *   - WASM linear memory helpers for marshaling data to/from host FFI calls
 *
 * NO computation logic is implemented here. All computation is performed
 * by libproven via host-provided WASM imports declared in ffi.ts.
 */

// ============================================================================
// STATUS CODES (must match ProvenStatus in proven.h exactly)
// ============================================================================

/**
 * Status codes returned by libproven operations.
 * Zero indicates success; negative values indicate specific error conditions.
 * These values MUST match the ProvenStatus enum in proven.h.
 */
export const enum ProvenStatus {
  Ok                   =   0,
  ErrNullPointer       =  -1,
  ErrInvalidArgument   =  -2,
  ErrOverflow          =  -3,
  ErrUnderflow         =  -4,
  ErrDivisionByZero    =  -5,
  ErrParseFailure      =  -6,
  ErrValidationFailed  =  -7,
  ErrOutOfBounds       =  -8,
  ErrEncodingError     =  -9,
  ErrAllocationFailed  = -10,
  ErrNotImplemented    = -99,
}

// ============================================================================
// RESULT TYPE
// ============================================================================

/**
 * Result type for operations that can fail.
 * Wraps a value with a ProvenStatus code from libproven.
 *
 * This is a data container for marshaling FFI results. It does not
 * perform any computation itself.
 */
export class Result<T> {
  private _status: ProvenStatus;
  private _value: T;

  private constructor(status: ProvenStatus, value: T) {
    this._status = status;
    this._value = value;
  }

  /** Create a successful result. */
  static ok<T>(value: T): Result<T> {
    return new Result<T>(ProvenStatus.Ok, value);
  }

  /** Create a failed result with the given status code. */
  static err<T>(status: ProvenStatus): Result<T> {
    return new Result<T>(status, changetype<T>(0));
  }

  /** True if the operation succeeded (status == Ok). */
  get isOk(): bool {
    return this._status == ProvenStatus.Ok;
  }

  /** True if the operation failed (status != Ok). */
  get isErr(): bool {
    return this._status != ProvenStatus.Ok;
  }

  /** The result value. Only meaningful when isOk is true. */
  get value(): T {
    return this._value;
  }

  /** The status code from libproven. */
  get status(): ProvenStatus {
    return this._status;
  }

  /** Return value if Ok, otherwise the provided default. */
  unwrapOr(defaultValue: T): T {
    return this._status == ProvenStatus.Ok ? this._value : defaultValue;
  }

  /** Transform the value if Ok, propagate error otherwise. */
  map<U>(fn: (value: T) => U): Result<U> {
    if (this._status == ProvenStatus.Ok) {
      return Result.ok<U>(fn(this._value));
    }
    return Result.err<U>(this._status);
  }

  /** Chain operations: apply fn if Ok, propagate error otherwise. */
  andThen<U>(fn: (value: T) => Result<U>): Result<U> {
    if (this._status == ProvenStatus.Ok) {
      return fn(this._value);
    }
    return Result.err<U>(this._status);
  }
}

// ============================================================================
// OPTION TYPE
// ============================================================================

/**
 * Option type for nullable values.
 * Data container only -- no computation logic.
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
// WASM LINEAR MEMORY MARSHALING HELPERS
// ============================================================================

/**
 * Size constants for result struct buffers allocated in WASM linear memory.
 * The host writes result structs into these buffers after each FFI call.
 */
export const INT_RESULT_SIZE: usize = 16;     // [status: i32 @ +0] [pad: 4] [value: i64 @ +8]
export const BOOL_RESULT_SIZE: usize = 8;     // [status: i32 @ +0] [value: i32 @ +4]
export const FLOAT_RESULT_SIZE: usize = 16;   // [status: i32 @ +0] [pad: 4] [value: f64 @ +8]
export const STRING_RESULT_SIZE: usize = 12;  // [status: i32 @ +0] [ptr: u32 @ +4] [len: u32 @ +8]

/**
 * Scratch buffer for result structs. Allocated once in WASM linear memory
 * using AssemblyScript's memory.data() intrinsic (static allocation).
 *
 * 64 bytes is enough for the largest result struct we use.
 */
// @ts-ignore: decorator
@lazy
export const RESULT_BUF: usize = memory.data(64);

/**
 * Read the status field (i32 at offset +0) from a result buffer.
 */
export function readStatus(ptr: usize): ProvenStatus {
  return load<i32>(ptr) as ProvenStatus;
}

/**
 * Read an IntResult: status i32 at +0, value i64 at +8.
 * Returns Result<i64>.
 */
export function readIntResult(ptr: usize): Result<i64> {
  const status = readStatus(ptr);
  if (status != ProvenStatus.Ok) {
    return Result.err<i64>(status);
  }
  return Result.ok<i64>(load<i64>(ptr, 8));
}

/**
 * Read a BoolResult: status i32 at +0, value i32 at +4.
 * Returns Result<bool>.
 */
export function readBoolResult(ptr: usize): Result<bool> {
  const status = readStatus(ptr);
  if (status != ProvenStatus.Ok) {
    return Result.err<bool>(status);
  }
  return Result.ok<bool>(load<i32>(ptr, 4) != 0);
}

/**
 * Read a FloatResult: status i32 at +0, value f64 at +8.
 * Returns Result<f64>.
 */
export function readFloatResult(ptr: usize): Result<f64> {
  const status = readStatus(ptr);
  if (status != ProvenStatus.Ok) {
    return Result.err<f64>(status);
  }
  return Result.ok<f64>(load<f64>(ptr, 8));
}

/**
 * Read a StringResult: status i32 at +0, ptr u32 at +4, len u32 at +8.
 *
 * The host has allocated the string bytes in WASM linear memory and
 * written the pointer and length into the result buffer. We copy the
 * bytes into an AssemblyScript string object.
 *
 * Returns Result<string>.
 */
export function readStringResult(ptr: usize): Result<string> {
  const status = readStatus(ptr);
  if (status != ProvenStatus.Ok) {
    return Result.err<string>(status);
  }
  const strPtr = load<u32>(ptr, 4) as usize;
  const strLen = load<u32>(ptr, 8) as usize;
  if (strLen == 0) {
    return Result.ok<string>("");
  }
  return Result.ok<string>(String.UTF8.decodeUnsafe(strPtr, strLen));
}

/**
 * Encode an AssemblyScript string to UTF-8 bytes in WASM linear memory.
 * Returns the pointer and length as a pair stored in a static buffer.
 *
 * IMPORTANT: The returned buffer is statically allocated and will be
 * overwritten on the next call. Copy data before calling again.
 */
// @ts-ignore: decorator
@lazy
const STR_ENCODE_BUF: usize = memory.data(8); // [ptr: u32, len: u32]

/**
 * Encode a string to UTF-8 and return (ptr, len) for passing to host FFI.
 *
 * Uses AssemblyScript's String.UTF8.encode() which allocates in the
 * managed heap. The host reads from this pointer during the FFI call.
 */
export function encodeString(s: string): usize {
  const buf = String.UTF8.encode(s, false);
  const ptr = changetype<usize>(buf);
  // ArrayBuffer header is 16 bytes in AssemblyScript; data starts after
  const dataPtr = ptr + 16;
  const len = <u32>buf.byteLength;
  store<u32>(STR_ENCODE_BUF, dataPtr as u32);
  store<u32>(STR_ENCODE_BUF, len, 4);
  return STR_ENCODE_BUF;
}

/**
 * Read the encoded string pointer from the encode buffer.
 */
export function encodedPtr(): usize {
  return load<u32>(STR_ENCODE_BUF) as usize;
}

/**
 * Read the encoded string length from the encode buffer.
 */
export function encodedLen(): usize {
  return load<u32>(STR_ENCODE_BUF, 4) as usize;
}
