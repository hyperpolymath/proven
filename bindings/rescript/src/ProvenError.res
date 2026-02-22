// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Error types and status codes for the Proven FFI layer.
 *
 * These mirror the status codes from libproven (Idris 2 + Zig).
 * This module provides ReScript type definitions only -- no reimplemented logic.
 */

/** Status codes from Proven FFI operations.
 *  Maps to integer values returned by the C ABI. */
type provenStatus =
  | Ok
  | NullPointer
  | InvalidArgument
  | Overflow
  | Underflow
  | DivisionByZero
  | ParseFailure
  | ValidationFailed
  | OutOfBounds
  | EncodingError
  | AllocationFailed
  | NotImplemented

/**
 * Map raw integer status codes from libproven to provenStatus variants.
 *
 * @param code The integer status code from the FFI layer.
 * @returns The corresponding provenStatus variant.
 */
let statusFromCode = (code: int): provenStatus => {
  switch code {
  | 0 => Ok
  | -1 => NullPointer
  | -2 => InvalidArgument
  | -3 => Overflow
  | -4 => Underflow
  | -5 => DivisionByZero
  | -6 => ParseFailure
  | -7 => ValidationFailed
  | -8 => OutOfBounds
  | -9 => EncodingError
  | -10 => AllocationFailed
  | -99 => NotImplemented
  | _ => InvalidArgument
  }
}

/**
 * Convert a provenStatus variant to a human-readable error message.
 *
 * @param status The provenStatus to describe.
 * @returns A descriptive error string.
 */
let statusToMessage = (status: provenStatus): string => {
  switch status {
  | Ok => "Operation succeeded"
  | NullPointer => "Null pointer passed to function"
  | InvalidArgument => "Invalid argument"
  | Overflow => "Integer overflow"
  | Underflow => "Integer underflow"
  | DivisionByZero => "Division by zero"
  | ParseFailure => "Parse failure"
  | ValidationFailed => "Validation failed"
  | OutOfBounds => "Index out of bounds"
  | EncodingError => "Encoding error"
  | AllocationFailed => "Memory allocation failed"
  | NotImplemented => "Not implemented"
  }
}
