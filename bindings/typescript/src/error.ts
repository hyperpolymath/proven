// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Error types and status codes for the Proven FFI layer.
 *
 * These mirror the status codes from libproven (Idris 2 + Zig).
 * This file provides TypeScript type definitions only -- no reimplemented logic.
 *
 * @module
 */

/**
 * Status codes from Proven FFI operations.
 * Maps to integer values returned by the C ABI.
 */
export type ProvenStatus =
  | 'ok'
  | 'null_pointer'
  | 'invalid_argument'
  | 'overflow'
  | 'underflow'
  | 'division_by_zero'
  | 'parse_failure'
  | 'validation_failed'
  | 'out_of_bounds'
  | 'encoding_error'
  | 'allocation_failed'
  | 'not_implemented';

/**
 * Map raw integer status codes from libproven to ProvenStatus strings.
 *
 * @param code - The integer status code from the FFI layer.
 * @returns The corresponding ProvenStatus string.
 */
export function statusFromCode(code: number): ProvenStatus {
  switch (code) {
    case 0:
      return 'ok';
    case -1:
      return 'null_pointer';
    case -2:
      return 'invalid_argument';
    case -3:
      return 'overflow';
    case -4:
      return 'underflow';
    case -5:
      return 'division_by_zero';
    case -6:
      return 'parse_failure';
    case -7:
      return 'validation_failed';
    case -8:
      return 'out_of_bounds';
    case -9:
      return 'encoding_error';
    case -10:
      return 'allocation_failed';
    case -99:
      return 'not_implemented';
    default:
      return 'invalid_argument';
  }
}

/**
 * Error thrown by Proven operations when an FFI call returns a non-OK status.
 */
export class ProvenError extends Error {
  readonly status: ProvenStatus;

  constructor(status: ProvenStatus, message?: string) {
    const defaultMessages: Record<ProvenStatus, string> = {
      ok: 'Operation succeeded',
      null_pointer: 'Null pointer passed to function',
      invalid_argument: 'Invalid argument',
      overflow: 'Integer overflow',
      underflow: 'Integer underflow',
      division_by_zero: 'Division by zero',
      parse_failure: 'Parse failure',
      validation_failed: 'Validation failed',
      out_of_bounds: 'Index out of bounds',
      encoding_error: 'Encoding error',
      allocation_failed: 'Memory allocation failed',
      not_implemented: 'Not implemented',
    };

    super(message ?? defaultMessages[status]);
    this.name = 'ProvenError';
    this.status = status;
  }
}
