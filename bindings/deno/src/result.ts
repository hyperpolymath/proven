// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Result type for operations that can fail.
 *
 * Provides a functional approach to error handling without exceptions.
 * These are pure data wrappers -- no FFI dependency.
 *
 * @module
 */

/** Discriminated union result type. */
export type Result<T, E = string> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: E };

/** Create a success result. */
export function ok<T>(value: T): Result<T, never> {
  return { ok: true, value };
}

/** Create an error result. */
export function err<E>(error: E): Result<never, E> {
  return { ok: false, error };
}

/** Check if a result is successful. */
export function isOk<T, E>(
  result: Result<T, E>,
): result is { ok: true; value: T } {
  return result.ok;
}

/** Check if a result is an error. */
export function isErr<T, E>(
  result: Result<T, E>,
): result is { ok: false; error: E } {
  return !result.ok;
}

/** Unwrap a result, throwing if it is an error. */
export function unwrap<T, E>(result: Result<T, E>): T {
  if (result.ok) {
    return result.value;
  }
  throw new Error(`Unwrap called on Err: ${result.error}`);
}

/** Unwrap a result with a default value. */
export function unwrapOr<T, E>(result: Result<T, E>, defaultValue: T): T {
  return result.ok ? result.value : defaultValue;
}
