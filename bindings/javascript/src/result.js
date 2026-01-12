// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * Result type for operations that can fail.
 *
 * Provides a functional approach to error handling without exceptions.
 * @module
 */

/**
 * Create a success result.
 *
 * @template T
 * @param {T} value - The success value.
 * @returns {{ ok: true, value: T }} The success result.
 *
 * @example
 * const successResult = ok(42);
 * // { ok: true, value: 42 }
 */
export function ok(value) {
  return { ok: true, value };
}

/**
 * Create an error result.
 *
 * @template E
 * @param {E} error - The error value.
 * @returns {{ ok: false, error: E }} The error result.
 *
 * @example
 * const errorResult = err('Invalid input');
 * // { ok: false, error: 'Invalid input' }
 */
export function err(error) {
  return { ok: false, error };
}

/**
 * Check if a result is successful.
 *
 * @template T, E
 * @param {{ ok: boolean, value?: T, error?: E }} result - The result to check.
 * @returns {boolean} True if the result is successful.
 */
export function isOk(result) {
  return result.ok === true;
}

/**
 * Check if a result is an error.
 *
 * @template T, E
 * @param {{ ok: boolean, value?: T, error?: E }} result - The result to check.
 * @returns {boolean} True if the result is an error.
 */
export function isErr(result) {
  return result.ok === false;
}

/**
 * Unwrap a result, throwing if it's an error.
 *
 * @template T, E
 * @param {{ ok: boolean, value?: T, error?: E }} result - The result to unwrap.
 * @returns {T} The success value.
 * @throws {Error} If the result is an error.
 */
export function unwrap(result) {
  if (result.ok) {
    return result.value;
  }
  throw new Error(`Unwrap called on Err: ${result.error}`);
}

/**
 * Unwrap a result with a default value.
 *
 * @template T, E
 * @param {{ ok: boolean, value?: T, error?: E }} result - The result to unwrap.
 * @param {T} defaultValue - The default value if error.
 * @returns {T} The success value or default.
 */
export function unwrapOr(result, defaultValue) {
  return result.ok ? result.value : defaultValue;
}
