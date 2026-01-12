// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeCrypto - Cryptographic operations that cannot crash.
 *
 * Provides timing-safe comparisons and secure random bytes.
 */

import { getExports, freePtr } from './wasm.js';
import { statusFromCode } from './error.js';

/**
 * Safe cryptographic operations with proven correctness guarantees.
 */
export class SafeCrypto {
  /**
   * Compare two byte arrays in constant time.
   *
   * Prevents timing attacks by ensuring comparison takes the same
   * time regardless of where differences occur.
   *
   * @example
   * SafeCrypto.constantTimeEq(
   *   new Uint8Array([1, 2, 3]),
   *   new Uint8Array([1, 2, 3])
   * )  // true
   */
  static constantTimeEq(a: Uint8Array, b: Uint8Array): boolean {
    const exports = getExports();
    const fn = exports['proven_crypto_constant_time_eq'] as (
      aPtr: number,
      aLen: number,
      bPtr: number,
      bLen: number
    ) => number;

    const allocFn = exports['proven_alloc'] as (size: number) => number;
    const memory = exports['memory'] as WebAssembly.Memory;

    // Allocate and copy arrays
    const aPtr = allocFn(a.length);
    const bPtr = allocFn(b.length);
    new Uint8Array(memory.buffer, aPtr, a.length).set(a);
    new Uint8Array(memory.buffer, bPtr, b.length).set(b);

    const result = fn(aPtr, a.length, bPtr, b.length);

    freePtr(aPtr);
    freePtr(bPtr);

    const status = result >> 16;
    const value = result & 0xffff;

    return statusFromCode(status) === 'ok' && value === 1;
  }

  /**
   * Compare two strings in constant time.
   *
   * Convenience wrapper for constantTimeEq with string encoding.
   */
  static secureCompare(a: string, b: string): boolean {
    const encoder = new TextEncoder();
    return SafeCrypto.constantTimeEq(encoder.encode(a), encoder.encode(b));
  }

  /**
   * Generate cryptographically secure random bytes.
   *
   * Uses the OS's secure random source.
   *
   * @example
   * const key = SafeCrypto.randomBytes(32);
   * // Uint8Array of 32 random bytes
   */
  static randomBytes(length: number): Uint8Array | null {
    if (length <= 0 || length > 1024 * 1024) {
      return null;
    }

    const exports = getExports();
    const fn = exports['proven_crypto_random_bytes'] as (ptr: number, len: number) => number;

    const allocFn = exports['proven_alloc'] as (size: number) => number;
    const memory = exports['memory'] as WebAssembly.Memory;

    const ptr = allocFn(length);
    const status = fn(ptr, length);

    if (statusFromCode(status) !== 'ok') {
      freePtr(ptr);
      return null;
    }

    const result = new Uint8Array(length);
    result.set(new Uint8Array(memory.buffer, ptr, length));
    freePtr(ptr);

    return result;
  }

  /**
   * Generate a random hex string.
   *
   * @example
   * SafeCrypto.randomHex(32)  // "a1b2c3d4..."
   */
  static randomHex(length: number): string | null {
    if (length <= 0 || length % 2 !== 0) {
      return null;
    }

    const bytes = SafeCrypto.randomBytes(length / 2);
    if (bytes === null) return null;

    const hexChars = '0123456789abcdef';
    let result = '';
    for (const byte of bytes) {
      result += hexChars[byte >> 4];
      result += hexChars[byte & 0x0f];
    }
    return result;
  }
}
