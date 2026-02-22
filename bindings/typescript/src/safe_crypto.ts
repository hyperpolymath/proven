// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCrypto - Typed wrapper for cryptographic operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeCrypto as JsSafeCrypto } from '../../javascript/src/safe_crypto.js';

/** Result type for crypto operations. */
export type CryptoResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Safe cryptographic operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export class SafeCrypto {
  /**
   * Compare two byte arrays in constant time (prevents timing attacks).
   * Delegates to proven_crypto_constant_time_eq via FFI.
   *
   * @param a - First byte array.
   * @param b - Second byte array.
   * @returns Result with boolean equality flag, or error.
   */
  static constantTimeEq(a: Uint8Array, b: Uint8Array): CryptoResult<boolean> {
    return JsSafeCrypto.constantTimeEq(a, b) as CryptoResult<boolean>;
  }

  /**
   * Generate cryptographically secure random bytes.
   * Delegates to proven_crypto_random_bytes via FFI.
   *
   * @param length - Number of random bytes to generate.
   * @returns Result with random bytes, or error.
   */
  static randomBytes(length: number): CryptoResult<Uint8Array> {
    return JsSafeCrypto.randomBytes(length) as CryptoResult<Uint8Array>;
  }
}
