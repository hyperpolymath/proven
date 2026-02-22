// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCrypto - Cryptographic primitives that cannot crash.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Safe cryptographic operations backed by formally verified Idris 2 code.
 */
export class SafeCrypto {
  /**
   * Constant-time byte comparison (timing-attack resistant).
   *
   * @param {Uint8Array} a - First byte array.
   * @param {Uint8Array} b - Second byte array.
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static constantTimeEq(a, b) {
    const symbols = getLib();
    const result = symbols.proven_crypto_constant_time_eq(a, a.length, b, b.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Fill a buffer with cryptographically secure random bytes.
   *
   * @param {Uint8Array} buffer - The buffer to fill.
   * @returns {{ ok: true, value: Uint8Array } | { ok: false, error: string }}
   */
  static randomBytes(buffer) {
    const symbols = getLib();
    const status = symbols.proven_crypto_random_bytes(buffer, buffer.length);
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    return ok(buffer);
  }
}

// Re-export convenience names for API compatibility
export const ConstantTime = SafeCrypto;
export const SecureRandom = SafeCrypto;

/**
 * Base64 operations (placeholder -- the FFI layer does not yet export
 * base64 functions; these will be wired when Idris base64 is complete).
 */
export class Base64 {}

/**
 * Hex operations (alias for SafeHex).
 */
export { SafeHex as Hex } from './safe_hex.js';

/**
 * Hash operations (placeholder -- will be wired to FFI digest functions).
 */
export class Hash {}

/**
 * HMAC operations (placeholder -- will be wired to FFI when available).
 */
export class Hmac {}
