// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCrypto - Cryptographic primitives that cannot crash.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Safe cryptographic operations backed by formally verified Idris 2 code.
 */
export class SafeCrypto {
  /**
   * Constant-time byte comparison (timing-attack resistant).
   *
   * @param a - First byte array.
   * @param b - Second byte array.
   * @returns Result containing a boolean or an error string.
   */
  static constantTimeEq(a: Uint8Array, b: Uint8Array): Result<boolean> {
    const symbols = getLib();
    const result = symbols.proven_crypto_constant_time_eq(a, a.length, b, b.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Fill a buffer with cryptographically secure random bytes.
   *
   * @param buffer - The buffer to fill.
   * @returns Result containing the filled buffer or an error string.
   */
  static randomBytes(buffer: Uint8Array): Result<Uint8Array> {
    const symbols = getLib();
    const status = symbols.proven_crypto_random_bytes(buffer, buffer.length);
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    return ok(buffer);
  }
}
