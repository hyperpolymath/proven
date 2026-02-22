// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeHex - Hexadecimal encoding/decoding.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, readAndFreeString, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Safe hex operations backed by formally verified Idris 2 code.
 */
export class SafeHex {
  /**
   * Encode bytes to hexadecimal string.
   *
   * @param bytes - The bytes to encode.
   * @param uppercase - Whether to use uppercase hex digits.
   * @returns Result containing the hex string or an error.
   */
  static encode(bytes: Uint8Array, uppercase = false): Result<string> {
    const symbols = getLib();
    const result = symbols.proven_hex_encode(bytes, bytes.length, uppercase);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const encoded = readAndFreeString(result[1], Number(result[2]));
    if (encoded === null) return err('Null string returned');
    return ok(encoded);
  }
}
