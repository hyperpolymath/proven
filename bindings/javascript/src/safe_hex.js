// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeHex - Hexadecimal encoding/decoding.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError, readAndFreeString } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Safe hex operations backed by formally verified Idris 2 code.
 */
export class SafeHex {
  /**
   * Encode bytes to hexadecimal string.
   *
   * @param {Uint8Array} bytes - The bytes to encode.
   * @param {boolean} [uppercase=false] - Whether to use uppercase hex digits.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static encode(bytes, uppercase = false) {
    const symbols = getLib();
    const result = symbols.proven_hex_encode(bytes, bytes.length, uppercase);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const encoded = readAndFreeString(result[1], Number(result[2]));
    if (encoded === null) return err('Null string returned');
    return ok(encoded);
  }

  // Note: proven_hex_decode returns HexDecodeResult which has a data
  // pointer + length instead of a null-terminated string. This requires
  // custom buffer handling which is not yet wired for Deno FFI struct
  // returns with non-string pointers. Will be completed when the buffer
  // protocol is finalized.
}
