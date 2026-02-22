// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeUrl - URL encoding/decoding operations.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_url_parse returns a complex struct that requires buffer-based
 * marshaling. URL encoding/decoding are directly callable via proven_http_*.
 *
 * @module
 */

import { getLib, ProvenStatus, readAndFreeString, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Safe URL operations backed by formally verified Idris 2 code.
 */
export class SafeUrl {
  /**
   * URL-encode a string.
   *
   * @param str - The string to encode.
   * @returns Result containing the encoded string or an error.
   */
  static encode(str: string): Result<string> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(str);
    const result = symbols.proven_http_url_encode(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const encoded = readAndFreeString(result[1], Number(result[2]));
    if (encoded === null) return err('Null string returned');
    return ok(encoded);
  }

  /**
   * URL-decode a string.
   *
   * @param str - The URL-encoded string to decode.
   * @returns Result containing the decoded string or an error.
   */
  static decode(str: string): Result<string> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(str);
    const result = symbols.proven_http_url_decode(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const decoded = readAndFreeString(result[1], Number(result[2]));
    if (decoded === null) return err('Null string returned');
    return ok(decoded);
  }
}
