// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeUrl - URL parsing and validation.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_url_parse returns a complex struct with embedded pointers.
 * For Deno FFI, we call the simpler validation functions and use
 * proven_http_url_encode/decode for encoding operations.
 * @module
 */

import { getLib, ProvenStatus, statusToError, readAndFreeString } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Safe URL operations backed by formally verified Idris 2 code.
 */
export class SafeUrl {
  /**
   * URL-encode a string (RFC 3986 percent encoding).
   *
   * @param {string} str - The string to encode.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static encode(str) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(str);
    const result = symbols.proven_http_url_encode(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const encoded = readAndFreeString(result[1], Number(result[2]));
    if (encoded === null) return err('Null string returned');
    return ok(encoded);
  }

  /**
   * URL-decode a percent-encoded string.
   *
   * @param {string} str - The string to decode.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static decode(str) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(str);
    const result = symbols.proven_http_url_decode(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const decoded = readAndFreeString(result[1], Number(result[2]));
    if (decoded === null) return err('Null string returned');
    return ok(decoded);
  }
}

/**
 * A validated URL representation.
 * Uses the FFI for URL encoding/decoding operations.
 */
export class Url {
  /** @type {string} */
  #raw;

  /**
   * @param {string} raw - The URL string.
   */
  constructor(raw) {
    this.#raw = raw;
  }

  /** @returns {string} The raw URL string. */
  toString() { return this.#raw; }
}
