// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeString - String operations that cannot crash.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError, encodeString, readAndFreeString } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Safe string operations backed by formally verified Idris 2 code.
 */
export class SafeString {
  /**
   * Check if a byte sequence is valid UTF-8.
   *
   * @param {string|Uint8Array} input - Input string or bytes.
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static isValidUtf8(input) {
    const symbols = getLib();
    const bytes = typeof input === 'string' ? new TextEncoder().encode(input) : input;
    const result = symbols.proven_string_is_valid_utf8(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Escape a string for safe use in SQL queries (single-quote escaping).
   *
   * @param {string} str - Input string.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static escapeSql(str) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(str);
    const result = symbols.proven_string_escape_sql(bytes, bytes.length);
    const status = result[0];
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    const escaped = readAndFreeString(result[1], Number(result[2]));
    if (escaped === null) return err('Null string returned');
    return ok(escaped);
  }

  /**
   * Escape a string for safe use in HTML (prevents XSS).
   *
   * @param {string} str - Input string.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static escapeHtml(str) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(str);
    const result = symbols.proven_string_escape_html(bytes, bytes.length);
    const status = result[0];
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    const escaped = readAndFreeString(result[1], Number(result[2]));
    if (escaped === null) return err('Null string returned');
    return ok(escaped);
  }

  /**
   * Escape a string for safe use in JavaScript string literals.
   *
   * @param {string} str - Input string.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static escapeJs(str) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(str);
    const result = symbols.proven_string_escape_js(bytes, bytes.length);
    const status = result[0];
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    const escaped = readAndFreeString(result[1], Number(result[2]));
    if (escaped === null) return err('Null string returned');
    return ok(escaped);
  }
}
