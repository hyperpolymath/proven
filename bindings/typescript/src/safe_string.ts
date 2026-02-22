// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeString - Typed wrapper for string operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeString as JsSafeString } from '../../javascript/src/safe_string.js';

/** Result type for string operations. */
export type StringResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Safe string operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export class SafeString {
  /**
   * Check if a byte sequence is valid UTF-8.
   * Delegates to proven_string_is_valid_utf8 via FFI.
   *
   * @param input - Input string or bytes.
   * @returns Result with boolean validity flag, or error.
   */
  static isValidUtf8(input: string | Uint8Array): StringResult<boolean> {
    return JsSafeString.isValidUtf8(input) as StringResult<boolean>;
  }

  /**
   * Escape a string for safe SQL interpolation.
   * Delegates to proven_string_escape_sql via FFI.
   *
   * @param value - Input string.
   * @returns Result with escaped string, or error.
   */
  static escapeSql(value: string): StringResult<string> {
    return JsSafeString.escapeSql(value) as StringResult<string>;
  }

  /**
   * Escape a string for safe HTML insertion (prevents XSS).
   * Delegates to proven_string_escape_html via FFI.
   *
   * @param value - Input string.
   * @returns Result with escaped string, or error.
   */
  static escapeHtml(value: string): StringResult<string> {
    return JsSafeString.escapeHtml(value) as StringResult<string>;
  }

  /**
   * Escape a string for safe JavaScript string literal insertion.
   * Delegates to proven_string_escape_js via FFI.
   *
   * @param value - Input string.
   * @returns Result with escaped string, or error.
   */
  static escapeJs(value: string): StringResult<string> {
    return JsSafeString.escapeJs(value) as StringResult<string>;
  }
}
