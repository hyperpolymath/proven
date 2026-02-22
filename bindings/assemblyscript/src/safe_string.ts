// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeString - Safe string operations.
 *
 * Thin wrapper around libproven's SafeString functions. All text processing
 * (UTF-8 validation, escaping for SQL/HTML/JS) is performed by the formally
 * verified Idris 2 implementation via host-provided WASM imports.
 * This module only handles data marshaling between AssemblyScript strings
 * and the WASM linear memory buffers expected by the host.
 *
 * Corresponds to the SafeString section in proven.h:
 *   - proven_string_is_valid_utf8
 *   - proven_string_escape_sql
 *   - proven_string_escape_html
 *   - proven_string_escape_js
 */

import {
  Result,
  RESULT_BUF,
  readBoolResult,
  readStringResult,
  encodeString,
  encodedPtr,
  encodedLen,
} from "./common";
import {
  proven_string_is_valid_utf8,
  proven_string_escape_sql,
  proven_string_escape_html,
  proven_string_escape_js,
} from "./ffi";

/**
 * SafeString provides validated and escaped string operations.
 *
 * Every method delegates to the corresponding libproven host function.
 * No string processing logic is implemented in AssemblyScript.
 */
export class SafeString {
  /**
   * Check if the given string bytes are valid UTF-8.
   * Delegates to proven_string_is_valid_utf8 (Idris 2 verified).
   *
   * @param s - The string to validate.
   * @returns Result<bool> indicating validity.
   */
  static isValidUtf8(s: string): Result<bool> {
    encodeString(s);
    proven_string_is_valid_utf8(encodedPtr(), encodedLen(), RESULT_BUF);
    return readBoolResult(RESULT_BUF);
  }

  /**
   * Escape a string for safe use in SQL queries (single-quote escaping).
   * Delegates to proven_string_escape_sql (Idris 2 verified).
   *
   * NOTE: Prefer parameterized queries over string escaping.
   *
   * @param s - The string to escape.
   * @returns Result<string> with escaped string, or error status.
   */
  static escapeSql(s: string): Result<string> {
    encodeString(s);
    proven_string_escape_sql(encodedPtr(), encodedLen(), RESULT_BUF);
    return readStringResult(RESULT_BUF);
  }

  /**
   * Escape a string for safe embedding in HTML (prevents XSS).
   * Delegates to proven_string_escape_html (Idris 2 verified).
   *
   * @param s - The string to escape.
   * @returns Result<string> with escaped string, or error status.
   */
  static escapeHtml(s: string): Result<string> {
    encodeString(s);
    proven_string_escape_html(encodedPtr(), encodedLen(), RESULT_BUF);
    return readStringResult(RESULT_BUF);
  }

  /**
   * Escape a string for safe use in JavaScript string literals.
   * Delegates to proven_string_escape_js (Idris 2 verified).
   *
   * @param s - The string to escape.
   * @returns Result<string> with escaped string, or error status.
   */
  static escapeJs(s: string): Result<string> {
    encodeString(s);
    proven_string_escape_js(encodedPtr(), encodedLen(), RESULT_BUF);
    return readStringResult(RESULT_BUF);
  }
}
