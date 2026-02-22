// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeString - Typed wrapper for string operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafeString FFI wrapper. */
module SafeStringJs = {
  @module("../../javascript/src/safe_string.js") @scope("SafeString")
  external isValidUtf8: string => jsResult<bool> = "isValidUtf8"

  @module("../../javascript/src/safe_string.js") @scope("SafeString")
  external escapeSql: string => jsResult<string> = "escapeSql"

  @module("../../javascript/src/safe_string.js") @scope("SafeString")
  external escapeHtml: string => jsResult<string> = "escapeHtml"

  @module("../../javascript/src/safe_string.js") @scope("SafeString")
  external escapeJs: string => jsResult<string> = "escapeJs"
}

/**
 * Check if a byte sequence is valid UTF-8.
 * Delegates to proven_string_is_valid_utf8 via FFI.
 *
 * @param input Input string.
 * @returns Ok(true/false) or Error.
 */
let isValidUtf8 = (input: string): result<bool, string> => {
  SafeStringJs.isValidUtf8(input)->fromJs
}

/**
 * Escape a string for safe SQL interpolation.
 * Delegates to proven_string_escape_sql via FFI.
 *
 * @param value Input string.
 * @returns Ok(escaped) or Error.
 */
let escapeSql = (value: string): result<string, string> => {
  SafeStringJs.escapeSql(value)->fromJs
}

/**
 * Escape a string for safe HTML insertion (prevents XSS).
 * Delegates to proven_string_escape_html via FFI.
 *
 * @param value Input string.
 * @returns Ok(escaped) or Error.
 */
let escapeHtml = (value: string): result<string, string> => {
  SafeStringJs.escapeHtml(value)->fromJs
}

/**
 * Escape a string for safe JavaScript string literal insertion.
 * Delegates to proven_string_escape_js via FFI.
 *
 * @param value Input string.
 * @returns Ok(escaped) or Error.
 */
let escapeJs = (value: string): result<string, string> => {
  SafeStringJs.escapeJs(value)->fromJs
}
