// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeJson - Typed wrapper for JSON operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafeJson FFI wrapper. */
module SafeJsonJs = {
  @module("../../javascript/src/safe_json.js") @scope("SafeJson")
  external isValid: string => jsResult<bool> = "isValid"

  @module("../../javascript/src/safe_json.js") @scope("SafeJson")
  external getType: string => int = "getType"
}

/**
 * Check if a JSON string is valid.
 * Delegates to proven_json_is_valid via FFI.
 *
 * @param input JSON string to validate.
 * @returns Ok(true/false) or Error.
 */
let isValid = (input: string): result<bool, string> => {
  SafeJsonJs.isValid(input)->fromJs
}

/**
 * Get the JSON value type.
 * Delegates to proven_json_get_type via FFI.
 *
 * @param input JSON string.
 * @returns The JSON type identifier (integer code).
 */
let getType = SafeJsonJs.getType
