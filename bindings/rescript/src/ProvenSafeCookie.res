// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCookie - Typed wrapper for cookie operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafeCookie FFI wrapper. */
module SafeCookieJs = {
  @module("../../javascript/src/safe_cookie.js") @scope("SafeCookie")
  external hasInjection: string => jsResult<bool> = "hasInjection"

  @module("../../javascript/src/safe_cookie.js") @scope("SafeCookie")
  external validateName: string => jsResult<bool> = "validateName"

  @module("../../javascript/src/safe_cookie.js") @scope("SafeCookie")
  external validateValue: string => jsResult<bool> = "validateValue"

  @module("../../javascript/src/safe_cookie.js") @scope("SafeCookie")
  external buildDelete: string => jsResult<string> = "buildDelete"
}

/**
 * Check if a cookie value contains injection characters.
 * Delegates to proven_cookie_has_injection via FFI.
 *
 * @param value Cookie value to check.
 * @returns Ok(true/false) or Error.
 */
let hasInjection = (value: string): result<bool, string> => {
  SafeCookieJs.hasInjection(value)->fromJs
}

/**
 * Validate a cookie name per RFC 6265.
 * Delegates to proven_cookie_validate_name via FFI.
 *
 * @param name Cookie name to validate.
 * @returns Ok(true/false) or Error.
 */
let validateName = (name: string): result<bool, string> => {
  SafeCookieJs.validateName(name)->fromJs
}

/**
 * Validate a cookie value per RFC 6265.
 * Delegates to proven_cookie_validate_value via FFI.
 *
 * @param value Cookie value to validate.
 * @returns Ok(true/false) or Error.
 */
let validateValue = (value: string): result<bool, string> => {
  SafeCookieJs.validateValue(value)->fromJs
}

/**
 * Build a delete (expiration) cookie header.
 * Delegates to proven_cookie_build_delete via FFI.
 *
 * @param name Cookie name to delete.
 * @returns Ok(set_cookie_header) or Error.
 */
let buildDelete = (name: string): result<string, string> => {
  SafeCookieJs.buildDelete(name)->fromJs
}
