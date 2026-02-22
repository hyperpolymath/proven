// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeEmail - Typed wrapper for email validation that cannot crash.
 *
 * Delegates all validation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafeEmail FFI wrapper. */
module SafeEmailJs = {
  @module("../../javascript/src/safe_email.js") @scope("SafeEmail")
  external isValid: string => jsResult<bool> = "isValid"
}

/**
 * Check if an email address is valid.
 * Delegates to proven_email_is_valid via FFI.
 *
 * @param email The email address to validate.
 * @returns Ok(true/false) or Error.
 */
let isValid = (email: string): result<bool, string> => {
  SafeEmailJs.isValid(email)->fromJs
}
