// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafePassword - Typed wrapper for password operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** Password strength levels as determined by libproven. */
type passwordStrength =
  | VeryWeak
  | Weak
  | Fair
  | Strong
  | VeryStrong

/** Convert an integer strength code to a passwordStrength variant. */
let strengthFromInt = (code: int): passwordStrength => {
  switch code {
  | 0 => VeryWeak
  | 1 => Weak
  | 2 => Fair
  | 3 => Strong
  | 4 => VeryStrong
  | _ => VeryWeak
  }
}

/** JavaScript bindings to the SafePassword FFI wrapper. */
module SafePasswordJs = {
  @module("../../javascript/src/safe_password.js") @scope("SafePassword")
  external isCommon: string => bool = "isCommon"

  @module("../../javascript/src/safe_password.js") @scope("SafePassword")
  external validate: string => jsResult<{..}> = "validate"
}

/**
 * Check if a password is in the common passwords list.
 * Delegates to proven_password_is_common via FFI.
 *
 * @param password The password to check.
 * @returns true if the password is common.
 */
let isCommon = SafePasswordJs.isCommon

/**
 * Validate a password against proven's built-in rules.
 * Delegates to proven_password_validate via FFI.
 *
 * @param password The password to validate.
 * @returns Ok(validation_data) or Error.
 */
let validate = (password: string): result<{..}, string> => {
  SafePasswordJs.validate(password)->fromJs
}
