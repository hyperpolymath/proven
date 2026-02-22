// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafePassword - Typed wrapper for password operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafePassword as JsSafePassword } from '../../javascript/src/safe_password.js';

/** Result type for password operations. */
export type PasswordResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Password strength levels as determined by libproven.
 */
export enum PasswordStrength {
  VeryWeak = 0,
  Weak = 1,
  Fair = 2,
  Strong = 3,
  VeryStrong = 4,
}

/**
 * Safe password operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export const SafePassword = {
  /**
   * Check if a password is in the common passwords list.
   * Delegates to proven_password_is_common via FFI.
   *
   * @param password - The password to check.
   * @returns true if the password is common.
   */
  isCommon(password: string): boolean {
    return JsSafePassword.isCommon(password);
  },

  /**
   * Validate a password against proven's built-in rules.
   * Delegates to proven_password_validate via FFI.
   *
   * @param password - The password to validate.
   * @returns Validation result from the FFI layer.
   */
  validate(password: string): PasswordResult<unknown> {
    return JsSafePassword.validate(password) as PasswordResult<unknown>;
  },
} as const;
