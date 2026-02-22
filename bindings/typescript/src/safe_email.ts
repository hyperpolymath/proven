// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeEmail - Typed wrapper for email validation that cannot crash.
 *
 * Delegates all validation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeEmail as JsSafeEmail } from '../../javascript/src/safe_email.js';

/** Result type for email operations. */
export type EmailResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Safe email operations backed by formally verified Idris 2 code.
 * All validation delegates to the JavaScript FFI wrapper.
 */
export class SafeEmail {
  /**
   * Check if an email address is valid.
   * Delegates to proven_email_is_valid via FFI.
   *
   * @param email - The email address to validate.
   * @returns Result with boolean validity flag, or error.
   */
  static isValid(email: string): EmailResult<boolean> {
    return JsSafeEmail.isValid(email) as EmailResult<boolean>;
  }
}
