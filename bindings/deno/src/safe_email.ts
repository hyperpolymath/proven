// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeEmail - Email validation that cannot crash.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Safe email operations backed by formally verified Idris 2 code.
 */
export class SafeEmail {
  /**
   * Validate an email address (RFC 5321 simplified).
   *
   * @param email - The email address to validate.
   * @returns Result containing a boolean or an error string.
   */
  static isValid(email: string): Result<boolean> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(email);
    const result = symbols.proven_email_is_valid(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }
}

/**
 * Parsed email representation.
 * Created via SafeEmail.isValid() check then string splitting.
 */
export class Email {
  #raw: string;

  constructor(raw: string) {
    this.#raw = raw;
  }

  /**
   * Parse and validate an email string.
   *
   * @param email - The email to parse.
   * @returns Result containing an Email instance or an error string.
   */
  static parse(email: string): Result<Email> {
    const valid = SafeEmail.isValid(email);
    if (!valid.ok) return valid;
    if (!valid.value) return err('Invalid email address');
    return ok(new Email(email));
  }

  /** The full email address. */
  toString(): string {
    return this.#raw;
  }
}
