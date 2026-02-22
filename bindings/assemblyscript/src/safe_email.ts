// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeEmail - Safe email address validation.
 *
 * Thin wrapper around libproven's SafeEmail function. Email validation
 * (RFC 5321 compliance) is performed by the formally verified Idris 2
 * implementation via host-provided WASM imports. This module only
 * handles data marshaling.
 *
 * Corresponds to the SafeEmail section in proven.h:
 *   - proven_email_is_valid
 */

import {
  Result,
  RESULT_BUF,
  readBoolResult,
  encodeString,
  encodedPtr,
  encodedLen,
} from "./common";
import { proven_email_is_valid } from "./ffi";

/**
 * SafeEmail provides validated email address handling.
 *
 * The validation method delegates to the corresponding libproven host function.
 * No email validation logic is implemented in AssemblyScript.
 */
export class SafeEmail {
  /**
   * Validate an email address (RFC 5321 simplified).
   * Delegates to proven_email_is_valid (Idris 2 verified).
   *
   * @param email - The email address string to validate.
   * @returns Result<bool> where true means the email is valid.
   */
  static isValid(email: string): Result<bool> {
    encodeString(email);
    proven_email_is_valid(encodedPtr(), encodedLen(), RESULT_BUF);
    return readBoolResult(RESULT_BUF);
  }
}
