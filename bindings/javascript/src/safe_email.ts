// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeEmail - Email validation that cannot crash.
 *
 * Provides safe email validation without regex catastrophic backtracking.
 */

import { getExports, encodeString, freePtr } from './wasm.js';
import { statusFromCode } from './error.js';

/**
 * Safe email operations with proven correctness guarantees.
 */
export class SafeEmail {
  /**
   * Check if an email address is valid.
   *
   * Uses a proven-correct parser that cannot suffer from
   * regex catastrophic backtracking.
   *
   * @example
   * SafeEmail.isValid("user@example.com")  // true
   * SafeEmail.isValid("not-an-email")      // false
   */
  static isValid(email: string): boolean {
    const exports = getExports();
    const fn = exports['proven_email_is_valid'] as (ptr: number, len: number) => number;

    const { ptr, len } = encodeString(email);
    const result = fn(ptr, len);
    freePtr(ptr);

    const status = result >> 16;
    const value = result & 0xffff;

    return statusFromCode(status) === 'ok' && value === 1;
  }

  /**
   * Split an email into local part and domain.
   *
   * @example
   * SafeEmail.split("user@example.com")  // ["user", "example.com"]
   */
  static split(email: string): [string, string] | null {
    if (!SafeEmail.isValid(email)) {
      return null;
    }

    const atPos = email.lastIndexOf('@');
    if (atPos === -1) {
      return null;
    }

    return [email.slice(0, atPos), email.slice(atPos + 1)];
  }

  /**
   * Extract the domain from an email address.
   *
   * @example
   * SafeEmail.getDomain("user@example.com")  // "example.com"
   */
  static getDomain(email: string): string | null {
    const parts = SafeEmail.split(email);
    return parts?.[1] ?? null;
  }

  /**
   * Extract the local part from an email address.
   *
   * @example
   * SafeEmail.getLocalPart("user@example.com")  // "user"
   */
  static getLocalPart(email: string): string | null {
    const parts = SafeEmail.split(email);
    return parts?.[0] ?? null;
  }

  /**
   * Normalize an email address (lowercase domain).
   *
   * @example
   * SafeEmail.normalize("User@EXAMPLE.COM")  // "User@example.com"
   */
  static normalize(email: string): string | null {
    const parts = SafeEmail.split(email);
    if (!parts) return null;
    return `${parts[0]}@${parts[1].toLowerCase()}`;
  }
}
