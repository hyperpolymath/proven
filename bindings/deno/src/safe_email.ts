// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe email validation.
 */

import { err, ok, type Result } from './result.ts';

export interface EmailParts {
  local: string;
  domain: string;
}

/**
 * Safe email operations.
 */
export class SafeEmail {
  /**
   * Validate an email address.
   *
   * @example
   * ```ts
   * const result = SafeEmail.validate("user@example.com");
   * if (result.ok) {
   *   console.log(result.value.local); // "user"
   *   console.log(result.value.domain); // "example.com"
   * }
   * ```
   */
  static validate(email: string): Result<EmailParts> {
    const trimmed = email.trim();
    if (!trimmed) {
      return err('Empty email');
    }

    const atIndex = trimmed.lastIndexOf('@');
    if (atIndex === -1) {
      return err('Missing @ symbol');
    }

    const local = trimmed.slice(0, atIndex);
    const domain = trimmed.slice(atIndex + 1);

    if (!local) {
      return err('Empty local part');
    }
    if (!domain) {
      return err('Empty domain');
    }
    if (local.length > 64) {
      return err('Local part too long');
    }
    if (domain.length > 255) {
      return err('Domain too long');
    }
    if (!domain.includes('.')) {
      return err('Domain must contain a dot');
    }

    return ok({ local, domain });
  }

  /** Check if an email is valid. */
  static isValid(email: string): boolean {
    return this.validate(email).ok;
  }

  /** Get the local part of an email. */
  static getLocalPart(email: string): Result<string> {
    const result = this.validate(email);
    if (!result.ok) return result;
    return ok(result.value.local);
  }

  /** Get the domain of an email. */
  static getDomain(email: string): Result<string> {
    const result = this.validate(email);
    if (!result.ok) return result;
    return ok(result.value.domain);
  }
}
