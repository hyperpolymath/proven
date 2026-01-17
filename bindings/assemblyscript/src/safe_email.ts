// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeEmail - Safe email address handling
 *
 * Provides email validation and safe manipulation
 * following RFC 5321/5322 standards.
 */

import { Result, Option, ErrorCode, isAlphanumeric, isDigit } from "./common";

/**
 * SafeEmail provides validated email address handling.
 */
export class SafeEmail {
  private _local: string;
  private _domain: string;

  private constructor(local: string, domain: string) {
    this._local = local;
    this._domain = domain;
  }

  /**
   * Get the local part (before @).
   */
  get local(): string {
    return this._local;
  }

  /**
   * Get the domain part (after @).
   */
  get domain(): string {
    return this._domain;
  }

  /**
   * Get the full email address.
   */
  get address(): string {
    return this._local + "@" + this._domain;
  }

  /**
   * Parse and validate an email address.
   */
  static parse(email: string): Result<SafeEmail> {
    const trimmed = email.trim();

    if (trimmed.length == 0) {
      return Result.err<SafeEmail>(ErrorCode.InvalidEmail);
    }

    if (trimmed.length > 254) {
      return Result.err<SafeEmail>(ErrorCode.InvalidEmail);
    }

    const atIndex = trimmed.indexOf("@");
    if (atIndex < 1 || atIndex == trimmed.length - 1) {
      return Result.err<SafeEmail>(ErrorCode.InvalidEmail);
    }

    // Check for multiple @ symbols
    if (trimmed.indexOf("@", atIndex + 1) >= 0) {
      return Result.err<SafeEmail>(ErrorCode.InvalidEmail);
    }

    const local = trimmed.substring(0, atIndex);
    const domain = trimmed.substring(atIndex + 1);

    // Validate local part
    if (!SafeEmail.isValidLocal(local)) {
      return Result.err<SafeEmail>(ErrorCode.InvalidEmail);
    }

    // Validate domain part
    if (!SafeEmail.isValidDomain(domain)) {
      return Result.err<SafeEmail>(ErrorCode.InvalidEmail);
    }

    return Result.ok<SafeEmail>(new SafeEmail(local, domain.toLowerCase()));
  }

  /**
   * Check if email format is valid.
   */
  static isValid(email: string): bool {
    return SafeEmail.parse(email).isOk;
  }

  /**
   * Validate local part of email.
   */
  private static isValidLocal(local: string): bool {
    if (local.length == 0 || local.length > 64) return false;

    // Cannot start or end with dot
    if (local.charCodeAt(0) == 46 || local.charCodeAt(local.length - 1) == 46) {
      return false;
    }

    // Check for consecutive dots
    let prevDot = false;
    for (let i = 0; i < local.length; i++) {
      const code = local.charCodeAt(i);

      if (code == 46) {  // .
        if (prevDot) return false;
        prevDot = true;
      } else {
        prevDot = false;
        // Valid characters: alphanumeric and !#$%&'*+/=?^_`{|}~-
        if (!isAlphanumeric(code) && !SafeEmail.isLocalSpecialChar(code)) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * Check if character is valid special character in local part.
   */
  private static isLocalSpecialChar(code: i32): bool {
    // !#$%&'*+/=?^_`{|}~-
    return code == 33 ||  // !
           code == 35 ||  // #
           code == 36 ||  // $
           code == 37 ||  // %
           code == 38 ||  // &
           code == 39 ||  // '
           code == 42 ||  // *
           code == 43 ||  // +
           code == 45 ||  // -
           code == 47 ||  // /
           code == 61 ||  // =
           code == 63 ||  // ?
           code == 94 ||  // ^
           code == 95 ||  // _
           code == 96 ||  // `
           code == 123 || // {
           code == 124 || // |
           code == 125 || // }
           code == 126;   // ~
  }

  /**
   * Validate domain part of email.
   */
  private static isValidDomain(domain: string): bool {
    if (domain.length == 0 || domain.length > 253) return false;

    // Cannot start or end with hyphen or dot
    const first = domain.charCodeAt(0);
    const last = domain.charCodeAt(domain.length - 1);
    if (first == 45 || first == 46 || last == 45 || last == 46) {
      return false;
    }

    // Split into labels
    const labels = domain.split(".");
    if (labels.length < 2) return false;

    for (let i = 0; i < labels.length; i++) {
      const label = labels[i];
      if (label.length == 0 || label.length > 63) return false;

      // Cannot start or end with hyphen
      if (label.charCodeAt(0) == 45 || label.charCodeAt(label.length - 1) == 45) {
        return false;
      }

      // Only alphanumeric and hyphen allowed
      for (let j = 0; j < label.length; j++) {
        const code = label.charCodeAt(j);
        if (!isAlphanumeric(code) && code != 45) {
          return false;
        }
      }
    }

    // TLD should not be all numeric
    const tld = labels[labels.length - 1];
    let allDigits = true;
    for (let i = 0; i < tld.length; i++) {
      if (!isDigit(tld.charCodeAt(i))) {
        allDigits = false;
        break;
      }
    }
    if (allDigits) return false;

    return true;
  }

  /**
   * Normalize an email address.
   */
  static normalize(email: string): Result<string> {
    const parsed = SafeEmail.parse(email);
    if (parsed.isErr()) {
      return Result.err<string>(parsed.error);
    }
    return Result.ok<string>(parsed.unwrap().address);
  }

  /**
   * Get domain from email.
   */
  static extractDomain(email: string): Option<string> {
    const parsed = SafeEmail.parse(email);
    if (parsed.isErr()) {
      return Option.none<string>();
    }
    return Option.some<string>(parsed.unwrap().domain);
  }

  /**
   * Check if email is from a specific domain.
   */
  static isFromDomain(email: string, domain: string): bool {
    const parsed = SafeEmail.parse(email);
    if (parsed.isErr()) return false;
    return parsed.unwrap().domain.toLowerCase() == domain.toLowerCase();
  }

  /**
   * Mask email for display (j***@example.com).
   */
  static mask(email: string): string {
    const parsed = SafeEmail.parse(email);
    if (parsed.isErr()) return "***@***";

    const local = parsed.unwrap().local;
    const domain = parsed.unwrap().domain;

    if (local.length <= 1) {
      return "*@" + domain;
    }

    return local.charAt(0) + "***@" + domain;
  }

  /**
   * Create a subaddress (plus addressing).
   * john@example.com + tag -> john+tag@example.com
   */
  addTag(tag: string): SafeEmail {
    const sanitizedTag = SafeEmail.sanitizeTag(tag);
    if (sanitizedTag.length == 0) return this;
    return new SafeEmail(this._local + "+" + sanitizedTag, this._domain);
  }

  /**
   * Remove plus addressing tag.
   */
  removeTag(): SafeEmail {
    const plusIndex = this._local.indexOf("+");
    if (plusIndex < 0) return this;
    return new SafeEmail(this._local.substring(0, plusIndex), this._domain);
  }

  /**
   * Sanitize a tag for plus addressing.
   */
  private static sanitizeTag(tag: string): string {
    let result = "";
    for (let i = 0; i < tag.length && result.length < 64; i++) {
      const code = tag.charCodeAt(i);
      if (isAlphanumeric(code) || code == 45 || code == 95) {
        result += String.fromCharCode(code);
      }
    }
    return result;
  }
}
