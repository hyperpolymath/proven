// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCookie - HTTP cookie operations that prevent injection attacks.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, readAndFreeString, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Cookie prefix type constants.
 */
export const CookiePrefix = {
  NONE: 0,
  SECURE: 1,
  HOST: 2,
} as const;

/**
 * Safe cookie operations backed by formally verified Idris 2 code.
 */
export class SafeCookie {
  /**
   * Check for cookie injection characters (semicolon, CR, LF).
   *
   * @param value - The value to check.
   * @returns Result containing a boolean or an error string.
   */
  static hasInjection(value: string): Result<boolean> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(value);
    const result = symbols.proven_cookie_has_injection(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Validate a cookie name.
   *
   * @param name - The cookie name.
   * @returns Result containing a boolean or an error string.
   */
  static validateName(name: string): Result<boolean> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(name);
    const result = symbols.proven_cookie_validate_name(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Validate a cookie value.
   *
   * @param value - The cookie value.
   * @returns Result containing a boolean or an error string.
   */
  static validateValue(value: string): Result<boolean> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(value);
    const result = symbols.proven_cookie_validate_value(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Get cookie prefix type (0=none, 1=__Secure-, 2=__Host-).
   *
   * @param name - The cookie name.
   * @returns Result containing the prefix type or an error string.
   */
  static getPrefix(name: string): Result<number> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(name);
    const result = symbols.proven_cookie_get_prefix(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Build a delete cookie header value (expired Set-Cookie).
   *
   * @param name - The cookie name to delete.
   * @returns Result containing the Set-Cookie header or an error string.
   */
  static buildDelete(name: string): Result<string> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(name);
    const result = symbols.proven_cookie_build_delete(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const header = readAndFreeString(result[1], Number(result[2]));
    if (header === null) return err('Null string returned');
    return ok(header);
  }
}
