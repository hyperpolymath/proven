// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCookie - HTTP cookie operations that prevent injection attacks.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError, readAndFreeString } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Cookie prefix type constants.
 * @readonly
 * @enum {number}
 */
export const CookiePrefix = Object.freeze({
  NONE: 0,
  SECURE: 1,
  HOST: 2,
});

/**
 * Safe cookie operations backed by formally verified Idris 2 code.
 */
export class SafeCookie {
  /**
   * Check for cookie injection characters (semicolon, CR, LF).
   *
   * @param {string} value - The value to check.
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static hasInjection(value) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(value);
    const result = symbols.proven_cookie_has_injection(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Validate a cookie name.
   *
   * @param {string} name - The cookie name.
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static validateName(name) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(name);
    const result = symbols.proven_cookie_validate_name(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Validate a cookie value.
   *
   * @param {string} value - The cookie value.
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static validateValue(value) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(value);
    const result = symbols.proven_cookie_validate_value(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Get cookie prefix type (0=none, 1=__Secure-, 2=__Host-).
   *
   * @param {string} name - The cookie name.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static getPrefix(name) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(name);
    const result = symbols.proven_cookie_get_prefix(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Build a delete cookie header value (expired Set-Cookie).
   *
   * @param {string} name - The cookie name to delete.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static buildDelete(name) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(name);
    const result = symbols.proven_cookie_build_delete(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const header = readAndFreeString(result[1], Number(result[2]));
    if (header === null) return err('Null string returned');
    return ok(header);
  }
}

/**
 * Cookie representation (for type compatibility with old API).
 */
export class Cookie {
  /** @type {string} */
  name;
  /** @type {string} */
  value;

  /**
   * @param {string} name - Cookie name.
   * @param {string} value - Cookie value.
   */
  constructor(name, value) {
    this.name = name;
    this.value = value;
  }
}
