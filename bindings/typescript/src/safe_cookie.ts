// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCookie - Typed wrapper for cookie operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeCookie as JsSafeCookie, Cookie as JsCookie } from '../../javascript/src/safe_cookie.js';

/** Result type for cookie operations. */
export type CookieResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/** Cookie type re-exported from JavaScript FFI layer. */
export type Cookie = typeof JsCookie;

/**
 * Safe cookie operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export const SafeCookie = {
  /**
   * Check if a cookie value contains injection characters.
   * Delegates to proven_cookie_has_injection via FFI.
   *
   * @param value - Cookie value to check.
   * @returns Result with boolean flag, or error.
   */
  hasInjection(value: string): CookieResult<boolean> {
    return JsSafeCookie.hasInjection(value) as CookieResult<boolean>;
  },

  /**
   * Validate a cookie name per RFC 6265.
   * Delegates to proven_cookie_validate_name via FFI.
   *
   * @param name - Cookie name to validate.
   * @returns Result with boolean validity flag, or error.
   */
  validateName(name: string): CookieResult<boolean> {
    return JsSafeCookie.validateName(name) as CookieResult<boolean>;
  },

  /**
   * Validate a cookie value per RFC 6265.
   * Delegates to proven_cookie_validate_value via FFI.
   *
   * @param value - Cookie value to validate.
   * @returns Result with boolean validity flag, or error.
   */
  validateValue(value: string): CookieResult<boolean> {
    return JsSafeCookie.validateValue(value) as CookieResult<boolean>;
  },

  /**
   * Build a delete (expiration) cookie header.
   * Delegates to proven_cookie_build_delete via FFI.
   *
   * @param name - Cookie name to delete.
   * @returns Result with Set-Cookie header string, or error.
   */
  buildDelete(name: string): CookieResult<string> {
    return JsSafeCookie.buildDelete(name) as CookieResult<string>;
  },
} as const;
