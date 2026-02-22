// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeHeader - Typed wrapper for HTTP header operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeHeader as JsSafeHeader } from '../../javascript/src/safe_header.js';

/** Result type for header operations. */
export type HeaderResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/** Header name wrapper. */
export type HeaderName = string;

/** Header value wrapper. */
export type HeaderValue = string;

/** Safe headers collection type. */
export type SafeHeaders = Record<string, string>;

/**
 * Safe HTTP header operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export const SafeHeader = {
  /**
   * Check if a header value contains CRLF injection.
   * Delegates to proven_header_has_crlf via FFI.
   *
   * @param value - Header value to check.
   * @returns Result with boolean flag, or error.
   */
  hasCrlf(value: string): HeaderResult<boolean> {
    return JsSafeHeader.hasCrlf(value) as HeaderResult<boolean>;
  },

  /**
   * Check if a header name is valid per HTTP specification.
   * Delegates to proven_header_is_valid_name via FFI.
   *
   * @param name - Header name to validate.
   * @returns Result with boolean validity flag, or error.
   */
  isValidName(name: string): HeaderResult<boolean> {
    return JsSafeHeader.isValidName(name) as HeaderResult<boolean>;
  },

  /**
   * Check if a header name is a dangerous header.
   * Delegates to proven_header_is_dangerous via FFI.
   *
   * @param name - Header name to check.
   * @returns Result with boolean flag, or error.
   */
  isDangerous(name: string): HeaderResult<boolean> {
    return JsSafeHeader.isDangerous(name) as HeaderResult<boolean>;
  },

  /**
   * Render a header name-value pair safely.
   * Delegates to proven_header_render via FFI.
   *
   * @param name - Header name.
   * @param value - Header value.
   * @returns Result with rendered header string, or error.
   */
  render(name: string, value: string): HeaderResult<string> {
    return JsSafeHeader.render(name, value) as HeaderResult<string>;
  },

  /**
   * Build a Content-Security-Policy header.
   * Delegates to proven_header_build_csp via FFI.
   *
   * @param policy - CSP policy string.
   * @returns Result with built CSP header, or error.
   */
  buildCsp(policy: string): HeaderResult<string> {
    return JsSafeHeader.buildCsp(policy) as HeaderResult<string>;
  },

  /**
   * Build an HSTS header.
   * Delegates to proven_header_build_hsts via FFI.
   *
   * @param maxAge - Max age in seconds.
   * @param includeSubDomains - Whether to include subdomains.
   * @param preload - Whether to include preload directive.
   * @returns Result with built HSTS header, or error.
   */
  buildHsts(
    maxAge: number,
    includeSubDomains: boolean,
    preload: boolean,
  ): HeaderResult<string> {
    return JsSafeHeader.buildHsts(maxAge, includeSubDomains, preload) as HeaderResult<string>;
  },
} as const;
