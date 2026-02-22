// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeHeader - HTTP header operations that prevent CRLF injection.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError, readAndFreeString } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Header name constants (for convenience, not exhaustive).
 * @readonly
 * @enum {string}
 */
export const HeaderName = Object.freeze({
  CONTENT_TYPE: 'Content-Type',
  CONTENT_LENGTH: 'Content-Length',
  AUTHORIZATION: 'Authorization',
  HOST: 'Host',
  USER_AGENT: 'User-Agent',
  ACCEPT: 'Accept',
  CACHE_CONTROL: 'Cache-Control',
  SET_COOKIE: 'Set-Cookie',
  STRICT_TRANSPORT_SECURITY: 'Strict-Transport-Security',
  CONTENT_SECURITY_POLICY: 'Content-Security-Policy',
});

/**
 * Safe HTTP header operations backed by formally verified Idris 2 code.
 */
export class SafeHeader {
  /**
   * Check for CRLF injection characters in a header value.
   *
   * @param {string} value - The header value to check.
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static hasCrlf(value) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(value);
    const result = symbols.proven_header_has_crlf(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Check if a header name is a valid token per RFC 7230.
   *
   * @param {string} name - The header name to validate.
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static isValidName(name) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(name);
    const result = symbols.proven_header_is_valid_name(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Check if a header name is in the dangerous headers list.
   *
   * @param {string} name - The header name to check.
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static isDangerous(name) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(name);
    const result = symbols.proven_header_is_dangerous(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Create a validated header string "Name: Value".
   *
   * @param {string} name - The header name.
   * @param {string} value - The header value.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static render(name, value) {
    const symbols = getLib();
    const nameBytes = new TextEncoder().encode(name);
    const valueBytes = new TextEncoder().encode(value);
    const result = symbols.proven_header_render(
      nameBytes, nameBytes.length,
      valueBytes, valueBytes.length,
    );
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const rendered = readAndFreeString(result[1], Number(result[2]));
    if (rendered === null) return err('Null string returned');
    return ok(rendered);
  }

  /**
   * Build Content-Security-Policy header value from directives JSON.
   *
   * @param {string} directivesJson - JSON string of CSP directives.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static buildCsp(directivesJson) {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(directivesJson);
    const result = symbols.proven_header_build_csp(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const csp = readAndFreeString(result[1], Number(result[2]));
    if (csp === null) return err('Null string returned');
    return ok(csp);
  }

  /**
   * Build HSTS (Strict-Transport-Security) header value.
   *
   * @param {number} maxAge - Max age in seconds.
   * @param {boolean} includeSubdomains - Whether to include subdomains.
   * @param {boolean} preload - Whether to include preload directive.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static buildHsts(maxAge, includeSubdomains, preload) {
    const symbols = getLib();
    const result = symbols.proven_header_build_hsts(BigInt(maxAge), includeSubdomains, preload);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const hsts = readAndFreeString(result[1], Number(result[2]));
    if (hsts === null) return err('Null string returned');
    return ok(hsts);
  }
}
