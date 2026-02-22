// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeHeader - HTTP header operations that prevent CRLF injection.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, readAndFreeString, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Safe HTTP header operations backed by formally verified Idris 2 code.
 */
export class SafeHeader {
  /**
   * Check for CRLF injection characters in a header value.
   *
   * @param value - The header value to check.
   * @returns Result containing a boolean or an error string.
   */
  static hasCrlf(value: string): Result<boolean> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(value);
    const result = symbols.proven_header_has_crlf(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Check if a header name is a valid token per RFC 7230.
   *
   * @param name - The header name to validate.
   * @returns Result containing a boolean or an error string.
   */
  static isValidName(name: string): Result<boolean> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(name);
    const result = symbols.proven_header_is_valid_name(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Check if a header name is in the dangerous headers list.
   *
   * @param name - The header name to check.
   * @returns Result containing a boolean or an error string.
   */
  static isDangerous(name: string): Result<boolean> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(name);
    const result = symbols.proven_header_is_dangerous(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Create a validated header string "Name: Value".
   *
   * @param name - The header name.
   * @param value - The header value.
   * @returns Result containing the rendered header or an error string.
   */
  static render(name: string, value: string): Result<string> {
    const symbols = getLib();
    const nameBytes = new TextEncoder().encode(name);
    const valueBytes = new TextEncoder().encode(value);
    const result = symbols.proven_header_render(
      nameBytes,
      nameBytes.length,
      valueBytes,
      valueBytes.length,
    );
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const rendered = readAndFreeString(result[1], Number(result[2]));
    if (rendered === null) return err('Null string returned');
    return ok(rendered);
  }

  /**
   * Build Content-Security-Policy header value from directives JSON.
   *
   * @param directivesJson - JSON string of CSP directives.
   * @returns Result containing the CSP header value or an error string.
   */
  static buildCsp(directivesJson: string): Result<string> {
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
   * @param maxAge - Max age in seconds.
   * @param includeSubdomains - Whether to include subdomains.
   * @param preload - Whether to include preload directive.
   * @returns Result containing the HSTS header value or an error string.
   */
  static buildHsts(
    maxAge: number,
    includeSubdomains: boolean,
    preload: boolean,
  ): Result<string> {
    const symbols = getLib();
    const result = symbols.proven_header_build_hsts(
      BigInt(maxAge),
      includeSubdomains,
      preload,
    );
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    const hsts = readAndFreeString(result[1], Number(result[2]));
    if (hsts === null) return err('Null string returned');
    return ok(hsts);
  }
}
