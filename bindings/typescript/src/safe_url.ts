// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeUrl - Typed wrapper for URL parsing that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeUrl as JsSafeUrl, Url as JsUrl } from '../../javascript/src/safe_url.js';

/** Typed re-export of parsed URL components. */
export interface ParsedUrl {
  scheme: string;
  host: string;
  port: number | null;
  path: string;
  query: string | null;
  fragment: string | null;
}

/** Result type for URL operations. */
export type UrlResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Safe URL operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export class SafeUrl {
  /**
   * Parse a URL into its components.
   * Delegates to the JavaScript FFI URL parser.
   *
   * @param url - URL string to parse.
   * @returns Parsed URL components or null on failure.
   */
  static parse(url: string): ParsedUrl | null {
    const result = JsSafeUrl.parse(url);
    if (!result || !result.ok) return null;
    return result.value as ParsedUrl;
  }

  /**
   * Check if a URL is valid.
   *
   * @param url - URL string to validate.
   * @returns true if the URL can be parsed.
   */
  static isValid(url: string): boolean {
    return SafeUrl.parse(url) !== null;
  }

  /**
   * Extract the host from a URL.
   *
   * @param url - URL string.
   * @returns The host component, or null on parse failure.
   */
  static getHost(url: string): string | null {
    const parsed = SafeUrl.parse(url);
    return parsed?.host ?? null;
  }

  /**
   * Extract the scheme from a URL.
   *
   * @param url - URL string.
   * @returns The scheme component, or null on parse failure.
   */
  static getScheme(url: string): string | null {
    const parsed = SafeUrl.parse(url);
    return parsed?.scheme ?? null;
  }

  /**
   * Check if a URL uses HTTPS.
   *
   * @param url - URL string.
   * @returns true if the scheme is "https".
   */
  static isHttps(url: string): boolean {
    const scheme = SafeUrl.getScheme(url);
    return scheme?.toLowerCase() === 'https';
  }
}
