// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeUrl - URL parsing that cannot crash.
 *
 * Provides safe URL parsing and validation without regex catastrophic backtracking.
 */

import { getExports, encodeString, decodeString, freePtr } from './wasm.js';
import { statusFromCode } from './error.js';

/**
 * A parsed URL with its components.
 */
export interface ParsedUrl {
  scheme: string;
  host: string;
  port: number | null;
  path: string;
  query: string | null;
  fragment: string | null;
}

/**
 * Safe URL operations with proven correctness guarantees.
 */
export class SafeUrl {
  /**
   * Parse a URL into its components.
   *
   * @example
   * const url = SafeUrl.parse("https://example.com:8080/path?q=1#frag");
   * // { scheme: "https", host: "example.com", port: 8080, ... }
   */
  static parse(url: string): ParsedUrl | null {
    const exports = getExports();
    const fn = exports['proven_url_parse'] as (ptr: number, len: number) => number;

    const { ptr, len } = encodeString(url);
    const resultPtr = fn(ptr, len);
    freePtr(ptr);

    if (resultPtr === 0) {
      return null;
    }

    const memory = exports['memory'] as WebAssembly.Memory;
    const view = new DataView(memory.buffer);

    // Parse the result structure
    let offset = resultPtr;

    const status = view.getInt32(offset, true);
    offset += 4;

    if (statusFromCode(status) !== 'ok') {
      freePtr(resultPtr);
      return null;
    }

    const readString = (): string => {
      const strPtr = view.getUint32(offset, true);
      offset += 4;
      const strLen = view.getUint32(offset, true);
      offset += 4;
      if (strPtr === 0 || strLen === 0) return '';
      return decodeString(strPtr, strLen);
    };

    const scheme = readString();
    const host = readString();
    const port = view.getUint16(offset, true);
    offset += 2;
    const hasPort = view.getUint8(offset) === 1;
    offset += 1;
    const path = readString() || '/';
    const query = readString() || null;
    const fragment = readString() || null;

    freePtr(resultPtr);

    return {
      scheme,
      host,
      port: hasPort ? port : null,
      path,
      query,
      fragment,
    };
  }

  /**
   * Check if a URL is valid.
   */
  static isValid(url: string): boolean {
    return SafeUrl.parse(url) !== null;
  }

  /**
   * Extract the host from a URL.
   */
  static getHost(url: string): string | null {
    const parsed = SafeUrl.parse(url);
    return parsed?.host ?? null;
  }

  /**
   * Extract the scheme from a URL.
   */
  static getScheme(url: string): string | null {
    const parsed = SafeUrl.parse(url);
    return parsed?.scheme ?? null;
  }

  /**
   * Check if a URL uses HTTPS.
   */
  static isHttps(url: string): boolean {
    const scheme = SafeUrl.getScheme(url);
    return scheme?.toLowerCase() === 'https';
  }

  /**
   * Reconstruct a URL from its components.
   */
  static toString(parsed: ParsedUrl): string {
    let result = `${parsed.scheme}://${parsed.host}`;
    if (parsed.port !== null) {
      result += `:${parsed.port}`;
    }
    result += parsed.path;
    if (parsed.query) {
      result += `?${parsed.query}`;
    }
    if (parsed.fragment) {
      result += `#${parsed.fragment}`;
    }
    return result;
  }
}
