// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeUrl - URL parsing that cannot crash.
 *
 * Provides safe URL parsing and manipulation.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Parsed URL components.
 * @typedef {Object} ParsedUrl
 * @property {string} protocol - URL protocol (e.g., "https:")
 * @property {string} host - Full host including port
 * @property {string} hostname - Hostname without port
 * @property {string} port - Port number as string
 * @property {string} pathname - URL path
 * @property {string} search - Query string including "?"
 * @property {string} hash - Fragment including "#"
 * @property {string} origin - Protocol + host
 * @property {string} href - Full URL string
 */

/**
 * Safe URL operations.
 */
export class SafeUrl {
  /**
   * Parse a URL string safely.
   *
   * @param {string} urlString - URL to parse
   * @param {string} [base] - Optional base URL
   * @returns {{ ok: true, value: ParsedUrl } | { ok: false, error: string }}
   *
   * @example
   * SafeUrl.parse("https://example.com/path?q=1")
   */
  static parse(urlString, base) {
    try {
      const url = base ? new URL(urlString, base) : new URL(urlString);
      return ok({
        protocol: url.protocol,
        host: url.host,
        hostname: url.hostname,
        port: url.port,
        pathname: url.pathname,
        search: url.search,
        hash: url.hash,
        origin: url.origin,
        href: url.href,
      });
    } catch (error) {
      return err(`Invalid URL: ${error.message}`);
    }
  }

  /**
   * Check if string is a valid URL.
   *
   * @param {string} urlString - URL to validate
   * @returns {boolean}
   */
  static isValid(urlString) {
    return SafeUrl.parse(urlString).ok;
  }

  /**
   * Get query parameter from URL.
   *
   * @param {string} urlString - URL string
   * @param {string} param - Parameter name
   * @returns {{ ok: true, value: string | null } | { ok: false, error: string }}
   */
  static getQueryParam(urlString, param) {
    const parseResult = SafeUrl.parse(urlString);
    if (!parseResult.ok) {
      return parseResult;
    }
    try {
      const url = new URL(urlString);
      return ok(url.searchParams.get(param));
    } catch (error) {
      return err(`Failed to get query param: ${error.message}`);
    }
  }

  /**
   * Get all query parameters.
   *
   * @param {string} urlString - URL string
   * @returns {{ ok: true, value: Record<string, string> } | { ok: false, error: string }}
   */
  static getQueryParams(urlString) {
    try {
      const url = new URL(urlString);
      /** @type {Record<string, string>} */
      const params = {};
      url.searchParams.forEach((value, key) => {
        params[key] = value;
      });
      return ok(params);
    } catch (error) {
      return err(`Invalid URL: ${error.message}`);
    }
  }

  /**
   * Set query parameter on URL.
   *
   * @param {string} urlString - URL string
   * @param {string} param - Parameter name
   * @param {string} value - Parameter value
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static setQueryParam(urlString, param, value) {
    try {
      const url = new URL(urlString);
      url.searchParams.set(param, value);
      return ok(url.href);
    } catch (error) {
      return err(`Invalid URL: ${error.message}`);
    }
  }

  /**
   * Remove query parameter from URL.
   *
   * @param {string} urlString - URL string
   * @param {string} param - Parameter name
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static removeQueryParam(urlString, param) {
    try {
      const url = new URL(urlString);
      url.searchParams.delete(param);
      return ok(url.href);
    } catch (error) {
      return err(`Invalid URL: ${error.message}`);
    }
  }

  /**
   * Join URL paths safely.
   *
   * @param {string} base - Base URL
   * @param {...string} paths - Path segments to join
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static join(base, ...paths) {
    try {
      const url = new URL(base);
      for (const path of paths) {
        const cleanPath = path.replace(/^\/+/, '').replace(/\/+$/, '');
        url.pathname = url.pathname.replace(/\/+$/, '') + '/' + cleanPath;
      }
      return ok(url.href);
    } catch (error) {
      return err(`Invalid URL: ${error.message}`);
    }
  }

  /**
   * Get the domain from a URL.
   *
   * @param {string} urlString - URL string
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static getDomain(urlString) {
    const parseResult = SafeUrl.parse(urlString);
    if (!parseResult.ok) {
      return parseResult;
    }
    return ok(parseResult.value.hostname);
  }

  /**
   * Check if URL uses HTTPS.
   *
   * @param {string} urlString - URL string
   * @returns {boolean}
   */
  static isHttps(urlString) {
    const parseResult = SafeUrl.parse(urlString);
    if (!parseResult.ok) {
      return false;
    }
    return parseResult.value.protocol === 'https:';
  }

  /**
   * Encode URL component safely.
   *
   * @param {string} str - String to encode
   * @returns {string}
   */
  static encode(str) {
    return encodeURIComponent(str);
  }

  /**
   * Decode URL component safely.
   *
   * @param {string} str - String to decode
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static decode(str) {
    try {
      return ok(decodeURIComponent(str));
    } catch (error) {
      return err(`Decode error: ${error.message}`);
    }
  }

  /**
   * Normalize a URL (lowercase scheme/host, remove default port).
   *
   * @param {string} urlString - URL string
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static normalize(urlString) {
    try {
      const url = new URL(urlString);
      // URL constructor already normalizes scheme and host to lowercase
      // and removes default ports
      return ok(url.href);
    } catch (error) {
      return err(`Invalid URL: ${error.message}`);
    }
  }
}
