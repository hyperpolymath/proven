// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeHeader - HTTP header validation without CRLF injection.
 *
 * Provides safe HTTP header operations.
 * @module
 */

import { ok, err } from './result.js';

/**
 * HTTP header.
 * @typedef {Object} Header
 * @property {string} name - Header name
 * @property {string} value - Header value
 */

/**
 * Safe HTTP header operations.
 */
export class SafeHeader {
  /**
   * Validate and create a header.
   *
   * @param {string} name - Header name
   * @param {string} value - Header value
   * @returns {{ ok: true, value: Header } | { ok: false, error: string }}
   *
   * @example
   * SafeHeader.create("Content-Type", "application/json")
   */
  static create(name, value) {
    // Validate name
    const nameResult = SafeHeader.validateName(name);
    if (!nameResult.ok) {
      return nameResult;
    }

    // Validate value
    const valueResult = SafeHeader.validateValue(value);
    if (!valueResult.ok) {
      return valueResult;
    }

    return ok({
      name: nameResult.value,
      value: valueResult.value,
    });
  }

  /**
   * Validate a header name.
   *
   * @param {string} name - Header name
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static validateName(name) {
    if (typeof name !== 'string') {
      return err('Header name must be a string');
    }

    const trimmed = name.trim();
    if (trimmed.length === 0) {
      return err('Header name cannot be empty');
    }

    // Check for invalid characters (only token characters allowed per RFC 7230)
    // Token = 1*tchar
    // tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
    //         "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
    if (!/^[\w!#$%&'*+\-.^`|~]+$/.test(trimmed)) {
      return err('Header name contains invalid characters');
    }

    // Check for CRLF injection
    if (/[\r\n]/.test(name)) {
      return err('Header name contains CRLF characters');
    }

    return ok(trimmed);
  }

  /**
   * Validate a header value.
   *
   * @param {string} value - Header value
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static validateValue(value) {
    if (typeof value !== 'string') {
      return err('Header value must be a string');
    }

    // Check for CRLF injection
    if (/[\r\n]/.test(value)) {
      return err('Header value contains CRLF characters (injection attempt)');
    }

    // Check for null bytes
    if (value.includes('\x00')) {
      return err('Header value contains null bytes');
    }

    return ok(value.trim());
  }

  /**
   * Sanitize a header value (remove dangerous characters).
   *
   * @param {string} value - Header value to sanitize
   * @returns {string}
   */
  static sanitizeValue(value) {
    // Remove CRLF and null bytes
    return value.replace(/[\r\n\x00]/g, '');
  }

  /**
   * Parse a raw header line.
   *
   * @param {string} line - Raw header line (e.g., "Content-Type: text/html")
   * @returns {{ ok: true, value: Header } | { ok: false, error: string }}
   */
  static parseLine(line) {
    const colonIndex = line.indexOf(':');
    if (colonIndex === -1) {
      return err('Invalid header line: missing colon');
    }

    const name = line.slice(0, colonIndex);
    const value = line.slice(colonIndex + 1);

    return SafeHeader.create(name, value);
  }

  /**
   * Format a header as a string.
   *
   * @param {Header} header - Header to format
   * @returns {string}
   */
  static format(header) {
    return `${header.name}: ${header.value}`;
  }

  /**
   * Check if header name is a standard HTTP header.
   *
   * @param {string} name - Header name
   * @returns {boolean}
   */
  static isStandardHeader(name) {
    const standardHeaders = new Set([
      'accept',
      'accept-charset',
      'accept-encoding',
      'accept-language',
      'accept-ranges',
      'access-control-allow-credentials',
      'access-control-allow-headers',
      'access-control-allow-methods',
      'access-control-allow-origin',
      'access-control-expose-headers',
      'access-control-max-age',
      'access-control-request-headers',
      'access-control-request-method',
      'age',
      'allow',
      'authorization',
      'cache-control',
      'connection',
      'content-disposition',
      'content-encoding',
      'content-language',
      'content-length',
      'content-location',
      'content-range',
      'content-security-policy',
      'content-type',
      'cookie',
      'date',
      'etag',
      'expect',
      'expires',
      'forwarded',
      'from',
      'host',
      'if-match',
      'if-modified-since',
      'if-none-match',
      'if-range',
      'if-unmodified-since',
      'last-modified',
      'location',
      'origin',
      'pragma',
      'proxy-authenticate',
      'proxy-authorization',
      'range',
      'referer',
      'retry-after',
      'server',
      'set-cookie',
      'strict-transport-security',
      'te',
      'trailer',
      'transfer-encoding',
      'upgrade',
      'user-agent',
      'vary',
      'via',
      'warning',
      'www-authenticate',
      'x-content-type-options',
      'x-frame-options',
      'x-xss-protection',
    ]);
    return standardHeaders.has(name.toLowerCase());
  }

  /**
   * Check if header is a security-sensitive header.
   *
   * @param {string} name - Header name
   * @returns {boolean}
   */
  static isSecurityHeader(name) {
    const securityHeaders = new Set([
      'authorization',
      'cookie',
      'set-cookie',
      'x-api-key',
      'x-auth-token',
      'www-authenticate',
      'proxy-authorization',
      'proxy-authenticate',
    ]);
    return securityHeaders.has(name.toLowerCase());
  }

  /**
   * Normalize header name to lowercase.
   *
   * @param {string} name - Header name
   * @returns {string}
   */
  static normalizeName(name) {
    return name.toLowerCase();
  }

  /**
   * Get Content-Type charset from header value.
   *
   * @param {string} contentType - Content-Type header value
   * @returns {string | null}
   */
  static getCharset(contentType) {
    const match = contentType.match(/charset=([^\s;]+)/i);
    return match ? match[1].replace(/["']/g, '') : null;
  }
}
