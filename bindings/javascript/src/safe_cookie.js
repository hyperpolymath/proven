// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeCookie - HTTP cookie validation without injection attacks.
 *
 * Provides safe cookie parsing and creation per RFC 6265.
 * @module
 */

import { ok, err } from './result.js';

/**
 * SameSite attribute values.
 * @readonly
 * @enum {string}
 */
export const SameSite = Object.freeze({
  STRICT: 'Strict',
  LAX: 'Lax',
  NONE: 'None',
});

/**
 * Cookie prefix values.
 * @readonly
 * @enum {string}
 */
export const CookiePrefix = Object.freeze({
  HOST: '__Host-',
  SECURE: '__Secure-',
});

/**
 * Cookie attributes.
 * @typedef {Object} CookieAttributes
 * @property {Date} [expires] - Expiration date
 * @property {number} [maxAge] - Max age in seconds
 * @property {string} [domain] - Domain scope
 * @property {string} [path] - Path scope
 * @property {boolean} [secure] - Secure flag
 * @property {boolean} [httpOnly] - HttpOnly flag
 * @property {string} [sameSite] - SameSite attribute
 */

/**
 * Cookie object.
 * @typedef {Object} Cookie
 * @property {string} name - Cookie name
 * @property {string} value - Cookie value
 * @property {CookieAttributes} attributes - Cookie attributes
 */

/**
 * Safe cookie operations.
 */
export class SafeCookie {
  /**
   * Create a cookie with validation.
   *
   * @param {string} name - Cookie name
   * @param {string} value - Cookie value
   * @param {CookieAttributes} [attributes={}] - Cookie attributes
   * @returns {{ ok: true, value: Cookie } | { ok: false, error: string }}
   *
   * @example
   * SafeCookie.create("session", "abc123", { httpOnly: true, secure: true })
   */
  static create(name, value, attributes = {}) {
    // Validate name
    const nameResult = SafeCookie.validateName(name);
    if (!nameResult.ok) {
      return nameResult;
    }

    // Validate value
    const valueResult = SafeCookie.validateValue(value);
    if (!valueResult.ok) {
      return valueResult;
    }

    // Validate prefix requirements
    if (name.startsWith(CookiePrefix.HOST)) {
      if (!attributes.secure) {
        return err('__Host- cookies must have Secure flag');
      }
      if (attributes.domain) {
        return err('__Host- cookies must not have Domain attribute');
      }
      if (attributes.path !== '/') {
        return err("__Host- cookies must have Path='/'");
      }
    } else if (name.startsWith(CookiePrefix.SECURE)) {
      if (!attributes.secure) {
        return err('__Secure- cookies must have Secure flag');
      }
    }

    // SameSite=None requires Secure
    if (attributes.sameSite === SameSite.NONE && !attributes.secure) {
      return err('SameSite=None requires Secure flag');
    }

    return ok({
      name: nameResult.value,
      value: valueResult.value,
      attributes,
    });
  }

  /**
   * Validate a cookie name.
   *
   * @param {string} name - Cookie name
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static validateName(name) {
    if (typeof name !== 'string') {
      return err('Cookie name must be a string');
    }

    if (name.length === 0) {
      return err('Cookie name cannot be empty');
    }

    // Check for invalid characters per RFC 6265
    // cookie-name = token
    if (!/^[\w!#$%&'*+\-.^`|~]+$/.test(name)) {
      return err('Cookie name contains invalid characters');
    }

    return ok(name);
  }

  /**
   * Validate a cookie value.
   *
   * @param {string} value - Cookie value
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static validateValue(value) {
    if (typeof value !== 'string') {
      return err('Cookie value must be a string');
    }

    // Check for invalid characters
    // cookie-value = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
    // cookie-octet = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
    // Simplified: no CTL, whitespace, double quote, comma, semicolon, backslash
    if (/[;\s,\\"]/.test(value)) {
      return err('Cookie value contains invalid characters');
    }

    return ok(value);
  }

  /**
   * Parse a Cookie header value.
   *
   * @param {string} cookieHeader - Cookie header value
   * @returns {Map<string, string>}
   */
  static parse(cookieHeader) {
    const cookies = new Map();

    if (!cookieHeader || typeof cookieHeader !== 'string') {
      return cookies;
    }

    const pairs = cookieHeader.split(';');
    for (const pair of pairs) {
      const trimmed = pair.trim();
      const equalsIndex = trimmed.indexOf('=');
      if (equalsIndex === -1) continue;

      const name = trimmed.slice(0, equalsIndex).trim();
      const value = trimmed.slice(equalsIndex + 1).trim();

      if (name && SafeCookie.validateName(name).ok) {
        cookies.set(name, value);
      }
    }

    return cookies;
  }

  /**
   * Format a cookie for Set-Cookie header.
   *
   * @param {Cookie} cookie - Cookie to format
   * @returns {string}
   */
  static format(cookie) {
    let result = `${cookie.name}=${cookie.value}`;
    const attrs = cookie.attributes;

    if (attrs.expires) {
      result += `; Expires=${attrs.expires.toUTCString()}`;
    }
    if (typeof attrs.maxAge === 'number') {
      result += `; Max-Age=${attrs.maxAge}`;
    }
    if (attrs.domain) {
      result += `; Domain=${attrs.domain}`;
    }
    if (attrs.path) {
      result += `; Path=${attrs.path}`;
    }
    if (attrs.secure) {
      result += '; Secure';
    }
    if (attrs.httpOnly) {
      result += '; HttpOnly';
    }
    if (attrs.sameSite) {
      result += `; SameSite=${attrs.sameSite}`;
    }

    return result;
  }

  /**
   * Create a secure session cookie.
   *
   * @param {string} name - Cookie name
   * @param {string} value - Cookie value
   * @returns {{ ok: true, value: Cookie } | { ok: false, error: string }}
   */
  static createSecure(name, value) {
    return SafeCookie.create(name, value, {
      secure: true,
      httpOnly: true,
      sameSite: SameSite.STRICT,
      path: '/',
    });
  }

  /**
   * Create an expiring/deletion cookie.
   *
   * @param {string} name - Cookie name to delete
   * @param {string} [path='/'] - Cookie path
   * @param {string} [domain] - Cookie domain
   * @returns {Cookie}
   */
  static createExpired(name, path = '/', domain) {
    return {
      name,
      value: '',
      attributes: {
        expires: new Date(0),
        maxAge: 0,
        path,
        domain,
      },
    };
  }

  /**
   * Check if cookie name uses a secure prefix.
   *
   * @param {string} name - Cookie name
   * @returns {boolean}
   */
  static hasSecurePrefix(name) {
    return name.startsWith(CookiePrefix.HOST) || name.startsWith(CookiePrefix.SECURE);
  }

  /**
   * Encode a cookie value (URL-safe).
   *
   * @param {string} value - Value to encode
   * @returns {string}
   */
  static encodeValue(value) {
    return encodeURIComponent(value);
  }

  /**
   * Decode a cookie value.
   *
   * @param {string} value - Value to decode
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static decodeValue(value) {
    try {
      return ok(decodeURIComponent(value));
    } catch {
      return err('Failed to decode cookie value');
    }
  }
}
