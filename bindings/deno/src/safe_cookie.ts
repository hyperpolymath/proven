// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe HTTP Cookie operations that prevent injection attacks.
 *
 * All operations handle injection attacks (semicolons, newlines) and
 * enforce security requirements without throwing.
 *
 * @module
 */

import { type Result, ok, err } from './result.ts';

/** SameSite attribute values */
export type SameSite = 'Strict' | 'Lax' | 'None';

/** Cookie prefix type */
export type CookiePrefix = 'none' | 'secure' | 'host';

/** Cookie attributes */
export interface CookieAttributes {
  readonly domain?: string;
  readonly path?: string;
  readonly maxAge?: number;
  readonly secure: boolean;
  readonly httpOnly: boolean;
  readonly sameSite?: SameSite;
  readonly partitioned: boolean;
}

/** Complete cookie */
export interface Cookie {
  readonly name: string;
  readonly value: string;
  readonly attributes: CookieAttributes;
}

/** Default secure attributes */
export const DEFAULT_ATTRIBUTES: CookieAttributes = {
  path: '/',
  secure: true,
  httpOnly: true,
  sameSite: 'Lax',
  partitioned: false,
};

/** Session cookie attributes */
export const SESSION_ATTRIBUTES: CookieAttributes = {
  path: '/',
  secure: true,
  httpOnly: true,
  sameSite: 'Strict',
  partitioned: false,
};

/** Safe cookie operations */
export const SafeCookie = {
  /** Check for injection characters (semicolon, CR, LF) */
  hasInjection(s: string): boolean {
    return s.includes(';') || s.includes('\r') || s.includes('\n');
  },

  /** Get prefix from cookie name */
  getPrefix(name: string): CookiePrefix {
    if (name.startsWith('__Host-')) {
      return 'host';
    }
    if (name.startsWith('__Secure-')) {
      return 'secure';
    }
    return 'none';
  },

  /** Validate cookie name */
  validateName(name: string): Result<string, string> {
    if (name.length === 0 || name.length > 256) {
      return err('Cookie name length invalid');
    }
    if (this.hasInjection(name)) {
      return err('Cookie name contains injection characters');
    }
    return ok(name);
  },

  /** Validate cookie value */
  validateValue(value: string): Result<string, string> {
    if (value.length > 4096) {
      return err('Cookie value too long');
    }
    if (this.hasInjection(value)) {
      return err('Cookie value contains injection characters');
    }
    return ok(value);
  },

  /** Create a validated cookie */
  make(name: string, value: string, attrs: CookieAttributes): Result<Cookie, string> {
    const nameResult = this.validateName(name);
    if (!nameResult.ok) return nameResult;

    const valueResult = this.validateValue(value);
    if (!valueResult.ok) return valueResult;

    // Validate prefix requirements
    const prefix = this.getPrefix(name);
    if (prefix === 'secure' && !attrs.secure) {
      return err('__Secure- prefix requires Secure flag');
    }
    if (prefix === 'host' && (!attrs.secure || attrs.domain || attrs.path !== '/')) {
      return err('__Host- prefix requires Secure, no Domain, Path=/');
    }

    // Validate SameSite=None requires Secure
    if (attrs.sameSite === 'None' && !attrs.secure) {
      return err('SameSite=None requires Secure');
    }

    return ok({ name, value, attributes: attrs });
  },

  /** Create cookie with default secure attributes */
  makeDefault(name: string, value: string): Result<Cookie, string> {
    return this.make(name, value, DEFAULT_ATTRIBUTES);
  },

  /** Create session cookie */
  makeSession(name: string, value: string): Result<Cookie, string> {
    return this.make(name, value, SESSION_ATTRIBUTES);
  },

  /** Build Set-Cookie header value */
  buildSetCookie(cookie: Cookie): string {
    const parts = [`${cookie.name}=${cookie.value}`];

    if (cookie.attributes.domain) {
      parts.push(`Domain=${cookie.attributes.domain}`);
    }
    if (cookie.attributes.path) {
      parts.push(`Path=${cookie.attributes.path}`);
    }
    if (cookie.attributes.maxAge !== undefined) {
      parts.push(`Max-Age=${cookie.attributes.maxAge}`);
    }
    if (cookie.attributes.secure) {
      parts.push('Secure');
    }
    if (cookie.attributes.httpOnly) {
      parts.push('HttpOnly');
    }
    if (cookie.attributes.sameSite) {
      parts.push(`SameSite=${cookie.attributes.sameSite}`);
    }
    if (cookie.attributes.partitioned) {
      parts.push('Partitioned');
    }

    return parts.join('; ');
  },

  /** Build delete cookie header value */
  buildDelete(name: string): Result<string, string> {
    const nameResult = this.validateName(name);
    if (!nameResult.ok) return nameResult;
    return ok(`${name}=; Max-Age=0; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT`);
  },

  /** Parse Cookie header into name-value pairs */
  parseCookieHeader(header: string): Array<[string, string]> {
    return header
      .split(';')
      .map((pair) => pair.trim())
      .filter((pair) => pair.length > 0)
      .map((pair) => {
        const idx = pair.indexOf('=');
        if (idx === -1) {
          return [pair, ''] as [string, string];
        }
        return [pair.slice(0, idx).trim(), pair.slice(idx + 1).trim()] as [string, string];
      });
  },

  /** Common expiration durations in seconds */
  ONE_HOUR: 3600,
  ONE_DAY: 86400,
  ONE_WEEK: 604800,
  THIRTY_DAYS: 2592000,
  ONE_YEAR: 31536000,
} as const;
