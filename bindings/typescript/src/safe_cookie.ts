// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

export type SameSite = 'Strict' | 'Lax' | 'None';

export interface CookieOptions {
  domain?: string;
  path?: string;
  expires?: Date;
  maxAge?: number;
  secure?: boolean;
  httpOnly?: boolean;
  sameSite?: SameSite;
  partitioned?: boolean;
}

/**
 * Cookie represents a validated HTTP cookie.
 */
export class Cookie {
  readonly name: string;
  readonly value: string;
  readonly domain?: string;
  readonly path: string;
  readonly expires?: Date;
  readonly maxAge?: number;
  readonly secure: boolean;
  readonly httpOnly: boolean;
  readonly sameSite: SameSite;
  readonly partitioned: boolean;

  private constructor(
    name: string,
    value: string,
    options: CookieOptions = {}
  ) {
    this.name = name;
    this.value = value;
    this.domain = options.domain;
    this.path = options.path ?? '/';
    this.expires = options.expires;
    this.maxAge = options.maxAge;
    this.secure = options.secure ?? false;
    this.httpOnly = options.httpOnly ?? false;
    this.sameSite = options.sameSite ?? 'Lax';
    this.partitioned = options.partitioned ?? false;
  }

  /**
   * Create a cookie with validation.
   */
  static create(
    name: string,
    value: string,
    options: CookieOptions = {}
  ): Result<Cookie> {
    // Validate name (RFC 6265)
    if (!Cookie.isValidName(name)) {
      return { ok: false, error: 'Invalid cookie name' };
    }

    // Validate value
    if (!Cookie.isValidValue(value)) {
      return { ok: false, error: 'Invalid cookie value' };
    }

    // SameSite=None requires Secure
    if (options.sameSite === 'None' && !options.secure) {
      return { ok: false, error: 'SameSite=None requires Secure flag' };
    }

    return { ok: true, value: new Cookie(name, value, options) };
  }

  /**
   * Create a secure cookie (HttpOnly, Secure, SameSite=Strict).
   */
  static secure(
    name: string,
    value: string,
    options: Omit<CookieOptions, 'secure' | 'httpOnly' | 'sameSite'> = {}
  ): Result<Cookie> {
    return Cookie.create(name, value, {
      ...options,
      secure: true,
      httpOnly: true,
      sameSite: 'Strict',
    });
  }

  /**
   * Check if a cookie name is valid per RFC 6265.
   */
  static isValidName(name: string): boolean {
    if (name.length === 0) return false;
    // Cookie names must be tokens
    return /^[a-zA-Z0-9!#$%&'*+\-.^_`|~]+$/.test(name);
  }

  /**
   * Check if a cookie value is valid per RFC 6265.
   */
  static isValidValue(value: string): boolean {
    // Cookie values can be any US-ASCII except control chars, whitespace, quotes, comma, semicolon, backslash
    return /^[\x21\x23-\x2B\x2D-\x3A\x3C-\x5B\x5D-\x7E]*$/.test(value);
  }

  /**
   * Encode a value for use as a cookie value.
   */
  static encodeValue(value: string): string {
    return encodeURIComponent(value);
  }

  /**
   * Decode a cookie value.
   */
  static decodeValue(value: string): string {
    try {
      return decodeURIComponent(value);
    } catch {
      return value;
    }
  }

  /**
   * Convert to Set-Cookie header string.
   */
  toString(): string {
    const parts = [`${this.name}=${this.value}`];

    if (this.domain) parts.push(`Domain=${this.domain}`);
    if (this.path !== '/') parts.push(`Path=${this.path}`);
    if (this.expires) parts.push(`Expires=${this.expires.toUTCString()}`);
    if (this.maxAge !== undefined) parts.push(`Max-Age=${this.maxAge}`);
    if (this.secure) parts.push('Secure');
    if (this.httpOnly) parts.push('HttpOnly');
    parts.push(`SameSite=${this.sameSite}`);
    if (this.partitioned) parts.push('Partitioned');

    return parts.join('; ');
  }

  /**
   * Check if cookie is expired.
   */
  isExpired(now: Date = new Date()): boolean {
    if (this.expires && this.expires < now) {
      return true;
    }
    return false;
  }

  /**
   * Check if cookie has __Host- prefix requirements.
   */
  isHostPrefixed(): boolean {
    return this.name.startsWith('__Host-');
  }

  /**
   * Check if cookie has __Secure- prefix requirements.
   */
  isSecurePrefixed(): boolean {
    return this.name.startsWith('__Secure-');
  }

  /**
   * Validate prefixed cookie requirements.
   */
  validatePrefix(): string[] {
    const errors: string[] = [];

    if (this.isHostPrefixed()) {
      if (!this.secure) errors.push('__Host- cookies must have Secure flag');
      if (this.domain) errors.push('__Host- cookies must not have Domain');
      if (this.path !== '/') errors.push('__Host- cookies must have Path=/');
    }

    if (this.isSecurePrefixed()) {
      if (!this.secure) errors.push('__Secure- cookies must have Secure flag');
    }

    return errors;
  }
}

/**
 * Parse a Cookie header string.
 */
export function parseCookieHeader(header: string): Map<string, string> {
  const cookies = new Map<string, string>();

  for (const pair of header.split(';')) {
    const [name, ...valueParts] = pair.trim().split('=');
    if (name && Cookie.isValidName(name.trim())) {
      const value = valueParts.join('=');
      cookies.set(name.trim(), Cookie.decodeValue(value));
    }
  }

  return cookies;
}

/**
 * Parse a Set-Cookie header string.
 */
export function parseSetCookieHeader(header: string): Result<Cookie> {
  const parts = header.split(';').map((p) => p.trim());
  if (parts.length === 0) {
    return { ok: false, error: 'Empty cookie header' };
  }

  const [nameValue, ...attributes] = parts;
  const [name, ...valueParts] = nameValue.split('=');
  const value = valueParts.join('=');

  if (!name || !Cookie.isValidName(name)) {
    return { ok: false, error: 'Invalid cookie name' };
  }

  const options: CookieOptions = {};

  for (const attr of attributes) {
    const [attrName, attrValue] = attr.split('=');
    const normalizedName = attrName.toLowerCase().trim();

    switch (normalizedName) {
      case 'domain':
        options.domain = attrValue?.trim();
        break;
      case 'path':
        options.path = attrValue?.trim();
        break;
      case 'expires':
        options.expires = new Date(attrValue?.trim() ?? '');
        break;
      case 'max-age':
        options.maxAge = parseInt(attrValue?.trim() ?? '', 10);
        break;
      case 'secure':
        options.secure = true;
        break;
      case 'httponly':
        options.httpOnly = true;
        break;
      case 'samesite':
        const sameSite = attrValue?.trim();
        if (sameSite === 'Strict' || sameSite === 'Lax' || sameSite === 'None') {
          options.sameSite = sameSite;
        }
        break;
      case 'partitioned':
        options.partitioned = true;
        break;
    }
  }

  return Cookie.create(name, value, options);
}

export const SafeCookie = {
  Cookie,
  parseCookieHeader,
  parseSetCookieHeader,
};
