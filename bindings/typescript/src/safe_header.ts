// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * HeaderValue provides safe HTTP header value handling.
 */
export class HeaderValue {
  private readonly value: string;

  private constructor(value: string) {
    this.value = value;
  }

  /**
   * Create a header value, validating it's safe.
   */
  static create(value: string): Result<HeaderValue> {
    if (!HeaderValue.isValid(value)) {
      return { ok: false, error: 'Invalid header value' };
    }
    return { ok: true, value: new HeaderValue(value) };
  }

  /**
   * Create a header value, sanitizing if necessary.
   */
  static sanitized(value: string): HeaderValue {
    return new HeaderValue(HeaderValue.sanitize(value));
  }

  /**
   * Check if a header value is valid (no CRLF injection).
   */
  static isValid(value: string): boolean {
    // Check for CRLF injection attempts
    return !/[\r\n\x00]/.test(value);
  }

  /**
   * Sanitize a header value by removing dangerous characters.
   */
  static sanitize(value: string): string {
    return value.replace(/[\r\n\x00]/g, '');
  }

  toString(): string {
    return this.value;
  }
}

/**
 * HeaderName provides safe HTTP header name handling.
 */
export class HeaderName {
  private readonly name: string;

  private constructor(name: string) {
    this.name = name;
  }

  /**
   * Create a header name, validating it's safe.
   */
  static create(name: string): Result<HeaderName> {
    if (!HeaderName.isValid(name)) {
      return { ok: false, error: 'Invalid header name' };
    }
    return { ok: true, value: new HeaderName(name.toLowerCase()) };
  }

  /**
   * Check if a header name is valid.
   * Header names must be tokens (no special characters).
   */
  static isValid(name: string): boolean {
    if (name.length === 0) return false;
    // RFC 7230: token = 1*tchar
    // tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
    //         "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
    return /^[a-zA-Z0-9!#$%&'*+\-.^_`|~]+$/.test(name);
  }

  toString(): string {
    return this.name;
  }

  equals(other: HeaderName): boolean {
    return this.name === other.name;
  }
}

/**
 * SafeHeaders provides a safe headers collection.
 */
export class SafeHeaders {
  private readonly headers: Map<string, string[]>;

  constructor() {
    this.headers = new Map();
  }

  /**
   * Set a header value (replaces existing).
   */
  set(name: string, value: string): Result<void> {
    const nameResult = HeaderName.create(name);
    if (!nameResult.ok) {
      return { ok: false, error: nameResult.error };
    }

    const valueResult = HeaderValue.create(value);
    if (!valueResult.ok) {
      return { ok: false, error: valueResult.error };
    }

    this.headers.set(nameResult.value!.toString(), [valueResult.value!.toString()]);
    return { ok: true };
  }

  /**
   * Append a header value.
   */
  append(name: string, value: string): Result<void> {
    const nameResult = HeaderName.create(name);
    if (!nameResult.ok) {
      return { ok: false, error: nameResult.error };
    }

    const valueResult = HeaderValue.create(value);
    if (!valueResult.ok) {
      return { ok: false, error: valueResult.error };
    }

    const key = nameResult.value!.toString();
    const existing = this.headers.get(key) || [];
    existing.push(valueResult.value!.toString());
    this.headers.set(key, existing);
    return { ok: true };
  }

  /**
   * Get header value(s).
   */
  get(name: string): string[] | undefined {
    const nameResult = HeaderName.create(name);
    if (!nameResult.ok) return undefined;
    return this.headers.get(nameResult.value!.toString());
  }

  /**
   * Get first header value.
   */
  getFirst(name: string): string | undefined {
    const values = this.get(name);
    return values?.[0];
  }

  /**
   * Check if header exists.
   */
  has(name: string): boolean {
    const nameResult = HeaderName.create(name);
    if (!nameResult.ok) return false;
    return this.headers.has(nameResult.value!.toString());
  }

  /**
   * Delete a header.
   */
  delete(name: string): boolean {
    const nameResult = HeaderName.create(name);
    if (!nameResult.ok) return false;
    return this.headers.delete(nameResult.value!.toString());
  }

  /**
   * Get all header names.
   */
  names(): string[] {
    return Array.from(this.headers.keys());
  }

  /**
   * Get all entries.
   */
  entries(): Array<[string, string[]]> {
    return Array.from(this.headers.entries());
  }

  /**
   * Convert to plain object.
   */
  toObject(): Record<string, string> {
    const result: Record<string, string> = {};
    for (const [name, values] of this.headers) {
      result[name] = values.join(', ');
    }
    return result;
  }
}

/**
 * Security headers with safe defaults.
 */
export const SECURITY_HEADERS = {
  'x-content-type-options': 'nosniff',
  'x-frame-options': 'DENY',
  'x-xss-protection': '1; mode=block',
  'referrer-policy': 'strict-origin-when-cross-origin',
  'content-security-policy': "default-src 'self'",
  'strict-transport-security': 'max-age=31536000; includeSubDomains',
  'permissions-policy': 'geolocation=(), microphone=(), camera=()',
};

/**
 * Check if a header name is a sensitive header.
 */
export function isSensitiveHeader(name: string): boolean {
  const sensitive = new Set([
    'authorization',
    'cookie',
    'set-cookie',
    'x-api-key',
    'x-auth-token',
    'proxy-authorization',
  ]);
  return sensitive.has(name.toLowerCase());
}

/**
 * Check if a header name is a request-only header.
 */
export function isRequestOnlyHeader(name: string): boolean {
  const requestOnly = new Set([
    'accept',
    'accept-language',
    'accept-encoding',
    'host',
    'user-agent',
    'referer',
    'origin',
  ]);
  return requestOnly.has(name.toLowerCase());
}

/**
 * Check if a header name is a response-only header.
 */
export function isResponseOnlyHeader(name: string): boolean {
  const responseOnly = new Set([
    'set-cookie',
    'www-authenticate',
    'proxy-authenticate',
    'server',
    'age',
    'location',
  ]);
  return responseOnly.has(name.toLowerCase());
}

export const SafeHeader = {
  HeaderValue,
  HeaderName,
  SafeHeaders,
  SECURITY_HEADERS,
  isSensitiveHeader,
  isRequestOnlyHeader,
  isResponseOnlyHeader,
};
