// SPDX-License-Identifier: PMPL-1.0

/**
 * Safe HTTP Header operations that prevent CRLF injection attacks.
 *
 * All operations handle injection attacks and size limits without throwing.
 * Operations return Result on failure.
 *
 * @module
 */

import { type Result, ok, err } from './result.ts';

/** HTTP header name/value pair */
export interface Header {
  readonly name: string;
  readonly value: string;
}

/** Dangerous headers that should not be set by user code */
const DANGEROUS_HEADERS = new Set([
  'proxy-authorization',
  'proxy-authenticate',
  'proxy-connection',
  'transfer-encoding',
  'content-length',
  'host',
  'connection',
  'keep-alive',
  'upgrade',
  'te',
  'trailer',
]);

/** Valid token characters per RFC 7230 */
const TOKEN_CHARS = new Set(
  '!#$%&\'*+-.^_`|~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
);

/** Safe header operations */
export const SafeHeader = {
  /** Check if a string contains CRLF injection characters */
  hasCrlf(s: string): boolean {
    return s.includes('\r') || s.includes('\n');
  },

  /** Check if header name is a valid token per RFC 7230 */
  isValidName(name: string): boolean {
    if (name.length === 0 || name.length > 256) {
      return false;
    }
    for (const char of name) {
      if (!TOKEN_CHARS.has(char)) {
        return false;
      }
    }
    return true;
  },

  /** Check if header name is in the dangerous headers list */
  isDangerous(name: string): boolean {
    return DANGEROUS_HEADERS.has(name.toLowerCase());
  },

  /** Create a validated header */
  make(name: string, value: string): Result<Header, string> {
    const trimmedName = name.trim();
    const trimmedValue = value.trim();

    if (!this.isValidName(trimmedName)) {
      return err('Invalid header name');
    }

    if (this.hasCrlf(trimmedValue)) {
      return err('Header value contains CRLF injection characters');
    }

    if (trimmedValue.length > 8192) {
      return err('Header value too long');
    }

    return ok({ name: trimmedName, value: trimmedValue });
  },

  /** Create header, blocking dangerous headers */
  makeSafe(name: string, value: string): Result<Header, string> {
    if (this.isDangerous(name)) {
      return err('Dangerous header not allowed');
    }
    return this.make(name, value);
  },

  /** Render header to "Name: Value" format */
  render(header: Header): string {
    return `${header.name}: ${header.value}`;
  },

  /** Build Strict-Transport-Security header value */
  buildHsts(maxAge: number, includeSubdomains = false, preload = false): string {
    let value = `max-age=${maxAge}`;
    if (includeSubdomains) {
      value += '; includeSubDomains';
    }
    if (preload) {
      value += '; preload';
    }
    return value;
  },

  /** Build Content-Security-Policy header value from directives */
  buildCsp(directives: Array<[string, string[]]>): string {
    return directives
      .map(([name, sources]) => (sources.length === 0 ? name : `${name} ${sources.join(' ')}`))
      .join('; ');
  },

  /** Get common security headers preset */
  securityHeaders(): Header[] {
    return [
      { name: 'X-Frame-Options', value: 'DENY' },
      { name: 'X-Content-Type-Options', value: 'nosniff' },
      { name: 'Referrer-Policy', value: 'strict-origin-when-cross-origin' },
      { name: 'X-XSS-Protection', value: '1; mode=block' },
    ];
  },
} as const;
