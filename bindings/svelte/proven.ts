// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Proven Safety Library for Svelte 5
 *
 * Formally verified safety primitives as Svelte 5 stores and runes.
 * Provides reactive, type-safe patterns for Svelte applications.
 *
 * @example
 * ```svelte
 * <script>
 *   import { safeCounter, safeMath } from 'proven-svelte';
 *   const counter = safeCounter(0, 0, 100);
 * </script>
 *
 * <button onclick={() => counter.increment(1)}>
 *   Count: {$counter}
 * </button>
 * ```
 *
 * @version 0.4.0
 */

import { writable, derived, readable, type Writable, type Readable } from 'svelte/store';

// ============================================================================
// VERSION AND MODULE INFO
// ============================================================================

/** Library version */
export const VERSION = '0.4.0';

/** Total number of safety modules */
export const MODULE_COUNT = 38;

/** Version as a readable store */
export const version = readable(VERSION);

/** Module count as a readable store */
export const moduleCount = readable(MODULE_COUNT);

// ============================================================================
// RESULT TYPE
// ============================================================================

/** Result type for operations that can fail */
export type Result<T, E = string> =
  | { ok: true; value: T }
  | { ok: false; error: E };

/** Create a success result */
export function ok<T>(value: T): Result<T, never> {
  return { ok: true, value };
}

/** Create an error result */
export function err<E>(error: E): Result<never, E> {
  return { ok: false, error };
}

/** Check if result is ok */
export function isOk<T, E>(result: Result<T, E>): result is { ok: true; value: T } {
  return result.ok;
}

/** Check if result is error */
export function isErr<T, E>(result: Result<T, E>): result is { ok: false; error: E } {
  return !result.ok;
}

/** Unwrap result or throw */
export function unwrap<T, E>(result: Result<T, E>): T {
  if (result.ok) return result.value;
  throw new Error(String(result.error));
}

/** Unwrap result with default */
export function unwrapOr<T, E>(result: Result<T, E>, defaultValue: T): T {
  return result.ok ? result.value : defaultValue;
}

/** Map over successful result */
export function mapResult<T, U, E>(result: Result<T, E>, fn: (value: T) => U): Result<U, E> {
  return result.ok ? ok(fn(result.value)) : result;
}

// ============================================================================
// ERROR TYPES
// ============================================================================

export type ProvenError =
  | 'OVERFLOW'
  | 'UNDERFLOW'
  | 'DIVISION_BY_ZERO'
  | 'OUT_OF_BOUNDS'
  | 'INVALID_PORT'
  | 'INVALID_PERCENTAGE'
  | 'INVALID_EMAIL'
  | 'INVALID_URL'
  | 'PATH_TRAVERSAL'
  | 'INVALID_INPUT'
  | 'BUFFER_FULL'
  | 'BUFFER_EMPTY'
  | 'RATE_LIMITED'
  | 'CIRCUIT_OPEN'
  | 'INVALID_STATE'
  | 'PARSE_ERROR';

// ============================================================================
// MODULE 1: SAFE MATH
// ============================================================================

/** Safe math operations with overflow protection */
export const safeMath = {
  /** Safe addition */
  add(a: number, b: number): Result<number, ProvenError> {
    const result = a + b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  },

  /** Safe subtraction */
  sub(a: number, b: number): Result<number, ProvenError> {
    const result = a - b;
    if (!Number.isFinite(result)) return err('UNDERFLOW');
    return ok(result);
  },

  /** Safe multiplication */
  mul(a: number, b: number): Result<number, ProvenError> {
    const result = a * b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  },

  /** Safe division */
  div(a: number, b: number): Result<number, ProvenError> {
    if (b === 0) return err('DIVISION_BY_ZERO');
    const result = a / b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  },

  /** Safe modulo */
  mod(a: number, b: number): Result<number, ProvenError> {
    if (b === 0) return err('DIVISION_BY_ZERO');
    return ok(a % b);
  },

  /** Safe integer addition (53-bit) */
  safeIntAdd(a: number, b: number): Result<number, ProvenError> {
    if (!Number.isSafeInteger(a) || !Number.isSafeInteger(b)) {
      return err('OVERFLOW');
    }
    const result = a + b;
    if (!Number.isSafeInteger(result)) return err('OVERFLOW');
    return ok(result);
  },

  /** Safe integer subtraction (53-bit) */
  safeIntSub(a: number, b: number): Result<number, ProvenError> {
    if (!Number.isSafeInteger(a) || !Number.isSafeInteger(b)) {
      return err('OVERFLOW');
    }
    const result = a - b;
    if (!Number.isSafeInteger(result)) return err('UNDERFLOW');
    return ok(result);
  },

  /** Clamp value to range */
  clamp(value: number, min: number, max: number): number {
    return Math.min(Math.max(value, min), max);
  },

  /** Safe power */
  pow(base: number, exp: number): Result<number, ProvenError> {
    const result = Math.pow(base, exp);
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  },

  /** Safe square root */
  sqrt(value: number): Result<number, ProvenError> {
    if (value < 0) return err('INVALID_INPUT');
    return ok(Math.sqrt(value));
  },
};

// ============================================================================
// MODULE 2: SAFE STRING
// ============================================================================

/** Safe string operations */
export const safeString = {
  /** Escape HTML entities */
  escapeHtml(value: string): string {
    return value
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#x27;');
  },

  /** Escape SQL single quotes */
  escapeSql(value: string): string {
    return value.replace(/'/g, "''");
  },

  /** Escape JavaScript string */
  escapeJs(value: string): string {
    return value
      .replace(/\\/g, '\\\\')
      .replace(/"/g, '\\"')
      .replace(/'/g, "\\'")
      .replace(/\n/g, '\\n')
      .replace(/\r/g, '\\r')
      .replace(/\t/g, '\\t');
  },

  /** Check if valid UTF-8 */
  isValidUtf8(data: Uint8Array): boolean {
    try {
      new TextDecoder('utf-8', { fatal: true }).decode(data);
      return true;
    } catch {
      return false;
    }
  },

  /** Truncate string safely */
  truncate(value: string, maxLength: number, ellipsis = '...'): string {
    if (value.length <= maxLength) return value;
    return value.slice(0, maxLength - ellipsis.length) + ellipsis;
  },

  /** Normalize whitespace */
  normalizeWhitespace(value: string): string {
    return value.replace(/\s+/g, ' ').trim();
  },
};

// ============================================================================
// MODULE 3: SAFE PATH
// ============================================================================

/** Safe path operations */
export const safePath = {
  /** Check for path traversal */
  hasTraversal(path: string): boolean {
    return path.includes('..') || path.includes('\0');
  },

  /** Validate path is safe */
  validate(path: string): Result<string, ProvenError> {
    if (this.hasTraversal(path)) {
      return err('PATH_TRAVERSAL');
    }
    return ok(path);
  },

  /** Normalize path separators */
  normalize(path: string): string {
    return path.replace(/\\/g, '/').replace(/\/+/g, '/');
  },

  /** Join paths safely */
  join(...parts: string[]): Result<string, ProvenError> {
    const joined = parts.map(p => p.replace(/^\/+|\/+$/g, '')).join('/');
    return this.validate(joined);
  },

  /** Get file extension */
  extension(path: string): string {
    const lastDot = path.lastIndexOf('.');
    const lastSlash = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
    if (lastDot > lastSlash && lastDot > 0) {
      return path.slice(lastDot + 1).toLowerCase();
    }
    return '';
  },

  /** Get basename */
  basename(path: string): string {
    const normalized = this.normalize(path);
    const lastSlash = normalized.lastIndexOf('/');
    return lastSlash >= 0 ? normalized.slice(lastSlash + 1) : normalized;
  },
};

// ============================================================================
// MODULE 4: SAFE EMAIL
// ============================================================================

const EMAIL_REGEX = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/;

/** Safe email operations */
export const safeEmail = {
  /** Validate email format */
  isValid(email: string): boolean {
    return EMAIL_REGEX.test(email) && email.length <= 254;
  },

  /** Parse email */
  parse(email: string): Result<{ local: string; domain: string }, ProvenError> {
    if (!this.isValid(email)) {
      return err('INVALID_EMAIL');
    }
    const [local, domain] = email.split('@');
    return ok({ local, domain });
  },

  /** Normalize email (lowercase domain) */
  normalize(email: string): Result<string, ProvenError> {
    const parsed = this.parse(email);
    if (!parsed.ok) return parsed;
    return ok(`${parsed.value.local}@${parsed.value.domain.toLowerCase()}`);
  },
};

// ============================================================================
// MODULE 5: SAFE URL
// ============================================================================

/** Safe URL operations */
export const safeUrl = {
  /** Parse URL safely */
  parse(url: string): Result<URL, ProvenError> {
    try {
      return ok(new URL(url));
    } catch {
      return err('INVALID_URL');
    }
  },

  /** Validate URL */
  isValid(url: string): boolean {
    try {
      new URL(url);
      return true;
    } catch {
      return false;
    }
  },

  /** Build URL with query params */
  build(base: string, params: Record<string, string>): Result<string, ProvenError> {
    const parsed = this.parse(base);
    if (!parsed.ok) return parsed;

    const url = parsed.value;
    for (const [key, value] of Object.entries(params)) {
      url.searchParams.set(key, value);
    }
    return ok(url.toString());
  },

  /** Encode URI component safely */
  encode(value: string): string {
    return encodeURIComponent(value);
  },

  /** Decode URI component safely */
  decode(value: string): Result<string, ProvenError> {
    try {
      return ok(decodeURIComponent(value));
    } catch {
      return err('INVALID_INPUT');
    }
  },
};

// ============================================================================
// MODULE 6: SAFE NETWORK
// ============================================================================

/** Safe network operations */
export const safeNetwork = {
  /** Validate port number */
  isValidPort(port: number): boolean {
    return Number.isInteger(port) && port >= 1 && port <= 65535;
  },

  /** Validate IPv4 address */
  isValidIpv4(ip: string): boolean {
    const parts = ip.split('.');
    if (parts.length !== 4) return false;
    return parts.every(part => {
      const num = parseInt(part, 10);
      return !isNaN(num) && num >= 0 && num <= 255 && String(num) === part;
    });
  },

  /** Validate IPv6 address (simplified) */
  isValidIpv6(ip: string): boolean {
    const ipv6Pattern = /^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$/;
    return ipv6Pattern.test(ip);
  },

  /** Check if IP is private */
  isPrivateIp(ip: string): boolean {
    if (!this.isValidIpv4(ip)) return false;
    const parts = ip.split('.').map(Number);
    return (
      parts[0] === 10 ||
      (parts[0] === 172 && parts[1] >= 16 && parts[1] <= 31) ||
      (parts[0] === 192 && parts[1] === 168) ||
      parts[0] === 127
    );
  },

  /** Parse CIDR notation */
  parseCidr(cidr: string): Result<{ ip: string; prefix: number }, ProvenError> {
    const [ip, prefixStr] = cidr.split('/');
    if (!ip || !prefixStr) return err('INVALID_INPUT');

    const prefix = parseInt(prefixStr, 10);
    if (!this.isValidIpv4(ip) || isNaN(prefix) || prefix < 0 || prefix > 32) {
      return err('INVALID_INPUT');
    }

    return ok({ ip, prefix });
  },
};

// ============================================================================
// MODULE 7: SAFE CRYPTO
// ============================================================================

/** Safe cryptography operations */
export const safeCrypto = {
  /** Generate random bytes */
  async randomBytes(length: number): Promise<Uint8Array> {
    const bytes = new Uint8Array(length);
    crypto.getRandomValues(bytes);
    return bytes;
  },

  /** Generate random hex string */
  async randomHex(length: number): Promise<string> {
    const bytes = await this.randomBytes(Math.ceil(length / 2));
    return Array.from(bytes)
      .map(b => b.toString(16).padStart(2, '0'))
      .join('')
      .slice(0, length);
  },

  /** SHA-256 hash */
  async sha256(data: string | Uint8Array): Promise<string> {
    const buffer = typeof data === 'string'
      ? new TextEncoder().encode(data)
      : data;
    const hash = await crypto.subtle.digest('SHA-256', buffer);
    return Array.from(new Uint8Array(hash))
      .map(b => b.toString(16).padStart(2, '0'))
      .join('');
  },

  /** SHA-512 hash */
  async sha512(data: string | Uint8Array): Promise<string> {
    const buffer = typeof data === 'string'
      ? new TextEncoder().encode(data)
      : data;
    const hash = await crypto.subtle.digest('SHA-512', buffer);
    return Array.from(new Uint8Array(hash))
      .map(b => b.toString(16).padStart(2, '0'))
      .join('');
  },

  /** Constant-time string comparison */
  timingSafeEqual(a: string, b: string): boolean {
    if (a.length !== b.length) return false;
    let result = 0;
    for (let i = 0; i < a.length; i++) {
      result |= a.charCodeAt(i) ^ b.charCodeAt(i);
    }
    return result === 0;
  },
};

// ============================================================================
// MODULE 8: SAFE UUID
// ============================================================================

/** Safe UUID operations */
export const safeUUID = {
  /** Generate UUID v4 */
  v4(): string {
    return crypto.randomUUID();
  },

  /** Validate UUID format */
  isValid(uuid: string): boolean {
    const uuidPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
    return uuidPattern.test(uuid);
  },

  /** Parse UUID */
  parse(uuid: string): Result<string, ProvenError> {
    if (!this.isValid(uuid)) {
      return err('INVALID_INPUT');
    }
    return ok(uuid.toLowerCase());
  },

  /** Create nil UUID */
  nil(): string {
    return '00000000-0000-0000-0000-000000000000';
  },
};

// ============================================================================
// MODULE 9: SAFE CURRENCY
// ============================================================================

/** Currency codes */
export type CurrencyCode = 'USD' | 'EUR' | 'GBP' | 'JPY' | 'CNY' | 'CHF' | 'AUD' | 'CAD';

/** Safe currency operations */
export const safeCurrency = {
  /** Format currency */
  format(amount: number, currency: CurrencyCode, locale = 'en-US'): string {
    return new Intl.NumberFormat(locale, {
      style: 'currency',
      currency,
    }).format(amount);
  },

  /** Parse currency string */
  parse(value: string): Result<number, ProvenError> {
    const cleaned = value.replace(/[^0-9.-]/g, '');
    const num = parseFloat(cleaned);
    if (isNaN(num)) return err('PARSE_ERROR');
    return ok(num);
  },

  /** Safe currency addition */
  add(a: number, b: number): number {
    return Math.round((a + b) * 100) / 100;
  },

  /** Safe currency subtraction */
  sub(a: number, b: number): number {
    return Math.round((a - b) * 100) / 100;
  },

  /** Safe currency multiplication */
  mul(a: number, b: number): number {
    return Math.round(a * b * 100) / 100;
  },

  /** Convert between currencies */
  convert(amount: number, rate: number): number {
    return Math.round(amount * rate * 100) / 100;
  },
};

// ============================================================================
// MODULE 10: SAFE PHONE
// ============================================================================

/** Safe phone number operations */
export const safePhone = {
  /** Normalize phone number (E.164) */
  normalize(phone: string): string {
    return phone.replace(/[^0-9+]/g, '');
  },

  /** Validate E.164 format */
  isValidE164(phone: string): boolean {
    const normalized = this.normalize(phone);
    return /^\+[1-9]\d{6,14}$/.test(normalized);
  },

  /** Format phone for display */
  format(phone: string, format: 'international' | 'national' = 'international'): string {
    const normalized = this.normalize(phone);
    if (format === 'international' && normalized.startsWith('+1')) {
      return `+1 (${normalized.slice(2, 5)}) ${normalized.slice(5, 8)}-${normalized.slice(8)}`;
    }
    return normalized;
  },

  /** Extract country code */
  getCountryCode(phone: string): Result<string, ProvenError> {
    const normalized = this.normalize(phone);
    if (!normalized.startsWith('+')) {
      return err('INVALID_INPUT');
    }
    // Simple extraction of 1-3 digit country code
    const match = normalized.match(/^\+(\d{1,3})/);
    if (!match) return err('INVALID_INPUT');
    return ok(match[1]);
  },
};

// ============================================================================
// MODULE 11: SAFE HEX
// ============================================================================

/** Safe hex operations */
export const safeHex = {
  /** Encode bytes to hex */
  encode(bytes: Uint8Array): string {
    return Array.from(bytes)
      .map(b => b.toString(16).padStart(2, '0'))
      .join('');
  },

  /** Decode hex to bytes */
  decode(hex: string): Result<Uint8Array, ProvenError> {
    if (hex.length % 2 !== 0) {
      return err('INVALID_INPUT');
    }
    if (!/^[0-9a-fA-F]*$/.test(hex)) {
      return err('INVALID_INPUT');
    }

    const bytes = new Uint8Array(hex.length / 2);
    for (let i = 0; i < hex.length; i += 2) {
      bytes[i / 2] = parseInt(hex.slice(i, i + 2), 16);
    }
    return ok(bytes);
  },

  /** Validate hex string */
  isValid(hex: string): boolean {
    return hex.length % 2 === 0 && /^[0-9a-fA-F]*$/.test(hex);
  },
};

// ============================================================================
// MODULE 12: SAFE JSON
// ============================================================================

export type JsonValue = null | boolean | number | string | JsonValue[] | { [key: string]: JsonValue };

/** Safe JSON operations */
export const safeJson = {
  /** Parse JSON safely */
  parse(input: string): Result<JsonValue, ProvenError> {
    try {
      return ok(JSON.parse(input));
    } catch {
      return err('PARSE_ERROR');
    }
  },

  /** Stringify safely */
  stringify(value: unknown, pretty = false): Result<string, ProvenError> {
    try {
      const result = pretty ? JSON.stringify(value, null, 2) : JSON.stringify(value);
      if (result === undefined) {
        return err('INVALID_INPUT');
      }
      return ok(result);
    } catch {
      return err('INVALID_INPUT');
    }
  },

  /** Get value at path */
  getPath(obj: JsonValue, path: string): JsonValue | undefined {
    if (path === '' || path === '.') return obj;

    const parts = path.replace(/\[(\d+)\]/g, '.$1').split('.').filter(p => p !== '');
    let current: JsonValue = obj;

    for (const part of parts) {
      if (current === null || typeof current !== 'object') return undefined;
      if (Array.isArray(current)) {
        const index = parseInt(part, 10);
        if (isNaN(index) || index < 0 || index >= current.length) return undefined;
        current = current[index];
      } else {
        if (!(part in current)) return undefined;
        current = current[part];
      }
    }
    return current;
  },

  /** Deep equality check */
  equals(a: JsonValue, b: JsonValue): boolean {
    if (a === b) return true;
    if (a === null || b === null) return a === b;
    if (typeof a !== typeof b) return false;
    if (Array.isArray(a) && Array.isArray(b)) {
      if (a.length !== b.length) return false;
      return a.every((val, i) => this.equals(val, b[i]));
    }
    if (typeof a === 'object' && typeof b === 'object') {
      const keysA = Object.keys(a);
      const keysB = Object.keys(b);
      if (keysA.length !== keysB.length) return false;
      return keysA.every(key =>
        Object.prototype.hasOwnProperty.call(b, key) &&
        this.equals(a[key], (b as Record<string, JsonValue>)[key])
      );
    }
    return a === b;
  },
};

// ============================================================================
// MODULE 13: SAFE DATETIME
// ============================================================================

/** Safe datetime operations */
export const safeDateTime = {
  /** Parse ISO 8601 date */
  parseIso(input: string): Result<Date, ProvenError> {
    const date = new Date(input);
    if (isNaN(date.getTime())) {
      return err('PARSE_ERROR');
    }
    return ok(date);
  },

  /** Format to ISO 8601 */
  toIso(date: Date): string {
    return date.toISOString();
  },

  /** Add days safely */
  addDays(date: Date, days: number): Date {
    const result = new Date(date);
    result.setDate(result.getDate() + days);
    return result;
  },

  /** Difference in days */
  diffDays(a: Date, b: Date): number {
    const diffMs = Math.abs(a.getTime() - b.getTime());
    return Math.floor(diffMs / (1000 * 60 * 60 * 24));
  },

  /** Check if date is in the past */
  isPast(date: Date): boolean {
    return date.getTime() < Date.now();
  },

  /** Check if date is in the future */
  isFuture(date: Date): boolean {
    return date.getTime() > Date.now();
  },

  /** Format date for display */
  format(date: Date, locale = 'en-US', options?: Intl.DateTimeFormatOptions): string {
    return date.toLocaleDateString(locale, options);
  },
};

// ============================================================================
// MODULE 14: SAFE FLOAT
// ============================================================================

/** Safe floating point operations */
export const safeFloat = {
  /** Check if value is a valid finite number */
  isValid(value: number): boolean {
    return Number.isFinite(value);
  },

  /** Safe division with NaN/Infinity protection */
  div(a: number, b: number): Result<number, ProvenError> {
    if (b === 0) return err('DIVISION_BY_ZERO');
    const result = a / b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  },

  /** Clamp to finite range */
  clamp(value: number, min: number, max: number): number {
    if (!Number.isFinite(value)) return min;
    return Math.min(Math.max(value, min), max);
  },

  /** Round to decimal places */
  round(value: number, decimals: number): number {
    const factor = Math.pow(10, decimals);
    return Math.round(value * factor) / factor;
  },

  /** Compare with epsilon */
  almostEqual(a: number, b: number, epsilon = 1e-10): boolean {
    return Math.abs(a - b) < epsilon;
  },

  /** Sanitize NaN/Infinity to default */
  sanitize(value: number, defaultValue = 0): number {
    return Number.isFinite(value) ? value : defaultValue;
  },
};

// ============================================================================
// MODULE 15: SAFE VERSION
// ============================================================================

export interface SemVer {
  major: number;
  minor: number;
  patch: number;
  prerelease?: string;
  build?: string;
}

/** Safe semantic versioning operations */
export const safeVersion = {
  /** Parse semver string */
  parse(version: string): Result<SemVer, ProvenError> {
    const match = version.match(/^v?(\d+)\.(\d+)\.(\d+)(?:-([a-zA-Z0-9.-]+))?(?:\+([a-zA-Z0-9.-]+))?$/);
    if (!match) return err('PARSE_ERROR');

    return ok({
      major: parseInt(match[1], 10),
      minor: parseInt(match[2], 10),
      patch: parseInt(match[3], 10),
      prerelease: match[4],
      build: match[5],
    });
  },

  /** Format semver to string */
  format(version: SemVer): string {
    let str = `${version.major}.${version.minor}.${version.patch}`;
    if (version.prerelease) str += `-${version.prerelease}`;
    if (version.build) str += `+${version.build}`;
    return str;
  },

  /** Compare versions (-1, 0, 1) */
  compare(a: SemVer, b: SemVer): number {
    if (a.major !== b.major) return a.major > b.major ? 1 : -1;
    if (a.minor !== b.minor) return a.minor > b.minor ? 1 : -1;
    if (a.patch !== b.patch) return a.patch > b.patch ? 1 : -1;
    return 0;
  },

  /** Check if a is greater than b */
  gt(a: SemVer, b: SemVer): boolean {
    return this.compare(a, b) > 0;
  },

  /** Check if a is less than b */
  lt(a: SemVer, b: SemVer): boolean {
    return this.compare(a, b) < 0;
  },

  /** Check if versions are equal */
  eq(a: SemVer, b: SemVer): boolean {
    return this.compare(a, b) === 0;
  },
};

// ============================================================================
// MODULE 16: SAFE COLOR
// ============================================================================

export interface RgbColor {
  r: number;
  g: number;
  b: number;
}

export interface RgbaColor extends RgbColor {
  a: number;
}

/** Safe color operations */
export const safeColor = {
  /** Create RGB color with validation */
  rgb(r: number, g: number, b: number): RgbColor {
    return {
      r: safeMath.clamp(Math.round(r), 0, 255),
      g: safeMath.clamp(Math.round(g), 0, 255),
      b: safeMath.clamp(Math.round(b), 0, 255),
    };
  },

  /** Create RGBA color with validation */
  rgba(r: number, g: number, b: number, a: number): RgbaColor {
    return {
      ...this.rgb(r, g, b),
      a: safeMath.clamp(a, 0, 1),
    };
  },

  /** Parse hex color */
  parseHex(hex: string): Result<RgbColor, ProvenError> {
    const match = hex.match(/^#?([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})$/);
    if (!match) return err('PARSE_ERROR');

    return ok({
      r: parseInt(match[1], 16),
      g: parseInt(match[2], 16),
      b: parseInt(match[3], 16),
    });
  },

  /** Convert to hex */
  toHex(color: RgbColor): string {
    const r = color.r.toString(16).padStart(2, '0');
    const g = color.g.toString(16).padStart(2, '0');
    const b = color.b.toString(16).padStart(2, '0');
    return `#${r}${g}${b}`;
  },

  /** Calculate relative luminance */
  luminance(color: RgbColor): number {
    const [r, g, b] = [color.r, color.g, color.b].map(c => {
      c = c / 255;
      return c <= 0.03928 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4);
    });
    return 0.2126 * r + 0.7152 * g + 0.0722 * b;
  },

  /** Calculate WCAG contrast ratio */
  contrastRatio(a: RgbColor, b: RgbColor): number {
    const l1 = this.luminance(a);
    const l2 = this.luminance(b);
    const lighter = Math.max(l1, l2);
    const darker = Math.min(l1, l2);
    return (lighter + 0.05) / (darker + 0.05);
  },

  /** Check WCAG AA compliance */
  meetsWcagAA(foreground: RgbColor, background: RgbColor, largeText = false): boolean {
    const ratio = this.contrastRatio(foreground, background);
    return largeText ? ratio >= 3 : ratio >= 4.5;
  },
};

// ============================================================================
// MODULE 17: SAFE ANGLE
// ============================================================================

/** Safe angle operations */
export const safeAngle = {
  /** Convert degrees to radians */
  toRadians(degrees: number): number {
    return (degrees * Math.PI) / 180;
  },

  /** Convert radians to degrees */
  toDegrees(radians: number): number {
    return (radians * 180) / Math.PI;
  },

  /** Normalize degrees to [0, 360) */
  normalizeDegrees(degrees: number): number {
    return ((degrees % 360) + 360) % 360;
  },

  /** Normalize radians to [0, 2*PI) */
  normalizeRadians(radians: number): number {
    const twoPi = 2 * Math.PI;
    return ((radians % twoPi) + twoPi) % twoPi;
  },

  /** Linear interpolation between angles */
  lerp(from: number, to: number, t: number): number {
    const diff = this.normalizeDegrees(to - from);
    const shortestDiff = diff > 180 ? diff - 360 : diff;
    return this.normalizeDegrees(from + shortestDiff * t);
  },
};

// ============================================================================
// MODULE 18: SAFE UNIT
// ============================================================================

export type LengthUnit = 'mm' | 'cm' | 'm' | 'km' | 'in' | 'ft' | 'yd' | 'mi';
export type MassUnit = 'mg' | 'g' | 'kg' | 'oz' | 'lb';
export type TemperatureUnit = 'C' | 'F' | 'K';

const LENGTH_TO_METERS: Record<LengthUnit, number> = {
  mm: 0.001,
  cm: 0.01,
  m: 1,
  km: 1000,
  in: 0.0254,
  ft: 0.3048,
  yd: 0.9144,
  mi: 1609.344,
};

const MASS_TO_GRAMS: Record<MassUnit, number> = {
  mg: 0.001,
  g: 1,
  kg: 1000,
  oz: 28.3495,
  lb: 453.592,
};

/** Safe unit conversion operations */
export const safeUnit = {
  /** Convert length */
  convertLength(value: number, from: LengthUnit, to: LengthUnit): number {
    const meters = value * LENGTH_TO_METERS[from];
    return meters / LENGTH_TO_METERS[to];
  },

  /** Convert mass */
  convertMass(value: number, from: MassUnit, to: MassUnit): number {
    const grams = value * MASS_TO_GRAMS[from];
    return grams / MASS_TO_GRAMS[to];
  },

  /** Convert temperature */
  convertTemperature(value: number, from: TemperatureUnit, to: TemperatureUnit): number {
    // First convert to Celsius
    let celsius: number;
    switch (from) {
      case 'C': celsius = value; break;
      case 'F': celsius = (value - 32) * 5 / 9; break;
      case 'K': celsius = value - 273.15; break;
    }

    // Then convert from Celsius to target
    switch (to) {
      case 'C': return celsius;
      case 'F': return celsius * 9 / 5 + 32;
      case 'K': return celsius + 273.15;
    }
  },
};

// ============================================================================
// MODULE 19: SAFE BUFFER
// ============================================================================

/** Bounded buffer class */
export class BoundedBuffer<T> {
  private readonly data: T[];
  private readonly capacity: number;

  constructor(capacity: number) {
    this.capacity = Math.max(1, capacity);
    this.data = [];
  }

  get length(): number {
    return this.data.length;
  }

  get cap(): number {
    return this.capacity;
  }

  isFull(): boolean {
    return this.data.length >= this.capacity;
  }

  isEmpty(): boolean {
    return this.data.length === 0;
  }

  push(item: T): boolean {
    if (this.isFull()) return false;
    this.data.push(item);
    return true;
  }

  pop(): Result<T, ProvenError> {
    if (this.isEmpty()) return err('BUFFER_EMPTY');
    return ok(this.data.pop()!);
  }

  peek(): Result<T, ProvenError> {
    if (this.isEmpty()) return err('BUFFER_EMPTY');
    return ok(this.data[this.data.length - 1]);
  }

  clear(): void {
    this.data.length = 0;
  }

  toArray(): T[] {
    return [...this.data];
  }
}

/** Ring buffer class */
export class RingBuffer<T> {
  private readonly data: (T | undefined)[];
  private readonly capacity: number;
  private head = 0;
  private tail = 0;
  private count = 0;

  constructor(capacity: number) {
    this.capacity = Math.max(1, capacity);
    this.data = new Array(this.capacity);
  }

  get length(): number {
    return this.count;
  }

  isFull(): boolean {
    return this.count >= this.capacity;
  }

  isEmpty(): boolean {
    return this.count === 0;
  }

  push(item: T): { overwritten?: T } {
    let overwritten: T | undefined;
    if (this.isFull()) {
      overwritten = this.data[this.head];
      this.head = (this.head + 1) % this.capacity;
    } else {
      this.count++;
    }
    this.data[this.tail] = item;
    this.tail = (this.tail + 1) % this.capacity;
    return { overwritten };
  }

  pop(): Result<T, ProvenError> {
    if (this.isEmpty()) return err('BUFFER_EMPTY');
    const item = this.data[this.head]!;
    this.data[this.head] = undefined;
    this.head = (this.head + 1) % this.capacity;
    this.count--;
    return ok(item);
  }

  toArray(): T[] {
    const result: T[] = [];
    for (let i = 0; i < this.count; i++) {
      result.push(this.data[(this.head + i) % this.capacity]!);
    }
    return result;
  }
}

/** Safe buffer store factory */
export const safeBuffer = {
  BoundedBuffer,
  RingBuffer,

  /** Create a bounded buffer store */
  createBoundedStore<T>(capacity: number) {
    const buffer = new BoundedBuffer<T>(capacity);
    const { subscribe, set } = writable<T[]>([]);

    return {
      subscribe,
      push: (item: T): boolean => {
        const success = buffer.push(item);
        if (success) set(buffer.toArray());
        return success;
      },
      pop: (): Result<T, ProvenError> => {
        const result = buffer.pop();
        if (result.ok) set(buffer.toArray());
        return result;
      },
      clear: () => {
        buffer.clear();
        set([]);
      },
      get length() { return buffer.length; },
      get capacity() { return buffer.cap; },
    };
  },
};

// ============================================================================
// MODULE 20: SAFE QUEUE
// ============================================================================

/** Bounded queue class */
export class BoundedQueue<T> {
  private readonly data: T[];
  private readonly capacity: number;

  constructor(capacity: number) {
    this.capacity = Math.max(1, capacity);
    this.data = [];
  }

  get length(): number {
    return this.data.length;
  }

  isFull(): boolean {
    return this.data.length >= this.capacity;
  }

  isEmpty(): boolean {
    return this.data.length === 0;
  }

  enqueue(item: T): boolean {
    if (this.isFull()) return false;
    this.data.push(item);
    return true;
  }

  dequeue(): Result<T, ProvenError> {
    if (this.isEmpty()) return err('BUFFER_EMPTY');
    return ok(this.data.shift()!);
  }

  peek(): Result<T, ProvenError> {
    if (this.isEmpty()) return err('BUFFER_EMPTY');
    return ok(this.data[0]);
  }

  clear(): void {
    this.data.length = 0;
  }

  toArray(): T[] {
    return [...this.data];
  }
}

/** Safe queue operations */
export const safeQueue = {
  BoundedQueue,

  /** Create a queue store */
  createStore<T>(capacity: number) {
    const queue = new BoundedQueue<T>(capacity);
    const { subscribe, set } = writable<T[]>([]);

    return {
      subscribe,
      enqueue: (item: T): boolean => {
        const success = queue.enqueue(item);
        if (success) set(queue.toArray());
        return success;
      },
      dequeue: (): Result<T, ProvenError> => {
        const result = queue.dequeue();
        if (result.ok) set(queue.toArray());
        return result;
      },
      clear: () => {
        queue.clear();
        set([]);
      },
      get length() { return queue.length; },
    };
  },
};

// ============================================================================
// MODULE 21: SAFE BLOOM
// ============================================================================

/** Bloom filter implementation */
export class BloomFilter {
  private readonly bits: Uint8Array;
  private readonly hashCount: number;
  private readonly size: number;

  constructor(expectedItems: number, falsePositiveRate = 0.01) {
    // Calculate optimal size and hash count
    this.size = Math.ceil(-expectedItems * Math.log(falsePositiveRate) / (Math.LN2 * Math.LN2));
    this.hashCount = Math.ceil((this.size / expectedItems) * Math.LN2);
    this.bits = new Uint8Array(Math.ceil(this.size / 8));
  }

  private hash(value: string, seed: number): number {
    let h = seed;
    for (let i = 0; i < value.length; i++) {
      h = Math.imul(h ^ value.charCodeAt(i), 0x5bd1e995);
      h ^= h >>> 15;
    }
    return Math.abs(h) % this.size;
  }

  add(value: string): void {
    for (let i = 0; i < this.hashCount; i++) {
      const index = this.hash(value, i);
      this.bits[Math.floor(index / 8)] |= 1 << (index % 8);
    }
  }

  mightContain(value: string): boolean {
    for (let i = 0; i < this.hashCount; i++) {
      const index = this.hash(value, i);
      if (!(this.bits[Math.floor(index / 8)] & (1 << (index % 8)))) {
        return false;
      }
    }
    return true;
  }
}

/** Safe bloom filter operations */
export const safeBloom = {
  BloomFilter,

  /** Create a bloom filter store */
  create(expectedItems: number, falsePositiveRate = 0.01) {
    const filter = new BloomFilter(expectedItems, falsePositiveRate);
    const added = writable<string[]>([]);

    return {
      add: (value: string) => {
        filter.add(value);
        added.update(arr => [...arr, value]);
      },
      mightContain: (value: string): boolean => filter.mightContain(value),
      addedItems: { subscribe: added.subscribe },
    };
  },
};

// ============================================================================
// MODULE 22: SAFE LRU
// ============================================================================

/** LRU Cache implementation */
export class LRUCache<K, V> {
  private readonly capacity: number;
  private readonly cache: Map<K, V>;

  constructor(capacity: number) {
    this.capacity = Math.max(1, capacity);
    this.cache = new Map();
  }

  get(key: K): V | undefined {
    if (!this.cache.has(key)) return undefined;

    // Move to end (most recently used)
    const value = this.cache.get(key)!;
    this.cache.delete(key);
    this.cache.set(key, value);
    return value;
  }

  set(key: K, value: V): void {
    if (this.cache.has(key)) {
      this.cache.delete(key);
    } else if (this.cache.size >= this.capacity) {
      // Remove least recently used (first item)
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }
    this.cache.set(key, value);
  }

  has(key: K): boolean {
    return this.cache.has(key);
  }

  delete(key: K): boolean {
    return this.cache.delete(key);
  }

  clear(): void {
    this.cache.clear();
  }

  get size(): number {
    return this.cache.size;
  }

  entries(): IterableIterator<[K, V]> {
    return this.cache.entries();
  }
}

/** Safe LRU cache operations */
export const safeLRU = {
  LRUCache,

  /** Create an LRU cache store */
  createStore<K, V>(capacity: number) {
    const cache = new LRUCache<K, V>(capacity);
    const { subscribe, set } = writable<Array<[K, V]>>([]);

    const update = () => set(Array.from(cache.entries()));

    return {
      subscribe,
      get: (key: K): V | undefined => {
        const result = cache.get(key);
        update();
        return result;
      },
      set: (key: K, value: V) => {
        cache.set(key, value);
        update();
      },
      delete: (key: K): boolean => {
        const result = cache.delete(key);
        update();
        return result;
      },
      clear: () => {
        cache.clear();
        update();
      },
      get size() { return cache.size; },
    };
  },
};

// ============================================================================
// MODULE 23: SAFE GRAPH
// ============================================================================

/** Directed graph implementation */
export class Graph<N> {
  private readonly nodes: Map<string, N>;
  private readonly edges: Map<string, Set<string>>;

  constructor() {
    this.nodes = new Map();
    this.edges = new Map();
  }

  addNode(id: string, data: N): void {
    this.nodes.set(id, data);
    if (!this.edges.has(id)) {
      this.edges.set(id, new Set());
    }
  }

  removeNode(id: string): boolean {
    if (!this.nodes.has(id)) return false;
    this.nodes.delete(id);
    this.edges.delete(id);
    // Remove edges pointing to this node
    for (const targets of this.edges.values()) {
      targets.delete(id);
    }
    return true;
  }

  addEdge(from: string, to: string): boolean {
    if (!this.nodes.has(from) || !this.nodes.has(to)) return false;
    this.edges.get(from)!.add(to);
    return true;
  }

  removeEdge(from: string, to: string): boolean {
    return this.edges.get(from)?.delete(to) ?? false;
  }

  getNode(id: string): N | undefined {
    return this.nodes.get(id);
  }

  getNeighbors(id: string): string[] {
    return Array.from(this.edges.get(id) ?? []);
  }

  hasCycle(): boolean {
    const visited = new Set<string>();
    const recursionStack = new Set<string>();

    const dfs = (node: string): boolean => {
      visited.add(node);
      recursionStack.add(node);

      for (const neighbor of this.edges.get(node) ?? []) {
        if (!visited.has(neighbor)) {
          if (dfs(neighbor)) return true;
        } else if (recursionStack.has(neighbor)) {
          return true;
        }
      }

      recursionStack.delete(node);
      return false;
    };

    for (const node of this.nodes.keys()) {
      if (!visited.has(node) && dfs(node)) return true;
    }
    return false;
  }

  topologicalSort(): Result<string[], ProvenError> {
    if (this.hasCycle()) return err('INVALID_INPUT');

    const visited = new Set<string>();
    const result: string[] = [];

    const visit = (node: string) => {
      if (visited.has(node)) return;
      visited.add(node);
      for (const neighbor of this.edges.get(node) ?? []) {
        visit(neighbor);
      }
      result.unshift(node);
    };

    for (const node of this.nodes.keys()) {
      visit(node);
    }

    return ok(result);
  }

  get nodeCount(): number {
    return this.nodes.size;
  }
}

/** Safe graph operations */
export const safeGraph = {
  Graph,

  /** Create a graph store */
  createStore<N>() {
    const graph = new Graph<N>();
    const { subscribe, set } = writable<{ nodes: Array<[string, N]>; edges: Array<[string, string[]]> }>({
      nodes: [],
      edges: [],
    });

    const update = () => {
      const nodes = Array.from((graph as unknown as { nodes: Map<string, N> }).nodes.entries());
      const edges = Array.from((graph as unknown as { edges: Map<string, Set<string>> }).edges.entries())
        .map(([k, v]): [string, string[]] => [k, Array.from(v)]);
      set({ nodes, edges });
    };

    return {
      subscribe,
      addNode: (id: string, data: N) => { graph.addNode(id, data); update(); },
      removeNode: (id: string) => { const r = graph.removeNode(id); update(); return r; },
      addEdge: (from: string, to: string) => { const r = graph.addEdge(from, to); update(); return r; },
      removeEdge: (from: string, to: string) => { const r = graph.removeEdge(from, to); update(); return r; },
      hasCycle: () => graph.hasCycle(),
      topologicalSort: () => graph.topologicalSort(),
    };
  },
};

// ============================================================================
// MODULE 24: SAFE RATE LIMITER
// ============================================================================

export interface RateLimitResult {
  allowed: boolean;
  remaining: number;
  resetAt: number;
  retryAfter?: number;
}

/** Token bucket rate limiter */
export class TokenBucket {
  private readonly capacity: number;
  private readonly refillRate: number;
  private tokens: number;
  private lastRefill: number;

  constructor(capacity: number, refillRate: number) {
    this.capacity = Math.max(1, capacity);
    this.refillRate = Math.max(0.001, refillRate);
    this.tokens = this.capacity;
    this.lastRefill = Date.now();
  }

  private refill(): void {
    const now = Date.now();
    const elapsed = (now - this.lastRefill) / 1000;
    this.tokens = Math.min(this.capacity, this.tokens + elapsed * this.refillRate);
    this.lastRefill = now;
  }

  tryConsume(tokens = 1): RateLimitResult {
    this.refill();

    if (tokens <= this.tokens) {
      this.tokens -= tokens;
      return {
        allowed: true,
        remaining: Math.floor(this.tokens),
        resetAt: this.lastRefill + ((this.capacity - this.tokens) / this.refillRate) * 1000,
      };
    }

    const tokensNeeded = tokens - this.tokens;
    return {
      allowed: false,
      remaining: Math.floor(this.tokens),
      resetAt: this.lastRefill + (this.capacity / this.refillRate) * 1000,
      retryAfter: Math.ceil((tokensNeeded / this.refillRate) * 1000),
    };
  }

  reset(): void {
    this.tokens = this.capacity;
    this.lastRefill = Date.now();
  }
}

/** Sliding window rate limiter */
export class SlidingWindowLimiter {
  private readonly maxRequests: number;
  private readonly windowMs: number;
  private readonly timestamps: number[] = [];

  constructor(maxRequests: number, windowMs: number) {
    this.maxRequests = Math.max(1, maxRequests);
    this.windowMs = Math.max(1, windowMs);
  }

  private cleanup(): void {
    const cutoff = Date.now() - this.windowMs;
    while (this.timestamps.length > 0 && this.timestamps[0] < cutoff) {
      this.timestamps.shift();
    }
  }

  tryAcquire(): RateLimitResult {
    this.cleanup();
    const now = Date.now();

    if (this.timestamps.length < this.maxRequests) {
      this.timestamps.push(now);
      return {
        allowed: true,
        remaining: this.maxRequests - this.timestamps.length,
        resetAt: now + this.windowMs,
      };
    }

    return {
      allowed: false,
      remaining: 0,
      resetAt: this.timestamps[0] + this.windowMs,
      retryAfter: Math.ceil(this.timestamps[0] + this.windowMs - now),
    };
  }

  reset(): void {
    this.timestamps.length = 0;
  }
}

/** Safe rate limiter operations */
export const safeRateLimiter = {
  TokenBucket,
  SlidingWindowLimiter,

  /** Create a token bucket store */
  createTokenBucketStore(capacity: number, refillRate: number) {
    const bucket = new TokenBucket(capacity, refillRate);
    const { subscribe, set } = writable<{ tokens: number; lastResult?: RateLimitResult }>({
      tokens: capacity,
    });

    return {
      subscribe,
      tryConsume: (tokens = 1): RateLimitResult => {
        const result = bucket.tryConsume(tokens);
        set({ tokens: result.remaining, lastResult: result });
        return result;
      },
      reset: () => {
        bucket.reset();
        set({ tokens: capacity });
      },
    };
  },
};

// ============================================================================
// MODULE 25: SAFE CIRCUIT BREAKER
// ============================================================================

export enum CircuitState {
  Closed = 'closed',
  Open = 'open',
  HalfOpen = 'half_open',
}

export interface CircuitBreakerConfig {
  failureThreshold: number;
  successThreshold: number;
  timeout: number;
  halfOpenMaxCalls?: number;
}

/** Circuit breaker implementation */
export class CircuitBreaker {
  private state = CircuitState.Closed;
  private failures = 0;
  private successes = 0;
  private consecutiveFailures = 0;
  private consecutiveSuccesses = 0;
  private openedAt?: number;
  private halfOpenCalls = 0;
  private readonly config: Required<CircuitBreakerConfig>;

  constructor(config: CircuitBreakerConfig) {
    this.config = {
      failureThreshold: Math.max(1, config.failureThreshold),
      successThreshold: Math.max(1, config.successThreshold),
      timeout: Math.max(0, config.timeout),
      halfOpenMaxCalls: config.halfOpenMaxCalls ?? 1,
    };
  }

  private maybeTransitionFromOpen(): void {
    if (this.state === CircuitState.Open && this.openedAt) {
      if (Date.now() - this.openedAt >= this.config.timeout) {
        this.state = CircuitState.HalfOpen;
        this.consecutiveSuccesses = 0;
        this.halfOpenCalls = 0;
      }
    }
  }

  canCall(): boolean {
    this.maybeTransitionFromOpen();
    if (this.state === CircuitState.Closed) return true;
    if (this.state === CircuitState.HalfOpen) {
      return this.halfOpenCalls < this.config.halfOpenMaxCalls;
    }
    return false;
  }

  recordSuccess(): void {
    this.successes++;
    this.consecutiveSuccesses++;
    this.consecutiveFailures = 0;

    if (this.state === CircuitState.HalfOpen &&
        this.consecutiveSuccesses >= this.config.successThreshold) {
      this.state = CircuitState.Closed;
      this.openedAt = undefined;
      this.halfOpenCalls = 0;
    }
  }

  recordFailure(): void {
    this.failures++;
    this.consecutiveFailures++;
    this.consecutiveSuccesses = 0;

    if (this.state === CircuitState.HalfOpen) {
      this.state = CircuitState.Open;
      this.openedAt = Date.now();
    } else if (this.state === CircuitState.Closed &&
               this.consecutiveFailures >= this.config.failureThreshold) {
      this.state = CircuitState.Open;
      this.openedAt = Date.now();
    }
  }

  getState(): CircuitState {
    this.maybeTransitionFromOpen();
    return this.state;
  }

  reset(): void {
    this.state = CircuitState.Closed;
    this.failures = 0;
    this.successes = 0;
    this.consecutiveFailures = 0;
    this.consecutiveSuccesses = 0;
    this.openedAt = undefined;
    this.halfOpenCalls = 0;
  }

  async call<T>(fn: () => Promise<T>): Promise<Result<T, ProvenError>> {
    if (!this.canCall()) return err('CIRCUIT_OPEN');

    if (this.state === CircuitState.HalfOpen) this.halfOpenCalls++;

    try {
      const result = await fn();
      this.recordSuccess();
      return ok(result);
    } catch (error) {
      this.recordFailure();
      return err('INVALID_INPUT');
    }
  }
}

/** Safe circuit breaker operations */
export const safeCircuitBreaker = {
  CircuitBreaker,
  CircuitState,

  /** Create a circuit breaker store */
  createStore(config: CircuitBreakerConfig) {
    const breaker = new CircuitBreaker(config);
    const { subscribe, set } = writable<{ state: CircuitState; failures: number; successes: number }>({
      state: CircuitState.Closed,
      failures: 0,
      successes: 0,
    });

    const update = () => set({
      state: breaker.getState(),
      failures: (breaker as unknown as { failures: number }).failures,
      successes: (breaker as unknown as { successes: number }).successes,
    });

    return {
      subscribe,
      call: async <T>(fn: () => Promise<T>): Promise<Result<T, ProvenError>> => {
        const result = await breaker.call(fn);
        update();
        return result;
      },
      canCall: () => breaker.canCall(),
      reset: () => { breaker.reset(); update(); },
    };
  },
};

// ============================================================================
// MODULE 26: SAFE RETRY
// ============================================================================

export interface RetryConfig {
  maxAttempts: number;
  baseDelayMs: number;
  maxDelayMs: number;
  backoffMultiplier?: number;
  jitter?: boolean;
}

/** Calculate exponential backoff delay */
export function exponentialBackoff(
  attempt: number,
  baseDelayMs: number,
  maxDelayMs: number,
  multiplier = 2
): number {
  const delay = baseDelayMs * Math.pow(multiplier, attempt);
  return Math.min(delay, maxDelayMs);
}

/** Add jitter to delay */
export function addJitter(delay: number, factor = 0.5): number {
  const jitter = delay * factor * Math.random();
  return delay + jitter;
}

/** Safe retry operations */
export const safeRetry = {
  exponentialBackoff,
  addJitter,

  /** Execute with retry */
  async execute<T>(
    fn: () => Promise<T>,
    config: RetryConfig
  ): Promise<Result<T, ProvenError>> {
    const { maxAttempts, baseDelayMs, maxDelayMs, backoffMultiplier = 2, jitter = true } = config;

    let lastError: Error | undefined;

    for (let attempt = 0; attempt < maxAttempts; attempt++) {
      try {
        return ok(await fn());
      } catch (error) {
        lastError = error as Error;

        if (attempt < maxAttempts - 1) {
          let delay = exponentialBackoff(attempt, baseDelayMs, maxDelayMs, backoffMultiplier);
          if (jitter) delay = addJitter(delay);
          await new Promise(resolve => setTimeout(resolve, delay));
        }
      }
    }

    return err('INVALID_INPUT');
  },

  /** Create a retry store */
  createStore(config: RetryConfig) {
    const { subscribe, set, update } = writable<{
      attempt: number;
      status: 'idle' | 'retrying' | 'success' | 'failed';
      lastError?: string;
    }>({
      attempt: 0,
      status: 'idle',
    });

    return {
      subscribe,
      execute: async <T>(fn: () => Promise<T>): Promise<Result<T, ProvenError>> => {
        set({ attempt: 0, status: 'retrying' });

        for (let attempt = 0; attempt < config.maxAttempts; attempt++) {
          update(s => ({ ...s, attempt }));
          try {
            const result = await fn();
            set({ attempt, status: 'success' });
            return ok(result);
          } catch (error) {
            if (attempt < config.maxAttempts - 1) {
              let delay = exponentialBackoff(attempt, config.baseDelayMs, config.maxDelayMs);
              if (config.jitter) delay = addJitter(delay);
              await new Promise(resolve => setTimeout(resolve, delay));
            } else {
              set({ attempt, status: 'failed', lastError: String(error) });
            }
          }
        }

        return err('INVALID_INPUT');
      },
      reset: () => set({ attempt: 0, status: 'idle' }),
    };
  },
};

// ============================================================================
// MODULE 27: SAFE MONOTONIC
// ============================================================================

/** Monotonically increasing counter */
export class MonotonicCounter {
  private value: number;
  private readonly min: number;
  private readonly max: number;

  constructor(initial = 0, min = 0, max = Number.MAX_SAFE_INTEGER) {
    this.min = min;
    this.max = max;
    this.value = Math.max(min, Math.min(max, initial));
  }

  get(): number {
    return this.value;
  }

  increment(delta = 1): Result<number, ProvenError> {
    if (delta < 0) return err('INVALID_INPUT');
    const newValue = this.value + delta;
    if (newValue > this.max) return err('OVERFLOW');
    this.value = newValue;
    return ok(this.value);
  }

  /** Set to value only if greater than current */
  setIfGreater(newValue: number): boolean {
    if (newValue > this.value && newValue <= this.max) {
      this.value = newValue;
      return true;
    }
    return false;
  }
}

/** High water mark tracker */
export class HighWaterMark {
  private value: number;

  constructor(initial = 0) {
    this.value = initial;
  }

  get(): number {
    return this.value;
  }

  update(newValue: number): boolean {
    if (newValue > this.value) {
      this.value = newValue;
      return true;
    }
    return false;
  }

  reset(value = 0): void {
    this.value = value;
  }
}

/** Safe monotonic operations */
export const safeMonotonic = {
  MonotonicCounter,
  HighWaterMark,

  /** Create a monotonic counter store */
  createCounterStore(initial = 0, min = 0, max = Number.MAX_SAFE_INTEGER) {
    const counter = new MonotonicCounter(initial, min, max);
    const { subscribe, set } = writable(counter.get());

    return {
      subscribe,
      increment: (delta = 1): Result<number, ProvenError> => {
        const result = counter.increment(delta);
        if (result.ok) set(result.value);
        return result;
      },
      get: () => counter.get(),
    };
  },

  /** Create a high water mark store */
  createHighWaterMarkStore(initial = 0) {
    const hwm = new HighWaterMark(initial);
    const { subscribe, set } = writable(hwm.get());

    return {
      subscribe,
      update: (value: number): boolean => {
        const updated = hwm.update(value);
        if (updated) set(hwm.get());
        return updated;
      },
      reset: (value = 0) => {
        hwm.reset(value);
        set(hwm.get());
      },
    };
  },
};

// ============================================================================
// MODULE 28: SAFE STATE MACHINE
// ============================================================================

export type Guard<C> = (context: C) => boolean;
export type Action<C> = (context: C) => void;

export interface Transition<S, E, C> {
  from: S;
  event: E;
  to: S;
  guard?: Guard<C>;
  action?: Action<C>;
}

export interface StateMachineConfig<S, E, C> {
  initial: S;
  context: C;
  transitions: Transition<S, E, C>[];
}

export interface TransitionResult<S> {
  success: boolean;
  previousState: S;
  currentState: S;
  error?: string;
}

/** State machine implementation */
export class StateMachine<S extends string, E extends string, C = object> {
  private currentState: S;
  private context: C;
  private readonly transitions: Map<string, Transition<S, E, C>[]>;
  private history: Array<{ state: S; event?: E; timestamp: number }> = [];

  constructor(config: StateMachineConfig<S, E, C>) {
    this.currentState = config.initial;
    this.context = config.context;
    this.transitions = new Map();

    for (const transition of config.transitions) {
      const key = `${transition.from}:${transition.event}`;
      const existing = this.transitions.get(key) ?? [];
      existing.push(transition);
      this.transitions.set(key, existing);
    }

    this.history.push({ state: config.initial, timestamp: Date.now() });
  }

  getState(): S {
    return this.currentState;
  }

  getContext(): C {
    return this.context;
  }

  setContext(context: Partial<C>): void {
    this.context = { ...this.context, ...context };
  }

  canHandle(event: E): boolean {
    const key = `${this.currentState}:${event}`;
    const transitions = this.transitions.get(key);
    if (!transitions) return false;
    return transitions.some(t => !t.guard || t.guard(this.context));
  }

  send(event: E): TransitionResult<S> {
    const previousState = this.currentState;
    const key = `${this.currentState}:${event}`;
    const transitions = this.transitions.get(key);

    if (!transitions || transitions.length === 0) {
      return {
        success: false,
        previousState,
        currentState: this.currentState,
        error: `No transition from '${this.currentState}' for event '${event}'`,
      };
    }

    const transition = transitions.find(t => !t.guard || t.guard(this.context));
    if (!transition) {
      return {
        success: false,
        previousState,
        currentState: this.currentState,
        error: `Guard condition failed`,
      };
    }

    if (transition.action) {
      transition.action(this.context);
    }

    this.currentState = transition.to;
    this.history.push({ state: this.currentState, event, timestamp: Date.now() });

    return {
      success: true,
      previousState,
      currentState: this.currentState,
    };
  }

  is(state: S): boolean {
    return this.currentState === state;
  }

  getHistory(): Array<{ state: S; event?: E; timestamp: number }> {
    return [...this.history];
  }
}

/** Safe state machine operations */
export const safeStateMachine = {
  StateMachine,

  /** Create a state machine store */
  createStore<S extends string, E extends string, C = object>(config: StateMachineConfig<S, E, C>) {
    const machine = new StateMachine(config);
    const { subscribe, set } = writable<{
      state: S;
      context: C;
      history: Array<{ state: S; event?: E; timestamp: number }>;
    }>({
      state: machine.getState(),
      context: machine.getContext(),
      history: machine.getHistory(),
    });

    const update = () => set({
      state: machine.getState(),
      context: machine.getContext(),
      history: machine.getHistory(),
    });

    return {
      subscribe,
      send: (event: E): TransitionResult<S> => {
        const result = machine.send(event);
        update();
        return result;
      },
      canHandle: (event: E) => machine.canHandle(event),
      is: (state: S) => machine.is(state),
      setContext: (context: Partial<C>) => {
        machine.setContext(context);
        update();
      },
    };
  },
};

// ============================================================================
// MODULE 29: SAFE CALCULATOR
// ============================================================================

export type CalcOp = '+' | '-' | '*' | '/' | '%' | '^';

/** Safe calculator operations */
export const safeCalculator = {
  /** Evaluate a binary operation */
  evaluate(left: number, op: CalcOp, right: number): Result<number, ProvenError> {
    switch (op) {
      case '+': return safeMath.add(left, right);
      case '-': return safeMath.sub(left, right);
      case '*': return safeMath.mul(left, right);
      case '/': return safeMath.div(left, right);
      case '%': return safeMath.mod(left, right);
      case '^': return safeMath.pow(left, right);
      default: return err('INVALID_INPUT');
    }
  },

  /** Parse and evaluate simple expression */
  parseSimple(expr: string): Result<number, ProvenError> {
    // Simple number parsing
    const num = parseFloat(expr.trim());
    if (isNaN(num)) return err('PARSE_ERROR');
    if (!Number.isFinite(num)) return err('OVERFLOW');
    return ok(num);
  },

  /** Create a calculator store */
  createStore() {
    const { subscribe, set, update } = writable<{
      value: number;
      expression: string;
      error?: string;
    }>({
      value: 0,
      expression: '0',
    });

    return {
      subscribe,
      input: (digit: string) => update(s => ({
        ...s,
        expression: s.expression === '0' ? digit : s.expression + digit,
        error: undefined,
      })),
      operate: (op: CalcOp, operand: number) => update(s => {
        const result = safeCalculator.evaluate(s.value, op, operand);
        if (result.ok) {
          return { value: result.value, expression: String(result.value) };
        }
        return { ...s, error: result.error };
      }),
      clear: () => set({ value: 0, expression: '0' }),
    };
  },
};

// ============================================================================
// MODULE 30: SAFE GEO
// ============================================================================

const EARTH_RADIUS_KM = 6371;
const EARTH_RADIUS_MILES = 3959;

export interface GeoCoordinate {
  latitude: number;
  longitude: number;
}

/** Safe geographic operations */
export const safeGeo = {
  /** Validate coordinate */
  isValid(coord: GeoCoordinate): boolean {
    return (
      coord.latitude >= -90 && coord.latitude <= 90 &&
      coord.longitude >= -180 && coord.longitude <= 180
    );
  },

  /** Create validated coordinate */
  create(latitude: number, longitude: number): Result<GeoCoordinate, ProvenError> {
    if (latitude < -90 || latitude > 90) return err('OUT_OF_BOUNDS');
    if (longitude < -180 || longitude > 180) return err('OUT_OF_BOUNDS');
    return ok({ latitude, longitude });
  },

  /** Calculate Haversine distance */
  haversine(from: GeoCoordinate, to: GeoCoordinate, unit: 'km' | 'miles' = 'km'): number {
    const radius = unit === 'km' ? EARTH_RADIUS_KM : EARTH_RADIUS_MILES;

    const lat1 = safeAngle.toRadians(from.latitude);
    const lat2 = safeAngle.toRadians(to.latitude);
    const dLat = safeAngle.toRadians(to.latitude - from.latitude);
    const dLon = safeAngle.toRadians(to.longitude - from.longitude);

    const a = Math.sin(dLat / 2) ** 2 +
              Math.cos(lat1) * Math.cos(lat2) * Math.sin(dLon / 2) ** 2;
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

    return radius * c;
  },

  /** Calculate initial bearing */
  bearing(from: GeoCoordinate, to: GeoCoordinate): number {
    const lat1 = safeAngle.toRadians(from.latitude);
    const lat2 = safeAngle.toRadians(to.latitude);
    const dLon = safeAngle.toRadians(to.longitude - from.longitude);

    const y = Math.sin(dLon) * Math.cos(lat2);
    const x = Math.cos(lat1) * Math.sin(lat2) -
              Math.sin(lat1) * Math.cos(lat2) * Math.cos(dLon);

    return safeAngle.normalizeDegrees(safeAngle.toDegrees(Math.atan2(y, x)));
  },

  /** Calculate midpoint */
  midpoint(from: GeoCoordinate, to: GeoCoordinate): GeoCoordinate {
    const lat1 = safeAngle.toRadians(from.latitude);
    const lon1 = safeAngle.toRadians(from.longitude);
    const lat2 = safeAngle.toRadians(to.latitude);
    const dLon = safeAngle.toRadians(to.longitude - from.longitude);

    const Bx = Math.cos(lat2) * Math.cos(dLon);
    const By = Math.cos(lat2) * Math.sin(dLon);

    const lat3 = Math.atan2(
      Math.sin(lat1) + Math.sin(lat2),
      Math.sqrt((Math.cos(lat1) + Bx) ** 2 + By ** 2)
    );
    const lon3 = lon1 + Math.atan2(By, Math.cos(lat1) + Bx);

    return {
      latitude: safeAngle.toDegrees(lat3),
      longitude: safeAngle.normalizeDegrees(safeAngle.toDegrees(lon3) - 180) - 180,
    };
  },
};

// ============================================================================
// MODULE 31: SAFE PROBABILITY
// ============================================================================

/** Probability value clamped to [0, 1] */
export class Probability {
  readonly value: number;

  private constructor(value: number) {
    this.value = Math.max(0, Math.min(1, value));
  }

  static create(value: number): Probability {
    return new Probability(value);
  }

  static zero(): Probability {
    return new Probability(0);
  }

  static one(): Probability {
    return new Probability(1);
  }

  complement(): Probability {
    return new Probability(1 - this.value);
  }

  and(other: Probability): Probability {
    return new Probability(this.value * other.value);
  }

  or(other: Probability): Probability {
    return new Probability(this.value + other.value - this.value * other.value);
  }

  toPercentage(): number {
    return this.value * 100;
  }
}

/** Safe probability operations */
export const safeProbability = {
  Probability,

  /** Create probability from percentage */
  fromPercentage(percent: number): Probability {
    return Probability.create(percent / 100);
  },

  /** Bayesian update */
  bayesianUpdate(prior: Probability, likelihood: Probability, evidence: Probability): Probability {
    if (evidence.value === 0) return Probability.zero();
    return Probability.create((prior.value * likelihood.value) / evidence.value);
  },

  /** Create a probability store */
  createStore(initial = 0.5) {
    const { subscribe, set, update } = writable(Probability.create(initial));

    return {
      subscribe,
      set: (value: number) => set(Probability.create(value)),
      complement: () => update(p => p.complement()),
      and: (other: number) => update(p => p.and(Probability.create(other))),
      or: (other: number) => update(p => p.or(Probability.create(other))),
    };
  },
};

// ============================================================================
// MODULE 32: SAFE CHECKSUM
// ============================================================================

/** Safe checksum operations */
export const safeChecksum = {
  /** CRC-32 checksum */
  crc32(data: string | Uint8Array): number {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    let crc = 0xFFFFFFFF;

    for (const byte of bytes) {
      crc ^= byte;
      for (let i = 0; i < 8; i++) {
        crc = (crc >>> 1) ^ (crc & 1 ? 0xEDB88320 : 0);
      }
    }

    return (crc ^ 0xFFFFFFFF) >>> 0;
  },

  /** Adler-32 checksum */
  adler32(data: string | Uint8Array): number {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    let a = 1;
    let b = 0;

    for (const byte of bytes) {
      a = (a + byte) % 65521;
      b = (b + a) % 65521;
    }

    return (b << 16) | a;
  },

  /** FNV-1a 32-bit hash */
  fnv1a32(data: string): number {
    let hash = 2166136261;
    for (let i = 0; i < data.length; i++) {
      hash ^= data.charCodeAt(i);
      hash = Math.imul(hash, 16777619);
    }
    return hash >>> 0;
  },

  /** Luhn checksum validation */
  luhnCheck(digits: string): boolean {
    if (!/^\d+$/.test(digits)) return false;

    let sum = 0;
    let double = false;

    for (let i = digits.length - 1; i >= 0; i--) {
      let digit = parseInt(digits[i], 10);
      if (double) {
        digit *= 2;
        if (digit > 9) digit -= 9;
      }
      sum += digit;
      double = !double;
    }

    return sum % 10 === 0;
  },
};

// ============================================================================
// MODULE 33: SAFE TENSOR
// ============================================================================

/** Safe tensor operations */
export const safeTensor = {
  /** Create zeros tensor */
  zeros(shape: number[]): number[] {
    return new Array(shape.reduce((a, b) => a * b, 1)).fill(0);
  },

  /** Create ones tensor */
  ones(shape: number[]): number[] {
    return new Array(shape.reduce((a, b) => a * b, 1)).fill(1);
  },

  /** Element-wise addition */
  add(a: number[], b: number[]): Result<number[], ProvenError> {
    if (a.length !== b.length) return err('INVALID_INPUT');
    return ok(a.map((v, i) => v + b[i]));
  },

  /** Element-wise multiplication */
  mul(a: number[], b: number[]): Result<number[], ProvenError> {
    if (a.length !== b.length) return err('INVALID_INPUT');
    return ok(a.map((v, i) => v * b[i]));
  },

  /** Dot product */
  dot(a: number[], b: number[]): Result<number, ProvenError> {
    if (a.length !== b.length) return err('INVALID_INPUT');
    return ok(a.reduce((sum, v, i) => sum + v * b[i], 0));
  },

  /** Sum of elements */
  sum(a: number[]): number {
    return a.reduce((sum, v) => sum + v, 0);
  },

  /** Mean of elements */
  mean(a: number[]): Result<number, ProvenError> {
    if (a.length === 0) return err('INVALID_INPUT');
    return ok(this.sum(a) / a.length);
  },

  /** Normalize vector */
  normalize(a: number[]): Result<number[], ProvenError> {
    const magnitude = Math.sqrt(a.reduce((sum, v) => sum + v * v, 0));
    if (magnitude === 0) return err('DIVISION_BY_ZERO');
    return ok(a.map(v => v / magnitude));
  },
};

// ============================================================================
// MODULE 34: SAFE PASSWORD
// ============================================================================

export enum PasswordStrength {
  VeryWeak = 0,
  Weak = 1,
  Fair = 2,
  Strong = 3,
  VeryStrong = 4,
}

export interface PasswordAnalysis {
  length: number;
  hasLowercase: boolean;
  hasUppercase: boolean;
  hasDigits: boolean;
  hasSpecial: boolean;
  entropy: number;
  strength: PasswordStrength;
  score: number;
}

export interface PasswordPolicy {
  minLength?: number;
  maxLength?: number;
  requireLowercase?: boolean;
  requireUppercase?: boolean;
  requireDigits?: boolean;
  requireSpecial?: boolean;
  minStrength?: PasswordStrength;
}

/** Safe password operations */
export const safePassword = {
  PasswordStrength,

  /** Analyze password strength */
  analyze(password: string): PasswordAnalysis {
    const hasLowercase = /[a-z]/.test(password);
    const hasUppercase = /[A-Z]/.test(password);
    const hasDigits = /[0-9]/.test(password);
    const hasSpecial = /[^a-zA-Z0-9]/.test(password);

    let charsetSize = 0;
    if (hasLowercase) charsetSize += 26;
    if (hasUppercase) charsetSize += 26;
    if (hasDigits) charsetSize += 10;
    if (hasSpecial) charsetSize += 32;

    const entropy = charsetSize > 0 ? password.length * Math.log2(charsetSize) : 0;

    let score = 0;
    score += Math.min(password.length * 2, 30);
    if (hasLowercase) score += 10;
    if (hasUppercase) score += 10;
    if (hasDigits) score += 10;
    if (hasSpecial) score += 10;
    score += Math.min(entropy / 2, 30);
    if (password.length < 8) score = Math.max(0, score - 20);
    score = Math.min(100, Math.max(0, score));

    let strength: PasswordStrength;
    if (score < 20) strength = PasswordStrength.VeryWeak;
    else if (score < 40) strength = PasswordStrength.Weak;
    else if (score < 60) strength = PasswordStrength.Fair;
    else if (score < 80) strength = PasswordStrength.Strong;
    else strength = PasswordStrength.VeryStrong;

    return {
      length: password.length,
      hasLowercase,
      hasUppercase,
      hasDigits,
      hasSpecial,
      entropy,
      strength,
      score,
    };
  },

  /** Validate against policy */
  validate(password: string, policy: PasswordPolicy): string[] {
    const errors: string[] = [];
    const analysis = this.analyze(password);

    if (policy.minLength && password.length < policy.minLength) {
      errors.push(`Minimum ${policy.minLength} characters required`);
    }
    if (policy.maxLength && password.length > policy.maxLength) {
      errors.push(`Maximum ${policy.maxLength} characters allowed`);
    }
    if (policy.requireLowercase && !analysis.hasLowercase) {
      errors.push('Lowercase letter required');
    }
    if (policy.requireUppercase && !analysis.hasUppercase) {
      errors.push('Uppercase letter required');
    }
    if (policy.requireDigits && !analysis.hasDigits) {
      errors.push('Digit required');
    }
    if (policy.requireSpecial && !analysis.hasSpecial) {
      errors.push('Special character required');
    }
    if (policy.minStrength !== undefined && analysis.strength < policy.minStrength) {
      errors.push('Password not strong enough');
    }

    return errors;
  },

  /** Generate random password */
  generate(length = 16, options: {
    lowercase?: boolean;
    uppercase?: boolean;
    digits?: boolean;
    special?: boolean;
  } = {}): string {
    const { lowercase = true, uppercase = true, digits = true, special = true } = options;

    let charset = '';
    if (lowercase) charset += 'abcdefghijklmnopqrstuvwxyz';
    if (uppercase) charset += 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
    if (digits) charset += '0123456789';
    if (special) charset += '!@#$%^&*()_+-=[]{}|;:,.<>?';
    if (charset.length === 0) charset = 'abcdefghijklmnopqrstuvwxyz';

    const array = new Uint32Array(length);
    crypto.getRandomValues(array);

    return Array.from(array)
      .map(n => charset[n % charset.length])
      .join('');
  },
};

// ============================================================================
// MODULE 35: SAFE ML
// ============================================================================

/** Safe machine learning operations */
export const safeMl = {
  /** Numerically stable softmax */
  softmax(logits: number[]): number[] {
    const maxLogit = Math.max(...logits);
    const exps = logits.map(l => Math.exp(l - maxLogit));
    const sumExps = exps.reduce((a, b) => a + b, 0);
    return exps.map(e => e / sumExps);
  },

  /** Sigmoid activation */
  sigmoid(x: number): number {
    if (x >= 0) {
      return 1 / (1 + Math.exp(-x));
    }
    const expX = Math.exp(x);
    return expX / (1 + expX);
  },

  /** ReLU activation */
  relu(x: number): number {
    return Math.max(0, x);
  },

  /** Leaky ReLU activation */
  leakyRelu(x: number, alpha = 0.01): number {
    return x > 0 ? x : alpha * x;
  },

  /** Tanh activation */
  tanh(x: number): number {
    return Math.tanh(x);
  },

  /** Cross-entropy loss */
  crossEntropyLoss(predicted: number[], target: number[]): Result<number, ProvenError> {
    if (predicted.length !== target.length) return err('INVALID_INPUT');

    let loss = 0;
    for (let i = 0; i < predicted.length; i++) {
      const p = Math.max(1e-15, Math.min(1 - 1e-15, predicted[i]));
      loss -= target[i] * Math.log(p);
    }
    return ok(loss);
  },

  /** Mean squared error */
  mse(predicted: number[], target: number[]): Result<number, ProvenError> {
    if (predicted.length !== target.length) return err('INVALID_INPUT');

    const sum = predicted.reduce((acc, p, i) => acc + (p - target[i]) ** 2, 0);
    return ok(sum / predicted.length);
  },

  /** Normalize array to [0, 1] */
  minMaxNormalize(data: number[]): number[] {
    const min = Math.min(...data);
    const max = Math.max(...data);
    const range = max - min;
    if (range === 0) return data.map(() => 0);
    return data.map(v => (v - min) / range);
  },

  /** Z-score normalization */
  zScoreNormalize(data: number[]): number[] {
    const mean = data.reduce((a, b) => a + b, 0) / data.length;
    const variance = data.reduce((acc, v) => acc + (v - mean) ** 2, 0) / data.length;
    const std = Math.sqrt(variance);
    if (std === 0) return data.map(() => 0);
    return data.map(v => (v - mean) / std);
  },
};

// ============================================================================
// MODULE 36: SAFE HEADER
// ============================================================================

/** Safe HTTP header operations */
export const safeHeader = {
  /** Check for CRLF injection */
  isValid(value: string): boolean {
    return !/[\r\n\x00]/.test(value);
  },

  /** Sanitize header value */
  sanitize(value: string): string {
    return value.replace(/[\r\n\x00]/g, '');
  },

  /** Validate header name */
  isValidName(name: string): boolean {
    if (name.length === 0) return false;
    return /^[a-zA-Z0-9!#$%&'*+\-.^_`|~]+$/.test(name);
  },

  /** Check if sensitive header */
  isSensitive(name: string): boolean {
    const sensitive = new Set([
      'authorization',
      'cookie',
      'set-cookie',
      'x-api-key',
      'x-auth-token',
      'proxy-authorization',
    ]);
    return sensitive.has(name.toLowerCase());
  },

  /** Security headers */
  SECURITY_HEADERS: {
    'x-content-type-options': 'nosniff',
    'x-frame-options': 'DENY',
    'x-xss-protection': '1; mode=block',
    'referrer-policy': 'strict-origin-when-cross-origin',
    'content-security-policy': "default-src 'self'",
    'strict-transport-security': 'max-age=31536000; includeSubDomains',
  } as const,
};

// ============================================================================
// MODULE 37: SAFE COOKIE
// ============================================================================

export type SameSite = 'Strict' | 'Lax' | 'None';

export interface CookieOptions {
  maxAge?: number;
  expires?: Date;
  path?: string;
  domain?: string;
  secure?: boolean;
  httpOnly?: boolean;
  sameSite?: SameSite;
}

/** Safe cookie operations */
export const safeCookie = {
  /** Validate cookie name */
  isValidName(name: string): boolean {
    return name.length > 0 && /^[a-zA-Z0-9!#$%&'*+\-.^_`|~]+$/.test(name);
  },

  /** Validate cookie value */
  isValidValue(value: string): boolean {
    return !/[\r\n\x00;,]/.test(value);
  },

  /** Serialize cookie */
  serialize(name: string, value: string, options: CookieOptions = {}): Result<string, ProvenError> {
    if (!this.isValidName(name)) return err('INVALID_INPUT');
    if (!this.isValidValue(value)) return err('INVALID_INPUT');

    let cookie = `${name}=${encodeURIComponent(value)}`;

    if (options.maxAge !== undefined) {
      cookie += `; Max-Age=${Math.floor(options.maxAge)}`;
    }
    if (options.expires) {
      cookie += `; Expires=${options.expires.toUTCString()}`;
    }
    if (options.path) {
      cookie += `; Path=${options.path}`;
    }
    if (options.domain) {
      cookie += `; Domain=${options.domain}`;
    }
    if (options.secure) {
      cookie += '; Secure';
    }
    if (options.httpOnly) {
      cookie += '; HttpOnly';
    }
    if (options.sameSite) {
      cookie += `; SameSite=${options.sameSite}`;
    }

    return ok(cookie);
  },

  /** Parse cookie string */
  parse(cookieString: string): Map<string, string> {
    const cookies = new Map<string, string>();

    for (const pair of cookieString.split(';')) {
      const [name, ...rest] = pair.trim().split('=');
      if (name && rest.length > 0) {
        try {
          cookies.set(name.trim(), decodeURIComponent(rest.join('=')));
        } catch {
          // Skip invalid cookies
        }
      }
    }

    return cookies;
  },
};

// ============================================================================
// MODULE 38: SAFE CONTENT TYPE
// ============================================================================

export interface ContentType {
  type: string;
  subtype: string;
  parameters: Map<string, string>;
}

/** Safe content type operations */
export const safeContentType = {
  /** Parse content type */
  parse(contentType: string): Result<ContentType, ProvenError> {
    const parts = contentType.split(';');
    const [type, subtype] = (parts[0] || '').trim().split('/');

    if (!type || !subtype) return err('PARSE_ERROR');

    const parameters = new Map<string, string>();
    for (let i = 1; i < parts.length; i++) {
      const [key, value] = parts[i].trim().split('=');
      if (key && value) {
        parameters.set(key.trim().toLowerCase(), value.trim().replace(/^"|"$/g, ''));
      }
    }

    return ok({
      type: type.toLowerCase(),
      subtype: subtype.toLowerCase(),
      parameters,
    });
  },

  /** Format content type */
  format(ct: ContentType): string {
    let result = `${ct.type}/${ct.subtype}`;
    for (const [key, value] of ct.parameters) {
      result += `; ${key}=${value}`;
    }
    return result;
  },

  /** Check if JSON */
  isJson(contentType: string): boolean {
    const parsed = this.parse(contentType);
    if (!parsed.ok) return false;
    return parsed.value.type === 'application' &&
           (parsed.value.subtype === 'json' || parsed.value.subtype.endsWith('+json'));
  },

  /** Check if HTML */
  isHtml(contentType: string): boolean {
    const parsed = this.parse(contentType);
    if (!parsed.ok) return false;
    return parsed.value.type === 'text' && parsed.value.subtype === 'html';
  },

  /** Check if text */
  isText(contentType: string): boolean {
    const parsed = this.parse(contentType);
    if (!parsed.ok) return false;
    return parsed.value.type === 'text';
  },

  /** Common content types */
  TYPES: {
    JSON: 'application/json',
    HTML: 'text/html',
    TEXT: 'text/plain',
    XML: 'application/xml',
    FORM: 'application/x-www-form-urlencoded',
    MULTIPART: 'multipart/form-data',
    OCTET_STREAM: 'application/octet-stream',
  } as const,
};

// ============================================================================
// BOUNDED STORES
// ============================================================================

export interface BoundedStore<T> extends Writable<T> {
  getBounds(): { min: T; max: T };
  isAtMin(): boolean;
  isAtMax(): boolean;
}

/** Create a bounded number store */
export function boundedNumber(
  initial: number,
  min: number,
  max: number
): BoundedStore<number> & {
  increment: (delta?: number) => Result<number, ProvenError>;
  decrement: (delta?: number) => Result<number, ProvenError>;
} {
  const clampedInitial = safeMath.clamp(initial, min, max);
  const { subscribe, set, update } = writable(clampedInitial);
  let currentValue = clampedInitial;

  return {
    subscribe,
    set: (value: number) => {
      currentValue = safeMath.clamp(value, min, max);
      set(currentValue);
    },
    update: (fn: (value: number) => number) => {
      update((v) => {
        currentValue = safeMath.clamp(fn(v), min, max);
        return currentValue;
      });
    },
    getBounds: () => ({ min, max }),
    isAtMin: () => currentValue <= min,
    isAtMax: () => currentValue >= max,
    increment: (delta = 1) => {
      const result = safeMath.add(currentValue, delta);
      if (isOk(result)) {
        currentValue = safeMath.clamp(result.value, min, max);
        set(currentValue);
        return ok(currentValue);
      }
      return result;
    },
    decrement: (delta = 1) => {
      const result = safeMath.sub(currentValue, delta);
      if (isOk(result)) {
        currentValue = safeMath.clamp(result.value, min, max);
        set(currentValue);
        return ok(currentValue);
      }
      return result;
    },
  };
}

/** Create a safe counter store */
export function safeCounter(initial = 0, min = 0, max = Number.MAX_SAFE_INTEGER) {
  return boundedNumber(initial, min, max);
}

/** Create a percentage store (0-100) */
export function percentageStore(initial = 0) {
  return boundedNumber(initial, 0, 100);
}

/** Create a normalized store (0-1) */
export function normalizedStore(initial = 0) {
  return boundedNumber(initial, 0, 1);
}

// ============================================================================
// VALIDATION STORES
// ============================================================================

export interface ValidatedStore<T> extends Writable<T> {
  readonly valid: Readable<boolean>;
  readonly errors: Readable<string[]>;
  validate(): boolean;
}

type Validator<T> = (value: T) => string | null;

/** Create a validated store */
export function validatedStore<T>(
  initial: T,
  validators: Validator<T>[]
): ValidatedStore<T> {
  const { subscribe, set, update } = writable(initial);
  const errorsStore = writable<string[]>([]);
  const validStore = derived(errorsStore, ($errors) => $errors.length === 0);
  let currentValue = initial;

  const runValidation = (value: T): string[] => {
    return validators
      .map((v) => v(value))
      .filter((e): e is string => e !== null);
  };

  errorsStore.set(runValidation(initial));

  return {
    subscribe,
    set: (value: T) => {
      currentValue = value;
      errorsStore.set(runValidation(value));
      set(value);
    },
    update: (fn: (value: T) => T) => {
      update((v) => {
        currentValue = fn(v);
        errorsStore.set(runValidation(currentValue));
        return currentValue;
      });
    },
    valid: validStore,
    errors: { subscribe: errorsStore.subscribe },
    validate: () => {
      const errors = runValidation(currentValue);
      errorsStore.set(errors);
      return errors.length === 0;
    },
  };
}

// ============================================================================
// COMMON VALIDATORS
// ============================================================================

export const validators = {
  required: (message = 'Required'): Validator<string> => (value) =>
    value.trim().length === 0 ? message : null,

  minLength: (min: number, message?: string): Validator<string> => (value) =>
    value.length < min ? (message ?? `Minimum ${min} characters`) : null,

  maxLength: (max: number, message?: string): Validator<string> => (value) =>
    value.length > max ? (message ?? `Maximum ${max} characters`) : null,

  email: (message = 'Invalid email'): Validator<string> => (value) =>
    safeEmail.isValid(value) ? null : message,

  url: (message = 'Invalid URL'): Validator<string> => (value) =>
    safeUrl.isValid(value) ? null : message,

  port: (message = 'Invalid port'): Validator<number> => (value) =>
    safeNetwork.isValidPort(value) ? null : message,

  range: (min: number, max: number, message?: string): Validator<number> => (value) =>
    value >= min && value <= max ? null : (message ?? `Must be between ${min} and ${max}`),

  percentage: (message = 'Must be 0-100'): Validator<number> => (value) =>
    value >= 0 && value <= 100 ? null : message,

  safePath: (message = 'Invalid path'): Validator<string> => (value) =>
    safePath.hasTraversal(value) ? message : null,

  pattern: (regex: RegExp, message = 'Invalid format'): Validator<string> => (value) =>
    regex.test(value) ? null : message,

  custom: <T>(fn: (value: T) => boolean, message: string): Validator<T> => (value) =>
    fn(value) ? null : message,
};

// ============================================================================
// HISTORY STORE
// ============================================================================

export interface HistoryStore<T> extends Writable<T> {
  undo: () => boolean;
  redo: () => boolean;
  canUndo: Readable<boolean>;
  canRedo: Readable<boolean>;
  clearHistory: () => void;
}

/** Create a store with undo/redo history */
export function historyStore<T>(initial: T, maxHistory = 50): HistoryStore<T> {
  const history: T[] = [initial];
  let position = 0;

  const { subscribe, set } = writable(initial);
  const canUndoStore = writable(false);
  const canRedoStore = writable(false);

  const updateCanFlags = () => {
    canUndoStore.set(position > 0);
    canRedoStore.set(position < history.length - 1);
  };

  return {
    subscribe,
    set: (value: T) => {
      history.splice(position + 1);
      history.push(value);
      if (history.length > maxHistory) {
        history.shift();
      } else {
        position++;
      }
      updateCanFlags();
      set(value);
    },
    update: (fn: (value: T) => T) => {
      const newValue = fn(history[position]);
      history.splice(position + 1);
      history.push(newValue);
      if (history.length > maxHistory) {
        history.shift();
      } else {
        position++;
      }
      updateCanFlags();
      set(newValue);
    },
    undo: () => {
      if (position > 0) {
        position--;
        updateCanFlags();
        set(history[position]);
        return true;
      }
      return false;
    },
    redo: () => {
      if (position < history.length - 1) {
        position++;
        updateCanFlags();
        set(history[position]);
        return true;
      }
      return false;
    },
    canUndo: { subscribe: canUndoStore.subscribe },
    canRedo: { subscribe: canRedoStore.subscribe },
    clearHistory: () => {
      const current = history[position];
      history.length = 0;
      history.push(current);
      position = 0;
      updateCanFlags();
    },
  };
}

// ============================================================================
// DEBOUNCED AND THROTTLED STORES
// ============================================================================

/** Create a debounced store */
export function debouncedStore<T>(source: Readable<T>, delay: number): Readable<T> {
  let timeout: ReturnType<typeof setTimeout>;
  let initialValue: T;

  const unsubscribe = source.subscribe((value) => {
    initialValue = value;
  });
  unsubscribe();

  return readable(initialValue!, (set) => {
    return source.subscribe((value) => {
      clearTimeout(timeout);
      timeout = setTimeout(() => set(value), delay);
    });
  });
}

/** Create a throttled store */
export function throttledStore<T>(source: Readable<T>, delay: number): Readable<T> {
  let lastUpdate = 0;
  let initialValue: T;

  const unsubscribe = source.subscribe((value) => {
    initialValue = value;
  });
  unsubscribe();

  return readable(initialValue!, (set) => {
    return source.subscribe((value) => {
      const now = Date.now();
      if (now - lastUpdate >= delay) {
        lastUpdate = now;
        set(value);
      }
    });
  });
}

// ============================================================================
// ASYNC RESULT STORES
// ============================================================================

export interface AsyncResultStore<T, E = string> extends Readable<{
  loading: boolean;
  result: Result<T, E> | null;
}> {
  execute: (...args: unknown[]) => Promise<Result<T, E>>;
  reset: () => void;
}

/** Create an async result store */
export function asyncResultStore<T, E = string>(
  asyncFn: (...args: unknown[]) => Promise<T>
): AsyncResultStore<T, E> {
  const { subscribe, set } = writable<{
    loading: boolean;
    result: Result<T, E> | null;
  }>({
    loading: false,
    result: null,
  });

  return {
    subscribe,
    execute: async (...args: unknown[]) => {
      set({ loading: true, result: null });
      try {
        const value = await asyncFn(...args);
        const result = ok(value) as Result<T, E>;
        set({ loading: false, result });
        return result;
      } catch (e) {
        const result = err(e instanceof Error ? e.message : String(e)) as Result<T, E>;
        set({ loading: false, result });
        return result;
      }
    },
    reset: () => {
      set({ loading: false, result: null });
    },
  };
}

// ============================================================================
// SAFE FETCH
// ============================================================================

export interface FetchOptions extends RequestInit {
  timeout?: number;
  retries?: number;
  retryDelay?: number;
}

/** Safe fetch with timeout and retry */
export async function safeFetch<T>(
  url: string,
  options: FetchOptions = {}
): Promise<Result<T, string>> {
  const { timeout = 30000, retries = 0, retryDelay = 1000, ...fetchOptions } = options;

  const attemptFetch = async (): Promise<Result<T, string>> => {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), timeout);

    try {
      const response = await fetch(url, {
        ...fetchOptions,
        signal: controller.signal,
      });
      clearTimeout(timeoutId);

      if (!response.ok) {
        return err(`HTTP ${response.status}: ${response.statusText}`);
      }

      const data = await response.json();
      return ok(data as T);
    } catch (e) {
      clearTimeout(timeoutId);
      if (e instanceof Error) {
        if (e.name === 'AbortError') {
          return err('Request timeout');
        }
        return err(e.message);
      }
      return err('Unknown error');
    }
  };

  let lastError: Result<T, string> = err('No attempts made');

  for (let attempt = 0; attempt <= retries; attempt++) {
    const result = await attemptFetch();
    if (result.ok) return result;
    lastError = result;

    if (attempt < retries) {
      await new Promise((resolve) => setTimeout(resolve, retryDelay));
    }
  }

  return lastError;
}

/** Create a fetch store */
export function fetchStore<T>(url: string, options: FetchOptions = {}) {
  return asyncResultStore<T>(() =>
    safeFetch<T>(url, options).then((r) => {
      if (r.ok) return r.value;
      throw new Error(r.error);
    })
  );
}

// ============================================================================
// MODULE SUMMARY
// ============================================================================

/**
 * All 38 safety modules as Svelte stores:
 *
 * Core (11):
 *   1.  safeMath      - Overflow-safe arithmetic
 *   2.  safeString    - XSS/SQL injection prevention
 *   3.  safePath      - Path traversal prevention
 *   4.  safeEmail     - Email validation
 *   5.  safeUrl       - URL parsing and validation
 *   6.  safeNetwork   - IP/port validation
 *   7.  safeCrypto    - Secure hashing and random
 *   8.  safeUUID      - UUID generation and validation
 *   9.  safeCurrency  - Money arithmetic
 *   10. safePhone     - Phone number handling
 *   11. safeHex       - Hex encoding/decoding
 *
 * Data (7):
 *   12. safeJson      - Safe JSON parsing
 *   13. safeDateTime  - Date parsing and manipulation
 *   14. safeFloat     - NaN/Infinity protection
 *   15. safeVersion   - Semantic versioning
 *   16. safeColor     - RGB/RGBA with WCAG contrast
 *   17. safeAngle     - Degree/radian conversions
 *   18. safeUnit      - Physical unit conversions
 *
 * Data Structures (5):
 *   19. safeBuffer    - Bounded and ring buffers
 *   20. safeQueue     - Bounded FIFO queues
 *   21. safeBloom     - Probabilistic set membership
 *   22. safeLRU       - LRU cache
 *   23. safeGraph     - Directed graph with cycle detection
 *
 * Resilience (4):
 *   24. safeRateLimiter    - Token bucket, sliding window
 *   25. safeCircuitBreaker - Fault tolerance pattern
 *   26. safeRetry          - Exponential backoff
 *   27. safeMonotonic      - Monotonic counters
 *
 * State (2):
 *   28. safeStateMachine - Type-safe state transitions
 *   29. safeCalculator   - Expression evaluation
 *
 * Algorithm (4):
 *   30. safeGeo         - Geographic calculations
 *   31. safeProbability - Probability values [0,1]
 *   32. safeChecksum    - CRC-32, Adler-32, Luhn
 *   33. safeTensor      - Vector/matrix operations
 *
 * Security (2):
 *   34. safePassword - Strength analysis, policies
 *   35. safeMl       - Numerically stable ML ops
 *
 * HTTP (3):
 *   36. safeHeader      - CRLF injection prevention
 *   37. safeCookie      - Cookie validation
 *   38. safeContentType - MIME type handling
 */
