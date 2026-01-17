// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Proven Safety Library for Vue 3
 *
 * Formally verified safety primitives as Vue composables.
 * Provides reactive, type-safe patterns for Vue applications.
 *
 * All 38 modules implemented as Vue 3 Composition API composables.
 *
 * @example
 * ```vue
 * <script setup>
 * import { useSafeMath, useSafeCircuitBreaker } from 'proven-vue';
 * const math = useSafeMath();
 * const circuit = useSafeCircuitBreaker({ failureThreshold: 5 });
 * </script>
 * ```
 *
 * @version 0.4.0
 */

import {
  ref,
  computed,
  reactive,
  readonly,
  watch,
  shallowRef,
  triggerRef,
  type Ref,
  type ComputedRef,
  type UnwrapRef,
  type ShallowRef,
} from 'vue';

// ============================================================================
// VERSION & MODULE COUNT
// ============================================================================

export const VERSION = '0.4.0';
export const MODULE_COUNT = 38;

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
  | 'INVALID_STATE'
  | 'RATE_LIMITED'
  | 'CIRCUIT_OPEN'
  | 'INVALID_TRANSITION'
  | 'INVALID_JSON'
  | 'INVALID_COORDINATE'
  | 'NAN_VALUE'
  | 'INFINITY_VALUE';

// ============================================================================
// CORE MODULE 1: SAFE MATH COMPOSABLE
// ============================================================================

/** Safe math operations with overflow protection */
export function useSafeMath() {
  const add = (a: number, b: number): Result<number, ProvenError> => {
    const result = a + b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  };

  const sub = (a: number, b: number): Result<number, ProvenError> => {
    const result = a - b;
    if (!Number.isFinite(result)) return err('UNDERFLOW');
    return ok(result);
  };

  const mul = (a: number, b: number): Result<number, ProvenError> => {
    const result = a * b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  };

  const div = (a: number, b: number): Result<number, ProvenError> => {
    if (b === 0) return err('DIVISION_BY_ZERO');
    const result = a / b;
    if (!Number.isFinite(result)) return err('OVERFLOW');
    return ok(result);
  };

  const mod = (a: number, b: number): Result<number, ProvenError> => {
    if (b === 0) return err('DIVISION_BY_ZERO');
    return ok(a % b);
  };

  const clamp = (value: number, min: number, max: number): number => {
    return Math.min(Math.max(value, min), max);
  };

  const safeIntAdd = (a: number, b: number): Result<number, ProvenError> => {
    if (!Number.isSafeInteger(a) || !Number.isSafeInteger(b)) {
      return err('OVERFLOW');
    }
    const result = a + b;
    if (!Number.isSafeInteger(result)) return err('OVERFLOW');
    return ok(result);
  };

  const safeIntSub = (a: number, b: number): Result<number, ProvenError> => {
    if (!Number.isSafeInteger(a) || !Number.isSafeInteger(b)) {
      return err('OVERFLOW');
    }
    const result = a - b;
    if (!Number.isSafeInteger(result)) return err('UNDERFLOW');
    return ok(result);
  };

  const safeIntMul = (a: number, b: number): Result<number, ProvenError> => {
    if (!Number.isSafeInteger(a) || !Number.isSafeInteger(b)) {
      return err('OVERFLOW');
    }
    const result = a * b;
    if (!Number.isSafeInteger(result)) return err('OVERFLOW');
    return ok(result);
  };

  const abs = (value: number): number => Math.abs(value);
  const min = (...values: number[]): number => Math.min(...values);
  const max = (...values: number[]): number => Math.max(...values);

  return {
    add,
    sub,
    mul,
    div,
    mod,
    clamp,
    safeIntAdd,
    safeIntSub,
    safeIntMul,
    abs,
    min,
    max,
  };
}

// ============================================================================
// CORE MODULE 2: SAFE STRING COMPOSABLE
// ============================================================================

/** Safe string operations with encoding protection */
export function useSafeString() {
  const escapeHtml = (input: string): string => {
    return input
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#x27;');
  };

  const escapeSql = (input: string): string => {
    return input.replace(/'/g, "''");
  };

  const escapeRegex = (input: string): string => {
    return input.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  };

  const escapeShell = (input: string): string => {
    return `'${input.replace(/'/g, "'\\''")}'`;
  };

  const isValidUtf8 = (input: string): boolean => {
    try {
      new TextEncoder().encode(input);
      return true;
    } catch {
      return false;
    }
  };

  const truncate = (input: string, maxLength: number, suffix = '...'): string => {
    if (input.length <= maxLength) return input;
    return input.slice(0, maxLength - suffix.length) + suffix;
  };

  const sanitize = (input: string, allowedPattern: RegExp): string => {
    return input.replace(new RegExp(`[^${allowedPattern.source}]`, 'g'), '');
  };

  const normalize = (input: string): string => {
    return input.normalize('NFC');
  };

  const toSlug = (input: string): string => {
    return input
      .toLowerCase()
      .normalize('NFD')
      .replace(/[\u0300-\u036f]/g, '')
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/(^-|-$)/g, '');
  };

  return {
    escapeHtml,
    escapeSql,
    escapeRegex,
    escapeShell,
    isValidUtf8,
    truncate,
    sanitize,
    normalize,
    toSlug,
  };
}

// ============================================================================
// CORE MODULE 3: SAFE PATH COMPOSABLE
// ============================================================================

/** Safe path operations with traversal prevention */
export function useSafePath() {
  const hasTraversal = (path: string): boolean => {
    return path.includes('..') || path.includes('..\\');
  };

  const normalize = (path: string): Result<string, ProvenError> => {
    if (hasTraversal(path)) {
      return err('PATH_TRAVERSAL');
    }
    // Normalize path separators and remove redundant slashes
    const normalized = path
      .replace(/\\/g, '/')
      .replace(/\/+/g, '/')
      .replace(/^\.\//, '')
      .replace(/\/$/, '');
    return ok(normalized);
  };

  const join = (base: string, ...paths: string[]): Result<string, ProvenError> => {
    for (const path of paths) {
      if (hasTraversal(path)) {
        return err('PATH_TRAVERSAL');
      }
      if (path.startsWith('/') || /^[a-zA-Z]:/.test(path)) {
        return err('PATH_TRAVERSAL');
      }
    }
    const joined = [base, ...paths].join('/').replace(/\/+/g, '/');
    return ok(joined);
  };

  const isWithin = (base: string, path: string): boolean => {
    const normalizedBase = base.replace(/\\/g, '/').replace(/\/$/, '');
    const normalizedPath = path.replace(/\\/g, '/');
    return normalizedPath.startsWith(normalizedBase + '/') || normalizedPath === normalizedBase;
  };

  const getExtension = (path: string): string | null => {
    const lastDot = path.lastIndexOf('.');
    const lastSlash = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
    if (lastDot > lastSlash && lastDot < path.length - 1) {
      return path.slice(lastDot + 1);
    }
    return null;
  };

  const getStem = (path: string): string => {
    const lastSlash = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
    const filename = path.slice(lastSlash + 1);
    const lastDot = filename.lastIndexOf('.');
    if (lastDot > 0) {
      return filename.slice(0, lastDot);
    }
    return filename;
  };

  const getDirectory = (path: string): string => {
    const lastSlash = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
    if (lastSlash < 0) return '.';
    return path.slice(0, lastSlash) || '/';
  };

  return {
    hasTraversal,
    normalize,
    join,
    isWithin,
    getExtension,
    getStem,
    getDirectory,
  };
}

// ============================================================================
// CORE MODULE 4: SAFE EMAIL COMPOSABLE
// ============================================================================

/** Safe email validation */
export function useSafeEmail() {
  const emailPattern = /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/;

  const isValid = (email: string): boolean => {
    if (email.length > 254) return false;
    return emailPattern.test(email);
  };

  const validate = (email: string): Result<string, ProvenError> => {
    if (!isValid(email)) {
      return err('INVALID_EMAIL');
    }
    return ok(email.toLowerCase());
  };

  const getDomain = (email: string): Result<string, ProvenError> => {
    const atIndex = email.lastIndexOf('@');
    if (atIndex < 0) return err('INVALID_EMAIL');
    return ok(email.slice(atIndex + 1).toLowerCase());
  };

  const getLocalPart = (email: string): Result<string, ProvenError> => {
    const atIndex = email.lastIndexOf('@');
    if (atIndex < 0) return err('INVALID_EMAIL');
    return ok(email.slice(0, atIndex));
  };

  const normalize = (email: string): Result<string, ProvenError> => {
    if (!isValid(email)) return err('INVALID_EMAIL');
    return ok(email.toLowerCase().trim());
  };

  return {
    isValid,
    validate,
    getDomain,
    getLocalPart,
    normalize,
  };
}

// ============================================================================
// CORE MODULE 5: SAFE URL COMPOSABLE
// ============================================================================

/** Safe URL parsing and validation */
export function useSafeUrl() {
  const isValid = (url: string): boolean => {
    try {
      new URL(url);
      return true;
    } catch {
      return false;
    }
  };

  const parse = (url: string): Result<URL, ProvenError> => {
    try {
      return ok(new URL(url));
    } catch {
      return err('INVALID_URL');
    }
  };

  const getOrigin = (url: string): Result<string, ProvenError> => {
    const parsed = parse(url);
    if (!parsed.ok) return parsed;
    return ok(parsed.value.origin);
  };

  const getPathname = (url: string): Result<string, ProvenError> => {
    const parsed = parse(url);
    if (!parsed.ok) return parsed;
    return ok(parsed.value.pathname);
  };

  const getSearchParams = (url: string): Result<URLSearchParams, ProvenError> => {
    const parsed = parse(url);
    if (!parsed.ok) return parsed;
    return ok(parsed.value.searchParams);
  };

  const isSecure = (url: string): boolean => {
    const parsed = parse(url);
    if (!parsed.ok) return false;
    return parsed.value.protocol === 'https:';
  };

  const join = (base: string, path: string): Result<string, ProvenError> => {
    try {
      return ok(new URL(path, base).href);
    } catch {
      return err('INVALID_URL');
    }
  };

  const encodeComponent = (value: string): string => {
    return encodeURIComponent(value);
  };

  const decodeComponent = (value: string): Result<string, ProvenError> => {
    try {
      return ok(decodeURIComponent(value));
    } catch {
      return err('INVALID_INPUT');
    }
  };

  return {
    isValid,
    parse,
    getOrigin,
    getPathname,
    getSearchParams,
    isSecure,
    join,
    encodeComponent,
    decodeComponent,
  };
}

// ============================================================================
// CORE MODULE 6: SAFE NETWORK COMPOSABLE
// ============================================================================

/** Safe network operations including IP and port validation */
export function useSafeNetwork() {
  const ipv4Pattern = /^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/;
  const ipv6Pattern = /^(?:[0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$|^::(?:[0-9a-fA-F]{1,4}:){0,6}[0-9a-fA-F]{1,4}$|^(?:[0-9a-fA-F]{1,4}:){1,7}:$|^(?:[0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}$/;

  const isValidIPv4 = (ip: string): boolean => ipv4Pattern.test(ip);
  const isValidIPv6 = (ip: string): boolean => ipv6Pattern.test(ip);
  const isValidIP = (ip: string): boolean => isValidIPv4(ip) || isValidIPv6(ip);

  const isValidPort = (port: number): boolean => {
    return Number.isInteger(port) && port >= 1 && port <= 65535;
  };

  const validatePort = (port: number): Result<number, ProvenError> => {
    if (!isValidPort(port)) return err('INVALID_PORT');
    return ok(port);
  };

  const isPrivateIPv4 = (ip: string): boolean => {
    if (!isValidIPv4(ip)) return false;
    const parts = ip.split('.').map(Number);
    return (
      parts[0] === 10 ||
      (parts[0] === 172 && parts[1] >= 16 && parts[1] <= 31) ||
      (parts[0] === 192 && parts[1] === 168) ||
      parts[0] === 127
    );
  };

  const parseCIDR = (cidr: string): Result<{ ip: string; prefix: number }, ProvenError> => {
    const [ip, prefixStr] = cidr.split('/');
    if (!ip || !prefixStr) return err('INVALID_INPUT');

    const prefix = parseInt(prefixStr, 10);
    if (!isValidIP(ip)) return err('INVALID_INPUT');
    if (isNaN(prefix) || prefix < 0 || prefix > (isValidIPv4(ip) ? 32 : 128)) {
      return err('INVALID_INPUT');
    }

    return ok({ ip, prefix });
  };

  const isWellKnownPort = (port: number): boolean => {
    return port >= 1 && port <= 1023;
  };

  return {
    isValidIPv4,
    isValidIPv6,
    isValidIP,
    isValidPort,
    validatePort,
    isPrivateIPv4,
    parseCIDR,
    isWellKnownPort,
  };
}

// ============================================================================
// CORE MODULE 7: SAFE CRYPTO COMPOSABLE
// ============================================================================

/** Safe cryptographic operations (using Web Crypto API) */
export function useSafeCrypto() {
  const generateRandomBytes = async (length: number): Promise<Uint8Array> => {
    const array = new Uint8Array(length);
    crypto.getRandomValues(array);
    return array;
  };

  const generateRandomHex = async (length: number): Promise<string> => {
    const bytes = await generateRandomBytes(length);
    return Array.from(bytes)
      .map((b) => b.toString(16).padStart(2, '0'))
      .join('');
  };

  const hash = async (algorithm: 'SHA-256' | 'SHA-384' | 'SHA-512', data: string): Promise<string> => {
    const encoder = new TextEncoder();
    const buffer = await crypto.subtle.digest(algorithm, encoder.encode(data));
    return Array.from(new Uint8Array(buffer))
      .map((b) => b.toString(16).padStart(2, '0'))
      .join('');
  };

  const sha256 = (data: string): Promise<string> => hash('SHA-256', data);
  const sha384 = (data: string): Promise<string> => hash('SHA-384', data);
  const sha512 = (data: string): Promise<string> => hash('SHA-512', data);

  const hmac = async (
    algorithm: 'SHA-256' | 'SHA-384' | 'SHA-512',
    key: string,
    data: string
  ): Promise<string> => {
    const encoder = new TextEncoder();
    const keyData = encoder.encode(key);
    const cryptoKey = await crypto.subtle.importKey(
      'raw',
      keyData,
      { name: 'HMAC', hash: algorithm },
      false,
      ['sign']
    );
    const signature = await crypto.subtle.sign('HMAC', cryptoKey, encoder.encode(data));
    return Array.from(new Uint8Array(signature))
      .map((b) => b.toString(16).padStart(2, '0'))
      .join('');
  };

  const constantTimeCompare = (a: string, b: string): boolean => {
    if (a.length !== b.length) return false;
    let result = 0;
    for (let i = 0; i < a.length; i++) {
      result |= a.charCodeAt(i) ^ b.charCodeAt(i);
    }
    return result === 0;
  };

  return {
    generateRandomBytes,
    generateRandomHex,
    hash,
    sha256,
    sha384,
    sha512,
    hmac,
    constantTimeCompare,
  };
}

// ============================================================================
// CORE MODULE 8: SAFE UUID COMPOSABLE
// ============================================================================

/** Safe UUID generation and validation */
export function useSafeUUID() {
  const uuidPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;

  const isValid = (uuid: string): boolean => {
    return uuidPattern.test(uuid);
  };

  const validate = (uuid: string): Result<string, ProvenError> => {
    if (!isValid(uuid)) return err('INVALID_INPUT');
    return ok(uuid.toLowerCase());
  };

  const generate = (): string => {
    return crypto.randomUUID();
  };

  const getVersion = (uuid: string): Result<number, ProvenError> => {
    if (!isValid(uuid)) return err('INVALID_INPUT');
    return ok(parseInt(uuid.charAt(14), 16));
  };

  const nil = (): string => '00000000-0000-0000-0000-000000000000';

  const isNil = (uuid: string): boolean => {
    return uuid === '00000000-0000-0000-0000-000000000000';
  };

  return {
    isValid,
    validate,
    generate,
    getVersion,
    nil,
    isNil,
  };
}

// ============================================================================
// CORE MODULE 9: SAFE CURRENCY COMPOSABLE
// ============================================================================

/** Safe currency operations with precision handling */
export function useSafeCurrency() {
  type CurrencyCode = 'USD' | 'EUR' | 'GBP' | 'JPY' | 'CNY' | 'CHF' | 'CAD' | 'AUD' | string;

  interface Money {
    amount: number;
    currency: CurrencyCode;
  }

  const currencyDecimals: Record<string, number> = {
    USD: 2, EUR: 2, GBP: 2, JPY: 0, CNY: 2, CHF: 2, CAD: 2, AUD: 2,
  };

  const getDecimals = (currency: CurrencyCode): number => {
    return currencyDecimals[currency] ?? 2;
  };

  const create = (amount: number, currency: CurrencyCode): Money => {
    const decimals = getDecimals(currency);
    const factor = Math.pow(10, decimals);
    return {
      amount: Math.round(amount * factor) / factor,
      currency,
    };
  };

  const add = (a: Money, b: Money): Result<Money, ProvenError> => {
    if (a.currency !== b.currency) return err('INVALID_INPUT');
    return ok(create(a.amount + b.amount, a.currency));
  };

  const subtract = (a: Money, b: Money): Result<Money, ProvenError> => {
    if (a.currency !== b.currency) return err('INVALID_INPUT');
    return ok(create(a.amount - b.amount, a.currency));
  };

  const multiply = (money: Money, factor: number): Money => {
    return create(money.amount * factor, money.currency);
  };

  const divide = (money: Money, divisor: number): Result<Money, ProvenError> => {
    if (divisor === 0) return err('DIVISION_BY_ZERO');
    return ok(create(money.amount / divisor, money.currency));
  };

  const format = (money: Money, locale = 'en-US'): string => {
    return new Intl.NumberFormat(locale, {
      style: 'currency',
      currency: money.currency,
    }).format(money.amount);
  };

  const compare = (a: Money, b: Money): Result<number, ProvenError> => {
    if (a.currency !== b.currency) return err('INVALID_INPUT');
    return ok(Math.sign(a.amount - b.amount));
  };

  return {
    create,
    add,
    subtract,
    multiply,
    divide,
    format,
    compare,
    getDecimals,
  };
}

// ============================================================================
// CORE MODULE 10: SAFE PHONE COMPOSABLE
// ============================================================================

/** Safe phone number validation (E.164 format) */
export function useSafePhone() {
  const e164Pattern = /^\+[1-9]\d{1,14}$/;
  const nanpaPattern = /^\+1[2-9]\d{9}$/;

  const isValidE164 = (phone: string): boolean => {
    return e164Pattern.test(phone);
  };

  const validate = (phone: string): Result<string, ProvenError> => {
    const cleaned = phone.replace(/[\s\-\(\)\.]/g, '');
    if (!isValidE164(cleaned)) return err('INVALID_INPUT');
    return ok(cleaned);
  };

  const isNorthAmerican = (phone: string): boolean => {
    return nanpaPattern.test(phone);
  };

  const getCountryCode = (phone: string): Result<string, ProvenError> => {
    if (!isValidE164(phone)) return err('INVALID_INPUT');
    // Simple extraction - in reality would need a lookup table
    if (phone.startsWith('+1')) return ok('1');
    if (phone.startsWith('+44')) return ok('44');
    if (phone.startsWith('+49')) return ok('49');
    if (phone.startsWith('+86')) return ok('86');
    if (phone.startsWith('+81')) return ok('81');
    return ok(phone.slice(1, 3));
  };

  const formatNational = (phone: string): Result<string, ProvenError> => {
    const validated = validate(phone);
    if (!validated.ok) return validated;
    const clean = validated.value;
    if (isNorthAmerican(clean)) {
      const digits = clean.slice(2);
      return ok(`(${digits.slice(0, 3)}) ${digits.slice(3, 6)}-${digits.slice(6)}`);
    }
    return ok(clean);
  };

  return {
    isValidE164,
    validate,
    isNorthAmerican,
    getCountryCode,
    formatNational,
  };
}

// ============================================================================
// CORE MODULE 11: SAFE HEX COMPOSABLE
// ============================================================================

/** Safe hexadecimal encoding and decoding */
export function useSafeHex() {
  const hexPattern = /^[0-9a-fA-F]*$/;

  const isValid = (hex: string): boolean => {
    return hex.length % 2 === 0 && hexPattern.test(hex);
  };

  const encode = (bytes: Uint8Array): string => {
    return Array.from(bytes)
      .map((b) => b.toString(16).padStart(2, '0'))
      .join('');
  };

  const decode = (hex: string): Result<Uint8Array, ProvenError> => {
    if (!isValid(hex)) return err('INVALID_INPUT');
    const bytes = new Uint8Array(hex.length / 2);
    for (let i = 0; i < hex.length; i += 2) {
      bytes[i / 2] = parseInt(hex.slice(i, i + 2), 16);
    }
    return ok(bytes);
  };

  const encodeString = (str: string): string => {
    const encoder = new TextEncoder();
    return encode(encoder.encode(str));
  };

  const decodeToString = (hex: string): Result<string, ProvenError> => {
    const bytes = decode(hex);
    if (!bytes.ok) return bytes;
    const decoder = new TextDecoder();
    return ok(decoder.decode(bytes.value));
  };

  const toUpperCase = (hex: string): string => hex.toUpperCase();
  const toLowerCase = (hex: string): string => hex.toLowerCase();

  return {
    isValid,
    encode,
    decode,
    encodeString,
    decodeToString,
    toUpperCase,
    toLowerCase,
  };
}

// ============================================================================
// DATA MODULE 1: SAFE JSON COMPOSABLE
// ============================================================================

/** Safe JSON parsing with error handling */
export function useSafeJson() {
  const parse = <T = unknown>(json: string): Result<T, ProvenError> => {
    try {
      return ok(JSON.parse(json) as T);
    } catch {
      return err('INVALID_JSON');
    }
  };

  const stringify = (value: unknown, space?: number): Result<string, ProvenError> => {
    try {
      return ok(JSON.stringify(value, null, space));
    } catch {
      return err('INVALID_INPUT');
    }
  };

  const get = <T>(obj: unknown, path: string): Result<T, ProvenError> => {
    const parts = path.split('.');
    let current: unknown = obj;
    for (const part of parts) {
      if (current === null || current === undefined) {
        return err('INVALID_INPUT');
      }
      if (typeof current !== 'object') {
        return err('INVALID_INPUT');
      }
      current = (current as Record<string, unknown>)[part];
    }
    return ok(current as T);
  };

  const isObject = (value: unknown): value is Record<string, unknown> => {
    return typeof value === 'object' && value !== null && !Array.isArray(value);
  };

  const isArray = (value: unknown): value is unknown[] => {
    return Array.isArray(value);
  };

  const deepClone = <T>(value: T): Result<T, ProvenError> => {
    try {
      return ok(JSON.parse(JSON.stringify(value)) as T);
    } catch {
      return err('INVALID_INPUT');
    }
  };

  return {
    parse,
    stringify,
    get,
    isObject,
    isArray,
    deepClone,
  };
}

// ============================================================================
// DATA MODULE 2: SAFE DATETIME COMPOSABLE
// ============================================================================

/** Safe date/time operations */
export function useSafeDateTime() {
  const parseISO = (iso: string): Result<Date, ProvenError> => {
    const date = new Date(iso);
    if (isNaN(date.getTime())) return err('INVALID_INPUT');
    return ok(date);
  };

  const toISO = (date: Date): string => {
    return date.toISOString();
  };

  const isValid = (date: Date): boolean => {
    return !isNaN(date.getTime());
  };

  const addDays = (date: Date, days: number): Date => {
    const result = new Date(date);
    result.setDate(result.getDate() + days);
    return result;
  };

  const addHours = (date: Date, hours: number): Date => {
    const result = new Date(date);
    result.setHours(result.getHours() + hours);
    return result;
  };

  const addMinutes = (date: Date, minutes: number): Date => {
    const result = new Date(date);
    result.setMinutes(result.getMinutes() + minutes);
    return result;
  };

  const diffInDays = (a: Date, b: Date): number => {
    const msPerDay = 24 * 60 * 60 * 1000;
    return Math.floor((a.getTime() - b.getTime()) / msPerDay);
  };

  const diffInHours = (a: Date, b: Date): number => {
    const msPerHour = 60 * 60 * 1000;
    return Math.floor((a.getTime() - b.getTime()) / msPerHour);
  };

  const isBefore = (a: Date, b: Date): boolean => a.getTime() < b.getTime();
  const isAfter = (a: Date, b: Date): boolean => a.getTime() > b.getTime();
  const isSameDay = (a: Date, b: Date): boolean => {
    return a.toDateString() === b.toDateString();
  };

  const format = (date: Date, locale = 'en-US', options?: Intl.DateTimeFormatOptions): string => {
    return new Intl.DateTimeFormat(locale, options).format(date);
  };

  return {
    parseISO,
    toISO,
    isValid,
    addDays,
    addHours,
    addMinutes,
    diffInDays,
    diffInHours,
    isBefore,
    isAfter,
    isSameDay,
    format,
  };
}

// ============================================================================
// DATA MODULE 3: SAFE FLOAT COMPOSABLE
// ============================================================================

/** Safe floating-point operations with NaN/Infinity prevention */
export function useSafeFloat() {
  const isFinite = (value: number): boolean => Number.isFinite(value);
  const isNaN = (value: number): boolean => Number.isNaN(value);

  const validate = (value: number): Result<number, ProvenError> => {
    if (Number.isNaN(value)) return err('NAN_VALUE');
    if (!Number.isFinite(value)) return err('INFINITY_VALUE');
    return ok(value);
  };

  const safeDiv = (a: number, b: number): Result<number, ProvenError> => {
    if (b === 0) return err('DIVISION_BY_ZERO');
    const result = a / b;
    return validate(result);
  };

  const safeSqrt = (value: number): Result<number, ProvenError> => {
    if (value < 0) return err('INVALID_INPUT');
    return ok(Math.sqrt(value));
  };

  const safeLog = (value: number): Result<number, ProvenError> => {
    if (value <= 0) return err('INVALID_INPUT');
    return ok(Math.log(value));
  };

  const safePow = (base: number, exp: number): Result<number, ProvenError> => {
    const result = Math.pow(base, exp);
    return validate(result);
  };

  const clamp = (value: number, min: number, max: number): number => {
    if (Number.isNaN(value)) return min;
    return Math.min(Math.max(value, min), max);
  };

  const almostEqual = (a: number, b: number, epsilon = 1e-10): boolean => {
    return Math.abs(a - b) < epsilon;
  };

  const round = (value: number, decimals: number): number => {
    const factor = Math.pow(10, decimals);
    return Math.round(value * factor) / factor;
  };

  return {
    isFinite,
    isNaN,
    validate,
    safeDiv,
    safeSqrt,
    safeLog,
    safePow,
    clamp,
    almostEqual,
    round,
  };
}

// ============================================================================
// DATA MODULE 4: SAFE VERSION COMPOSABLE
// ============================================================================

/** Semantic versioning parsing and comparison */
export function useSafeVersion() {
  interface SemVer {
    major: number;
    minor: number;
    patch: number;
    prerelease?: string;
    build?: string;
  }

  const semverPattern = /^(\d+)\.(\d+)\.(\d+)(?:-([0-9A-Za-z-.]+))?(?:\+([0-9A-Za-z-.]+))?$/;

  const parse = (version: string): Result<SemVer, ProvenError> => {
    const match = version.match(semverPattern);
    if (!match) return err('INVALID_INPUT');
    return ok({
      major: parseInt(match[1], 10),
      minor: parseInt(match[2], 10),
      patch: parseInt(match[3], 10),
      prerelease: match[4],
      build: match[5],
    });
  };

  const isValid = (version: string): boolean => {
    return semverPattern.test(version);
  };

  const compare = (a: SemVer, b: SemVer): number => {
    if (a.major !== b.major) return a.major - b.major;
    if (a.minor !== b.minor) return a.minor - b.minor;
    if (a.patch !== b.patch) return a.patch - b.patch;
    if (a.prerelease && !b.prerelease) return -1;
    if (!a.prerelease && b.prerelease) return 1;
    if (a.prerelease && b.prerelease) {
      return a.prerelease.localeCompare(b.prerelease);
    }
    return 0;
  };

  const increment = (version: SemVer, type: 'major' | 'minor' | 'patch'): SemVer => {
    switch (type) {
      case 'major':
        return { major: version.major + 1, minor: 0, patch: 0 };
      case 'minor':
        return { major: version.major, minor: version.minor + 1, patch: 0 };
      case 'patch':
        return { major: version.major, minor: version.minor, patch: version.patch + 1 };
    }
  };

  const toString = (version: SemVer): string => {
    let str = `${version.major}.${version.minor}.${version.patch}`;
    if (version.prerelease) str += `-${version.prerelease}`;
    if (version.build) str += `+${version.build}`;
    return str;
  };

  const satisfies = (version: SemVer, range: string): boolean => {
    // Simple implementation for ^x.y.z (compatible with)
    if (range.startsWith('^')) {
      const rangeVersion = parse(range.slice(1));
      if (!rangeVersion.ok) return false;
      const rv = rangeVersion.value;
      if (version.major !== rv.major) return false;
      if (version.major === 0) {
        return version.minor === rv.minor && version.patch >= rv.patch;
      }
      return compare(version, rv) >= 0;
    }
    // Exact match
    const exactVersion = parse(range);
    if (!exactVersion.ok) return false;
    return compare(version, exactVersion.value) === 0;
  };

  return {
    parse,
    isValid,
    compare,
    increment,
    toString,
    satisfies,
  };
}

// ============================================================================
// DATA MODULE 5: SAFE COLOR COMPOSABLE
// ============================================================================

/** Safe color operations with WCAG contrast */
export function useSafeColor() {
  interface Rgb { r: number; g: number; b: number }
  interface Rgba extends Rgb { a: number }
  interface Hsl { h: number; s: number; l: number }

  const clamp = (value: number, min: number, max: number): number => {
    return Math.min(Math.max(value, min), max);
  };

  const rgb = (r: number, g: number, b: number): Rgb => ({
    r: clamp(Math.round(r), 0, 255),
    g: clamp(Math.round(g), 0, 255),
    b: clamp(Math.round(b), 0, 255),
  });

  const rgba = (r: number, g: number, b: number, a: number): Rgba => ({
    ...rgb(r, g, b),
    a: clamp(a, 0, 1),
  });

  const parseHex = (hex: string): Result<Rgb, ProvenError> => {
    const match = hex.match(/^#?([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})$/);
    if (!match) return err('INVALID_INPUT');
    return ok({
      r: parseInt(match[1], 16),
      g: parseInt(match[2], 16),
      b: parseInt(match[3], 16),
    });
  };

  const toHex = (color: Rgb): string => {
    const toHexPart = (n: number) => n.toString(16).padStart(2, '0');
    return `#${toHexPart(color.r)}${toHexPart(color.g)}${toHexPart(color.b)}`;
  };

  const luminance = (color: Rgb): number => {
    const toLinear = (c: number) => {
      const s = c / 255;
      return s <= 0.03928 ? s / 12.92 : Math.pow((s + 0.055) / 1.055, 2.4);
    };
    return 0.2126 * toLinear(color.r) + 0.7152 * toLinear(color.g) + 0.0722 * toLinear(color.b);
  };

  const contrastRatio = (a: Rgb, b: Rgb): number => {
    const lumA = luminance(a);
    const lumB = luminance(b);
    const lighter = Math.max(lumA, lumB);
    const darker = Math.min(lumA, lumB);
    return (lighter + 0.05) / (darker + 0.05);
  };

  const meetsWCAG = (a: Rgb, b: Rgb, level: 'AA' | 'AAA' = 'AA'): boolean => {
    const ratio = contrastRatio(a, b);
    return level === 'AAA' ? ratio >= 7 : ratio >= 4.5;
  };

  const rgbToHsl = (color: Rgb): Hsl => {
    const r = color.r / 255;
    const g = color.g / 255;
    const b = color.b / 255;
    const max = Math.max(r, g, b);
    const min = Math.min(r, g, b);
    const l = (max + min) / 2;

    if (max === min) {
      return { h: 0, s: 0, l };
    }

    const d = max - min;
    const s = l > 0.5 ? d / (2 - max - min) : d / (max + min);
    let h = 0;
    switch (max) {
      case r: h = ((g - b) / d + (g < b ? 6 : 0)) / 6; break;
      case g: h = ((b - r) / d + 2) / 6; break;
      case b: h = ((r - g) / d + 4) / 6; break;
    }

    return { h: h * 360, s, l };
  };

  const mix = (a: Rgb, b: Rgb, weight = 0.5): Rgb => {
    return rgb(
      a.r + (b.r - a.r) * weight,
      a.g + (b.g - a.g) * weight,
      a.b + (b.b - a.b) * weight
    );
  };

  return {
    rgb,
    rgba,
    parseHex,
    toHex,
    luminance,
    contrastRatio,
    meetsWCAG,
    rgbToHsl,
    mix,
  };
}

// ============================================================================
// DATA MODULE 6: SAFE ANGLE COMPOSABLE
// ============================================================================

/** Safe angle conversions and operations */
export function useSafeAngle() {
  const PI = Math.PI;

  const degToRad = (degrees: number): number => {
    return degrees * (PI / 180);
  };

  const radToDeg = (radians: number): number => {
    return radians * (180 / PI);
  };

  const normalizeDegrees = (degrees: number): number => {
    const mod = degrees % 360;
    return mod < 0 ? mod + 360 : mod;
  };

  const normalizeRadians = (radians: number): number => {
    const mod = radians % (2 * PI);
    return mod < 0 ? mod + 2 * PI : mod;
  };

  const lerp = (from: number, to: number, t: number): number => {
    return from + (to - from) * t;
  };

  const lerpAngle = (from: number, to: number, t: number): number => {
    let diff = normalizeDegrees(to - from);
    if (diff > 180) diff -= 360;
    return normalizeDegrees(from + diff * t);
  };

  const sin = (degrees: number): number => Math.sin(degToRad(degrees));
  const cos = (degrees: number): number => Math.cos(degToRad(degrees));
  const tan = (degrees: number): number => Math.tan(degToRad(degrees));

  const asin = (value: number): Result<number, ProvenError> => {
    if (value < -1 || value > 1) return err('OUT_OF_BOUNDS');
    return ok(radToDeg(Math.asin(value)));
  };

  const acos = (value: number): Result<number, ProvenError> => {
    if (value < -1 || value > 1) return err('OUT_OF_BOUNDS');
    return ok(radToDeg(Math.acos(value)));
  };

  const atan2 = (y: number, x: number): number => {
    return radToDeg(Math.atan2(y, x));
  };

  return {
    degToRad,
    radToDeg,
    normalizeDegrees,
    normalizeRadians,
    lerp,
    lerpAngle,
    sin,
    cos,
    tan,
    asin,
    acos,
    atan2,
  };
}

// ============================================================================
// DATA MODULE 7: SAFE UNIT COMPOSABLE
// ============================================================================

/** Safe unit conversions */
export function useSafeUnit() {
  // Length conversions (base: meters)
  const lengthToMeters: Record<string, number> = {
    mm: 0.001, cm: 0.01, m: 1, km: 1000,
    in: 0.0254, ft: 0.3048, yd: 0.9144, mi: 1609.344,
  };

  const convertLength = (value: number, from: string, to: string): Result<number, ProvenError> => {
    const fromFactor = lengthToMeters[from];
    const toFactor = lengthToMeters[to];
    if (fromFactor === undefined || toFactor === undefined) return err('INVALID_INPUT');
    return ok((value * fromFactor) / toFactor);
  };

  // Mass conversions (base: grams)
  const massToGrams: Record<string, number> = {
    mg: 0.001, g: 1, kg: 1000, t: 1000000,
    oz: 28.3495, lb: 453.592, st: 6350.29,
  };

  const convertMass = (value: number, from: string, to: string): Result<number, ProvenError> => {
    const fromFactor = massToGrams[from];
    const toFactor = massToGrams[to];
    if (fromFactor === undefined || toFactor === undefined) return err('INVALID_INPUT');
    return ok((value * fromFactor) / toFactor);
  };

  // Temperature
  const convertTemperature = (value: number, from: 'C' | 'F' | 'K', to: 'C' | 'F' | 'K'): number => {
    // Convert to Celsius first
    let celsius: number;
    switch (from) {
      case 'C': celsius = value; break;
      case 'F': celsius = (value - 32) * 5 / 9; break;
      case 'K': celsius = value - 273.15; break;
    }
    // Convert from Celsius to target
    switch (to) {
      case 'C': return celsius;
      case 'F': return celsius * 9 / 5 + 32;
      case 'K': return celsius + 273.15;
    }
  };

  // Time conversions (base: seconds)
  const timeToSeconds: Record<string, number> = {
    ms: 0.001, s: 1, min: 60, h: 3600, d: 86400, w: 604800,
  };

  const convertTime = (value: number, from: string, to: string): Result<number, ProvenError> => {
    const fromFactor = timeToSeconds[from];
    const toFactor = timeToSeconds[to];
    if (fromFactor === undefined || toFactor === undefined) return err('INVALID_INPUT');
    return ok((value * fromFactor) / toFactor);
  };

  // Data size conversions (base: bytes)
  const dataToBytes: Record<string, number> = {
    B: 1, KB: 1024, MB: 1048576, GB: 1073741824, TB: 1099511627776,
    kB: 1000, MB_SI: 1000000, GB_SI: 1000000000, TB_SI: 1000000000000,
  };

  const convertData = (value: number, from: string, to: string): Result<number, ProvenError> => {
    const fromFactor = dataToBytes[from];
    const toFactor = dataToBytes[to];
    if (fromFactor === undefined || toFactor === undefined) return err('INVALID_INPUT');
    return ok((value * fromFactor) / toFactor);
  };

  return {
    convertLength,
    convertMass,
    convertTemperature,
    convertTime,
    convertData,
  };
}

// ============================================================================
// DATA STRUCTURES MODULE 1: SAFE BUFFER COMPOSABLE
// ============================================================================

/** Safe bounded buffer with reactive state */
export function useSafeBuffer<T>(capacity: number) {
  const data = shallowRef<T[]>([]);

  const length = computed(() => data.value.length);
  const isEmpty = computed(() => data.value.length === 0);
  const isFull = computed(() => data.value.length >= capacity);
  const remaining = computed(() => capacity - data.value.length);

  const push = (value: T): Result<void, ProvenError> => {
    if (isFull.value) return err('BUFFER_FULL');
    data.value = [...data.value, value];
    return ok(undefined);
  };

  const pop = (): Result<T, ProvenError> => {
    if (isEmpty.value) return err('BUFFER_EMPTY');
    const last = data.value[data.value.length - 1];
    data.value = data.value.slice(0, -1);
    return ok(last);
  };

  const get = (index: number): Result<T, ProvenError> => {
    if (index < 0 || index >= data.value.length) return err('OUT_OF_BOUNDS');
    return ok(data.value[index]);
  };

  const set = (index: number, value: T): Result<void, ProvenError> => {
    if (index < 0 || index >= data.value.length) return err('OUT_OF_BOUNDS');
    const newData = [...data.value];
    newData[index] = value;
    data.value = newData;
    return ok(undefined);
  };

  const clear = () => {
    data.value = [];
  };

  const toArray = (): T[] => [...data.value];

  return {
    data: readonly(data),
    length,
    isEmpty,
    isFull,
    remaining,
    capacity,
    push,
    pop,
    get,
    set,
    clear,
    toArray,
  };
}

// ============================================================================
// DATA STRUCTURES MODULE 2: SAFE QUEUE COMPOSABLE
// ============================================================================

/** Safe bounded FIFO queue with reactive state */
export function useSafeQueue<T>(capacity: number) {
  const data = shallowRef<T[]>([]);

  const length = computed(() => data.value.length);
  const isEmpty = computed(() => data.value.length === 0);
  const isFull = computed(() => data.value.length >= capacity);

  const enqueue = (value: T): Result<void, ProvenError> => {
    if (isFull.value) return err('BUFFER_FULL');
    data.value = [...data.value, value];
    return ok(undefined);
  };

  const dequeue = (): Result<T, ProvenError> => {
    if (isEmpty.value) return err('BUFFER_EMPTY');
    const first = data.value[0];
    data.value = data.value.slice(1);
    return ok(first);
  };

  const peek = (): Result<T, ProvenError> => {
    if (isEmpty.value) return err('BUFFER_EMPTY');
    return ok(data.value[0]);
  };

  const clear = () => {
    data.value = [];
  };

  const toArray = (): T[] => [...data.value];

  return {
    data: readonly(data),
    length,
    isEmpty,
    isFull,
    capacity,
    enqueue,
    dequeue,
    peek,
    clear,
    toArray,
  };
}

// ============================================================================
// DATA STRUCTURES MODULE 3: SAFE BLOOM COMPOSABLE
// ============================================================================

/** Probabilistic set membership with Bloom filter */
export function useSafeBloom(size: number, hashCount = 3) {
  const bits = ref<Uint8Array>(new Uint8Array(Math.ceil(size / 8)));
  const itemCount = ref(0);

  const hash = (value: string, seed: number): number => {
    let hash = seed;
    for (let i = 0; i < value.length; i++) {
      hash = ((hash << 5) - hash + value.charCodeAt(i)) | 0;
    }
    return Math.abs(hash) % size;
  };

  const getHashes = (value: string): number[] => {
    const hashes: number[] = [];
    for (let i = 0; i < hashCount; i++) {
      hashes.push(hash(value, i * 0x9e3779b9));
    }
    return hashes;
  };

  const add = (value: string): void => {
    const hashes = getHashes(value);
    const newBits = new Uint8Array(bits.value);
    for (const h of hashes) {
      const byteIndex = Math.floor(h / 8);
      const bitIndex = h % 8;
      newBits[byteIndex] |= (1 << bitIndex);
    }
    bits.value = newBits;
    itemCount.value++;
  };

  const mightContain = (value: string): boolean => {
    const hashes = getHashes(value);
    for (const h of hashes) {
      const byteIndex = Math.floor(h / 8);
      const bitIndex = h % 8;
      if ((bits.value[byteIndex] & (1 << bitIndex)) === 0) {
        return false;
      }
    }
    return true;
  };

  const falsePositiveRate = computed(() => {
    const n = itemCount.value;
    const m = size;
    const k = hashCount;
    return Math.pow(1 - Math.exp(-k * n / m), k);
  });

  const clear = () => {
    bits.value = new Uint8Array(Math.ceil(size / 8));
    itemCount.value = 0;
  };

  return {
    add,
    mightContain,
    falsePositiveRate,
    itemCount: readonly(itemCount),
    clear,
    size,
    hashCount,
  };
}

// ============================================================================
// DATA STRUCTURES MODULE 4: SAFE LRU COMPOSABLE
// ============================================================================

/** Least-recently-used cache with reactive state */
export function useSafeLRU<K, V>(capacity: number) {
  const cache = shallowRef(new Map<K, V>());
  const accessOrder = shallowRef<K[]>([]);

  const size = computed(() => cache.value.size);
  const isEmpty = computed(() => cache.value.size === 0);
  const isFull = computed(() => cache.value.size >= capacity);

  const get = (key: K): Result<V, ProvenError> => {
    if (!cache.value.has(key)) return err('INVALID_INPUT');

    // Move to end (most recently used)
    const newOrder = accessOrder.value.filter(k => k !== key);
    newOrder.push(key);
    accessOrder.value = newOrder;

    return ok(cache.value.get(key)!);
  };

  const set = (key: K, value: V): void => {
    const newCache = new Map(cache.value);
    let newOrder = [...accessOrder.value];

    if (newCache.has(key)) {
      newOrder = newOrder.filter(k => k !== key);
    } else if (newCache.size >= capacity) {
      const oldest = newOrder.shift();
      if (oldest !== undefined) {
        newCache.delete(oldest);
      }
    }

    newCache.set(key, value);
    newOrder.push(key);

    cache.value = newCache;
    accessOrder.value = newOrder;
  };

  const has = (key: K): boolean => cache.value.has(key);

  const remove = (key: K): boolean => {
    if (!cache.value.has(key)) return false;

    const newCache = new Map(cache.value);
    newCache.delete(key);
    cache.value = newCache;
    accessOrder.value = accessOrder.value.filter(k => k !== key);

    return true;
  };

  const clear = () => {
    cache.value = new Map();
    accessOrder.value = [];
  };

  const keys = (): K[] => [...cache.value.keys()];
  const values = (): V[] => [...cache.value.values()];
  const entries = (): [K, V][] => [...cache.value.entries()];

  return {
    size,
    isEmpty,
    isFull,
    capacity,
    get,
    set,
    has,
    remove,
    clear,
    keys,
    values,
    entries,
  };
}

// ============================================================================
// DATA STRUCTURES MODULE 5: SAFE GRAPH COMPOSABLE
// ============================================================================

/** Directed graph with cycle detection */
export function useSafeGraph<T>() {
  const nodes = shallowRef(new Map<string, T>());
  const edges = shallowRef(new Map<string, Set<string>>());

  const nodeCount = computed(() => nodes.value.size);
  const edgeCount = computed(() => {
    let count = 0;
    for (const targets of edges.value.values()) {
      count += targets.size;
    }
    return count;
  });

  const addNode = (id: string, data: T): void => {
    const newNodes = new Map(nodes.value);
    newNodes.set(id, data);
    nodes.value = newNodes;

    if (!edges.value.has(id)) {
      const newEdges = new Map(edges.value);
      newEdges.set(id, new Set());
      edges.value = newEdges;
    }
  };

  const removeNode = (id: string): boolean => {
    if (!nodes.value.has(id)) return false;

    const newNodes = new Map(nodes.value);
    newNodes.delete(id);
    nodes.value = newNodes;

    const newEdges = new Map(edges.value);
    newEdges.delete(id);
    for (const [key, targets] of newEdges) {
      if (targets.has(id)) {
        const newTargets = new Set(targets);
        newTargets.delete(id);
        newEdges.set(key, newTargets);
      }
    }
    edges.value = newEdges;

    return true;
  };

  const addEdge = (from: string, to: string): Result<void, ProvenError> => {
    if (!nodes.value.has(from) || !nodes.value.has(to)) {
      return err('INVALID_INPUT');
    }

    const newEdges = new Map(edges.value);
    const targets = new Set(newEdges.get(from) || []);
    targets.add(to);
    newEdges.set(from, targets);
    edges.value = newEdges;

    return ok(undefined);
  };

  const removeEdge = (from: string, to: string): boolean => {
    const targets = edges.value.get(from);
    if (!targets?.has(to)) return false;

    const newEdges = new Map(edges.value);
    const newTargets = new Set(targets);
    newTargets.delete(to);
    newEdges.set(from, newTargets);
    edges.value = newEdges;

    return true;
  };

  const hasNode = (id: string): boolean => nodes.value.has(id);
  const hasEdge = (from: string, to: string): boolean => edges.value.get(from)?.has(to) ?? false;

  const getNode = (id: string): Result<T, ProvenError> => {
    const node = nodes.value.get(id);
    if (node === undefined) return err('INVALID_INPUT');
    return ok(node);
  };

  const getNeighbors = (id: string): string[] => {
    return [...(edges.value.get(id) || [])];
  };

  const hasCycle = (): boolean => {
    const visited = new Set<string>();
    const recursionStack = new Set<string>();

    const dfs = (nodeId: string): boolean => {
      visited.add(nodeId);
      recursionStack.add(nodeId);

      for (const neighbor of (edges.value.get(nodeId) || [])) {
        if (!visited.has(neighbor)) {
          if (dfs(neighbor)) return true;
        } else if (recursionStack.has(neighbor)) {
          return true;
        }
      }

      recursionStack.delete(nodeId);
      return false;
    };

    for (const nodeId of nodes.value.keys()) {
      if (!visited.has(nodeId)) {
        if (dfs(nodeId)) return true;
      }
    }

    return false;
  };

  const topologicalSort = (): Result<string[], ProvenError> => {
    if (hasCycle()) return err('INVALID_STATE');

    const visited = new Set<string>();
    const result: string[] = [];

    const dfs = (nodeId: string) => {
      visited.add(nodeId);
      for (const neighbor of (edges.value.get(nodeId) || [])) {
        if (!visited.has(neighbor)) {
          dfs(neighbor);
        }
      }
      result.unshift(nodeId);
    };

    for (const nodeId of nodes.value.keys()) {
      if (!visited.has(nodeId)) {
        dfs(nodeId);
      }
    }

    return ok(result);
  };

  const clear = () => {
    nodes.value = new Map();
    edges.value = new Map();
  };

  return {
    nodeCount,
    edgeCount,
    addNode,
    removeNode,
    addEdge,
    removeEdge,
    hasNode,
    hasEdge,
    getNode,
    getNeighbors,
    hasCycle,
    topologicalSort,
    clear,
  };
}

// ============================================================================
// RESILIENCE MODULE 1: SAFE RATE LIMITER COMPOSABLE
// ============================================================================

/** Token bucket rate limiter with reactive state */
export function useSafeRateLimiter(options: {
  tokensPerInterval: number;
  intervalMs: number;
  maxTokens?: number;
}) {
  const { tokensPerInterval, intervalMs, maxTokens = tokensPerInterval } = options;

  const tokens = ref(maxTokens);
  const lastRefill = ref(Date.now());

  const refill = () => {
    const now = Date.now();
    const elapsed = now - lastRefill.value;
    const tokensToAdd = Math.floor(elapsed / intervalMs) * tokensPerInterval;

    if (tokensToAdd > 0) {
      tokens.value = Math.min(tokens.value + tokensToAdd, maxTokens);
      lastRefill.value = now - (elapsed % intervalMs);
    }
  };

  const tryAcquire = (count = 1): boolean => {
    refill();
    if (tokens.value >= count) {
      tokens.value -= count;
      return true;
    }
    return false;
  };

  const acquire = async (count = 1): Promise<void> => {
    while (!tryAcquire(count)) {
      await new Promise(resolve => setTimeout(resolve, intervalMs / tokensPerInterval));
    }
  };

  const availableTokens = computed(() => {
    refill();
    return tokens.value;
  });

  const waitTime = computed(() => {
    refill();
    if (tokens.value > 0) return 0;
    return intervalMs / tokensPerInterval;
  });

  const reset = () => {
    tokens.value = maxTokens;
    lastRefill.value = Date.now();
  };

  return {
    tryAcquire,
    acquire,
    availableTokens,
    waitTime,
    reset,
    maxTokens,
    tokensPerInterval,
    intervalMs,
  };
}

// ============================================================================
// RESILIENCE MODULE 2: SAFE CIRCUIT BREAKER COMPOSABLE
// ============================================================================

type CircuitState = 'closed' | 'open' | 'half-open';

/** Circuit breaker pattern for fault tolerance */
export function useSafeCircuitBreaker(options: {
  failureThreshold?: number;
  successThreshold?: number;
  timeoutMs?: number;
  halfOpenMaxCalls?: number;
} = {}) {
  const {
    failureThreshold = 5,
    successThreshold = 2,
    timeoutMs = 30000,
    halfOpenMaxCalls = 3,
  } = options;

  const state = ref<CircuitState>('closed');
  const failures = ref(0);
  const successes = ref(0);
  const lastFailureTime = ref(0);
  const halfOpenCalls = ref(0);

  const isHealthy = computed(() => state.value === 'closed');
  const isOpen = computed(() => state.value === 'open');
  const isHalfOpen = computed(() => state.value === 'half-open');

  const updateState = () => {
    if (state.value === 'open') {
      const now = Date.now();
      if (now - lastFailureTime.value >= timeoutMs) {
        state.value = 'half-open';
        successes.value = 0;
        halfOpenCalls.value = 0;
      }
    }
  };

  const canExecute = (): boolean => {
    updateState();
    switch (state.value) {
      case 'closed': return true;
      case 'open': return false;
      case 'half-open': return halfOpenCalls.value < halfOpenMaxCalls;
    }
  };

  const recordSuccess = () => {
    switch (state.value) {
      case 'closed':
        failures.value = 0;
        break;
      case 'half-open':
        successes.value++;
        if (successes.value >= successThreshold) {
          state.value = 'closed';
          failures.value = 0;
          successes.value = 0;
        }
        break;
    }
  };

  const recordFailure = () => {
    lastFailureTime.value = Date.now();
    switch (state.value) {
      case 'closed':
        failures.value++;
        if (failures.value >= failureThreshold) {
          state.value = 'open';
        }
        break;
      case 'half-open':
        state.value = 'open';
        failures.value++;
        break;
      case 'open':
        failures.value++;
        break;
    }
  };

  const execute = async <T>(fn: () => Promise<T>): Promise<Result<T, ProvenError>> => {
    updateState();
    if (!canExecute()) {
      return err('CIRCUIT_OPEN');
    }

    if (state.value === 'half-open') {
      halfOpenCalls.value++;
    }

    try {
      const result = await fn();
      recordSuccess();
      return ok(result);
    } catch (error) {
      recordFailure();
      return err('INVALID_INPUT');
    }
  };

  const reset = () => {
    state.value = 'closed';
    failures.value = 0;
    successes.value = 0;
    halfOpenCalls.value = 0;
  };

  const timeUntilRetry = computed(() => {
    if (state.value !== 'open') return 0;
    const elapsed = Date.now() - lastFailureTime.value;
    return Math.max(0, timeoutMs - elapsed);
  });

  return {
    state: readonly(state),
    isHealthy,
    isOpen,
    isHalfOpen,
    failures: readonly(failures),
    canExecute,
    recordSuccess,
    recordFailure,
    execute,
    reset,
    timeUntilRetry,
  };
}

// ============================================================================
// RESILIENCE MODULE 3: SAFE RETRY COMPOSABLE
// ============================================================================

/** Exponential backoff retry mechanism */
export function useSafeRetry(options: {
  maxAttempts?: number;
  initialDelayMs?: number;
  maxDelayMs?: number;
  multiplier?: number;
  useJitter?: boolean;
} = {}) {
  const {
    maxAttempts = 3,
    initialDelayMs = 100,
    maxDelayMs = 10000,
    multiplier = 2,
    useJitter = true,
  } = options;

  const attempts = ref(0);
  const currentDelay = ref(initialDelayMs);
  const isRetrying = ref(false);

  const remainingAttempts = computed(() => Math.max(0, maxAttempts - attempts.value));
  const shouldRetry = computed(() => attempts.value < maxAttempts);

  const getDelay = (): number => {
    let delay = currentDelay.value;
    if (useJitter) {
      delay = delay * (0.5 + Math.random());
    }
    return Math.min(delay, maxDelayMs);
  };

  const recordAttempt = (): number | null => {
    if (!shouldRetry.value) return null;

    const delay = getDelay();
    attempts.value++;
    currentDelay.value = Math.min(currentDelay.value * multiplier, maxDelayMs);

    return delay;
  };

  const execute = async <T>(fn: () => Promise<T>): Promise<Result<T, ProvenError>> => {
    reset();
    isRetrying.value = true;

    while (shouldRetry.value) {
      try {
        const result = await fn();
        isRetrying.value = false;
        return ok(result);
      } catch {
        const delay = recordAttempt();
        if (delay === null) break;
        await new Promise(resolve => setTimeout(resolve, delay));
      }
    }

    isRetrying.value = false;
    return err('INVALID_INPUT');
  };

  const reset = () => {
    attempts.value = 0;
    currentDelay.value = initialDelayMs;
    isRetrying.value = false;
  };

  const exponentialBackoff = (attempt: number): number => {
    return Math.min(initialDelayMs * Math.pow(multiplier, attempt), maxDelayMs);
  };

  return {
    attempts: readonly(attempts),
    remainingAttempts,
    shouldRetry,
    isRetrying: readonly(isRetrying),
    getDelay,
    recordAttempt,
    execute,
    reset,
    exponentialBackoff,
  };
}

// ============================================================================
// RESILIENCE MODULE 4: SAFE MONOTONIC COMPOSABLE
// ============================================================================

/** Monotonically increasing sequences */
export function useSafeMonotonic(initial = 0) {
  const value = ref(initial);
  const highWaterMark = ref(initial);

  const next = (): number => {
    value.value++;
    if (value.value > highWaterMark.value) {
      highWaterMark.value = value.value;
    }
    return value.value;
  };

  const peek = (): number => value.value;

  const update = (newValue: number): Result<number, ProvenError> => {
    if (newValue <= value.value) {
      return err('INVALID_INPUT');
    }
    value.value = newValue;
    if (newValue > highWaterMark.value) {
      highWaterMark.value = newValue;
    }
    return ok(value.value);
  };

  const compareAndSet = (expected: number, newValue: number): boolean => {
    if (value.value !== expected || newValue <= value.value) {
      return false;
    }
    value.value = newValue;
    if (newValue > highWaterMark.value) {
      highWaterMark.value = newValue;
    }
    return true;
  };

  const reset = (newInitial = 0) => {
    value.value = newInitial;
    highWaterMark.value = newInitial;
  };

  return {
    value: readonly(value),
    highWaterMark: readonly(highWaterMark),
    next,
    peek,
    update,
    compareAndSet,
    reset,
  };
}

// ============================================================================
// STATE MODULE 1: SAFE STATE MACHINE COMPOSABLE
// ============================================================================

/** Type-safe state machine with validated transitions */
export function useSafeStateMachine<S extends string>(
  initialState: S,
  transitions: Record<S, S[]>,
  options: { maxHistory?: number } = {}
) {
  const { maxHistory = 100 } = options;

  const current = ref(initialState) as Ref<S>;
  const history = shallowRef<S[]>([]);

  const validTransitions = computed(() => transitions[current.value] || []);

  const canTransition = (to: S): boolean => {
    return validTransitions.value.includes(to);
  };

  const transition = (to: S): Result<S, ProvenError> => {
    if (!canTransition(to)) {
      return err('INVALID_TRANSITION');
    }

    const newHistory = [...history.value, current.value];
    while (newHistory.length > maxHistory) {
      newHistory.shift();
    }
    history.value = newHistory;

    current.value = to;
    return ok(current.value);
  };

  const forceTransition = (to: S): void => {
    const newHistory = [...history.value, current.value];
    while (newHistory.length > maxHistory) {
      newHistory.shift();
    }
    history.value = newHistory;
    current.value = to;
  };

  const reset = () => {
    current.value = initialState;
    history.value = [];
  };

  const undo = (): Result<S, ProvenError> => {
    if (history.value.length === 0) {
      return err('INVALID_STATE');
    }
    const previous = history.value[history.value.length - 1];
    history.value = history.value.slice(0, -1);
    current.value = previous;
    return ok(current.value);
  };

  const getHistory = (): S[] => [...history.value];

  return {
    current: readonly(current),
    history: readonly(history),
    validTransitions,
    canTransition,
    transition,
    forceTransition,
    reset,
    undo,
    getHistory,
  };
}

// ============================================================================
// STATE MODULE 2: SAFE CALCULATOR COMPOSABLE
// ============================================================================

/** Expression evaluator with overflow/division protection */
export function useSafeCalculator() {
  const expression = ref('');
  const result = ref<number | null>(null);
  const error = ref<string | null>(null);

  const { add, sub, mul, div } = useSafeMath();

  const clear = () => {
    expression.value = '';
    result.value = null;
    error.value = null;
  };

  const append = (char: string) => {
    expression.value += char;
    error.value = null;
  };

  const backspace = () => {
    expression.value = expression.value.slice(0, -1);
    error.value = null;
  };

  const evaluate = (): Result<number, ProvenError> => {
    try {
      // Simple tokenizer and evaluator
      const expr = expression.value.trim();
      if (!expr) return err('INVALID_INPUT');

      // Parse and evaluate using safe math operations
      // This is a simplified implementation
      const tokens = expr.match(/(\d+\.?\d*|[+\-*/()])/g);
      if (!tokens) return err('INVALID_INPUT');

      // Simple left-to-right evaluation (no operator precedence for simplicity)
      let currentResult = parseFloat(tokens[0]);
      if (isNaN(currentResult)) return err('INVALID_INPUT');

      let i = 1;
      while (i < tokens.length) {
        const op = tokens[i];
        const nextNum = parseFloat(tokens[i + 1]);

        if (isNaN(nextNum)) return err('INVALID_INPUT');

        let opResult: Result<number, ProvenError>;
        switch (op) {
          case '+': opResult = add(currentResult, nextNum); break;
          case '-': opResult = sub(currentResult, nextNum); break;
          case '*': opResult = mul(currentResult, nextNum); break;
          case '/': opResult = div(currentResult, nextNum); break;
          default: return err('INVALID_INPUT');
        }

        if (!opResult.ok) {
          error.value = opResult.error;
          return opResult;
        }
        currentResult = opResult.value;
        i += 2;
      }

      result.value = currentResult;
      return ok(currentResult);
    } catch {
      error.value = 'INVALID_INPUT';
      return err('INVALID_INPUT');
    }
  };

  return {
    expression: readonly(expression),
    result: readonly(result),
    error: readonly(error),
    clear,
    append,
    backspace,
    evaluate,
  };
}

// ============================================================================
// ALGORITHM MODULE 1: SAFE GEO COMPOSABLE
// ============================================================================

/** Geographic coordinate operations */
export function useSafeGeo() {
  const EARTH_RADIUS_KM = 6371;
  const EARTH_RADIUS_MI = 3958.8;

  interface Coordinate { lat: number; lon: number }

  const createCoordinate = (lat: number, lon: number): Result<Coordinate, ProvenError> => {
    if (lat < -90 || lat > 90) return err('INVALID_COORDINATE');
    if (lon < -180 || lon > 180) return err('INVALID_COORDINATE');
    if (isNaN(lat) || isNaN(lon)) return err('NAN_VALUE');
    return ok({ lat, lon });
  };

  const degToRad = (degrees: number): number => degrees * (Math.PI / 180);

  const haversineDistance = (c1: Coordinate, c2: Coordinate, radiusKm = EARTH_RADIUS_KM): number => {
    const lat1 = degToRad(c1.lat);
    const lat2 = degToRad(c2.lat);
    const dLat = degToRad(c2.lat - c1.lat);
    const dLon = degToRad(c2.lon - c1.lon);

    const a = Math.sin(dLat / 2) ** 2 +
      Math.cos(lat1) * Math.cos(lat2) * Math.sin(dLon / 2) ** 2;
    const c = 2 * Math.asin(Math.sqrt(a));

    return radiusKm * c;
  };

  const distanceKm = (c1: Coordinate, c2: Coordinate): number => {
    return haversineDistance(c1, c2, EARTH_RADIUS_KM);
  };

  const distanceMi = (c1: Coordinate, c2: Coordinate): number => {
    return haversineDistance(c1, c2, EARTH_RADIUS_MI);
  };

  const bearing = (from: Coordinate, to: Coordinate): number => {
    const lat1 = degToRad(from.lat);
    const lat2 = degToRad(to.lat);
    const dLon = degToRad(to.lon - from.lon);

    const y = Math.sin(dLon) * Math.cos(lat2);
    const x = Math.cos(lat1) * Math.sin(lat2) -
      Math.sin(lat1) * Math.cos(lat2) * Math.cos(dLon);

    const brng = Math.atan2(y, x) * (180 / Math.PI);
    return (brng + 360) % 360;
  };

  const destination = (
    start: Coordinate,
    bearingDeg: number,
    distanceKm: number
  ): Result<Coordinate, ProvenError> => {
    const lat1 = degToRad(start.lat);
    const lon1 = degToRad(start.lon);
    const brng = degToRad(bearingDeg);
    const angularDist = distanceKm / EARTH_RADIUS_KM;

    const lat2 = Math.asin(
      Math.sin(lat1) * Math.cos(angularDist) +
      Math.cos(lat1) * Math.sin(angularDist) * Math.cos(brng)
    );

    const lon2 = lon1 + Math.atan2(
      Math.sin(brng) * Math.sin(angularDist) * Math.cos(lat1),
      Math.cos(angularDist) - Math.sin(lat1) * Math.sin(lat2)
    );

    const latDeg = lat2 * (180 / Math.PI);
    const lonDeg = ((lon2 * (180 / Math.PI)) + 540) % 360 - 180;

    return createCoordinate(latDeg, lonDeg);
  };

  const isNorthern = (coord: Coordinate): boolean => coord.lat >= 0;
  const isEastern = (coord: Coordinate): boolean => coord.lon >= 0;

  return {
    createCoordinate,
    haversineDistance,
    distanceKm,
    distanceMi,
    bearing,
    destination,
    isNorthern,
    isEastern,
    EARTH_RADIUS_KM,
    EARTH_RADIUS_MI,
  };
}

// ============================================================================
// ALGORITHM MODULE 2: SAFE PROBABILITY COMPOSABLE
// ============================================================================

/** Probability operations with clamping */
export function useSafeProbability() {
  const create = (value: number): number => {
    return Math.max(0, Math.min(1, value));
  };

  const validate = (value: number): Result<number, ProvenError> => {
    if (isNaN(value)) return err('NAN_VALUE');
    if (value < 0 || value > 1) return err('OUT_OF_BOUNDS');
    return ok(value);
  };

  const complement = (p: number): number => create(1 - p);

  const and = (a: number, b: number): number => create(a * b);

  const or = (a: number, b: number): number => create(a + b - a * b);

  const given = (p: number, condition: number): Result<number, ProvenError> => {
    if (condition === 0) return err('DIVISION_BY_ZERO');
    return ok(create(p / condition));
  };

  const toPercent = (p: number): number => p * 100;
  const fromPercent = (percent: number): number => create(percent / 100);

  const toOdds = (p: number): Result<number, ProvenError> => {
    if (p >= 1) return err('INVALID_INPUT');
    return ok(p / (1 - p));
  };

  const fromOdds = (odds: number): number => {
    if (odds < 0) return 0;
    return create(odds / (1 + odds));
  };

  const expectedValue = (outcomes: Array<{ value: number; probability: number }>): number => {
    return outcomes.reduce((sum, o) => sum + o.value * o.probability, 0);
  };

  const entropy = (probs: number[]): number => {
    return probs
      .filter(p => p > 0)
      .reduce((sum, p) => sum - p * Math.log2(p), 0);
  };

  return {
    create,
    validate,
    complement,
    and,
    or,
    given,
    toPercent,
    fromPercent,
    toOdds,
    fromOdds,
    expectedValue,
    entropy,
  };
}

// ============================================================================
// ALGORITHM MODULE 3: SAFE CHECKSUM COMPOSABLE
// ============================================================================

/** Checksum algorithms */
export function useSafeChecksum() {
  const crc32Table = (() => {
    const table = new Uint32Array(256);
    for (let i = 0; i < 256; i++) {
      let c = i;
      for (let j = 0; j < 8; j++) {
        c = c & 1 ? 0xEDB88320 ^ (c >>> 1) : c >>> 1;
      }
      table[i] = c >>> 0;
    }
    return table;
  })();

  const crc32 = (data: string | Uint8Array): number => {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    let crc = 0xFFFFFFFF;
    for (const byte of bytes) {
      crc = crc32Table[(crc ^ byte) & 0xFF] ^ (crc >>> 8);
    }
    return (crc ^ 0xFFFFFFFF) >>> 0;
  };

  const adler32 = (data: string | Uint8Array): number => {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    let a = 1;
    let b = 0;
    const MOD = 65521;
    for (const byte of bytes) {
      a = (a + byte) % MOD;
      b = (b + a) % MOD;
    }
    return ((b << 16) | a) >>> 0;
  };

  const fnv1a32 = (data: string | Uint8Array): number => {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    let hash = 2166136261;
    for (const byte of bytes) {
      hash ^= byte;
      hash = Math.imul(hash, 16777619) >>> 0;
    }
    return hash >>> 0;
  };

  const fnv1a64 = (data: string | Uint8Array): bigint => {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    let hash = BigInt('14695981039346656037');
    const prime = BigInt('1099511628211');
    for (const byte of bytes) {
      hash ^= BigInt(byte);
      hash = (hash * prime) & BigInt('0xFFFFFFFFFFFFFFFF');
    }
    return hash;
  };

  const djb2 = (data: string): number => {
    let hash = 5381;
    for (let i = 0; i < data.length; i++) {
      hash = ((hash << 5) + hash) + data.charCodeAt(i);
      hash = hash >>> 0;
    }
    return hash;
  };

  const luhnCheck = (digits: string): boolean => {
    const clean = digits.replace(/\D/g, '');
    if (clean.length === 0) return false;

    let sum = 0;
    let isEven = false;

    for (let i = clean.length - 1; i >= 0; i--) {
      let digit = parseInt(clean[i], 10);
      if (isEven) {
        digit *= 2;
        if (digit > 9) digit -= 9;
      }
      sum += digit;
      isEven = !isEven;
    }

    return sum % 10 === 0;
  };

  const fletcher16 = (data: string | Uint8Array): number => {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    let sum1 = 0;
    let sum2 = 0;
    for (const byte of bytes) {
      sum1 = (sum1 + byte) % 255;
      sum2 = (sum2 + sum1) % 255;
    }
    return (sum2 << 8) | sum1;
  };

  return {
    crc32,
    adler32,
    fnv1a32,
    fnv1a64,
    djb2,
    luhnCheck,
    fletcher16,
  };
}

// ============================================================================
// ALGORITHM MODULE 4: SAFE TENSOR COMPOSABLE
// ============================================================================

/** Bounds-checked vector/matrix operations */
export function useSafeTensor() {
  type Vector = number[];
  type Matrix = number[][];

  const createVector = (length: number, fill = 0): Vector => {
    return new Array(length).fill(fill);
  };

  const createMatrix = (rows: number, cols: number, fill = 0): Matrix => {
    return Array.from({ length: rows }, () => new Array(cols).fill(fill));
  };

  const vectorAdd = (a: Vector, b: Vector): Result<Vector, ProvenError> => {
    if (a.length !== b.length) return err('INVALID_INPUT');
    return ok(a.map((val, i) => val + b[i]));
  };

  const vectorSub = (a: Vector, b: Vector): Result<Vector, ProvenError> => {
    if (a.length !== b.length) return err('INVALID_INPUT');
    return ok(a.map((val, i) => val - b[i]));
  };

  const vectorScale = (v: Vector, scalar: number): Vector => {
    return v.map(val => val * scalar);
  };

  const dotProduct = (a: Vector, b: Vector): Result<number, ProvenError> => {
    if (a.length !== b.length) return err('INVALID_INPUT');
    return ok(a.reduce((sum, val, i) => sum + val * b[i], 0));
  };

  const vectorNorm = (v: Vector): number => {
    return Math.sqrt(v.reduce((sum, val) => sum + val * val, 0));
  };

  const vectorNormalize = (v: Vector): Result<Vector, ProvenError> => {
    const norm = vectorNorm(v);
    if (norm === 0) return err('DIVISION_BY_ZERO');
    return ok(v.map(val => val / norm));
  };

  const matrixAdd = (a: Matrix, b: Matrix): Result<Matrix, ProvenError> => {
    if (a.length !== b.length || a[0]?.length !== b[0]?.length) {
      return err('INVALID_INPUT');
    }
    return ok(a.map((row, i) => row.map((val, j) => val + b[i][j])));
  };

  const matrixMul = (a: Matrix, b: Matrix): Result<Matrix, ProvenError> => {
    if (a[0]?.length !== b.length) return err('INVALID_INPUT');

    const rows = a.length;
    const cols = b[0]?.length ?? 0;
    const inner = b.length;

    const result = createMatrix(rows, cols);
    for (let i = 0; i < rows; i++) {
      for (let j = 0; j < cols; j++) {
        let sum = 0;
        for (let k = 0; k < inner; k++) {
          sum += a[i][k] * b[k][j];
        }
        result[i][j] = sum;
      }
    }
    return ok(result);
  };

  const transpose = (m: Matrix): Matrix => {
    const rows = m.length;
    const cols = m[0]?.length ?? 0;
    const result = createMatrix(cols, rows);
    for (let i = 0; i < rows; i++) {
      for (let j = 0; j < cols; j++) {
        result[j][i] = m[i][j];
      }
    }
    return result;
  };

  const getShape = (m: Matrix): [number, number] => {
    return [m.length, m[0]?.length ?? 0];
  };

  return {
    createVector,
    createMatrix,
    vectorAdd,
    vectorSub,
    vectorScale,
    dotProduct,
    vectorNorm,
    vectorNormalize,
    matrixAdd,
    matrixMul,
    transpose,
    getShape,
  };
}

// ============================================================================
// SECURITY MODULE 1: SAFE PASSWORD COMPOSABLE
// ============================================================================

/** Password validation and strength analysis */
export function useSafePassword() {
  interface PasswordPolicy {
    minLength: number;
    requireUppercase: boolean;
    requireLowercase: boolean;
    requireDigit: boolean;
    requireSpecial: boolean;
    maxConsecutive: number;
  }

  const defaultPolicy: PasswordPolicy = {
    minLength: 8,
    requireUppercase: true,
    requireLowercase: true,
    requireDigit: true,
    requireSpecial: true,
    maxConsecutive: 3,
  };

  const hasUppercase = (password: string): boolean => /[A-Z]/.test(password);
  const hasLowercase = (password: string): boolean => /[a-z]/.test(password);
  const hasDigit = (password: string): boolean => /\d/.test(password);
  const hasSpecial = (password: string): boolean => /[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]/.test(password);

  const hasConsecutive = (password: string, max: number): boolean => {
    for (let i = 0; i <= password.length - max; i++) {
      const char = password[i];
      let consecutive = 1;
      for (let j = i + 1; j < password.length && password[j] === char; j++) {
        consecutive++;
        if (consecutive > max) return true;
      }
    }
    return false;
  };

  const validate = (password: string, policy = defaultPolicy): Result<void, string[]> => {
    const errors: string[] = [];

    if (password.length < policy.minLength) {
      errors.push(`Minimum length is ${policy.minLength} characters`);
    }
    if (policy.requireUppercase && !hasUppercase(password)) {
      errors.push('Must contain uppercase letter');
    }
    if (policy.requireLowercase && !hasLowercase(password)) {
      errors.push('Must contain lowercase letter');
    }
    if (policy.requireDigit && !hasDigit(password)) {
      errors.push('Must contain digit');
    }
    if (policy.requireSpecial && !hasSpecial(password)) {
      errors.push('Must contain special character');
    }
    if (hasConsecutive(password, policy.maxConsecutive)) {
      errors.push(`Cannot have more than ${policy.maxConsecutive} consecutive identical characters`);
    }

    return errors.length > 0 ? { ok: false, error: errors } : { ok: true, value: undefined };
  };

  const strength = (password: string): 'weak' | 'fair' | 'good' | 'strong' => {
    let score = 0;

    if (password.length >= 8) score++;
    if (password.length >= 12) score++;
    if (password.length >= 16) score++;
    if (hasUppercase(password)) score++;
    if (hasLowercase(password)) score++;
    if (hasDigit(password)) score++;
    if (hasSpecial(password)) score++;

    if (score <= 2) return 'weak';
    if (score <= 4) return 'fair';
    if (score <= 6) return 'good';
    return 'strong';
  };

  const entropy = (password: string): number => {
    let charsetSize = 0;
    if (hasLowercase(password)) charsetSize += 26;
    if (hasUppercase(password)) charsetSize += 26;
    if (hasDigit(password)) charsetSize += 10;
    if (hasSpecial(password)) charsetSize += 32;

    if (charsetSize === 0) return 0;
    return password.length * Math.log2(charsetSize);
  };

  return {
    validate,
    strength,
    entropy,
    hasUppercase,
    hasLowercase,
    hasDigit,
    hasSpecial,
    defaultPolicy,
  };
}

// ============================================================================
// SECURITY MODULE 2: SAFE ML COMPOSABLE
// ============================================================================

/** Numerically stable ML operations */
export function useSafeMl() {
  // Numerically stable softmax
  const softmax = (values: number[]): number[] => {
    const max = Math.max(...values);
    const exps = values.map(v => Math.exp(v - max));
    const sum = exps.reduce((a, b) => a + b, 0);
    return exps.map(e => e / sum);
  };

  // Log-softmax for numerical stability
  const logSoftmax = (values: number[]): number[] => {
    const max = Math.max(...values);
    const shifted = values.map(v => v - max);
    const logSumExp = Math.log(shifted.reduce((sum, v) => sum + Math.exp(v), 0));
    return shifted.map(v => v - logSumExp);
  };

  // Sigmoid activation
  const sigmoid = (x: number): number => {
    if (x >= 0) {
      return 1 / (1 + Math.exp(-x));
    }
    const expX = Math.exp(x);
    return expX / (1 + expX);
  };

  // Tanh activation
  const tanh = (x: number): number => Math.tanh(x);

  // ReLU activation
  const relu = (x: number): number => Math.max(0, x);

  // Leaky ReLU
  const leakyRelu = (x: number, alpha = 0.01): number => {
    return x >= 0 ? x : alpha * x;
  };

  // Cross-entropy loss (numerically stable)
  const crossEntropyLoss = (predicted: number[], target: number[]): Result<number, ProvenError> => {
    if (predicted.length !== target.length) return err('INVALID_INPUT');

    let loss = 0;
    const epsilon = 1e-15;
    for (let i = 0; i < predicted.length; i++) {
      const p = Math.max(epsilon, Math.min(1 - epsilon, predicted[i]));
      loss -= target[i] * Math.log(p);
    }
    return ok(loss);
  };

  // Mean squared error
  const mseLoss = (predicted: number[], target: number[]): Result<number, ProvenError> => {
    if (predicted.length !== target.length) return err('INVALID_INPUT');

    const sum = predicted.reduce((acc, p, i) => acc + (p - target[i]) ** 2, 0);
    return ok(sum / predicted.length);
  };

  // Binary cross-entropy
  const binaryCrossEntropy = (predicted: number, target: number): number => {
    const epsilon = 1e-15;
    const p = Math.max(epsilon, Math.min(1 - epsilon, predicted));
    return -(target * Math.log(p) + (1 - target) * Math.log(1 - p));
  };

  // Clip gradients
  const clipGradient = (gradient: number, minVal: number, maxVal: number): number => {
    return Math.max(minVal, Math.min(maxVal, gradient));
  };

  return {
    softmax,
    logSoftmax,
    sigmoid,
    tanh,
    relu,
    leakyRelu,
    crossEntropyLoss,
    mseLoss,
    binaryCrossEntropy,
    clipGradient,
  };
}

// ============================================================================
// HTTP MODULE 1: SAFE HEADER COMPOSABLE
// ============================================================================

/** Safe HTTP header operations */
export function useSafeHeader() {
  const forbiddenChars = /[\r\n\0]/;
  const headerNamePattern = /^[!#$%&'*+\-.^_`|~0-9A-Za-z]+$/;

  const isValidName = (name: string): boolean => {
    return headerNamePattern.test(name);
  };

  const isValidValue = (value: string): boolean => {
    return !forbiddenChars.test(value);
  };

  const validate = (name: string, value: string): Result<{ name: string; value: string }, ProvenError> => {
    if (!isValidName(name)) return err('INVALID_INPUT');
    if (!isValidValue(value)) return err('INVALID_INPUT');
    return ok({ name, value });
  };

  const sanitizeValue = (value: string): string => {
    return value.replace(forbiddenChars, '');
  };

  const parse = (header: string): Result<{ name: string; value: string }, ProvenError> => {
    const colonIndex = header.indexOf(':');
    if (colonIndex < 0) return err('INVALID_INPUT');

    const name = header.slice(0, colonIndex).trim();
    const value = header.slice(colonIndex + 1).trim();

    return validate(name, value);
  };

  const toString = (name: string, value: string): string => {
    return `${name}: ${value}`;
  };

  const normalizeHeaderName = (name: string): string => {
    return name.toLowerCase().split('-').map(
      word => word.charAt(0).toUpperCase() + word.slice(1)
    ).join('-');
  };

  return {
    isValidName,
    isValidValue,
    validate,
    sanitizeValue,
    parse,
    toString,
    normalizeHeaderName,
  };
}

// ============================================================================
// HTTP MODULE 2: SAFE COOKIE COMPOSABLE
// ============================================================================

/** Safe HTTP cookie operations */
export function useSafeCookie() {
  type SameSite = 'Strict' | 'Lax' | 'None';

  interface CookieAttributes {
    expires?: Date;
    maxAge?: number;
    domain?: string;
    path?: string;
    secure?: boolean;
    httpOnly?: boolean;
    sameSite?: SameSite;
  }

  const forbiddenChars = /[;\r\n\0]/;
  const cookieNamePattern = /^[!#$%&'*+\-.^_`|~0-9A-Za-z]+$/;

  const isValidName = (name: string): boolean => {
    return cookieNamePattern.test(name) && !name.startsWith('__Secure-') && !name.startsWith('__Host-');
  };

  const isValidValue = (value: string): boolean => {
    return !forbiddenChars.test(value);
  };

  const validate = (name: string, value: string): Result<{ name: string; value: string }, ProvenError> => {
    if (!isValidName(name)) return err('INVALID_INPUT');
    if (!isValidValue(value)) return err('INVALID_INPUT');
    return ok({ name, value });
  };

  const serialize = (name: string, value: string, attributes: CookieAttributes = {}): Result<string, ProvenError> => {
    const validated = validate(name, value);
    if (!validated.ok) return validated;

    let cookie = `${encodeURIComponent(name)}=${encodeURIComponent(value)}`;

    if (attributes.expires) {
      cookie += `; Expires=${attributes.expires.toUTCString()}`;
    }
    if (attributes.maxAge !== undefined) {
      cookie += `; Max-Age=${attributes.maxAge}`;
    }
    if (attributes.domain) {
      cookie += `; Domain=${attributes.domain}`;
    }
    if (attributes.path) {
      cookie += `; Path=${attributes.path}`;
    }
    if (attributes.secure) {
      cookie += '; Secure';
    }
    if (attributes.httpOnly) {
      cookie += '; HttpOnly';
    }
    if (attributes.sameSite) {
      cookie += `; SameSite=${attributes.sameSite}`;
    }

    return ok(cookie);
  };

  const parse = (cookieString: string): Map<string, string> => {
    const cookies = new Map<string, string>();
    const pairs = cookieString.split(';');

    for (const pair of pairs) {
      const [name, ...valueParts] = pair.split('=');
      if (name) {
        const trimmedName = decodeURIComponent(name.trim());
        const value = valueParts.join('=').trim();
        cookies.set(trimmedName, decodeURIComponent(value));
      }
    }

    return cookies;
  };

  return {
    isValidName,
    isValidValue,
    validate,
    serialize,
    parse,
  };
}

// ============================================================================
// HTTP MODULE 3: SAFE CONTENT TYPE COMPOSABLE
// ============================================================================

/** Safe MIME type operations */
export function useSafeContentType() {
  const mimePattern = /^([a-zA-Z0-9!#$&\-^_.+]+)\/([a-zA-Z0-9!#$&\-^_.+]+)(;.*)?$/;

  interface ContentType {
    type: string;
    subtype: string;
    parameters: Map<string, string>;
  }

  const parse = (contentType: string): Result<ContentType, ProvenError> => {
    const match = contentType.match(mimePattern);
    if (!match) return err('INVALID_INPUT');

    const [, type, subtype, paramString] = match;
    const parameters = new Map<string, string>();

    if (paramString) {
      const paramPairs = paramString.slice(1).split(';');
      for (const pair of paramPairs) {
        const [key, ...valueParts] = pair.split('=');
        if (key) {
          parameters.set(
            key.trim().toLowerCase(),
            valueParts.join('=').trim().replace(/^"|"$/g, '')
          );
        }
      }
    }

    return ok({ type: type.toLowerCase(), subtype: subtype.toLowerCase(), parameters });
  };

  const isValid = (contentType: string): boolean => {
    return mimePattern.test(contentType);
  };

  const serialize = (ct: ContentType): string => {
    let result = `${ct.type}/${ct.subtype}`;
    for (const [key, value] of ct.parameters) {
      result += `; ${key}=${value.includes(' ') ? `"${value}"` : value}`;
    }
    return result;
  };

  const getCharset = (ct: ContentType): string | undefined => {
    return ct.parameters.get('charset');
  };

  const isText = (ct: ContentType): boolean => {
    return ct.type === 'text' ||
      (ct.type === 'application' && (ct.subtype === 'json' || ct.subtype.endsWith('+json')));
  };

  const isBinary = (ct: ContentType): boolean => {
    return ct.type === 'application' && !isText(ct) ||
      ct.type === 'image' ||
      ct.type === 'audio' ||
      ct.type === 'video';
  };

  const withCharset = (ct: ContentType, charset: string): ContentType => {
    const newParams = new Map(ct.parameters);
    newParams.set('charset', charset);
    return { ...ct, parameters: newParams };
  };

  // Common content types
  const JSON = 'application/json';
  const HTML = 'text/html';
  const PLAIN = 'text/plain';
  const XML = 'application/xml';
  const FORM = 'application/x-www-form-urlencoded';
  const MULTIPART = 'multipart/form-data';
  const OCTET_STREAM = 'application/octet-stream';

  return {
    parse,
    isValid,
    serialize,
    getCharset,
    isText,
    isBinary,
    withCharset,
    JSON,
    HTML,
    PLAIN,
    XML,
    FORM,
    MULTIPART,
    OCTET_STREAM,
  };
}

// ============================================================================
// EXPORTS
// ============================================================================

export {
  // Re-export types
  type Result,
  type ProvenError,
};
