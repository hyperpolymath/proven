// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeChecksum - Checksum algorithms that cannot crash.
 *
 * Provides CRC-32, Adler-32, FNV, and Luhn implementations.
 * @module
 */

import { ok, err } from './result.js';

/** @type {Uint32Array} CRC-32 lookup table */
const CRC32_TABLE = new Uint32Array(256);
for (let tableIndex = 0; tableIndex < 256; tableIndex++) {
  let crc = tableIndex;
  for (let bitIndex = 0; bitIndex < 8; bitIndex++) {
    crc = crc & 1 ? (crc >>> 1) ^ 0xedb88320 : crc >>> 1;
  }
  CRC32_TABLE[tableIndex] = crc >>> 0;
}

/**
 * Safe checksum operations.
 */
export class SafeChecksum {
  /**
   * Calculate CRC-32 checksum.
   *
   * @param {Uint8Array | string} data - Input data
   * @returns {number}
   *
   * @example
   * SafeChecksum.crc32("hello")  // 0x3610a686
   */
  static crc32(data) {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;

    let crc = 0xffffffff;
    for (let byteIndex = 0; byteIndex < bytes.length; byteIndex++) {
      const tableIndex = (crc ^ bytes[byteIndex]) & 0xff;
      crc = (crc >>> 8) ^ CRC32_TABLE[tableIndex];
    }

    return (crc ^ 0xffffffff) >>> 0;
  }

  /**
   * Calculate Adler-32 checksum.
   *
   * @param {Uint8Array | string} data - Input data
   * @returns {number}
   */
  static adler32(data) {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;

    let sumA = 1;
    let sumB = 0;
    const modValue = 65521;

    for (let byteIndex = 0; byteIndex < bytes.length; byteIndex++) {
      sumA = (sumA + bytes[byteIndex]) % modValue;
      sumB = (sumB + sumA) % modValue;
    }

    return ((sumB << 16) | sumA) >>> 0;
  }

  /**
   * Calculate FNV-1a 32-bit hash.
   *
   * @param {Uint8Array | string} data - Input data
   * @returns {number}
   */
  static fnv1a32(data) {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;

    let hash = 0x811c9dc5;
    for (let byteIndex = 0; byteIndex < bytes.length; byteIndex++) {
      hash ^= bytes[byteIndex];
      hash = Math.imul(hash, 0x01000193) >>> 0;
    }

    return hash >>> 0;
  }

  /**
   * Calculate FNV-1a 64-bit hash (as BigInt).
   *
   * @param {Uint8Array | string} data - Input data
   * @returns {bigint}
   */
  static fnv1a64(data) {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;

    let hash = 0xcbf29ce484222325n;
    const prime = 0x100000001b3n;
    const mask = 0xffffffffffffffffn;

    for (let byteIndex = 0; byteIndex < bytes.length; byteIndex++) {
      hash ^= BigInt(bytes[byteIndex]);
      hash = (hash * prime) & mask;
    }

    return hash;
  }

  /**
   * DJB2 hash function.
   *
   * @param {Uint8Array | string} data - Input data
   * @returns {number}
   */
  static djb2(data) {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;

    let hash = 5381;
    for (let byteIndex = 0; byteIndex < bytes.length; byteIndex++) {
      hash = ((hash << 5) + hash + bytes[byteIndex]) >>> 0;
    }

    return hash >>> 0;
  }

  /**
   * SDBM hash function.
   *
   * @param {Uint8Array | string} data - Input data
   * @returns {number}
   */
  static sdbm(data) {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;

    let hash = 0;
    for (let byteIndex = 0; byteIndex < bytes.length; byteIndex++) {
      hash = bytes[byteIndex] + (hash << 6) + (hash << 16) - hash;
      hash = hash >>> 0;
    }

    return hash >>> 0;
  }

  /**
   * Fletcher-16 checksum.
   *
   * @param {Uint8Array | string} data - Input data
   * @returns {number}
   */
  static fletcher16(data) {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;

    let sum1 = 0;
    let sum2 = 0;

    for (let byteIndex = 0; byteIndex < bytes.length; byteIndex++) {
      sum1 = (sum1 + bytes[byteIndex]) % 255;
      sum2 = (sum2 + sum1) % 255;
    }

    return (sum2 << 8) | sum1;
  }

  /**
   * Luhn checksum validation (credit card numbers).
   *
   * @param {string} number - Number string (digits only)
   * @returns {boolean}
   */
  static luhnCheck(number) {
    // Remove spaces and dashes
    const cleaned = number.replace(/[\s-]/g, '');

    if (!/^\d+$/.test(cleaned)) {
      return false;
    }

    if (cleaned.length < 2) {
      return false;
    }

    let sum = 0;
    let isSecond = false;

    for (let digitIndex = cleaned.length - 1; digitIndex >= 0; digitIndex--) {
      let digit = parseInt(cleaned[digitIndex], 10);

      if (isSecond) {
        digit *= 2;
        if (digit > 9) {
          digit -= 9;
        }
      }

      sum += digit;
      isSecond = !isSecond;
    }

    return sum % 10 === 0;
  }

  /**
   * Generate Luhn check digit.
   *
   * @param {string} number - Number string without check digit
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static luhnGenerate(number) {
    const cleaned = number.replace(/[\s-]/g, '');

    if (!/^\d+$/.test(cleaned)) {
      return err('Number must contain only digits');
    }

    let sum = 0;
    let isSecond = true;

    for (let digitIndex = cleaned.length - 1; digitIndex >= 0; digitIndex--) {
      let digit = parseInt(cleaned[digitIndex], 10);

      if (isSecond) {
        digit *= 2;
        if (digit > 9) {
          digit -= 9;
        }
      }

      sum += digit;
      isSecond = !isSecond;
    }

    const checkDigit = (10 - (sum % 10)) % 10;
    return ok(checkDigit);
  }
}

// Export convenience functions
export const crc32 = SafeChecksum.crc32;
export const adler32 = SafeChecksum.adler32;
export const fnv1a32 = SafeChecksum.fnv1a32;
export const fnv1a64 = SafeChecksum.fnv1a64;
export const luhnCheck = SafeChecksum.luhnCheck;
