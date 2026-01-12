// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeUUID - UUID operations that cannot crash.
 *
 * Provides safe UUID parsing, validation, and generation with version detection.
 * @module
 */

import { ok, err } from './result.js';

/**
 * UUID version enumeration.
 * @readonly
 * @enum {string}
 */
export const UuidVersion = Object.freeze({
  V1: 'v1',
  V3: 'v3',
  V4: 'v4',
  V5: 'v5',
  V6: 'v6',
  V7: 'v7',
  NIL: 'nil',
  MAX: 'max',
  UNKNOWN: 'unknown',
});

/**
 * UUID variant enumeration.
 * @readonly
 * @enum {string}
 */
export const UuidVariant = Object.freeze({
  NCS: 'ncs',
  RFC4122: 'rfc4122',
  MICROSOFT: 'microsoft',
  FUTURE: 'future',
});

/**
 * UUID regex pattern for validation.
 * @type {RegExp}
 */
const UUID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

/**
 * Nil UUID constant (all zeros).
 * @type {string}
 */
const NIL_UUID = '00000000-0000-0000-0000-000000000000';

/**
 * Max UUID constant (all ones).
 * @type {string}
 */
const MAX_UUID = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

/**
 * Safe UUID class with validation and parsing.
 */
export class SafeUUID {
  /** @type {Uint8Array} */
  #bytes;

  /**
   * Create a SafeUUID from validated bytes.
   * Use static methods to create instances.
   *
   * @param {Uint8Array} bytes - The 16-byte UUID data.
   */
  constructor(bytes) {
    if (!(bytes instanceof Uint8Array) || bytes.length !== 16) {
      throw new Error('SafeUUID requires exactly 16 bytes');
    }
    this.#bytes = new Uint8Array(bytes);
  }

  /**
   * Parse a UUID string.
   *
   * Accepts standard hyphenated format (8-4-4-4-12).
   *
   * @param {string} uuidString - The UUID string to parse.
   * @returns {{ ok: true, value: SafeUUID } | { ok: false, error: string }}
   *
   * @example
   * const result = SafeUUID.parse('550e8400-e29b-41d4-a716-446655440000');
   * if (result.ok) {
   *   console.log(result.value.toString());
   * }
   */
  static parse(uuidString) {
    if (typeof uuidString !== 'string') {
      return err('UUID must be a string');
    }

    const normalized = uuidString.trim().toLowerCase();

    if (!UUID_PATTERN.test(normalized)) {
      return err('Invalid UUID format');
    }

    const hexString = normalized.replace(/-/g, '');
    const bytes = new Uint8Array(16);

    for (let byteIndex = 0; byteIndex < 16; byteIndex++) {
      const hexOffset = byteIndex * 2;
      const byteValue = parseInt(hexString.slice(hexOffset, hexOffset + 2), 16);
      bytes[byteIndex] = byteValue;
    }

    return ok(new SafeUUID(bytes));
  }

  /**
   * Create a UUID v4 from random bytes.
   *
   * The bytes should be 16 cryptographically random bytes.
   * This method sets the version (4) and variant (RFC4122) bits.
   *
   * @param {Uint8Array} randomBytes - 16 random bytes.
   * @returns {{ ok: true, value: SafeUUID } | { ok: false, error: string }}
   *
   * @example
   * const randomData = crypto.getRandomValues(new Uint8Array(16));
   * const result = SafeUUID.v4FromBytes(randomData);
   */
  static v4FromBytes(randomBytes) {
    if (!(randomBytes instanceof Uint8Array)) {
      return err('Random bytes must be a Uint8Array');
    }

    if (randomBytes.length !== 16) {
      return err('Random bytes must be exactly 16 bytes');
    }

    const bytes = new Uint8Array(randomBytes);

    // Set version (4) in the 7th byte (bits 4-7)
    bytes[6] = (bytes[6] & 0x0f) | 0x40;

    // Set variant (RFC4122: 10xx) in the 9th byte (bits 6-7)
    bytes[8] = (bytes[8] & 0x3f) | 0x80;

    return ok(new SafeUUID(bytes));
  }

  /**
   * Create a nil UUID (all zeros).
   *
   * @returns {SafeUUID}
   */
  static nil() {
    return new SafeUUID(new Uint8Array(16));
  }

  /**
   * Create a max UUID (all ones).
   *
   * @returns {SafeUUID}
   */
  static max() {
    const bytes = new Uint8Array(16).fill(0xff);
    return new SafeUUID(bytes);
  }

  /**
   * Check if a string is a valid UUID.
   *
   * @param {string} uuidString - The string to validate.
   * @returns {boolean}
   */
  static isValid(uuidString) {
    const result = SafeUUID.parse(uuidString);
    return result.ok;
  }

  /**
   * Get the UUID bytes.
   *
   * @returns {Uint8Array} Copy of the 16-byte UUID data.
   */
  getBytes() {
    return new Uint8Array(this.#bytes);
  }

  /**
   * Get the UUID version.
   *
   * @returns {string} The version string from UuidVersion enum.
   */
  getVersion() {
    // Check for nil UUID
    if (this.#bytes.every((byte) => byte === 0)) {
      return UuidVersion.NIL;
    }

    // Check for max UUID
    if (this.#bytes.every((byte) => byte === 0xff)) {
      return UuidVersion.MAX;
    }

    // Version is stored in bits 4-7 of byte 6
    const versionNibble = (this.#bytes[6] >> 4) & 0x0f;

    switch (versionNibble) {
      case 1:
        return UuidVersion.V1;
      case 3:
        return UuidVersion.V3;
      case 4:
        return UuidVersion.V4;
      case 5:
        return UuidVersion.V5;
      case 6:
        return UuidVersion.V6;
      case 7:
        return UuidVersion.V7;
      default:
        return UuidVersion.UNKNOWN;
    }
  }

  /**
   * Get the UUID variant.
   *
   * @returns {string} The variant string from UuidVariant enum.
   */
  getVariant() {
    const variantByte = this.#bytes[8];

    // Check variant bits in high bits of byte 8
    if ((variantByte & 0x80) === 0) {
      return UuidVariant.NCS;
    } else if ((variantByte & 0xc0) === 0x80) {
      return UuidVariant.RFC4122;
    } else if ((variantByte & 0xe0) === 0xc0) {
      return UuidVariant.MICROSOFT;
    } else {
      return UuidVariant.FUTURE;
    }
  }

  /**
   * Convert to standard hyphenated string format.
   *
   * @returns {string} UUID in 8-4-4-4-12 format.
   *
   * @example
   * uuid.toString()  // "550e8400-e29b-41d4-a716-446655440000"
   */
  toString() {
    const hexChars = '0123456789abcdef';
    let result = '';

    for (let byteIndex = 0; byteIndex < 16; byteIndex++) {
      if (byteIndex === 4 || byteIndex === 6 || byteIndex === 8 || byteIndex === 10) {
        result += '-';
      }
      const byte = this.#bytes[byteIndex];
      result += hexChars[(byte >> 4) & 0x0f];
      result += hexChars[byte & 0x0f];
    }

    return result;
  }

  /**
   * Convert to URN format.
   *
   * @returns {string} UUID in urn:uuid:... format.
   *
   * @example
   * uuid.toUrn()  // "urn:uuid:550e8400-e29b-41d4-a716-446655440000"
   */
  toUrn() {
    return `urn:uuid:${this.toString()}`;
  }

  /**
   * Convert to uppercase string format.
   *
   * @returns {string} UUID in uppercase 8-4-4-4-12 format.
   */
  toUpperCase() {
    return this.toString().toUpperCase();
  }

  /**
   * Check equality with another SafeUUID.
   *
   * @param {SafeUUID} other - The other UUID to compare.
   * @returns {boolean}
   */
  equals(other) {
    if (!(other instanceof SafeUUID)) {
      return false;
    }

    const otherBytes = other.getBytes();
    for (let byteIndex = 0; byteIndex < 16; byteIndex++) {
      if (this.#bytes[byteIndex] !== otherBytes[byteIndex]) {
        return false;
      }
    }

    return true;
  }

  /**
   * Check if this is a nil UUID.
   *
   * @returns {boolean}
   */
  isNil() {
    return this.#bytes.every((byte) => byte === 0);
  }

  /**
   * Check if this is a max UUID.
   *
   * @returns {boolean}
   */
  isMax() {
    return this.#bytes.every((byte) => byte === 0xff);
  }
}
