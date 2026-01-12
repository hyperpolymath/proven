// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * Safe hexadecimal encoding/decoding with constant-time comparison.
 */

import { err, ok, type Result } from './result.ts';

/** Hex encoding case. */
export enum HexCase {
  /** Lowercase hex (a-f). */
  Lower = 'lower',
  /** Uppercase hex (A-F). */
  Upper = 'upper',
}

/** Valid hex string regex. */
const HEX_REGEX = /^[0-9a-fA-F]*$/;

/**
 * Safe hexadecimal operations.
 */
export class SafeHex {
  /**
   * Encode bytes to hex string.
   *
   * @example
   * ```ts
   * const bytes = new Uint8Array([0xde, 0xad, 0xbe, 0xef]);
   * console.log(SafeHex.encode(bytes)); // "deadbeef"
   * console.log(SafeHex.encode(bytes, HexCase.Upper)); // "DEADBEEF"
   * ```
   */
  static encode(data: Uint8Array, hexCase: HexCase = HexCase.Lower): string {
    const chars = hexCase === HexCase.Lower ? '0123456789abcdef' : '0123456789ABCDEF';
    let result = '';
    for (let i = 0; i < data.length; i++) {
      result += chars[(data[i] >> 4) & 0x0f];
      result += chars[data[i] & 0x0f];
    }
    return result;
  }

  /**
   * Decode hex string to bytes.
   *
   * @example
   * ```ts
   * const result = SafeHex.decode("deadbeef");
   * if (result.ok) {
   *   console.log(result.value); // Uint8Array([0xde, 0xad, 0xbe, 0xef])
   * }
   * ```
   */
  static decode(hex: string): Result<Uint8Array> {
    const trimmed = hex.trim();

    if (trimmed.length === 0) {
      return ok(new Uint8Array(0));
    }

    if (trimmed.length % 2 !== 0) {
      return err('Hex string must have even length');
    }

    if (!HEX_REGEX.test(trimmed)) {
      return err('Invalid hex character');
    }

    const bytes = new Uint8Array(trimmed.length / 2);
    for (let i = 0; i < bytes.length; i++) {
      bytes[i] = parseInt(trimmed.slice(i * 2, i * 2 + 2), 16);
    }

    return ok(bytes);
  }

  /**
   * Check if a string is valid hex.
   *
   * @example
   * ```ts
   * SafeHex.isValid("deadbeef"); // true
   * SafeHex.isValid("DEADBEEF"); // true
   * SafeHex.isValid("xyz123");   // false
   * ```
   */
  static isValid(hex: string): boolean {
    const trimmed = hex.trim();
    return trimmed.length % 2 === 0 && HEX_REGEX.test(trimmed);
  }

  /**
   * Constant-time comparison of two hex strings.
   * Prevents timing attacks when comparing secrets like tokens or hashes.
   *
   * @example
   * ```ts
   * const userToken = "a1b2c3d4";
   * const storedToken = "a1b2c3d4";
   * if (SafeHex.constantTimeEqual(userToken, storedToken)) {
   *   // Token is valid
   * }
   * ```
   */
  static constantTimeEqual(hexA: string, hexB: string): boolean {
    const a = hexA.toLowerCase();
    const b = hexB.toLowerCase();

    if (a.length !== b.length) {
      // Still do constant-time work to prevent length timing
      let diff = 0;
      const maxLen = Math.max(a.length, b.length);
      for (let i = 0; i < maxLen; i++) {
        diff |= (a.charCodeAt(i % a.length) || 0) ^ (b.charCodeAt(i % b.length) || 0);
      }
      return false;
    }

    let diff = 0;
    for (let i = 0; i < a.length; i++) {
      diff |= a.charCodeAt(i) ^ b.charCodeAt(i);
    }
    return diff === 0;
  }

  /**
   * Constant-time comparison of two byte arrays.
   * Prevents timing attacks when comparing secrets.
   *
   * @example
   * ```ts
   * const a = new Uint8Array([1, 2, 3]);
   * const b = new Uint8Array([1, 2, 3]);
   * SafeHex.constantTimeEqualBytes(a, b); // true
   * ```
   */
  static constantTimeEqualBytes(a: Uint8Array, b: Uint8Array): boolean {
    if (a.length !== b.length) {
      // Still do constant-time work to prevent length timing
      let diff = 0;
      const maxLen = Math.max(a.length, b.length);
      for (let i = 0; i < maxLen; i++) {
        diff |= (a[i % a.length] || 0) ^ (b[i % b.length] || 0);
      }
      return false;
    }

    let diff = 0;
    for (let i = 0; i < a.length; i++) {
      diff |= a[i] ^ b[i];
    }
    return diff === 0;
  }

  /**
   * Encode a string (UTF-8) to hex.
   *
   * @example
   * ```ts
   * SafeHex.encodeString("hello"); // "68656c6c6f"
   * ```
   */
  static encodeString(str: string, hexCase: HexCase = HexCase.Lower): string {
    const encoder = new TextEncoder();
    return this.encode(encoder.encode(str), hexCase);
  }

  /**
   * Decode hex to a UTF-8 string.
   *
   * @example
   * ```ts
   * SafeHex.decodeString("68656c6c6f"); // Result<"hello">
   * ```
   */
  static decodeString(hex: string): Result<string> {
    const bytesResult = this.decode(hex);
    if (!bytesResult.ok) {
      return bytesResult;
    }

    try {
      const decoder = new TextDecoder('utf-8', { fatal: true });
      return ok(decoder.decode(bytesResult.value));
    } catch {
      return err('Invalid UTF-8 sequence');
    }
  }

  /**
   * Encode a number to hex with optional padding.
   *
   * @example
   * ```ts
   * SafeHex.encodeNumber(255);      // "ff"
   * SafeHex.encodeNumber(255, 4);   // "00ff"
   * SafeHex.encodeNumber(255, 4, HexCase.Upper); // "00FF"
   * ```
   */
  static encodeNumber(
    num: number,
    minLength = 0,
    hexCase: HexCase = HexCase.Lower,
  ): Result<string> {
    if (num < 0) {
      return err('Cannot encode negative number');
    }
    if (!Number.isInteger(num)) {
      return err('Cannot encode non-integer');
    }
    if (num > Number.MAX_SAFE_INTEGER) {
      return err('Number too large');
    }

    let hex = num.toString(16);
    if (hexCase === HexCase.Upper) {
      hex = hex.toUpperCase();
    }

    // Pad to even length
    if (hex.length % 2 !== 0) {
      hex = '0' + hex;
    }

    // Pad to minimum length
    while (hex.length < minLength) {
      hex = '0' + hex;
    }

    return ok(hex);
  }

  /**
   * Encode a bigint to hex with optional padding.
   *
   * @example
   * ```ts
   * SafeHex.encodeBigInt(255n);     // "ff"
   * SafeHex.encodeBigInt(2n ** 128n); // "0100000000000000000000000000000000"
   * ```
   */
  static encodeBigInt(
    num: bigint,
    minLength = 0,
    hexCase: HexCase = HexCase.Lower,
  ): Result<string> {
    if (num < 0n) {
      return err('Cannot encode negative bigint');
    }

    let hex = num.toString(16);
    if (hexCase === HexCase.Upper) {
      hex = hex.toUpperCase();
    }

    // Pad to even length
    if (hex.length % 2 !== 0) {
      hex = '0' + hex;
    }

    // Pad to minimum length
    while (hex.length < minLength) {
      hex = '0' + hex;
    }

    return ok(hex);
  }

  /**
   * Decode hex to a number.
   *
   * @example
   * ```ts
   * SafeHex.decodeNumber("ff"); // Result<255>
   * ```
   */
  static decodeNumber(hex: string): Result<number> {
    const trimmed = hex.trim();
    if (!HEX_REGEX.test(trimmed)) {
      return err('Invalid hex character');
    }
    if (trimmed.length === 0) {
      return ok(0);
    }

    const num = parseInt(trimmed, 16);
    if (num > Number.MAX_SAFE_INTEGER) {
      return err('Number too large for safe integer');
    }

    return ok(num);
  }

  /**
   * Decode hex to a bigint.
   *
   * @example
   * ```ts
   * SafeHex.decodeBigInt("ff"); // Result<255n>
   * ```
   */
  static decodeBigInt(hex: string): Result<bigint> {
    const trimmed = hex.trim();
    if (!HEX_REGEX.test(trimmed)) {
      return err('Invalid hex character');
    }
    if (trimmed.length === 0) {
      return ok(0n);
    }

    return ok(BigInt('0x' + trimmed));
  }

  /**
   * Add "0x" prefix if not present.
   *
   * @example
   * ```ts
   * SafeHex.addPrefix("deadbeef"); // "0xdeadbeef"
   * SafeHex.addPrefix("0xdeadbeef"); // "0xdeadbeef"
   * ```
   */
  static addPrefix(hex: string): string {
    const trimmed = hex.trim();
    if (trimmed.startsWith('0x') || trimmed.startsWith('0X')) {
      return trimmed;
    }
    return '0x' + trimmed;
  }

  /**
   * Remove "0x" prefix if present.
   *
   * @example
   * ```ts
   * SafeHex.removePrefix("0xdeadbeef"); // "deadbeef"
   * SafeHex.removePrefix("deadbeef"); // "deadbeef"
   * ```
   */
  static removePrefix(hex: string): string {
    const trimmed = hex.trim();
    if (trimmed.startsWith('0x') || trimmed.startsWith('0X')) {
      return trimmed.slice(2);
    }
    return trimmed;
  }

  /**
   * Normalize hex to lowercase without prefix.
   *
   * @example
   * ```ts
   * SafeHex.normalize("0xDEADBEEF"); // "deadbeef"
   * ```
   */
  static normalize(hex: string): Result<string> {
    const withoutPrefix = this.removePrefix(hex);
    if (!HEX_REGEX.test(withoutPrefix)) {
      return err('Invalid hex character');
    }
    if (withoutPrefix.length % 2 !== 0) {
      return err('Hex string must have even length');
    }
    return ok(withoutPrefix.toLowerCase());
  }

  /**
   * XOR two hex strings of equal length.
   *
   * @example
   * ```ts
   * SafeHex.xor("ff00", "0ff0"); // Result<"f0f0">
   * ```
   */
  static xor(hexA: string, hexB: string): Result<string> {
    const a = this.decode(hexA);
    const b = this.decode(hexB);

    if (!a.ok) return a;
    if (!b.ok) return b;

    if (a.value.length !== b.value.length) {
      return err('Hex strings must be same length for XOR');
    }

    const result = new Uint8Array(a.value.length);
    for (let i = 0; i < result.length; i++) {
      result[i] = a.value[i] ^ b.value[i];
    }

    return ok(this.encode(result));
  }
}
