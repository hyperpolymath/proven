// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeHex - Hexadecimal operations that cannot crash.
 *
 * Provides safe hex encoding/decoding with constant-time comparison
 * for security-sensitive operations.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Hex character lookup table for encoding.
 * @type {string}
 */
const HEX_CHARS = '0123456789abcdef';

/**
 * Hex character to value lookup table.
 * @type {Map<string, number>}
 */
const HEX_VALUES = new Map([
  ['0', 0],
  ['1', 1],
  ['2', 2],
  ['3', 3],
  ['4', 4],
  ['5', 5],
  ['6', 6],
  ['7', 7],
  ['8', 8],
  ['9', 9],
  ['a', 10],
  ['b', 11],
  ['c', 12],
  ['d', 13],
  ['e', 14],
  ['f', 15],
  ['A', 10],
  ['B', 11],
  ['C', 12],
  ['D', 13],
  ['E', 14],
  ['F', 15],
]);

/**
 * Safe hexadecimal operations class.
 */
export class SafeHex {
  /**
   * Encode bytes to hexadecimal string.
   *
   * @param {Uint8Array} bytes - The bytes to encode.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   *
   * @example
   * const result = SafeHex.encode(new Uint8Array([0xde, 0xad, 0xbe, 0xef]));
   * if (result.ok) {
   *   console.log(result.value);  // "deadbeef"
   * }
   */
  static encode(bytes) {
    if (!(bytes instanceof Uint8Array)) {
      return err('Input must be a Uint8Array');
    }

    let hexString = '';
    for (let byteIndex = 0; byteIndex < bytes.length; byteIndex++) {
      const byte = bytes[byteIndex];
      hexString += HEX_CHARS[(byte >> 4) & 0x0f];
      hexString += HEX_CHARS[byte & 0x0f];
    }

    return ok(hexString);
  }

  /**
   * Encode bytes to uppercase hexadecimal string.
   *
   * @param {Uint8Array} bytes - The bytes to encode.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   *
   * @example
   * SafeHex.encodeUppercase(new Uint8Array([0xde, 0xad]))  // "DEAD"
   */
  static encodeUppercase(bytes) {
    const result = SafeHex.encode(bytes);
    if (!result.ok) {
      return result;
    }
    return ok(result.value.toUpperCase());
  }

  /**
   * Decode hexadecimal string to bytes.
   *
   * @param {string} hexString - The hex string to decode.
   * @returns {{ ok: true, value: Uint8Array } | { ok: false, error: string }}
   *
   * @example
   * const result = SafeHex.decode('deadbeef');
   * if (result.ok) {
   *   console.log(result.value);  // Uint8Array([0xde, 0xad, 0xbe, 0xef])
   * }
   */
  static decode(hexString) {
    if (typeof hexString !== 'string') {
      return err('Input must be a string');
    }

    // Remove optional 0x prefix and whitespace
    const cleaned = hexString.replace(/^0x/i, '').replace(/\s/g, '');

    if (cleaned.length === 0) {
      return ok(new Uint8Array(0));
    }

    if (cleaned.length % 2 !== 0) {
      return err('Hex string must have even length');
    }

    const bytes = new Uint8Array(cleaned.length / 2);

    for (let charIndex = 0; charIndex < cleaned.length; charIndex += 2) {
      const highNibbleChar = cleaned[charIndex];
      const lowNibbleChar = cleaned[charIndex + 1];

      const highNibble = HEX_VALUES.get(highNibbleChar);
      const lowNibble = HEX_VALUES.get(lowNibbleChar);

      if (highNibble === undefined) {
        return err(`Invalid hex character at position ${charIndex}: '${highNibbleChar}'`);
      }

      if (lowNibble === undefined) {
        return err(`Invalid hex character at position ${charIndex + 1}: '${lowNibbleChar}'`);
      }

      bytes[charIndex / 2] = (highNibble << 4) | lowNibble;
    }

    return ok(bytes);
  }

  /**
   * Format hex string with spaces between bytes.
   *
   * @param {string|Uint8Array} input - Hex string or bytes to format.
   * @param {Object} [options] - Formatting options.
   * @param {number} [options.groupSize=1] - Bytes per group.
   * @param {string} [options.separator=' '] - Group separator.
   * @param {boolean} [options.uppercase=false] - Use uppercase.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   *
   * @example
   * SafeHex.formatSpaced('deadbeef')  // "de ad be ef"
   * SafeHex.formatSpaced('deadbeef', { groupSize: 2 })  // "dead beef"
   */
  static formatSpaced(input, options = {}) {
    const { groupSize = 1, separator = ' ', uppercase = false } = options;

    let hexString;

    if (input instanceof Uint8Array) {
      const encodeResult = SafeHex.encode(input);
      if (!encodeResult.ok) {
        return encodeResult;
      }
      hexString = encodeResult.value;
    } else if (typeof input === 'string') {
      // Validate the hex string by decoding and re-encoding
      const decodeResult = SafeHex.decode(input);
      if (!decodeResult.ok) {
        return decodeResult;
      }
      const encodeResult = SafeHex.encode(decodeResult.value);
      if (!encodeResult.ok) {
        return encodeResult;
      }
      hexString = encodeResult.value;
    } else {
      return err('Input must be a string or Uint8Array');
    }

    if (uppercase) {
      hexString = hexString.toUpperCase();
    }

    // Group bytes (2 hex chars per byte)
    const charsPerGroup = groupSize * 2;
    const groups = [];

    for (let charOffset = 0; charOffset < hexString.length; charOffset += charsPerGroup) {
      groups.push(hexString.slice(charOffset, charOffset + charsPerGroup));
    }

    return ok(groups.join(separator));
  }

  /**
   * Compare two hex strings or byte arrays in constant time.
   *
   * This prevents timing attacks by ensuring the comparison takes
   * the same amount of time regardless of where differences occur.
   *
   * @param {string|Uint8Array} inputA - First input to compare.
   * @param {string|Uint8Array} inputB - Second input to compare.
   * @returns {boolean}
   *
   * @example
   * const isEqual = SafeHex.constantTimeEqual('deadbeef', 'deadbeef');  // true
   * const isNotEqual = SafeHex.constantTimeEqual('dead', 'beef');  // false
   */
  static constantTimeEqual(inputA, inputB) {
    let bytesA;
    let bytesB;

    // Convert inputs to byte arrays
    if (typeof inputA === 'string') {
      const result = SafeHex.decode(inputA);
      if (!result.ok) {
        return false;
      }
      bytesA = result.value;
    } else if (inputA instanceof Uint8Array) {
      bytesA = inputA;
    } else {
      return false;
    }

    if (typeof inputB === 'string') {
      const result = SafeHex.decode(inputB);
      if (!result.ok) {
        return false;
      }
      bytesB = result.value;
    } else if (inputB instanceof Uint8Array) {
      bytesB = inputB;
    } else {
      return false;
    }

    // Length check (cannot be constant-time, but necessary)
    if (bytesA.length !== bytesB.length) {
      return false;
    }

    // Constant-time comparison
    // XOR all bytes and OR the results together
    // If any bytes differ, the result will be non-zero
    let difference = 0;
    for (let byteIndex = 0; byteIndex < bytesA.length; byteIndex++) {
      difference |= bytesA[byteIndex] ^ bytesB[byteIndex];
    }

    return difference === 0;
  }

  /**
   * Check if a string is valid hexadecimal.
   *
   * @param {string} hexString - The string to validate.
   * @returns {boolean}
   *
   * @example
   * SafeHex.isValid('deadbeef')  // true
   * SafeHex.isValid('0xDEAD')    // true
   * SafeHex.isValid('xyz')       // false
   */
  static isValid(hexString) {
    if (typeof hexString !== 'string') {
      return false;
    }

    const cleaned = hexString.replace(/^0x/i, '').replace(/\s/g, '');

    if (cleaned.length === 0) {
      return true;
    }

    if (cleaned.length % 2 !== 0) {
      return false;
    }

    return /^[0-9a-fA-F]+$/.test(cleaned);
  }

  /**
   * Get the byte length of a hex string.
   *
   * @param {string} hexString - The hex string.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static byteLength(hexString) {
    if (typeof hexString !== 'string') {
      return err('Input must be a string');
    }

    const cleaned = hexString.replace(/^0x/i, '').replace(/\s/g, '');

    if (cleaned.length % 2 !== 0) {
      return err('Hex string must have even length');
    }

    return ok(cleaned.length / 2);
  }

  /**
   * Pad a hex string to a specific byte length.
   *
   * @param {string} hexString - The hex string to pad.
   * @param {number} byteLength - Target length in bytes.
   * @param {Object} [options] - Padding options.
   * @param {boolean} [options.padLeft=true] - Pad on the left (default) or right.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   *
   * @example
   * SafeHex.pad('ff', 4)  // "000000ff"
   * SafeHex.pad('ff', 4, { padLeft: false })  // "ff000000"
   */
  static pad(hexString, byteLength, options = {}) {
    const { padLeft = true } = options;

    if (typeof hexString !== 'string') {
      return err('Input must be a string');
    }

    if (!Number.isInteger(byteLength) || byteLength < 0) {
      return err('Byte length must be a non-negative integer');
    }

    // Validate and normalize the hex string
    const decodeResult = SafeHex.decode(hexString);
    if (!decodeResult.ok) {
      return decodeResult;
    }

    const encodeResult = SafeHex.encode(decodeResult.value);
    if (!encodeResult.ok) {
      return encodeResult;
    }

    const normalized = encodeResult.value;
    const targetLength = byteLength * 2;

    if (normalized.length >= targetLength) {
      return ok(normalized);
    }

    const padding = '0'.repeat(targetLength - normalized.length);

    if (padLeft) {
      return ok(padding + normalized);
    }
    return ok(normalized + padding);
  }

  /**
   * Concatenate multiple hex strings or byte arrays.
   *
   * @param {...(string|Uint8Array)} inputs - Inputs to concatenate.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   *
   * @example
   * SafeHex.concat('dead', 'beef')  // "deadbeef"
   */
  static concat(...inputs) {
    const allBytes = [];

    for (let inputIndex = 0; inputIndex < inputs.length; inputIndex++) {
      const input = inputs[inputIndex];
      let bytes;

      if (typeof input === 'string') {
        const result = SafeHex.decode(input);
        if (!result.ok) {
          return err(`Invalid hex at argument ${inputIndex}: ${result.error}`);
        }
        bytes = result.value;
      } else if (input instanceof Uint8Array) {
        bytes = input;
      } else {
        return err(`Argument ${inputIndex} must be a string or Uint8Array`);
      }

      for (let byteIndex = 0; byteIndex < bytes.length; byteIndex++) {
        allBytes.push(bytes[byteIndex]);
      }
    }

    return SafeHex.encode(new Uint8Array(allBytes));
  }

  /**
   * Extract a slice of bytes from a hex string.
   *
   * @param {string} hexString - The hex string to slice.
   * @param {number} start - Start byte index (inclusive).
   * @param {number} [end] - End byte index (exclusive).
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   *
   * @example
   * SafeHex.slice('deadbeef', 1, 3)  // "adbe"
   */
  static slice(hexString, start, end) {
    const decodeResult = SafeHex.decode(hexString);
    if (!decodeResult.ok) {
      return decodeResult;
    }

    const bytes = decodeResult.value;
    const slicedBytes = bytes.slice(start, end);

    return SafeHex.encode(slicedBytes);
  }

  /**
   * XOR two hex strings or byte arrays.
   *
   * @param {string|Uint8Array} inputA - First input.
   * @param {string|Uint8Array} inputB - Second input.
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   *
   * @example
   * SafeHex.xor('ff00', '00ff')  // "ffff"
   */
  static xor(inputA, inputB) {
    let bytesA;
    let bytesB;

    if (typeof inputA === 'string') {
      const result = SafeHex.decode(inputA);
      if (!result.ok) {
        return result;
      }
      bytesA = result.value;
    } else if (inputA instanceof Uint8Array) {
      bytesA = inputA;
    } else {
      return err('First argument must be a string or Uint8Array');
    }

    if (typeof inputB === 'string') {
      const result = SafeHex.decode(inputB);
      if (!result.ok) {
        return result;
      }
      bytesB = result.value;
    } else if (inputB instanceof Uint8Array) {
      bytesB = inputB;
    } else {
      return err('Second argument must be a string or Uint8Array');
    }

    if (bytesA.length !== bytesB.length) {
      return err('Inputs must have the same length');
    }

    const resultBytes = new Uint8Array(bytesA.length);
    for (let byteIndex = 0; byteIndex < bytesA.length; byteIndex++) {
      resultBytes[byteIndex] = bytesA[byteIndex] ^ bytesB[byteIndex];
    }

    return SafeHex.encode(resultBytes);
  }
}
