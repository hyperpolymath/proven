// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeCrypto - Safe cryptographic operations.
 *
 * Provides safe wrappers for encoding, hashing, and random generation.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Base64 encoding utilities.
 */
export class Base64 {
  /**
   * Encode data to base64.
   *
   * @param {Uint8Array | string} data - Data to encode
   * @returns {string}
   */
  static encode(data) {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    let binary = '';
    for (let byteIndex = 0; byteIndex < bytes.length; byteIndex++) {
      binary += String.fromCharCode(bytes[byteIndex]);
    }
    return btoa(binary);
  }

  /**
   * Decode base64 to bytes.
   *
   * @param {string} encoded - Base64 string
   * @returns {{ ok: true, value: Uint8Array } | { ok: false, error: string }}
   */
  static decode(encoded) {
    try {
      const binary = atob(encoded);
      const bytes = new Uint8Array(binary.length);
      for (let charIndex = 0; charIndex < binary.length; charIndex++) {
        bytes[charIndex] = binary.charCodeAt(charIndex);
      }
      return ok(bytes);
    } catch (error) {
      return err('Invalid base64 string');
    }
  }

  /**
   * Decode base64 to string.
   *
   * @param {string} encoded - Base64 string
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  static decodeToString(encoded) {
    const result = Base64.decode(encoded);
    if (!result.ok) {
      return result;
    }
    return ok(new TextDecoder().decode(result.value));
  }

  /**
   * Encode to URL-safe base64.
   *
   * @param {Uint8Array | string} data - Data to encode
   * @returns {string}
   */
  static encodeUrlSafe(data) {
    return Base64.encode(data).replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '');
  }

  /**
   * Decode URL-safe base64.
   *
   * @param {string} encoded - URL-safe base64 string
   * @returns {{ ok: true, value: Uint8Array } | { ok: false, error: string }}
   */
  static decodeUrlSafe(encoded) {
    let str = encoded.replace(/-/g, '+').replace(/_/g, '/');
    // Add padding if needed
    while (str.length % 4 !== 0) {
      str += '=';
    }
    return Base64.decode(str);
  }
}

/**
 * Hex encoding utilities.
 */
export class Hex {
  /**
   * Encode data to hex.
   *
   * @param {Uint8Array} data - Data to encode
   * @returns {string}
   */
  static encode(data) {
    return Array.from(data)
      .map((byte) => byte.toString(16).padStart(2, '0'))
      .join('');
  }

  /**
   * Decode hex to bytes.
   *
   * @param {string} hex - Hex string
   * @returns {{ ok: true, value: Uint8Array } | { ok: false, error: string }}
   */
  static decode(hex) {
    const str = hex.toLowerCase();
    if (str.length % 2 !== 0) {
      return err('Hex string must have even length');
    }
    if (!/^[0-9a-f]*$/.test(str)) {
      return err('Invalid hex characters');
    }

    const bytes = new Uint8Array(str.length / 2);
    for (let charIndex = 0; charIndex < str.length; charIndex += 2) {
      bytes[charIndex / 2] = parseInt(str.slice(charIndex, charIndex + 2), 16);
    }
    return ok(bytes);
  }
}

/**
 * Secure random number generation.
 */
export class SecureRandom {
  /**
   * Generate random bytes.
   *
   * @param {number} length - Number of bytes
   * @returns {Uint8Array}
   */
  static bytes(length) {
    if (length < 0 || length > 65536) {
      throw new Error('Length must be between 0 and 65536');
    }
    const bytes = new Uint8Array(length);
    crypto.getRandomValues(bytes);
    return bytes;
  }

  /**
   * Generate a random integer in range [min, max).
   *
   * @param {number} min - Minimum (inclusive)
   * @param {number} max - Maximum (exclusive)
   * @returns {number}
   */
  static int(min, max) {
    if (min >= max) {
      throw new Error('min must be less than max');
    }
    const range = max - min;
    const randomBytes = new Uint32Array(1);
    crypto.getRandomValues(randomBytes);
    return min + (randomBytes[0] % range);
  }

  /**
   * Generate a random float in range [0, 1).
   *
   * @returns {number}
   */
  static float() {
    const randomBytes = new Uint32Array(1);
    crypto.getRandomValues(randomBytes);
    return randomBytes[0] / 0x100000000;
  }

  /**
   * Generate a random hex string.
   *
   * @param {number} length - Number of hex characters
   * @returns {string}
   */
  static hex(length) {
    const byteLength = Math.ceil(length / 2);
    const bytes = SecureRandom.bytes(byteLength);
    return Hex.encode(bytes).slice(0, length);
  }

  /**
   * Generate a random alphanumeric string.
   *
   * @param {number} length - String length
   * @returns {string}
   */
  static alphanumeric(length) {
    const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    let result = '';
    const randomBytes = SecureRandom.bytes(length);
    for (let charIndex = 0; charIndex < length; charIndex++) {
      result += chars[randomBytes[charIndex] % chars.length];
    }
    return result;
  }

  /**
   * Shuffle an array (Fisher-Yates).
   *
   * @template T
   * @param {T[]} array - Array to shuffle
   * @returns {T[]}
   */
  static shuffle(array) {
    const result = [...array];
    for (let swapIndex = result.length - 1; swapIndex > 0; swapIndex--) {
      const randomIndex = SecureRandom.int(0, swapIndex + 1);
      [result[swapIndex], result[randomIndex]] = [result[randomIndex], result[swapIndex]];
    }
    return result;
  }

  /**
   * Pick random elements from array.
   *
   * @template T
   * @param {T[]} array - Source array
   * @param {number} count - Number of elements to pick
   * @returns {T[]}
   */
  static sample(array, count) {
    if (count > array.length) {
      count = array.length;
    }
    return SecureRandom.shuffle(array).slice(0, count);
  }
}

/**
 * Constant-time comparison to prevent timing attacks.
 */
export class ConstantTime {
  /**
   * Compare two byte arrays in constant time.
   *
   * @param {Uint8Array} a - First array
   * @param {Uint8Array} b - Second array
   * @returns {boolean}
   */
  static compare(a, b) {
    if (a.length !== b.length) {
      return false;
    }

    let diff = 0;
    for (let byteIndex = 0; byteIndex < a.length; byteIndex++) {
      diff |= a[byteIndex] ^ b[byteIndex];
    }
    return diff === 0;
  }

  /**
   * Compare two strings in constant time.
   *
   * @param {string} a - First string
   * @param {string} b - Second string
   * @returns {boolean}
   */
  static compareStrings(a, b) {
    const aBytes = new TextEncoder().encode(a);
    const bBytes = new TextEncoder().encode(b);
    return ConstantTime.compare(aBytes, bBytes);
  }
}

/**
 * Hash using Web Crypto API.
 */
export class Hash {
  /**
   * Compute SHA-256 hash.
   *
   * @param {Uint8Array | string} data - Data to hash
   * @returns {Promise<Uint8Array>}
   */
  static async sha256(data) {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    const hashBuffer = await crypto.subtle.digest('SHA-256', bytes);
    return new Uint8Array(hashBuffer);
  }

  /**
   * Compute SHA-256 hash as hex string.
   *
   * @param {Uint8Array | string} data - Data to hash
   * @returns {Promise<string>}
   */
  static async sha256Hex(data) {
    const hash = await Hash.sha256(data);
    return Hex.encode(hash);
  }

  /**
   * Compute SHA-384 hash.
   *
   * @param {Uint8Array | string} data - Data to hash
   * @returns {Promise<Uint8Array>}
   */
  static async sha384(data) {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    const hashBuffer = await crypto.subtle.digest('SHA-384', bytes);
    return new Uint8Array(hashBuffer);
  }

  /**
   * Compute SHA-512 hash.
   *
   * @param {Uint8Array | string} data - Data to hash
   * @returns {Promise<Uint8Array>}
   */
  static async sha512(data) {
    const bytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;
    const hashBuffer = await crypto.subtle.digest('SHA-512', bytes);
    return new Uint8Array(hashBuffer);
  }
}

/**
 * HMAC using Web Crypto API.
 */
export class Hmac {
  /**
   * Compute HMAC-SHA256.
   *
   * @param {Uint8Array | string} key - Secret key
   * @param {Uint8Array | string} data - Data to authenticate
   * @returns {Promise<Uint8Array>}
   */
  static async sha256(key, data) {
    const keyBytes = typeof key === 'string' ? new TextEncoder().encode(key) : key;
    const dataBytes = typeof data === 'string' ? new TextEncoder().encode(data) : data;

    const cryptoKey = await crypto.subtle.importKey('raw', keyBytes, { name: 'HMAC', hash: 'SHA-256' }, false, ['sign']);

    const signature = await crypto.subtle.sign('HMAC', cryptoKey, dataBytes);
    return new Uint8Array(signature);
  }

  /**
   * Compute HMAC-SHA256 as hex string.
   *
   * @param {Uint8Array | string} key - Secret key
   * @param {Uint8Array | string} data - Data to authenticate
   * @returns {Promise<string>}
   */
  static async sha256Hex(key, data) {
    const hmac = await Hmac.sha256(key, data);
    return Hex.encode(hmac);
  }

  /**
   * Verify HMAC-SHA256.
   *
   * @param {Uint8Array | string} key - Secret key
   * @param {Uint8Array | string} data - Data to verify
   * @param {Uint8Array | string} expected - Expected HMAC
   * @returns {Promise<boolean>}
   */
  static async verifySha256(key, data, expected) {
    const computed = await Hmac.sha256(key, data);
    const expectedBytes = typeof expected === 'string' ? Hex.decode(expected) : { ok: true, value: expected };

    if (!expectedBytes.ok) {
      return false;
    }

    return ConstantTime.compare(computed, expectedBytes.value);
  }
}

/**
 * Safe crypto utilities.
 */
export class SafeCrypto {
  /**
   * Generate a secure token.
   *
   * @param {number} [length=32] - Token length in bytes
   * @returns {string} Hex-encoded token
   */
  static generateToken(length = 32) {
    return Hex.encode(SecureRandom.bytes(length));
  }

  /**
   * Generate a URL-safe token.
   *
   * @param {number} [length=32] - Token length in bytes
   * @returns {string} URL-safe base64 token
   */
  static generateUrlSafeToken(length = 32) {
    return Base64.encodeUrlSafe(SecureRandom.bytes(length));
  }

  /**
   * Hash a password (simple - use bcrypt/argon2 in production).
   *
   * @param {string} password - Password to hash
   * @param {string} [salt] - Optional salt (generated if not provided)
   * @returns {Promise<{hash: string, salt: string}>}
   */
  static async hashPassword(password, salt) {
    const saltBytes = salt ? (Hex.decode(salt).ok ? Hex.decode(salt).value : SecureRandom.bytes(16)) : SecureRandom.bytes(16);
    const saltHex = Hex.encode(saltBytes);

    // Combine password and salt
    const combined = new TextEncoder().encode(password + saltHex);
    const hash = await Hash.sha256Hex(combined);

    return { hash, salt: saltHex };
  }

  /**
   * Verify a password.
   *
   * @param {string} password - Password to verify
   * @param {string} hash - Expected hash
   * @param {string} salt - Salt used in hashing
   * @returns {Promise<boolean>}
   */
  static async verifyPassword(password, hash, salt) {
    const result = await SafeCrypto.hashPassword(password, salt);
    return ConstantTime.compareStrings(result.hash, hash);
  }
}
