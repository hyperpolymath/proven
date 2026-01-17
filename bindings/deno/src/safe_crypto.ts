// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe cryptographic operations using Web Crypto API.
 */

import { err, ok, type Result } from './result.ts';

/** Supported hash algorithms. */
export type HashAlgorithm = 'SHA-1' | 'SHA-256' | 'SHA-384' | 'SHA-512';

/** Supported symmetric encryption algorithms. */
export type SymmetricAlgorithm = 'AES-GCM' | 'AES-CBC' | 'AES-CTR';

/** Key sizes in bits for AES. */
export type AesKeySize = 128 | 192 | 256;

/** Encrypted data with IV/nonce. */
export interface EncryptedData {
  readonly ciphertext: Uint8Array;
  readonly iv: Uint8Array;
  readonly algorithm: SymmetricAlgorithm;
}

/** HMAC result. */
export interface HmacResult {
  readonly signature: Uint8Array;
  readonly algorithm: HashAlgorithm;
}

/**
 * Safe cryptographic operations using Web Crypto API.
 */
export class SafeCrypto {
  /**
   * Hash data using specified algorithm.
   *
   * @example
   * ```ts
   * const result = await SafeCrypto.hash("hello", "SHA-256");
   * if (result.ok) {
   *   console.log(SafeHex.encode(result.value));
   * }
   * ```
   */
  static async hash(data: string | Uint8Array, algorithm: HashAlgorithm): Promise<Result<Uint8Array>> {
    try {
      const encoded = typeof data === 'string' ? new TextEncoder().encode(data) : data;
      const hashBuffer = await crypto.subtle.digest(algorithm, encoded);
      return ok(new Uint8Array(hashBuffer));
    } catch (error) {
      return err(error instanceof Error ? error.message : 'Hash failed');
    }
  }

  /** Hash data with SHA-256. */
  static async sha256(data: string | Uint8Array): Promise<Result<Uint8Array>> {
    return this.hash(data, 'SHA-256');
  }

  /** Hash data with SHA-512. */
  static async sha512(data: string | Uint8Array): Promise<Result<Uint8Array>> {
    return this.hash(data, 'SHA-512');
  }

  /**
   * Generate cryptographically secure random bytes.
   *
   * @example
   * ```ts
   * const bytes = SafeCrypto.randomBytes(32);
   * ```
   */
  static randomBytes(length: number): Uint8Array {
    const buffer = new Uint8Array(length);
    crypto.getRandomValues(buffer);
    return buffer;
  }

  /**
   * Generate a random integer in range [min, max).
   */
  static randomInt(min: number, max: number): Result<number> {
    if (min >= max) {
      return err('min must be less than max');
    }
    if (!Number.isInteger(min) || !Number.isInteger(max)) {
      return err('min and max must be integers');
    }

    const range = max - min;
    if (range > 2 ** 32) {
      return err('Range too large');
    }

    const bytes = new Uint8Array(4);
    crypto.getRandomValues(bytes);
    const randomValue = new DataView(bytes.buffer).getUint32(0, true);
    return ok(min + (randomValue % range));
  }

  /**
   * Generate an AES key.
   *
   * @example
   * ```ts
   * const key = await SafeCrypto.generateAesKey(256, "AES-GCM");
   * ```
   */
  static async generateAesKey(
    keySize: AesKeySize,
    algorithm: SymmetricAlgorithm,
  ): Promise<Result<CryptoKey>> {
    try {
      const key = await crypto.subtle.generateKey(
        { name: algorithm, length: keySize },
        true,
        ['encrypt', 'decrypt'],
      );
      return ok(key);
    } catch (error) {
      return err(error instanceof Error ? error.message : 'Key generation failed');
    }
  }

  /**
   * Import a raw AES key.
   */
  static async importAesKey(
    keyData: Uint8Array,
    algorithm: SymmetricAlgorithm,
  ): Promise<Result<CryptoKey>> {
    try {
      const key = await crypto.subtle.importKey(
        'raw',
        keyData,
        { name: algorithm },
        true,
        ['encrypt', 'decrypt'],
      );
      return ok(key);
    } catch (error) {
      return err(error instanceof Error ? error.message : 'Key import failed');
    }
  }

  /**
   * Export a CryptoKey to raw bytes.
   */
  static async exportKey(key: CryptoKey): Promise<Result<Uint8Array>> {
    try {
      const exported = await crypto.subtle.exportKey('raw', key);
      return ok(new Uint8Array(exported));
    } catch (error) {
      return err(error instanceof Error ? error.message : 'Key export failed');
    }
  }

  /**
   * Encrypt data using AES-GCM.
   *
   * @example
   * ```ts
   * const key = await SafeCrypto.generateAesKey(256, "AES-GCM");
   * if (key.ok) {
   *   const encrypted = await SafeCrypto.encryptAesGcm(key.value, plaintext);
   * }
   * ```
   */
  static async encryptAesGcm(
    key: CryptoKey,
    plaintext: Uint8Array,
    additionalData?: Uint8Array,
  ): Promise<Result<EncryptedData>> {
    try {
      const iv = this.randomBytes(12); // 96-bit nonce for GCM
      const algorithm: AesGcmParams = {
        name: 'AES-GCM',
        iv,
        additionalData,
      };
      const ciphertext = await crypto.subtle.encrypt(algorithm, key, plaintext);
      return ok({
        ciphertext: new Uint8Array(ciphertext),
        iv,
        algorithm: 'AES-GCM',
      });
    } catch (error) {
      return err(error instanceof Error ? error.message : 'Encryption failed');
    }
  }

  /**
   * Decrypt data using AES-GCM.
   */
  static async decryptAesGcm(
    key: CryptoKey,
    encrypted: EncryptedData,
    additionalData?: Uint8Array,
  ): Promise<Result<Uint8Array>> {
    try {
      const algorithm: AesGcmParams = {
        name: 'AES-GCM',
        iv: encrypted.iv,
        additionalData,
      };
      const plaintext = await crypto.subtle.decrypt(algorithm, key, encrypted.ciphertext);
      return ok(new Uint8Array(plaintext));
    } catch (error) {
      return err(error instanceof Error ? error.message : 'Decryption failed');
    }
  }

  /**
   * Generate HMAC key.
   */
  static async generateHmacKey(algorithm: HashAlgorithm): Promise<Result<CryptoKey>> {
    try {
      const key = await crypto.subtle.generateKey(
        { name: 'HMAC', hash: algorithm },
        true,
        ['sign', 'verify'],
      );
      return ok(key);
    } catch (error) {
      return err(error instanceof Error ? error.message : 'HMAC key generation failed');
    }
  }

  /**
   * Import HMAC key from raw bytes.
   */
  static async importHmacKey(
    keyData: Uint8Array,
    algorithm: HashAlgorithm,
  ): Promise<Result<CryptoKey>> {
    try {
      const key = await crypto.subtle.importKey(
        'raw',
        keyData,
        { name: 'HMAC', hash: algorithm },
        true,
        ['sign', 'verify'],
      );
      return ok(key);
    } catch (error) {
      return err(error instanceof Error ? error.message : 'HMAC key import failed');
    }
  }

  /**
   * Sign data using HMAC.
   */
  static async hmacSign(
    key: CryptoKey,
    data: Uint8Array,
    algorithm: HashAlgorithm,
  ): Promise<Result<HmacResult>> {
    try {
      const signature = await crypto.subtle.sign('HMAC', key, data);
      return ok({
        signature: new Uint8Array(signature),
        algorithm,
      });
    } catch (error) {
      return err(error instanceof Error ? error.message : 'HMAC signing failed');
    }
  }

  /**
   * Verify HMAC signature.
   */
  static async hmacVerify(
    key: CryptoKey,
    signature: Uint8Array,
    data: Uint8Array,
  ): Promise<Result<boolean>> {
    try {
      const valid = await crypto.subtle.verify('HMAC', key, signature, data);
      return ok(valid);
    } catch (error) {
      return err(error instanceof Error ? error.message : 'HMAC verification failed');
    }
  }

  /**
   * Derive a key using PBKDF2.
   *
   * @example
   * ```ts
   * const salt = SafeCrypto.randomBytes(16);
   * const key = await SafeCrypto.deriveKeyPbkdf2(password, salt, 100000);
   * ```
   */
  static async deriveKeyPbkdf2(
    password: string,
    salt: Uint8Array,
    iterations: number,
    keyLength: AesKeySize = 256,
    hash: HashAlgorithm = 'SHA-256',
  ): Promise<Result<CryptoKey>> {
    try {
      const passwordKey = await crypto.subtle.importKey(
        'raw',
        new TextEncoder().encode(password),
        'PBKDF2',
        false,
        ['deriveBits', 'deriveKey'],
      );

      const derivedKey = await crypto.subtle.deriveKey(
        {
          name: 'PBKDF2',
          salt,
          iterations,
          hash,
        },
        passwordKey,
        { name: 'AES-GCM', length: keyLength },
        true,
        ['encrypt', 'decrypt'],
      );

      return ok(derivedKey);
    } catch (error) {
      return err(error instanceof Error ? error.message : 'Key derivation failed');
    }
  }

  /**
   * Constant-time comparison of two byte arrays.
   * Prevents timing attacks.
   */
  static constantTimeEqual(a: Uint8Array, b: Uint8Array): boolean {
    if (a.length !== b.length) {
      // Still do work to prevent length timing
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
   * Generate a secure random token as a hex string.
   */
  static generateToken(byteLength: number = 32): string {
    const bytes = this.randomBytes(byteLength);
    return Array.from(bytes, (b) => b.toString(16).padStart(2, '0')).join('');
  }

  /**
   * Generate a secure random UUID v4.
   */
  static generateUuidV4(): string {
    const bytes = this.randomBytes(16);
    bytes[6] = (bytes[6] & 0x0f) | 0x40;
    bytes[8] = (bytes[8] & 0x3f) | 0x80;

    const hex = Array.from(bytes, (b) => b.toString(16).padStart(2, '0')).join('');
    return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`;
  }
}
