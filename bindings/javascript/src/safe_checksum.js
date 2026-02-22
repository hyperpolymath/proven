// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeChecksum - CRC and hash verification.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * CRC-32 checksum wrapper.
 */
export class Crc32 {
  /** @type {number} */
  value;

  /** @param {number} value */
  constructor(value) { this.value = value; }

  /** @returns {string} Hex representation. */
  toString() { return this.value.toString(16).padStart(8, '0'); }
}

/**
 * Adler-32 checksum wrapper (placeholder for FFI extension).
 */
export class Adler32 {
  /** @type {number} */
  value;

  /** @param {number} value */
  constructor(value) { this.value = value; }
}

/**
 * FNV hash wrapper (placeholder for FFI extension).
 */
export class Fnv {
  /** @type {number} */
  value;

  /** @param {number} value */
  constructor(value) { this.value = value; }
}

/**
 * Luhn check digit wrapper (placeholder for FFI extension).
 */
export class Luhn {
  /** @type {number} */
  value;

  /** @param {number} value */
  constructor(value) { this.value = value; }
}

/**
 * Safe checksum operations backed by formally verified Idris 2 code.
 */
export class SafeChecksum {
  /**
   * Calculate CRC-32 checksum of bytes.
   *
   * @param {Uint8Array|string} input - Input data.
   * @returns {{ ok: true, value: Crc32 } | { ok: false, error: string }}
   */
  static crc32(input) {
    const symbols = getLib();
    const bytes = typeof input === 'string' ? new TextEncoder().encode(input) : input;
    const result = symbols.proven_checksum_crc32(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(new Crc32(Number(result[1])));
  }

  /**
   * Verify that a CRC-32 checksum matches expected.
   *
   * @param {Uint8Array|string} input - Input data.
   * @param {number} expected - Expected CRC-32 value.
   * @returns {{ ok: true, value: boolean } | { ok: false, error: string }}
   */
  static verifyCrc32(input, expected) {
    const symbols = getLib();
    const bytes = typeof input === 'string' ? new TextEncoder().encode(input) : input;
    const result = symbols.proven_checksum_verify_crc32(bytes, bytes.length, expected >>> 0);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }
}
