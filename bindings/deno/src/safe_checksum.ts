// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeChecksum - CRC and hash verification.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Safe checksum operations backed by formally verified Idris 2 code.
 */
export class SafeChecksum {
  /**
   * Calculate CRC-32 checksum of bytes.
   *
   * @param input - Input data.
   * @returns Result containing the CRC-32 value or an error string.
   */
  static crc32(input: Uint8Array | string): Result<number> {
    const symbols = getLib();
    const bytes = typeof input === 'string' ? new TextEncoder().encode(input) : input;
    const result = symbols.proven_checksum_crc32(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Verify that a CRC-32 checksum matches expected.
   *
   * @param input - Input data.
   * @param expected - Expected CRC-32 value.
   * @returns Result containing a boolean or an error string.
   */
  static verifyCrc32(input: Uint8Array | string, expected: number): Result<boolean> {
    const symbols = getLib();
    const bytes = typeof input === 'string' ? new TextEncoder().encode(input) : input;
    const result = symbols.proven_checksum_verify_crc32(bytes, bytes.length, expected >>> 0);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }
}
