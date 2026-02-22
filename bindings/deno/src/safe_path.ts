// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafePath - Filesystem path operations that prevent traversal attacks.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, readAndFreeString, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Safe path operations backed by formally verified Idris 2 code.
 */
export class SafePath {
  /**
   * Check if a path attempts directory traversal (e.g. "../../../etc/passwd").
   *
   * @param path - The path to check.
   * @returns Result containing a boolean or an error string.
   */
  static hasTraversal(path: string): Result<boolean> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(path);
    const result = symbols.proven_path_has_traversal(bytes, bytes.length);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Sanitize a filename by removing dangerous characters.
   *
   * @param filename - The filename to sanitize.
   * @returns Result containing the sanitized filename or an error string.
   */
  static sanitizeFilename(filename: string): Result<string> {
    const symbols = getLib();
    const bytes = new TextEncoder().encode(filename);
    const result = symbols.proven_path_sanitize_filename(bytes, bytes.length);
    const status = result[0];
    if (status !== ProvenStatus.OK) return err(statusToError(status));
    const sanitized = readAndFreeString(result[1], Number(result[2]));
    if (sanitized === null) return err('Null string returned');
    return ok(sanitized);
  }
}
