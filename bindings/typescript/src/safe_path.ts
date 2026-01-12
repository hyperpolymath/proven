// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafePath - Filesystem path operations that cannot crash.
 *
 * Provides path sanitization and traversal attack prevention.
 */

import { getExports, encodeString, decodeString, freePtr } from './wasm.js';
import { statusFromCode } from './error.js';

/**
 * Safe path operations with proven correctness guarantees.
 */
export class SafePath {
  /**
   * Check if a path contains directory traversal sequences.
   *
   * Detects ../ and similar attacks that could escape a base directory.
   *
   * @example
   * SafePath.hasTraversal("foo/bar.txt")          // false
   * SafePath.hasTraversal("../etc/passwd")        // true
   * SafePath.hasTraversal("foo/../../../etc")     // true
   */
  static hasTraversal(path: string): boolean {
    const exports = getExports();
    const fn = exports['proven_path_has_traversal'] as (ptr: number, len: number) => number;

    const { ptr, len } = encodeString(path);
    const result = fn(ptr, len);
    freePtr(ptr);

    const status = result >> 16;
    const value = result & 0xffff;

    // On error, assume unsafe
    if (statusFromCode(status) !== 'ok') {
      return true;
    }
    return value === 1;
  }

  /**
   * Check if a path is safe (no traversal attacks).
   *
   * Convenience method, inverse of hasTraversal.
   */
  static isSafe(path: string): boolean {
    return !SafePath.hasTraversal(path);
  }

  /**
   * Sanitize a filename by removing dangerous characters.
   *
   * Removes path separators, null bytes, and other dangerous chars.
   *
   * @example
   * SafePath.sanitizeFilename("report.pdf")           // "report.pdf"
   * SafePath.sanitizeFilename("../../../etc/passwd") // "etc_passwd"
   */
  static sanitizeFilename(filename: string): string | null {
    const exports = getExports();
    const fn = exports['proven_path_sanitize_filename'] as (ptr: number, len: number) => number;

    const { ptr, len } = encodeString(filename);
    const resultPtr = fn(ptr, len);
    freePtr(ptr);

    if (resultPtr === 0) {
      return null;
    }

    const memory = exports['memory'] as WebAssembly.Memory;
    const view = new DataView(memory.buffer);
    const resultLen = view.getUint32(resultPtr, true);

    const sanitized = decodeString(resultPtr + 4, resultLen);
    freePtr(resultPtr);

    return sanitized;
  }

  /**
   * Safely join path components, rejecting traversal attempts.
   *
   * @example
   * SafePath.safeJoin("/var/data", ["user", "file.txt"])
   * // "/var/data/user/file.txt"
   *
   * SafePath.safeJoin("/var/data", ["../etc/passwd"])
   * // null
   */
  static safeJoin(base: string, parts: string[]): string | null {
    for (const part of parts) {
      if (SafePath.hasTraversal(part)) {
        return null;
      }
    }

    let result = base;
    for (const part of parts) {
      const sanitized = SafePath.sanitizeFilename(part);
      if (sanitized === null) {
        return null;
      }
      // Use forward slash for consistency
      result = result.endsWith('/') ? result + sanitized : result + '/' + sanitized;
    }

    return result;
  }
}
