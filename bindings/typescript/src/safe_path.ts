// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafePath - Typed wrapper for path operations that prevent traversal attacks.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafePath as JsSafePath } from '../../javascript/src/safe_path.js';

/** Result type for path operations. */
export type PathResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Safe path operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export class SafePath {
  /**
   * Check if a path contains directory traversal sequences.
   * Delegates to proven_path_has_traversal via FFI.
   *
   * @param path - The path to check.
   * @returns Result with boolean traversal flag, or error.
   */
  static hasTraversal(path: string): PathResult<boolean> {
    return JsSafePath.hasTraversal(path) as PathResult<boolean>;
  }

  /**
   * Sanitize a filename by removing dangerous characters.
   * Delegates to proven_path_sanitize_filename via FFI.
   *
   * @param filename - The filename to sanitize.
   * @returns Result with sanitized filename, or error.
   */
  static sanitizeFilename(filename: string): PathResult<string> {
    return JsSafePath.sanitizeFilename(filename) as PathResult<string>;
  }
}
