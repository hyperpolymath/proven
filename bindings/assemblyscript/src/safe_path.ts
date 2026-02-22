// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafePath - Safe filesystem path operations.
 *
 * Thin wrapper around libproven's SafePath functions. All path analysis
 * (traversal detection, filename sanitization) is performed by the formally
 * verified Idris 2 implementation via host-provided WASM imports.
 * This module only handles data marshaling.
 *
 * Corresponds to the SafePath section in proven.h:
 *   - proven_path_has_traversal
 *   - proven_path_sanitize_filename
 */

import {
  Result,
  RESULT_BUF,
  readBoolResult,
  readStringResult,
  encodeString,
  encodedPtr,
  encodedLen,
} from "./common";
import {
  proven_path_has_traversal,
  proven_path_sanitize_filename,
} from "./ffi";

/**
 * SafePath provides safe filesystem path operations.
 *
 * Every method delegates to the corresponding libproven host function.
 * No path manipulation logic is implemented in AssemblyScript.
 */
export class SafePath {
  /**
   * Check if a path contains directory traversal sequences ("..").
   * Delegates to proven_path_has_traversal (Idris 2 verified).
   *
   * @param path - The path to check.
   * @returns Result<bool> where true means traversal was detected.
   */
  static hasTraversal(path: string): Result<bool> {
    encodeString(path);
    proven_path_has_traversal(encodedPtr(), encodedLen(), RESULT_BUF);
    return readBoolResult(RESULT_BUF);
  }

  /**
   * Sanitize a filename by removing dangerous characters.
   * Delegates to proven_path_sanitize_filename (Idris 2 verified).
   *
   * @param filename - The filename to sanitize.
   * @returns Result<string> with sanitized filename, or error status.
   */
  static sanitizeFilename(filename: string): Result<string> {
    encodeString(filename);
    proven_path_sanitize_filename(encodedPtr(), encodedLen(), RESULT_BUF);
    return readStringResult(RESULT_BUF);
  }
}
