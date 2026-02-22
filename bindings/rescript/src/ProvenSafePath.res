// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafePath - Typed wrapper for path operations that prevent traversal attacks.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafePath FFI wrapper. */
module SafePathJs = {
  @module("../../javascript/src/safe_path.js") @scope("SafePath")
  external hasTraversal: string => jsResult<bool> = "hasTraversal"

  @module("../../javascript/src/safe_path.js") @scope("SafePath")
  external sanitizeFilename: string => jsResult<string> = "sanitizeFilename"
}

/**
 * Check if a path contains directory traversal sequences.
 * Delegates to proven_path_has_traversal via FFI.
 *
 * @param path The path to check.
 * @returns Ok(true/false) or Error.
 */
let hasTraversal = (path: string): result<bool, string> => {
  SafePathJs.hasTraversal(path)->fromJs
}

/**
 * Sanitize a filename by removing dangerous characters.
 * Delegates to proven_path_sanitize_filename via FFI.
 *
 * @param filename The filename to sanitize.
 * @returns Ok(sanitized) or Error.
 */
let sanitizeFilename = (filename: string): result<string, string> => {
  SafePathJs.sanitizeFilename(filename)->fromJs
}
