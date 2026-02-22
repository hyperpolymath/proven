// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_path.carbon - Safe filesystem path operations via libproven FFI.
//
// Prevents directory traversal attacks and sanitizes filenames.
// All operations delegate to Idris 2 verified code.
// No path logic is implemented in Carbon.

package Proven api;

import Proven library "lib_proven";

// Safe path operations that prevent traversal attacks.
//
// All methods call the formally verified Idris 2 implementation via FFI.
class SafePath {

  // Check if a path contains directory traversal sequences (e.g., "../").
  //
  // Returns Optional(bool): true if traversal is detected, false if the
  // path is safe. None indicates the FFI call failed.
  fn HasTraversal(data: Cpp.Ptr(u8), len: i64) -> Optional(bool) {
    var result: BoolResult = proven_path_has_traversal(data, len);
    if (Succeeded(result.status)) {
      return Optional(bool).Create(result.value != 0);
    }
    return Optional(bool).None();
  }

  // Sanitize a filename by removing dangerous characters.
  //
  // Returns Optional(StringResult) containing the sanitized filename.
  // The caller must free the returned string using proven_free_string().
  // Returns None on failure.
  fn SanitizeFilename(data: Cpp.Ptr(u8), len: i64) -> Optional(StringResult) {
    var result: StringResult = proven_path_sanitize_filename(data, len);
    if (Succeeded(result.status)) {
      return Optional(StringResult).Create(result);
    }
    return Optional(StringResult).None();
  }
}
