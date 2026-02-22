// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafePath - Filesystem traversal prevention via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This module is a thin wrapper; it does NOT reimplement any logic.

module SafePath {

  public use LibProven;

  /**
   * Check if path contains directory traversal sequences ("..").
   *
   * :arg path: Filesystem path to check.
   * :returns: ``none`` on error, true if traversal detected, false if safe.
   */
  proc hasTraversal(path: string): bool? {
    var (ptr, len) = toCBytes(path);
    var r = provenPathHasTraversal(ptr, len);
    if isOk(r.status) then return r.value;
    return none;
  }

  /**
   * Sanitize a filename by removing dangerous characters.
   *
   * :arg filename: Raw filename to sanitize.
   * :returns: ``none`` on error, otherwise the sanitized filename.
   */
  proc sanitizeFilename(filename: string): string? {
    var (ptr, len) = toCBytes(filename);
    var r = provenPathSanitizeFilename(ptr, len);
    if isOk(r.status) {
      var result = extractString(r);
      return result;
    }
    return none;
  }

}
