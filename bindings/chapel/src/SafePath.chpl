// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
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
   * :returns: ``absent(bool)`` on FFI error; otherwise ``some(true)``
   *           if traversal is detected, ``some(false)`` if safe.
   */
  proc hasTraversal(path: string): Maybe(bool) {
    var (ptr, len) = toCBytes(path);
    var r = provenPathHasTraversal(ptr, len);
    if isOk(r.status) then return some(r.value);
    return absent(bool);
  }

  /**
   * Sanitize a filename by removing dangerous characters.
   *
   * :arg filename: Raw filename to sanitize.
   * :returns: ``absent(string)`` on FFI error; otherwise ``some(sanitised)``.
   *
   * .. note::
   *    GATED on proven#88 — the underlying ``proven_path_sanitize_filename``
   *    Zig export is not yet present in libproven 0.9.0.  Calling this
   *    today produces a linker error.
   */
  proc sanitizeFilename(filename: string): Maybe(string) {
    var (ptr, len) = toCBytes(filename);
    var r = provenPathSanitizeFilename(ptr, len);
    if isOk(r.status) {
      return some(extractString(r));
    }
    return absent(string);
  }

}
