// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeString - Safe string operations via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This module is a thin wrapper; it does NOT reimplement any logic.

module SafeString {

  public use LibProven;

  /**
   * Check if bytes are valid UTF-8.
   *
   * :arg s: Input string.
   * :returns: ``none`` on error, otherwise true/false.
   */
  proc isValidUtf8(s: string): bool? {
    var (ptr, len) = toCBytes(s);
    var r = provenStringIsValidUtf8(ptr, len);
    if isOk(r.status) then return r.value;
    return none;
  }

  /**
   * Escape string for SQL (single quotes).
   *
   * Note: Prefer parameterized queries over string escaping.
   *
   * :arg s: Input string.
   * :returns: ``none`` on error, otherwise the escaped string.
   */
  proc escapeSql(s: string): string? {
    var (ptr, len) = toCBytes(s);
    var r = provenStringEscapeSql(ptr, len);
    if isOk(r.status) {
      var result = extractString(r);
      return result;
    }
    return none;
  }

  /**
   * Escape string for HTML (prevents XSS).
   *
   * :arg s: Input string.
   * :returns: ``none`` on error, otherwise the escaped string.
   */
  proc escapeHtml(s: string): string? {
    var (ptr, len) = toCBytes(s);
    var r = provenStringEscapeHtml(ptr, len);
    if isOk(r.status) {
      var result = extractString(r);
      return result;
    }
    return none;
  }

  /**
   * Escape string for JavaScript string literals.
   *
   * :arg s: Input string.
   * :returns: ``none`` on error, otherwise the escaped string.
   */
  proc escapeJs(s: string): string? {
    var (ptr, len) = toCBytes(s);
    var r = provenStringEscapeJs(ptr, len);
    if isOk(r.status) {
      var result = extractString(r);
      return result;
    }
    return none;
  }

}
