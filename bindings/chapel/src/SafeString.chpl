// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// SafeString - Safe string operations via libproven FFI.
//
// Status: GATED on proven#88 — the proven_string_* Zig exports are not
// yet present in libproven 0.9.0.

module SafeString {

  public use LibProven;

  /** Check if bytes are valid UTF-8. */
  proc isValidUtf8(s: string): Maybe(bool) {
    var (ptr, len) = toCBytes(s);
    var r = provenStringIsValidUtf8(ptr, len);
    if isOk(r.status) then return some(r.value);
    return absent(bool);
  }

  /** Escape string for SQL (single quotes).  Prefer parameterised queries. */
  proc escapeSql(s: string): Maybe(string) {
    var (ptr, len) = toCBytes(s);
    var r = provenStringEscapeSql(ptr, len);
    if isOk(r.status) then return some(extractString(r));
    return absent(string);
  }

  /** Escape string for HTML (prevents XSS). */
  proc escapeHtml(s: string): Maybe(string) {
    var (ptr, len) = toCBytes(s);
    var r = provenStringEscapeHtml(ptr, len);
    if isOk(r.status) then return some(extractString(r));
    return absent(string);
  }

  /** Escape string for JavaScript string literals. */
  proc escapeJs(s: string): Maybe(string) {
    var (ptr, len) = toCBytes(s);
    var r = provenStringEscapeJs(ptr, len);
    if isOk(r.status) then return some(extractString(r));
    return absent(string);
  }

}
