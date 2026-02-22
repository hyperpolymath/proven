// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_string.carbon - Safe string operations via libproven FFI.
//
// Provides UTF-8 validation and injection-safe escaping for SQL, HTML,
// and JavaScript contexts. All operations delegate to Idris 2 verified code.
// No string logic is implemented in Carbon.

package Proven api;

import Proven library "lib_proven";

// Safe string operations that prevent injection attacks.
//
// All methods call the formally verified Idris 2 implementation via FFI.
class SafeString {

  // Check if a byte sequence is valid UTF-8.
  //
  // Returns Optional(bool) where None indicates the FFI call failed.
  fn IsValidUtf8(data: Cpp.Ptr(u8), len: i64) -> Optional(bool) {
    var result: BoolResult = proven_string_is_valid_utf8(data, len);
    if (Succeeded(result.status)) {
      return Optional(bool).Create(result.value != 0);
    }
    return Optional(bool).None();
  }

  // Escape a string for safe use in SQL queries.
  //
  // Escapes single quotes by doubling them. The returned string must
  // be freed by the caller using proven_free_string().
  // Returns None on failure.
  fn EscapeSql(data: Cpp.Ptr(u8), len: i64) -> Optional(StringResult) {
    var result: StringResult = proven_string_escape_sql(data, len);
    if (Succeeded(result.status)) {
      return Optional(StringResult).Create(result);
    }
    return Optional(StringResult).None();
  }

  // Escape a string for safe use in HTML content.
  //
  // Escapes <, >, &, ", and ' characters. The returned string must
  // be freed by the caller using proven_free_string().
  // Returns None on failure.
  fn EscapeHtml(data: Cpp.Ptr(u8), len: i64) -> Optional(StringResult) {
    var result: StringResult = proven_string_escape_html(data, len);
    if (Succeeded(result.status)) {
      return Optional(StringResult).Create(result);
    }
    return Optional(StringResult).None();
  }

  // Escape a string for safe use in JavaScript string literals.
  //
  // The returned string must be freed by the caller using
  // proven_free_string().
  // Returns None on failure.
  fn EscapeJs(data: Cpp.Ptr(u8), len: i64) -> Optional(StringResult) {
    var result: StringResult = proven_string_escape_js(data, len);
    if (Succeeded(result.status)) {
      return Optional(StringResult).Create(result);
    }
    return Optional(StringResult).None();
  }

  // Free a string previously allocated by any Proven string function.
  //
  // Safe to call with a null pointer.
  fn FreeString(ptr: Cpp.Ptr(i8)) -> () {
    proven_free_string(ptr);
  }
}
