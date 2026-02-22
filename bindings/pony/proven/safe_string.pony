// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeString - Safe string operations via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This class is a thin wrapper; it does NOT reimplement any logic.

primitive SafeString
  """
  Formally verified text operations that handle encoding safely.

  Methods return `(String | None)` or `(Bool | None)`.

  Example:
  ```pony
  match SafeString.escape_html("<script>alert(1)</script>")
  | let s: String => env.out.print(s)
  | None => env.out.print("Error")
  end
  ```
  """

  fun is_valid_utf8(s: String): (Bool | None) =>
    """Check if bytes are valid UTF-8."""
    let r = _LibProven.string_is_valid_utf8(s.cpointer(), s.size())
    if r.status == ProvenOk() then r.value else None end

  fun escape_sql(s: String): (String | None) =>
    """
    Escape string for SQL (single quotes).
    Note: Prefer parameterized queries over string escaping.
    """
    let r = _LibProven.string_escape_sql(s.cpointer(), s.size())
    _LibProven._extract_string(r)

  fun escape_html(s: String): (String | None) =>
    """Escape string for HTML (prevents XSS)."""
    let r = _LibProven.string_escape_html(s.cpointer(), s.size())
    _LibProven._extract_string(r)

  fun escape_js(s: String): (String | None) =>
    """Escape string for JavaScript string literals."""
    let r = _LibProven.string_escape_js(s.cpointer(), s.size())
    _LibProven._extract_string(r)
