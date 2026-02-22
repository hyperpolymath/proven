// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeString - String operations that cannot crash.
////
//// Thin FFI wrapper over libproven proven_string_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// Check if bytes are valid UTF-8.
@external(erlang, "proven_nif", "string_is_valid_utf8")
pub fn is_valid_utf8(value: String) -> Result(Bool, String)

/// Escape a string for safe SQL interpolation.
/// Note: Prefer parameterized queries over string escaping.
@external(erlang, "proven_nif", "string_escape_sql")
pub fn escape_sql(value: String) -> Result(String, String)

/// Escape a string for safe HTML insertion.
/// Escapes < > & " ' to prevent XSS.
@external(erlang, "proven_nif", "string_escape_html")
pub fn escape_html(value: String) -> Result(String, String)

/// Escape a string for safe JavaScript string literal insertion.
@external(erlang, "proven_nif", "string_escape_js")
pub fn escape_js(value: String) -> Result(String, String)
