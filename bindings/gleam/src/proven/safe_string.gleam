// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeString - String operations that cannot crash.
////
//// Provides safe escaping for SQL, HTML, and JavaScript.

import gleam/string

/// Escape a string for safe SQL interpolation.
pub fn escape_sql(value: String) -> String {
  string.replace(value, "'", "''")
}

/// Escape a string for safe HTML insertion.
pub fn escape_html(value: String) -> String {
  value
  |> string.replace("&", "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
  |> string.replace("\"", "&quot;")
  |> string.replace("'", "&#x27;")
}

/// Escape a string for safe JavaScript string literal insertion.
pub fn escape_js(value: String) -> String {
  value
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("'", "\\'")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}
