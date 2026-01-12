// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafePath - Filesystem path operations that cannot crash.
////
//// Provides path sanitization and traversal attack prevention.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Check if a path contains directory traversal sequences.
pub fn has_traversal(path: String) -> Bool {
  string.contains(path, "..")
  || string.contains(path, "~")
}

/// Check if a path is safe (no traversal attacks).
pub fn is_safe(path: String) -> Bool {
  !has_traversal(path)
}

/// Sanitize a filename by removing dangerous characters.
pub fn sanitize_filename(filename: String) -> String {
  filename
  |> string.replace("..", "_")
  |> string.replace("/", "_")
  |> string.replace("\\", "_")
  |> string.replace("<", "_")
  |> string.replace(">", "_")
  |> string.replace(":", "_")
  |> string.replace("\"", "_")
  |> string.replace("|", "_")
  |> string.replace("?", "_")
  |> string.replace("*", "_")
}

/// Safely join path components, rejecting traversal attempts.
pub fn safe_join(base: String, parts: List(String)) -> Option(String) {
  let has_unsafe = list.any(parts, has_traversal)
  case has_unsafe {
    True -> None
    False -> {
      let sanitized = list.map(parts, sanitize_filename)
      let path = list.fold(sanitized, base, fn(acc, part) {
        case string.ends_with(acc, "/") {
          True -> acc <> part
          False -> acc <> "/" <> part
        }
      })
      Some(path)
    }
  }
}
