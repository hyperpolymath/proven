# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafePath - Path operations without traversal attacks for Nickel
#
# Validates and sanitizes filesystem paths to prevent directory traversal,
# null byte injection, and other path-based attacks.

{
  # Result type for path operations
  PathResult = {
    path | String,
    ok | Bool,
    error | String,
  },

  # Check if path contains directory traversal sequences
  has_traversal
    : String -> Bool
    = fun path =>
      std.string.is_match "\\.\\." path
      || std.string.is_match "\\x00" path,

  # Check if path is absolute (starts with /)
  is_absolute
    : String -> Bool
    = fun path =>
      std.string.is_match "^/" path,

  # Check if path is relative (does not start with /)
  is_relative
    : String -> Bool
    = fun path =>
      !(std.string.is_match "^/" path),

  # Sanitize filename (remove dangerous characters)
  sanitize_filename
    : String -> String
    = fun filename =>
      filename
      |> std.string.replace_regex "[/\\\\\\x00]" ""
      |> std.string.replace_regex "^\\.+" ""
      |> std.string.replace_regex "\\s+$" ""
      |> std.string.replace_regex "^\\s+" "",

  # Validate path is safe (no traversal, no null bytes)
  validate
    : String -> { path : String, ok : Bool, error : String }
    = fun path =>
      if std.string.length path == 0 then
        { path = "", ok = false, error = "Empty path" }
      else if std.string.is_match "\\x00" path then
        { path = "", ok = false, error = "Null byte in path" }
      else if std.string.is_match "\\.\\." path then
        { path = "", ok = false, error = "Path traversal detected" }
      else
        { path = path, ok = true, error = "" },

  # Join paths safely (prevents traversal in second component)
  join_safe
    : String -> String -> { path : String, ok : Bool, error : String }
    = fun base child =>
      let validated = validate child in
      if !validated.ok then
        validated
      else if is_absolute child then
        { path = "", ok = false, error = "Child path must be relative" }
      else
        { path = "%{base}/%{child}", ok = true, error = "" },

  # Get file extension (returns empty string if none)
  extension
    : String -> String
    = fun path =>
      let parts = std.string.split "." path in
      if std.array.length parts <= 1 then
        ""
      else
        std.array.last parts,

  # Get basename (filename without directory)
  basename
    : String -> String
    = fun path =>
      let parts = std.string.split "/" path in
      std.array.last parts,

  # Get dirname (directory without filename)
  dirname
    : String -> String
    = fun path =>
      let parts = std.string.split "/" path in
      if std.array.length parts <= 1 then
        "."
      else
        std.string.join "/" (std.array.drop_last 1 parts),

  # Safe path contract (no traversal sequences)
  SafePath = std.contract.from_predicate (fun path =>
    !(std.string.is_match "\\.\\." path)
    && !(std.string.is_match "\\x00" path)
  ),

  # Relative path contract
  RelativePath = std.contract.from_predicate (fun path =>
    !(std.string.is_match "^/" path)
    && !(std.string.is_match "\\.\\." path)
  ),

  # Filename contract (no path separators)
  Filename = std.contract.from_predicate (fun name =>
    !(std.string.is_match "[/\\\\]" name)
    && !(std.string.is_match "\\x00" name)
    && std.string.length name > 0
  ),
}
