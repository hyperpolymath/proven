# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeString - XSS and injection prevention for Nickel
#
# Provides safe escaping for SQL, HTML, JavaScript, and shell contexts.
# All operations are pure and cannot crash.

{
  # Escape HTML special characters to prevent XSS
  escape_html
    : String -> String
    = fun input =>
      input
      |> std.string.replace "&" "&amp;"
      |> std.string.replace "<" "&lt;"
      |> std.string.replace ">" "&gt;"
      |> std.string.replace "\"" "&quot;"
      |> std.string.replace "'" "&#x27;",

  # Escape SQL single quotes to prevent SQL injection
  escape_sql
    : String -> String
    = fun input =>
      std.string.replace "'" "''" input,

  # Escape JavaScript special characters
  escape_js
    : String -> String
    = fun input =>
      input
      |> std.string.replace "\\" "\\\\"
      |> std.string.replace "\"" "\\\""
      |> std.string.replace "'" "\\'"
      |> std.string.replace "\n" "\\n"
      |> std.string.replace "\r" "\\r"
      |> std.string.replace "\t" "\\t"
      |> std.string.replace "<" "\\u003C"
      |> std.string.replace ">" "\\u003E",

  # Escape shell metacharacters
  escape_shell
    : String -> String
    = fun input =>
      input
      |> std.string.replace "\\" "\\\\"
      |> std.string.replace "\"" "\\\""
      |> std.string.replace "$" "\\$"
      |> std.string.replace "`" "\\`"
      |> std.string.replace "!" "\\!",

  # Check if string contains only alphanumeric characters
  is_alphanumeric
    : String -> Bool
    = fun input =>
      std.string.is_match "^[a-zA-Z0-9]*$" input,

  # Check if string is a valid identifier (alphanumeric + underscore, not starting with digit)
  is_identifier
    : String -> Bool
    = fun input =>
      std.string.is_match "^[a-zA-Z_][a-zA-Z0-9_]*$" input,

  # Sanitize to alphanumeric + underscore + hyphen only
  sanitize
    : String -> String
    = fun input =>
      std.string.replace_regex "[^a-zA-Z0-9_-]" "" input,

  # Convert to URL-safe slug
  slugify
    : String -> String
    = fun input =>
      input
      |> std.string.lowercase
      |> std.string.replace_regex "[^a-z0-9\\s-]" ""
      |> std.string.replace_regex "\\s+" "-"
      |> std.string.replace_regex "-+" "-",

  # Truncate string to max length (safe, no crash on short strings)
  truncate
    : String -> Number -> String
    = fun input max_len =>
      if std.string.length input <= max_len then
        input
      else
        std.string.substring 0 max_len input,

  # Non-empty string contract
  NonEmpty = std.contract.from_predicate (fun s => std.string.length s > 0),

  # Max length string contract factory
  MaxLength = fun max_len =>
    std.contract.from_predicate (fun s => std.string.length s <= max_len),

  # Identifier contract (valid programming identifier)
  Identifier = std.contract.from_predicate (fun s =>
    std.string.is_match "^[a-zA-Z_][a-zA-Z0-9_]*$" s
  ),
}
