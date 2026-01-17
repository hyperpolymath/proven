// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeUrl - URL parsing and manipulation that cannot crash.
////
//// Provides safe URL operations following RFC 3986.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// URL components after parsing.
pub type UrlComponents {
  UrlComponents(
    scheme: Option(String),
    host: Option(String),
    port: Option(Int),
    path: String,
    query: Option(String),
    fragment: Option(String),
  )
}

/// Error types for URL operations.
pub type UrlError {
  EmptyUrl
  InvalidScheme(scheme: String)
  InvalidPort(port: String)
  InvalidCharacter(position: Int)
  MalformedUrl(message: String)
}

/// Check if a character is a valid scheme character.
fn is_scheme_char(char: String) -> Bool {
  case char {
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" -> True
    "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" -> True
    "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" -> True
    "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" -> True
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    "+" | "-" | "." -> True
    _ -> False
  }
}

/// Check if a scheme is valid (known safe schemes).
pub fn is_safe_scheme(scheme: String) -> Bool {
  case string.lowercase(scheme) {
    "http" | "https" | "ftp" | "ftps" | "mailto" | "tel" | "file" -> True
    "ws" | "wss" | "data" | "blob" -> True
    _ -> False
  }
}

/// Parse a URL string into components.
pub fn parse(url: String) -> Result(UrlComponents, UrlError) {
  let trimmed = string.trim(url)
  case string.is_empty(trimmed) {
    True -> Error(EmptyUrl)
    False -> parse_url(trimmed)
  }
}

fn parse_url(url: String) -> Result(UrlComponents, UrlError) {
  // Extract scheme if present
  case string.split_once(url, "://") {
    Ok(#(scheme, rest)) -> {
      case validate_scheme(scheme) {
        False -> Error(InvalidScheme(scheme: scheme))
        True -> parse_after_scheme(Some(string.lowercase(scheme)), rest)
      }
    }
    Error(_) -> parse_after_scheme(None, url)
  }
}

fn validate_scheme(scheme: String) -> Bool {
  let chars = string.to_graphemes(scheme)
  case chars {
    [] -> False
    [first, ..rest] -> {
      is_alpha(first) && list.all(rest, is_scheme_char)
    }
  }
}

fn is_alpha(char: String) -> Bool {
  case char {
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" -> True
    "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" -> True
    "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" -> True
    "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" -> True
    _ -> False
  }
}

fn parse_after_scheme(
  scheme: Option(String),
  rest: String,
) -> Result(UrlComponents, UrlError) {
  // Split off fragment
  let #(before_fragment, fragment) = case string.split_once(rest, "#") {
    Ok(#(before, after)) -> #(before, Some(after))
    Error(_) -> #(rest, None)
  }

  // Split off query
  let #(before_query, query) = case string.split_once(before_fragment, "?") {
    Ok(#(before, after)) -> #(before, Some(after))
    Error(_) -> #(before_fragment, None)
  }

  // Split host/port from path
  case string.split_once(before_query, "/") {
    Ok(#(host_port, path_part)) -> {
      let parsed_host_port = parse_host_port(host_port)
      Ok(UrlComponents(
        scheme: scheme,
        host: parsed_host_port.0,
        port: parsed_host_port.1,
        path: "/" <> path_part,
        query: query,
        fragment: fragment,
      ))
    }
    Error(_) -> {
      let parsed_host_port = parse_host_port(before_query)
      Ok(UrlComponents(
        scheme: scheme,
        host: parsed_host_port.0,
        port: parsed_host_port.1,
        path: "",
        query: query,
        fragment: fragment,
      ))
    }
  }
}

fn parse_host_port(host_port: String) -> #(Option(String), Option(Int)) {
  case string.is_empty(host_port) {
    True -> #(None, None)
    False ->
      case string.split_once(host_port, ":") {
        Ok(#(host, port_str)) ->
          case int.parse(port_str) {
            Ok(port) -> #(Some(host), Some(port))
            Error(_) -> #(Some(host_port), None)
          }
        Error(_) -> #(Some(host_port), None)
      }
  }
}

/// Check if a string is a valid URL.
pub fn is_valid(url: String) -> Bool {
  case parse(url) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Get the host from a URL.
pub fn get_host(url: String) -> Option(String) {
  case parse(url) {
    Ok(components) -> components.host
    Error(_) -> None
  }
}

/// Get the scheme from a URL.
pub fn get_scheme(url: String) -> Option(String) {
  case parse(url) {
    Ok(components) -> components.scheme
    Error(_) -> None
  }
}

/// Get the path from a URL.
pub fn get_path(url: String) -> Option(String) {
  case parse(url) {
    Ok(components) -> Some(components.path)
    Error(_) -> None
  }
}

/// Get the port from a URL.
pub fn get_port(url: String) -> Option(Int) {
  case parse(url) {
    Ok(components) -> components.port
    Error(_) -> None
  }
}

/// Get the query string from a URL.
pub fn get_query(url: String) -> Option(String) {
  case parse(url) {
    Ok(components) -> components.query
    Error(_) -> None
  }
}

/// Get the fragment from a URL.
pub fn get_fragment(url: String) -> Option(String) {
  case parse(url) {
    Ok(components) -> components.fragment
    Error(_) -> None
  }
}

/// Encode a string for safe URL usage (percent encoding).
pub fn encode(input: String) -> String {
  input
  |> string.to_graphemes()
  |> list.map(encode_char)
  |> string.concat()
}

fn encode_char(char: String) -> String {
  case char {
    // Unreserved characters (RFC 3986)
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" -> char
    "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" -> char
    "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" -> char
    "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" -> char
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> char
    "-" | "_" | "." | "~" -> char
    // Encode everything else
    " " -> "%20"
    "!" -> "%21"
    "#" -> "%23"
    "$" -> "%24"
    "%" -> "%25"
    "&" -> "%26"
    "'" -> "%27"
    "(" -> "%28"
    ")" -> "%29"
    "*" -> "%2A"
    "+" -> "%2B"
    "," -> "%2C"
    "/" -> "%2F"
    ":" -> "%3A"
    ";" -> "%3B"
    "=" -> "%3D"
    "?" -> "%3F"
    "@" -> "%40"
    "[" -> "%5B"
    "]" -> "%5D"
    _ -> char
  }
}

/// Decode a percent-encoded URL string.
pub fn decode(input: String) -> Result(String, UrlError) {
  decode_recursive(string.to_graphemes(input), [], 0)
}

fn decode_recursive(
  chars: List(String),
  accumulated: List(String),
  position: Int,
) -> Result(String, UrlError) {
  case chars {
    [] -> Ok(string.concat(list.reverse(accumulated)))
    ["%", high, low, ..rest] ->
      case decode_hex_pair(high, low) {
        Ok(decoded) -> decode_recursive(rest, [decoded, ..accumulated], position + 3)
        Error(_) -> Error(InvalidCharacter(position: position))
      }
    ["+", ..rest] -> decode_recursive(rest, [" ", ..accumulated], position + 1)
    [char, ..rest] -> decode_recursive(rest, [char, ..accumulated], position + 1)
  }
}

fn decode_hex_pair(high: String, low: String) -> Result(String, Nil) {
  case hex_char_value(high), hex_char_value(low) {
    Ok(high_val), Ok(low_val) -> {
      let code = high_val * 16 + low_val
      Ok(code_to_char(code))
    }
    _, _ -> Error(Nil)
  }
}

fn hex_char_value(char: String) -> Result(Int, Nil) {
  case char {
    "0" -> Ok(0)
    "1" -> Ok(1)
    "2" -> Ok(2)
    "3" -> Ok(3)
    "4" -> Ok(4)
    "5" -> Ok(5)
    "6" -> Ok(6)
    "7" -> Ok(7)
    "8" -> Ok(8)
    "9" -> Ok(9)
    "a" | "A" -> Ok(10)
    "b" | "B" -> Ok(11)
    "c" | "C" -> Ok(12)
    "d" | "D" -> Ok(13)
    "e" | "E" -> Ok(14)
    "f" | "F" -> Ok(15)
    _ -> Error(Nil)
  }
}

fn code_to_char(code: Int) -> String {
  // Basic ASCII characters
  case code {
    32 -> " "
    33 -> "!"
    34 -> "\""
    35 -> "#"
    36 -> "$"
    37 -> "%"
    38 -> "&"
    39 -> "'"
    40 -> "("
    41 -> ")"
    42 -> "*"
    43 -> "+"
    44 -> ","
    45 -> "-"
    46 -> "."
    47 -> "/"
    48 -> "0"
    49 -> "1"
    50 -> "2"
    51 -> "3"
    52 -> "4"
    53 -> "5"
    54 -> "6"
    55 -> "7"
    56 -> "8"
    57 -> "9"
    58 -> ":"
    59 -> ";"
    60 -> "<"
    61 -> "="
    62 -> ">"
    63 -> "?"
    64 -> "@"
    91 -> "["
    92 -> "\\"
    93 -> "]"
    94 -> "^"
    95 -> "_"
    96 -> "`"
    123 -> "{"
    124 -> "|"
    125 -> "}"
    126 -> "~"
    _ -> "?"
  }
}

/// Build a URL from components.
pub fn build(components: UrlComponents) -> String {
  let scheme_part = case components.scheme {
    Some(s) -> s <> "://"
    None -> ""
  }

  let host_part = case components.host {
    Some(h) -> h
    None -> ""
  }

  let port_part = case components.port {
    Some(p) -> ":" <> int.to_string(p)
    None -> ""
  }

  let query_part = case components.query {
    Some(q) -> "?" <> q
    None -> ""
  }

  let fragment_part = case components.fragment {
    Some(f) -> "#" <> f
    None -> ""
  }

  scheme_part <> host_part <> port_part <> components.path <> query_part <> fragment_part
}

/// Join a base URL with a relative path.
pub fn join(base: String, relative: String) -> Result(String, UrlError) {
  case parse(base) {
    Error(err) -> Error(err)
    Ok(base_components) -> {
      case string.starts_with(relative, "/") {
        True ->
          Ok(build(UrlComponents(
            ..base_components,
            path: relative,
            query: None,
            fragment: None,
          )))
        False -> {
          let base_path = base_components.path
          let new_path = case string.ends_with(base_path, "/") {
            True -> base_path <> relative
            False -> {
              let parts = string.split(base_path, "/")
              let without_last = list.take(parts, list.length(parts) - 1)
              string.join(without_last, "/") <> "/" <> relative
            }
          }
          Ok(build(UrlComponents(
            ..base_components,
            path: new_path,
            query: None,
            fragment: None,
          )))
        }
      }
    }
  }
}

/// Normalize a URL (lowercase scheme and host, remove default ports).
pub fn normalize(url: String) -> Result(String, UrlError) {
  case parse(url) {
    Error(err) -> Error(err)
    Ok(components) -> {
      let normalized_port = case components.scheme, components.port {
        Some("http"), Some(80) -> None
        Some("https"), Some(443) -> None
        _, port -> port
      }
      let normalized_host = case components.host {
        Some(h) -> Some(string.lowercase(h))
        None -> None
      }
      Ok(build(UrlComponents(
        ..components,
        host: normalized_host,
        port: normalized_port,
      )))
    }
  }
}
