// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeJson - JSON operations that cannot crash.
////
//// Provides safe JSON parsing and serialization following RFC 8259.

import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// JSON value types.
pub type JsonValue {
  JsonNull
  JsonBool(Bool)
  JsonNumber(Float)
  JsonString(String)
  JsonArray(List(JsonValue))
  JsonObject(Dict(String, JsonValue))
}

/// Error types for JSON operations.
pub type JsonError {
  UnexpectedEndOfInput
  UnexpectedCharacter(position: Int, character: String)
  InvalidNumber(position: Int)
  InvalidString(position: Int, message: String)
  InvalidEscape(position: Int)
  MaxDepthExceeded(depth: Int)
  InvalidUtf8
}

/// Maximum parsing depth to prevent stack overflow.
pub const max_depth = 100

/// Parse a JSON string into a JsonValue.
pub fn parse(input: String) -> Result(JsonValue, JsonError) {
  let trimmed = string.trim(input)
  case string.is_empty(trimmed) {
    True -> Error(UnexpectedEndOfInput)
    False -> {
      let chars = string.to_graphemes(trimmed)
      case parse_value(chars, 0, 0) {
        Ok(#(value, _, _)) -> Ok(value)
        Error(err) -> Error(err)
      }
    }
  }
}

fn parse_value(
  chars: List(String),
  position: Int,
  depth: Int,
) -> Result(#(JsonValue, List(String), Int), JsonError) {
  case depth > max_depth {
    True -> Error(MaxDepthExceeded(depth: depth))
    False -> {
      let skipped = skip_whitespace(chars)
      case skipped {
        [] -> Error(UnexpectedEndOfInput)
        ["n", "u", "l", "l", ..rest] -> Ok(#(JsonNull, rest, position + 4))
        ["t", "r", "u", "e", ..rest] -> Ok(#(JsonBool(True), rest, position + 4))
        ["f", "a", "l", "s", "e", ..rest] -> Ok(#(JsonBool(False), rest, position + 5))
        ["\"", ..rest] -> parse_string(rest, position + 1)
        ["[", ..rest] -> parse_array(rest, position + 1, depth + 1)
        ["{", ..rest] -> parse_object(rest, position + 1, depth + 1)
        ["-", ..rest] -> parse_number(["-", ..rest], position)
        [c, ..] ->
          case is_digit(c) {
            True -> parse_number(skipped, position)
            False -> Error(UnexpectedCharacter(position: position, character: c))
          }
      }
    }
  }
}

fn skip_whitespace(chars: List(String)) -> List(String) {
  case chars {
    [" ", ..rest] -> skip_whitespace(rest)
    ["\t", ..rest] -> skip_whitespace(rest)
    ["\n", ..rest] -> skip_whitespace(rest)
    ["\r", ..rest] -> skip_whitespace(rest)
    _ -> chars
  }
}

fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn parse_string(
  chars: List(String),
  position: Int,
) -> Result(#(JsonValue, List(String), Int), JsonError) {
  parse_string_content(chars, position, [])
}

fn parse_string_content(
  chars: List(String),
  position: Int,
  accumulated: List(String),
) -> Result(#(JsonValue, List(String), Int), JsonError) {
  case chars {
    [] -> Error(UnexpectedEndOfInput)
    ["\"", ..rest] ->
      Ok(#(JsonString(string.concat(list.reverse(accumulated))), rest, position + 1))
    ["\\", escaped, ..rest] -> {
      case parse_escape(escaped) {
        Ok(unescaped) ->
          parse_string_content(rest, position + 2, [unescaped, ..accumulated])
        Error(_) -> Error(InvalidEscape(position: position))
      }
    }
    [c, ..rest] -> parse_string_content(rest, position + 1, [c, ..accumulated])
  }
}

fn parse_escape(char: String) -> Result(String, Nil) {
  case char {
    "\"" -> Ok("\"")
    "\\" -> Ok("\\")
    "/" -> Ok("/")
    "b" -> Ok("\u{0008}")
    "f" -> Ok("\u{000C}")
    "n" -> Ok("\n")
    "r" -> Ok("\r")
    "t" -> Ok("\t")
    _ -> Error(Nil)
  }
}

fn parse_number(
  chars: List(String),
  position: Int,
) -> Result(#(JsonValue, List(String), Int), JsonError) {
  let #(number_chars, rest) = collect_number_chars(chars, [])
  let number_str = string.concat(list.reverse(number_chars))
  case float.parse(number_str) {
    Ok(f) -> Ok(#(JsonNumber(f), rest, position + list.length(number_chars)))
    Error(_) ->
      case int.parse(number_str) {
        Ok(i) ->
          Ok(#(JsonNumber(int.to_float(i)), rest, position + list.length(number_chars)))
        Error(_) -> Error(InvalidNumber(position: position))
      }
  }
}

fn collect_number_chars(
  chars: List(String),
  accumulated: List(String),
) -> #(List(String), List(String)) {
  case chars {
    [c, ..rest] ->
      case is_number_char(c) {
        True -> collect_number_chars(rest, [c, ..accumulated])
        False -> #(accumulated, chars)
      }
    [] -> #(accumulated, [])
  }
}

fn is_number_char(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    "-" | "+" | "." | "e" | "E" -> True
    _ -> False
  }
}

fn parse_array(
  chars: List(String),
  position: Int,
  depth: Int,
) -> Result(#(JsonValue, List(String), Int), JsonError) {
  let skipped = skip_whitespace(chars)
  case skipped {
    ["]", ..rest] -> Ok(#(JsonArray([]), rest, position + 1))
    _ -> parse_array_elements(skipped, position, depth, [])
  }
}

fn parse_array_elements(
  chars: List(String),
  position: Int,
  depth: Int,
  accumulated: List(JsonValue),
) -> Result(#(JsonValue, List(String), Int), JsonError) {
  case parse_value(chars, position, depth) {
    Error(err) -> Error(err)
    Ok(#(value, rest, new_position)) -> {
      let skipped = skip_whitespace(rest)
      case skipped {
        ["]", ..remaining] ->
          Ok(#(JsonArray(list.reverse([value, ..accumulated])), remaining, new_position + 1))
        [",", ..remaining] ->
          parse_array_elements(
            skip_whitespace(remaining),
            new_position + 1,
            depth,
            [value, ..accumulated],
          )
        [c, ..] -> Error(UnexpectedCharacter(position: new_position, character: c))
        [] -> Error(UnexpectedEndOfInput)
      }
    }
  }
}

fn parse_object(
  chars: List(String),
  position: Int,
  depth: Int,
) -> Result(#(JsonValue, List(String), Int), JsonError) {
  let skipped = skip_whitespace(chars)
  case skipped {
    ["}", ..rest] -> Ok(#(JsonObject(dict.new()), rest, position + 1))
    _ -> parse_object_members(skipped, position, depth, dict.new())
  }
}

fn parse_object_members(
  chars: List(String),
  position: Int,
  depth: Int,
  accumulated: Dict(String, JsonValue),
) -> Result(#(JsonValue, List(String), Int), JsonError) {
  let skipped = skip_whitespace(chars)
  case skipped {
    ["\"", ..rest] -> {
      case parse_string_content(rest, position + 1, []) {
        Error(err) -> Error(err)
        Ok(#(JsonString(key), after_key, key_position)) -> {
          let after_colon = skip_whitespace(after_key)
          case after_colon {
            [":", ..value_chars] -> {
              case parse_value(skip_whitespace(value_chars), key_position + 1, depth) {
                Error(err) -> Error(err)
                Ok(#(value, rest_after_value, new_position)) -> {
                  let new_accumulated = dict.insert(accumulated, key, value)
                  let skipped_after = skip_whitespace(rest_after_value)
                  case skipped_after {
                    ["}", ..remaining] ->
                      Ok(#(JsonObject(new_accumulated), remaining, new_position + 1))
                    [",", ..remaining] ->
                      parse_object_members(
                        skip_whitespace(remaining),
                        new_position + 1,
                        depth,
                        new_accumulated,
                      )
                    [c, ..] ->
                      Error(UnexpectedCharacter(position: new_position, character: c))
                    [] -> Error(UnexpectedEndOfInput)
                  }
                }
              }
            }
            [c, ..] -> Error(UnexpectedCharacter(position: key_position, character: c))
            [] -> Error(UnexpectedEndOfInput)
          }
        }
        Ok(_) -> Error(InvalidString(position: position, message: "Expected string key"))
      }
    }
    [c, ..] -> Error(UnexpectedCharacter(position: position, character: c))
    [] -> Error(UnexpectedEndOfInput)
  }
}

/// Stringify a JsonValue to a JSON string.
pub fn stringify(value: JsonValue) -> String {
  stringify_value(value)
}

fn stringify_value(value: JsonValue) -> String {
  case value {
    JsonNull -> "null"
    JsonBool(True) -> "true"
    JsonBool(False) -> "false"
    JsonNumber(n) -> float.to_string(n)
    JsonString(s) -> stringify_string(s)
    JsonArray(items) -> stringify_array(items)
    JsonObject(obj) -> stringify_object(obj)
  }
}

fn stringify_string(input: String) -> String {
  let escaped =
    input
    |> string.to_graphemes()
    |> list.map(escape_char)
    |> string.concat()
  "\"" <> escaped <> "\""
}

fn escape_char(char: String) -> String {
  case char {
    "\"" -> "\\\""
    "\\" -> "\\\\"
    "\n" -> "\\n"
    "\r" -> "\\r"
    "\t" -> "\\t"
    _ -> char
  }
}

fn stringify_array(items: List(JsonValue)) -> String {
  let elements =
    items
    |> list.map(stringify_value)
    |> string.join(",")
  "[" <> elements <> "]"
}

fn stringify_object(obj: Dict(String, JsonValue)) -> String {
  let pairs =
    obj
    |> dict.to_list()
    |> list.map(fn(pair) {
      let #(key, value) = pair
      stringify_string(key) <> ":" <> stringify_value(value)
    })
    |> string.join(",")
  "{" <> pairs <> "}"
}

/// Get a value from a JSON object by key.
pub fn get(obj: JsonValue, key: String) -> Option(JsonValue) {
  case obj {
    JsonObject(d) -> dict.get(d, key) |> option.from_result()
    _ -> None
  }
}

/// Get a nested value using a path of keys.
pub fn get_path(value: JsonValue, path: List(String)) -> Option(JsonValue) {
  case path {
    [] -> Some(value)
    [key, ..rest] ->
      case get(value, key) {
        Some(nested) -> get_path(nested, rest)
        None -> None
      }
  }
}

/// Get an array element by index.
pub fn get_index(value: JsonValue, index: Int) -> Option(JsonValue) {
  case value {
    JsonArray(items) ->
      case index >= 0 && index < list.length(items) {
        True ->
          items
          |> list.drop(index)
          |> list.first()
          |> option.from_result()
        False -> None
      }
    _ -> None
  }
}

/// Convert JsonValue to String if it's a string.
pub fn as_string(value: JsonValue) -> Option(String) {
  case value {
    JsonString(s) -> Some(s)
    _ -> None
  }
}

/// Convert JsonValue to Int if it's a number.
pub fn as_int(value: JsonValue) -> Option(Int) {
  case value {
    JsonNumber(n) -> Some(float.truncate(n))
    _ -> None
  }
}

/// Convert JsonValue to Float if it's a number.
pub fn as_float(value: JsonValue) -> Option(Float) {
  case value {
    JsonNumber(n) -> Some(n)
    _ -> None
  }
}

/// Convert JsonValue to Bool if it's a boolean.
pub fn as_bool(value: JsonValue) -> Option(Bool) {
  case value {
    JsonBool(b) -> Some(b)
    _ -> None
  }
}

/// Convert JsonValue to List if it's an array.
pub fn as_array(value: JsonValue) -> Option(List(JsonValue)) {
  case value {
    JsonArray(items) -> Some(items)
    _ -> None
  }
}

/// Convert JsonValue to Dict if it's an object.
pub fn as_object(value: JsonValue) -> Option(Dict(String, JsonValue)) {
  case value {
    JsonObject(obj) -> Some(obj)
    _ -> None
  }
}

/// Check if a value is null.
pub fn is_null(value: JsonValue) -> Bool {
  case value {
    JsonNull -> True
    _ -> False
  }
}

/// Create a JSON object from key-value pairs.
pub fn object(pairs: List(#(String, JsonValue))) -> JsonValue {
  JsonObject(dict.from_list(pairs))
}

/// Create a JSON array from values.
pub fn array(items: List(JsonValue)) -> JsonValue {
  JsonArray(items)
}

/// Create a JSON string.
pub fn string(value: String) -> JsonValue {
  JsonString(value)
}

/// Create a JSON number from an int.
pub fn number_int(value: Int) -> JsonValue {
  JsonNumber(int.to_float(value))
}

/// Create a JSON number from a float.
pub fn number_float(value: Float) -> JsonValue {
  JsonNumber(value)
}

/// Create a JSON boolean.
pub fn bool(value: Bool) -> JsonValue {
  JsonBool(value)
}

/// Create a JSON null.
pub fn null() -> JsonValue {
  JsonNull
}
