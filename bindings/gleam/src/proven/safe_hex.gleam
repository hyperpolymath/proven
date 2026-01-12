// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeHex - Hexadecimal encoding and decoding that cannot crash.
////
//// Provides safe hex operations with constant-time comparison for security.

import gleam/bit_array
import gleam/int
import gleam/list
import gleam/string

/// Error types for hex operations.
pub type HexError {
  OddLength(actual_length: Int)
  InvalidHexCharacter(position: Int, character: String)
}

/// Check if a character is a valid hexadecimal character.
pub fn is_hex_char(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    "a" | "b" | "c" | "d" | "e" | "f" -> True
    "A" | "B" | "C" | "D" | "E" | "F" -> True
    _ -> False
  }
}

/// Convert a hex character to its nibble value (0-15).
pub fn hex_char_to_nibble(char: String) -> Result(Int, Nil) {
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

/// Convert a nibble value (0-15) to a lowercase hex character.
pub fn nibble_to_hex_char(nibble: Int) -> String {
  case int.bitwise_and(nibble, 0x0F) {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    _ -> "f"
  }
}

/// Convert a nibble value (0-15) to an uppercase hex character.
pub fn nibble_to_hex_char_upper(nibble: Int) -> String {
  case int.bitwise_and(nibble, 0x0F) {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "A"
    11 -> "B"
    12 -> "C"
    13 -> "D"
    14 -> "E"
    _ -> "F"
  }
}

/// Encode a BitArray to a lowercase hex string.
pub fn encode(bytes: BitArray) -> String {
  encode_with_case(bytes, False)
}

/// Encode a BitArray to an uppercase hex string.
pub fn encode_upper(bytes: BitArray) -> String {
  encode_with_case(bytes, True)
}

fn encode_with_case(bytes: BitArray, uppercase: Bool) -> String {
  bytes
  |> bit_array_to_list([])
  |> list.map(fn(byte) { byte_to_hex(byte, uppercase) })
  |> string.concat()
}

fn bit_array_to_list(bytes: BitArray, accumulated: List(Int)) -> List(Int) {
  case bytes {
    <<>> -> list.reverse(accumulated)
    <<byte:8, rest:bits>> -> bit_array_to_list(rest, [byte, ..accumulated])
    _ -> list.reverse(accumulated)
  }
}

fn byte_to_hex(byte: Int, uppercase: Bool) -> String {
  let high = int.bitwise_and(int.bitwise_shift_right(byte, 4), 0x0F)
  let low = int.bitwise_and(byte, 0x0F)
  case uppercase {
    True -> nibble_to_hex_char_upper(high) <> nibble_to_hex_char_upper(low)
    False -> nibble_to_hex_char(high) <> nibble_to_hex_char(low)
  }
}

/// Decode a hex string to a BitArray.
pub fn decode(hex: String) -> Result(BitArray, HexError) {
  let length = string.length(hex)
  case length % 2 {
    0 -> decode_chars(string.to_graphemes(hex), 0, [])
    _ -> Error(OddLength(actual_length: length))
  }
}

fn decode_chars(
  chars: List(String),
  position: Int,
  accumulated: List(Int),
) -> Result(BitArray, HexError) {
  case chars {
    [] -> {
      let bytes_list = list.reverse(accumulated)
      Ok(list_to_bit_array(bytes_list))
    }
    [high, low, ..rest] -> {
      case hex_char_to_nibble(high), hex_char_to_nibble(low) {
        Ok(high_val), Ok(low_val) -> {
          let byte = int.bitwise_or(int.bitwise_shift_left(high_val, 4), low_val)
          decode_chars(rest, position + 2, [byte, ..accumulated])
        }
        Error(_), _ -> Error(InvalidHexCharacter(position: position, character: high))
        _, Error(_) ->
          Error(InvalidHexCharacter(position: position + 1, character: low))
      }
    }
    [single] -> Error(InvalidHexCharacter(position: position, character: single))
  }
}

fn list_to_bit_array(bytes: List(Int)) -> BitArray {
  case bytes {
    [] -> <<>>
    [byte, ..rest] -> {
      let rest_bits = list_to_bit_array(rest)
      <<byte:8, rest_bits:bits>>
    }
  }
}

/// Check if a string contains only valid hex characters.
pub fn is_valid(input: String) -> Bool {
  input
  |> string.to_graphemes()
  |> list.all(is_hex_char)
}

/// Check if a string is a valid hex string that can be decoded to bytes.
/// (Must have even length and contain only valid hex characters.)
pub fn is_valid_bytes(input: String) -> Bool {
  string.length(input) % 2 == 0 && is_valid(input)
}

/// Format a hex string with spaces between bytes (e.g., "aa bb cc").
pub fn format_spaced(hex: String) -> Result(String, HexError) {
  let length = string.length(hex)
  case length % 2 {
    0 ->
      case length {
        0 -> Ok("")
        _ -> {
          let chars = string.to_graphemes(hex)
          Ok(format_with_separator(chars, " ", []))
        }
      }
    _ -> Error(OddLength(actual_length: length))
  }
}

/// Format a hex string with colons between bytes (e.g., "aa:bb:cc").
pub fn format_colons(hex: String) -> Result(String, HexError) {
  let length = string.length(hex)
  case length % 2 {
    0 ->
      case length {
        0 -> Ok("")
        _ -> {
          let chars = string.to_graphemes(hex)
          Ok(format_with_separator(chars, ":", []))
        }
      }
    _ -> Error(OddLength(actual_length: length))
  }
}

fn format_with_separator(
  chars: List(String),
  separator: String,
  accumulated: List(String),
) -> String {
  case chars {
    [] -> string.concat(list.reverse(accumulated))
    [high, low, ..rest] -> {
      let byte_str = high <> low
      case accumulated {
        [] -> format_with_separator(rest, separator, [byte_str])
        _ ->
          format_with_separator(rest, separator, [byte_str, separator, ..accumulated])
      }
    }
    [single] -> string.concat(list.reverse([single, ..accumulated]))
  }
}

/// Constant-time comparison of two hex strings.
/// Compares in constant time to prevent timing attacks.
/// Case-insensitive comparison.
pub fn constant_time_equal(hex_a: String, hex_b: String) -> Bool {
  let len_a = string.length(hex_a)
  let len_b = string.length(hex_b)
  case len_a == len_b {
    False -> False
    True -> {
      let chars_a = string.to_graphemes(string.lowercase(hex_a))
      let chars_b = string.to_graphemes(string.lowercase(hex_b))
      constant_time_compare(chars_a, chars_b, 0)
    }
  }
}

fn constant_time_compare(
  chars_a: List(String),
  chars_b: List(String),
  diff_accumulator: Int,
) -> Bool {
  case chars_a, chars_b {
    [], [] -> diff_accumulator == 0
    [a, ..rest_a], [b, ..rest_b] -> {
      // Use character code XOR to accumulate differences
      let char_diff = xor_first_chars(a, b)
      let new_diff = int.bitwise_or(diff_accumulator, char_diff)
      constant_time_compare(rest_a, rest_b, new_diff)
    }
    _, _ -> False
  }
}

fn xor_first_chars(char_a: String, char_b: String) -> Int {
  let code_a = char_to_code(char_a)
  let code_b = char_to_code(char_b)
  int.bitwise_exclusive_or(code_a, code_b)
}

fn char_to_code(char: String) -> Int {
  case char {
    "0" -> 48
    "1" -> 49
    "2" -> 50
    "3" -> 51
    "4" -> 52
    "5" -> 53
    "6" -> 54
    "7" -> 55
    "8" -> 56
    "9" -> 57
    "a" -> 97
    "b" -> 98
    "c" -> 99
    "d" -> 100
    "e" -> 101
    "f" -> 102
    _ -> 0
  }
}

/// Convert an integer to a hex string with minimum width (zero-padded).
pub fn int_to_hex(value: Int, min_width: Int) -> String {
  let hex = int_to_hex_recursive(value, "")
  let hex_len = string.length(hex)
  case hex_len >= min_width {
    True -> hex
    False -> pad_left(hex, min_width, "0")
  }
}

fn int_to_hex_recursive(value: Int, accumulated: String) -> String {
  case value {
    0 ->
      case accumulated {
        "" -> "0"
        _ -> accumulated
      }
    _ -> {
      let nibble = int.bitwise_and(value, 0x0F)
      let remaining = int.bitwise_shift_right(value, 4)
      int_to_hex_recursive(remaining, nibble_to_hex_char(nibble) <> accumulated)
    }
  }
}

fn pad_left(input: String, target_length: Int, pad_char: String) -> String {
  let current_length = string.length(input)
  case current_length >= target_length {
    True -> input
    False -> pad_left(pad_char <> input, target_length, pad_char)
  }
}

/// Parse a hex string to an integer.
pub fn hex_to_int(hex: String) -> Result(Int, HexError) {
  let chars = string.to_graphemes(hex)
  hex_to_int_recursive(chars, 0, 0)
}

fn hex_to_int_recursive(
  chars: List(String),
  position: Int,
  accumulated: Int,
) -> Result(Int, HexError) {
  case chars {
    [] -> Ok(accumulated)
    [char, ..rest] -> {
      case hex_char_to_nibble(char) {
        Ok(nibble) -> {
          let new_accumulated =
            int.bitwise_or(int.bitwise_shift_left(accumulated, 4), nibble)
          hex_to_int_recursive(rest, position + 1, new_accumulated)
        }
        Error(_) -> Error(InvalidHexCharacter(position: position, character: char))
      }
    }
  }
}

/// Encode a list of integers (bytes) to hex string.
pub fn encode_bytes(bytes: List(Int)) -> String {
  bytes
  |> list.map(fn(byte) { byte_to_hex(int.bitwise_and(byte, 0xFF), False) })
  |> string.concat()
}

/// Decode hex string to a list of integers (bytes).
pub fn decode_to_list(hex: String) -> Result(List(Int), HexError) {
  case decode(hex) {
    Ok(bits) -> Ok(bit_array_to_list(bits, []))
    Error(err) -> Error(err)
  }
}
