// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeUuid - UUID parsing and validation that cannot crash.
////
//// Provides safe UUID operations following RFC 4122.

import gleam/bit_array
import gleam/int
import gleam/list
import gleam/result
import gleam/string

/// UUID version types per RFC 4122.
pub type UuidVersion {
  V1
  V2
  V3
  V4
  V5
  NilVersion
  UnknownVersion
}

/// UUID variant types per RFC 4122.
pub type UuidVariant {
  Ncs
  Rfc4122
  Microsoft
  Future
}

/// Error types for UUID operations.
pub type UuidError {
  InvalidLength(actual: Int)
  InvalidFormat(message: String)
  InvalidHexCharacter(position: Int)
}

/// A validated UUID (128 bits).
/// Opaque type to ensure UUIDs are always valid.
pub opaque type Uuid {
  Uuid(bytes: BitArray)
}

/// The nil UUID (all zeros).
pub const nil_uuid_string = "00000000-0000-0000-0000-000000000000"

/// DNS namespace UUID.
pub const namespace_dns_string = "6ba7b810-9dad-11d1-80b4-00c04fd430c8"

/// URL namespace UUID.
pub const namespace_url_string = "6ba7b811-9dad-11d1-80b4-00c04fd430c8"

/// Create a nil UUID.
pub fn nil() -> Uuid {
  Uuid(bytes: <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>)
}

/// Create UUID from bytes (must be exactly 16 bytes).
pub fn from_bytes(bytes: BitArray) -> Result(Uuid, UuidError) {
  case bit_array.byte_size(bytes) {
    16 -> Ok(Uuid(bytes: bytes))
    actual -> Error(InvalidLength(actual: actual))
  }
}

/// Get the UUID bytes.
pub fn to_bytes(uuid: Uuid) -> BitArray {
  uuid.bytes
}

/// Get the UUID version.
pub fn version(uuid: Uuid) -> UuidVersion {
  case uuid.bytes {
    <<_:48, version_byte:8, _:56>> -> {
      let version_nibble = int.bitwise_shift_right(version_byte, 4)
      case int.bitwise_and(version_nibble, 0x0F) {
        1 -> V1
        2 -> V2
        3 -> V3
        4 -> V4
        5 -> V5
        0 -> NilVersion
        _ -> UnknownVersion
      }
    }
    _ -> UnknownVersion
  }
}

/// Get the UUID variant.
pub fn variant(uuid: Uuid) -> UuidVariant {
  case uuid.bytes {
    <<_:64, variant_byte:8, _:48>> -> {
      let high_bit = int.bitwise_shift_right(variant_byte, 7)
      let high_two = int.bitwise_shift_right(variant_byte, 6)
      let high_three = int.bitwise_shift_right(variant_byte, 5)
      case high_bit, high_two, high_three {
        0, _, _ -> Ncs
        _, 0b10, _ -> Rfc4122
        _, _, 0b110 -> Microsoft
        _, _, _ -> Future
      }
    }
    _ -> Future
  }
}

/// Check if this is the nil UUID.
pub fn is_nil(uuid: Uuid) -> Bool {
  uuid.bytes == <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>
}

/// Parse UUID from canonical string format (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).
pub fn parse(input: String) -> Result(Uuid, UuidError) {
  let trimmed = string.trim(input)
  case string.length(trimmed) {
    36 -> parse_canonical(trimmed)
    actual -> Error(InvalidLength(actual: actual))
  }
}

fn parse_canonical(input: String) -> Result(Uuid, UuidError) {
  let chars = string.to_graphemes(input)
  case chars {
    [
      c0, c1, c2, c3, c4, c5, c6, c7, "-", c8, c9, c10, c11, "-", c12, c13, c14,
      c15, "-", c16, c17, c18, c19, "-", c20, c21, c22, c23, c24, c25, c26, c27,
      c28, c29, c30, c31,
    ] -> {
      let hex_chars = [
        c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15,
        c16, c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29,
        c30, c31,
      ]
      parse_hex_chars_to_uuid(hex_chars, 0, [])
    }
    _ -> Error(InvalidFormat(message: "Invalid UUID format"))
  }
}

fn parse_hex_chars_to_uuid(
  chars: List(String),
  position: Int,
  accumulated_bytes: List(Int),
) -> Result(Uuid, UuidError) {
  case chars {
    [] -> {
      let bytes_list = list.reverse(accumulated_bytes)
      let bytes = bytes_list_to_bit_array(bytes_list)
      Ok(Uuid(bytes: bytes))
    }
    [high, low, ..rest] -> {
      case hex_char_to_nibble(high), hex_char_to_nibble(low) {
        Ok(high_val), Ok(low_val) -> {
          let byte = int.bitwise_or(int.bitwise_shift_left(high_val, 4), low_val)
          parse_hex_chars_to_uuid(rest, position + 2, [byte, ..accumulated_bytes])
        }
        Error(_), _ -> Error(InvalidHexCharacter(position: position))
        _, Error(_) -> Error(InvalidHexCharacter(position: position + 1))
      }
    }
    [_] -> Error(InvalidFormat(message: "Unexpected end of hex string"))
  }
}

fn bytes_list_to_bit_array(bytes: List(Int)) -> BitArray {
  case bytes {
    [] -> <<>>
    [b, ..rest] -> {
      let rest_bits = bytes_list_to_bit_array(rest)
      <<b:8, rest_bits:bits>>
    }
  }
}

fn hex_char_to_nibble(char: String) -> Result(Int, Nil) {
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

fn nibble_to_hex_char(nibble: Int) -> String {
  case nibble {
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

fn byte_to_hex(byte: Int) -> String {
  let high = int.bitwise_and(int.bitwise_shift_right(byte, 4), 0x0F)
  let low = int.bitwise_and(byte, 0x0F)
  nibble_to_hex_char(high) <> nibble_to_hex_char(low)
}

/// Format UUID as canonical string (lowercase).
pub fn to_string(uuid: Uuid) -> String {
  case uuid.bytes {
    <<
      b0:8,
      b1:8,
      b2:8,
      b3:8,
      b4:8,
      b5:8,
      b6:8,
      b7:8,
      b8:8,
      b9:8,
      b10:8,
      b11:8,
      b12:8,
      b13:8,
      b14:8,
      b15:8,
    >> ->
      byte_to_hex(b0)
      <> byte_to_hex(b1)
      <> byte_to_hex(b2)
      <> byte_to_hex(b3)
      <> "-"
      <> byte_to_hex(b4)
      <> byte_to_hex(b5)
      <> "-"
      <> byte_to_hex(b6)
      <> byte_to_hex(b7)
      <> "-"
      <> byte_to_hex(b8)
      <> byte_to_hex(b9)
      <> "-"
      <> byte_to_hex(b10)
      <> byte_to_hex(b11)
      <> byte_to_hex(b12)
      <> byte_to_hex(b13)
      <> byte_to_hex(b14)
      <> byte_to_hex(b15)
    _ -> nil_uuid_string
  }
}

/// Format UUID as URN (urn:uuid:...).
pub fn to_urn(uuid: Uuid) -> String {
  "urn:uuid:" <> to_string(uuid)
}

/// Check if a string is a valid UUID format.
pub fn is_valid(input: String) -> Bool {
  case parse(input) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Create a v4 (random) UUID from provided random bytes.
/// The caller must provide 16 random bytes.
pub fn v4_from_bytes(random_bytes: BitArray) -> Result(Uuid, UuidError) {
  case bit_array.byte_size(random_bytes) {
    16 -> {
      case random_bytes {
        <<
          b0:8,
          b1:8,
          b2:8,
          b3:8,
          b4:8,
          b5:8,
          b6:8,
          b7:8,
          b8:8,
          b9:8,
          b10:8,
          b11:8,
          b12:8,
          b13:8,
          b14:8,
          b15:8,
        >> -> {
          // Set version to 4: (b6 & 0x0F) | 0x40
          let b6_versioned =
            int.bitwise_or(int.bitwise_and(b6, 0x0F), 0x40)
          // Set variant to RFC 4122: (b8 & 0x3F) | 0x80
          let b8_varianted =
            int.bitwise_or(int.bitwise_and(b8, 0x3F), 0x80)

          let uuid_bytes = <<
            b0:8,
            b1:8,
            b2:8,
            b3:8,
            b4:8,
            b5:8,
            b6_versioned:8,
            b7:8,
            b8_varianted:8,
            b9:8,
            b10:8,
            b11:8,
            b12:8,
            b13:8,
            b14:8,
            b15:8,
          >>
          Ok(Uuid(bytes: uuid_bytes))
        }
        _ -> Error(InvalidLength(actual: 16))
      }
    }
    actual -> Error(InvalidLength(actual: actual))
  }
}

/// Compare two UUIDs for equality.
pub fn equals(uuid_a: Uuid, uuid_b: Uuid) -> Bool {
  uuid_a.bytes == uuid_b.bytes
}
