// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeCrypto - Cryptographic operations that cannot crash.
////
//// Provides safe cryptographic primitives with constant-time operations.
//// Note: This module provides interfaces for crypto operations.
//// Actual cryptographic algorithms should use platform-native implementations.

import gleam/bit_array
import gleam/int
import gleam/list
import gleam/string

/// Hash algorithm types.
pub type HashAlgorithm {
  Sha256
  Sha384
  Sha512
  Sha3_256
  Sha3_512
  Blake2b
  Blake2s
}

/// Error types for crypto operations.
pub type CryptoError {
  InvalidKeyLength(expected: Int, actual: Int)
  InvalidIvLength(expected: Int, actual: Int)
  InvalidInputLength(message: String)
  EncryptionFailed(message: String)
  DecryptionFailed(message: String)
  InvalidSignature
  UnsupportedAlgorithm(algorithm: String)
}

/// Get the output size in bytes for a hash algorithm.
pub fn hash_output_size(algorithm: HashAlgorithm) -> Int {
  case algorithm {
    Sha256 -> 32
    Sha384 -> 48
    Sha512 -> 64
    Sha3_256 -> 32
    Sha3_512 -> 64
    Blake2b -> 64
    Blake2s -> 32
  }
}

/// Get the block size in bytes for a hash algorithm.
pub fn hash_block_size(algorithm: HashAlgorithm) -> Int {
  case algorithm {
    Sha256 -> 64
    Sha384 -> 128
    Sha512 -> 128
    Sha3_256 -> 136
    Sha3_512 -> 72
    Blake2b -> 128
    Blake2s -> 64
  }
}

/// Constant-time comparison of two byte arrays.
/// Returns True if equal, False otherwise.
/// Compares all bytes regardless of differences to prevent timing attacks.
pub fn constant_time_equal(array_a: BitArray, array_b: BitArray) -> Bool {
  let size_a = bit_array.byte_size(array_a)
  let size_b = bit_array.byte_size(array_b)
  case size_a == size_b {
    False -> False
    True -> {
      let list_a = bit_array_to_list(array_a, [])
      let list_b = bit_array_to_list(array_b, [])
      constant_time_compare_lists(list_a, list_b, 0) == 0
    }
  }
}

fn bit_array_to_list(bytes: BitArray, accumulated: List(Int)) -> List(Int) {
  case bytes {
    <<>> -> list.reverse(accumulated)
    <<byte:8, rest:bits>> -> bit_array_to_list(rest, [byte, ..accumulated])
    _ -> list.reverse(accumulated)
  }
}

fn constant_time_compare_lists(
  list_a: List(Int),
  list_b: List(Int),
  diff_accumulator: Int,
) -> Int {
  case list_a, list_b {
    [], [] -> diff_accumulator
    [a, ..rest_a], [b, ..rest_b] -> {
      let byte_diff = int.bitwise_exclusive_or(a, b)
      let new_diff = int.bitwise_or(diff_accumulator, byte_diff)
      constant_time_compare_lists(rest_a, rest_b, new_diff)
    }
    _, _ -> 1
  }
}

/// Securely zero out a byte array (best effort in pure Gleam).
/// Note: In production, use platform-native secure zeroing.
pub fn secure_zero(size: Int) -> BitArray {
  create_zero_bytes(size, <<>>)
}

fn create_zero_bytes(remaining: Int, accumulated: BitArray) -> BitArray {
  case remaining <= 0 {
    True -> accumulated
    False -> create_zero_bytes(remaining - 1, <<0:8, accumulated:bits>>)
  }
}

/// XOR two byte arrays of equal length.
pub fn xor_bytes(array_a: BitArray, array_b: BitArray) -> Result(BitArray, CryptoError) {
  let size_a = bit_array.byte_size(array_a)
  let size_b = bit_array.byte_size(array_b)
  case size_a == size_b {
    False -> Error(InvalidInputLength(message: "Arrays must be the same length"))
    True -> {
      let list_a = bit_array_to_list(array_a, [])
      let list_b = bit_array_to_list(array_b, [])
      let xored = xor_lists(list_a, list_b, [])
      Ok(list_to_bit_array(xored))
    }
  }
}

fn xor_lists(
  list_a: List(Int),
  list_b: List(Int),
  accumulated: List(Int),
) -> List(Int) {
  case list_a, list_b {
    [], [] -> list.reverse(accumulated)
    [a, ..rest_a], [b, ..rest_b] -> {
      let xored = int.bitwise_exclusive_or(a, b)
      xor_lists(rest_a, rest_b, [xored, ..accumulated])
    }
    _, _ -> list.reverse(accumulated)
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

/// Validate key length for AES.
pub fn validate_aes_key_length(key: BitArray) -> Result(Nil, CryptoError) {
  let size = bit_array.byte_size(key)
  case size {
    16 | 24 | 32 -> Ok(Nil)
    _ -> Error(InvalidKeyLength(expected: 16, actual: size))
  }
}

/// Validate IV length for AES-GCM.
pub fn validate_aes_gcm_iv_length(iv: BitArray) -> Result(Nil, CryptoError) {
  let size = bit_array.byte_size(iv)
  case size {
    12 -> Ok(Nil)
    _ -> Error(InvalidIvLength(expected: 12, actual: size))
  }
}

/// Validate IV length for AES-CBC.
pub fn validate_aes_cbc_iv_length(iv: BitArray) -> Result(Nil, CryptoError) {
  let size = bit_array.byte_size(iv)
  case size {
    16 -> Ok(Nil)
    _ -> Error(InvalidIvLength(expected: 16, actual: size))
  }
}

/// PKCS7 padding for block ciphers.
pub fn pkcs7_pad(data: BitArray, block_size: Int) -> BitArray {
  let data_size = bit_array.byte_size(data)
  let padding_length = block_size - data_size % block_size
  let padding = create_padding(padding_length, padding_length, <<>>)
  <<data:bits, padding:bits>>
}

fn create_padding(
  byte_value: Int,
  remaining: Int,
  accumulated: BitArray,
) -> BitArray {
  case remaining <= 0 {
    True -> accumulated
    False -> create_padding(byte_value, remaining - 1, <<byte_value:8, accumulated:bits>>)
  }
}

/// PKCS7 unpadding for block ciphers.
pub fn pkcs7_unpad(data: BitArray) -> Result(BitArray, CryptoError) {
  let size = bit_array.byte_size(data)
  case size == 0 {
    True -> Error(InvalidInputLength(message: "Empty input"))
    False -> {
      let bytes = bit_array_to_list(data, [])
      case list.last(bytes) {
        Error(_) -> Error(InvalidInputLength(message: "Empty input"))
        Ok(padding_length) -> {
          case padding_length > 0 && padding_length <= 16 && padding_length <= size {
            False -> Error(InvalidInputLength(message: "Invalid padding"))
            True -> {
              let unpadded_length = size - padding_length
              let unpadded_bytes = list.take(bytes, unpadded_length)
              Ok(list_to_bit_array(unpadded_bytes))
            }
          }
        }
      }
    }
  }
}

/// Generate random bytes placeholder.
/// In production, use platform-native CSPRNG.
/// This returns a predictable pattern for testing only.
pub fn random_bytes_placeholder(size: Int) -> BitArray {
  generate_placeholder_bytes(size, 0, <<>>)
}

fn generate_placeholder_bytes(
  remaining: Int,
  counter: Int,
  accumulated: BitArray,
) -> BitArray {
  case remaining <= 0 {
    True -> accumulated
    False -> {
      let byte = int.bitwise_and(counter, 0xFF)
      generate_placeholder_bytes(remaining - 1, counter + 1, <<accumulated:bits, byte:8>>)
    }
  }
}

/// Encode bytes to base64.
pub fn base64_encode(data: BitArray) -> String {
  base64_encode_bytes(bit_array_to_list(data, []), "")
}

fn base64_encode_bytes(bytes: List(Int), accumulated: String) -> String {
  case bytes {
    [] -> accumulated
    [a] -> {
      let b0 = int.bitwise_shift_right(a, 2)
      let b1 = int.bitwise_and(int.bitwise_shift_left(a, 4), 0x3F)
      accumulated <> base64_char(b0) <> base64_char(b1) <> "=="
    }
    [a, b] -> {
      let b0 = int.bitwise_shift_right(a, 2)
      let b1 = int.bitwise_or(
        int.bitwise_and(int.bitwise_shift_left(a, 4), 0x30),
        int.bitwise_shift_right(b, 4),
      )
      let b2 = int.bitwise_and(int.bitwise_shift_left(b, 2), 0x3F)
      accumulated <> base64_char(b0) <> base64_char(b1) <> base64_char(b2) <> "="
    }
    [a, b, c, ..rest] -> {
      let b0 = int.bitwise_shift_right(a, 2)
      let b1 = int.bitwise_or(
        int.bitwise_and(int.bitwise_shift_left(a, 4), 0x30),
        int.bitwise_shift_right(b, 4),
      )
      let b2 = int.bitwise_or(
        int.bitwise_and(int.bitwise_shift_left(b, 2), 0x3C),
        int.bitwise_shift_right(c, 6),
      )
      let b3 = int.bitwise_and(c, 0x3F)
      base64_encode_bytes(
        rest,
        accumulated <> base64_char(b0) <> base64_char(b1) <> base64_char(b2) <> base64_char(b3),
      )
    }
  }
}

fn base64_char(index: Int) -> String {
  case index {
    0 -> "A"
    1 -> "B"
    2 -> "C"
    3 -> "D"
    4 -> "E"
    5 -> "F"
    6 -> "G"
    7 -> "H"
    8 -> "I"
    9 -> "J"
    10 -> "K"
    11 -> "L"
    12 -> "M"
    13 -> "N"
    14 -> "O"
    15 -> "P"
    16 -> "Q"
    17 -> "R"
    18 -> "S"
    19 -> "T"
    20 -> "U"
    21 -> "V"
    22 -> "W"
    23 -> "X"
    24 -> "Y"
    25 -> "Z"
    26 -> "a"
    27 -> "b"
    28 -> "c"
    29 -> "d"
    30 -> "e"
    31 -> "f"
    32 -> "g"
    33 -> "h"
    34 -> "i"
    35 -> "j"
    36 -> "k"
    37 -> "l"
    38 -> "m"
    39 -> "n"
    40 -> "o"
    41 -> "p"
    42 -> "q"
    43 -> "r"
    44 -> "s"
    45 -> "t"
    46 -> "u"
    47 -> "v"
    48 -> "w"
    49 -> "x"
    50 -> "y"
    51 -> "z"
    52 -> "0"
    53 -> "1"
    54 -> "2"
    55 -> "3"
    56 -> "4"
    57 -> "5"
    58 -> "6"
    59 -> "7"
    60 -> "8"
    61 -> "9"
    62 -> "+"
    _ -> "/"
  }
}

/// Decode base64 to bytes.
pub fn base64_decode(input: String) -> Result(BitArray, CryptoError) {
  let chars = string.to_graphemes(input)
  let filtered = list.filter(chars, fn(c) { c != "\n" && c != "\r" && c != " " })
  case list.length(filtered) % 4 {
    0 -> base64_decode_chars(filtered, [])
    _ -> Error(InvalidInputLength(message: "Base64 input length must be multiple of 4"))
  }
}

fn base64_decode_chars(
  chars: List(String),
  accumulated: List(Int),
) -> Result(BitArray, CryptoError) {
  case chars {
    [] -> Ok(list_to_bit_array(list.reverse(accumulated)))
    [a, b, "=", "="] -> {
      case base64_value(a), base64_value(b) {
        Ok(va), Ok(vb) -> {
          let byte0 = int.bitwise_or(
            int.bitwise_shift_left(va, 2),
            int.bitwise_shift_right(vb, 4),
          )
          Ok(list_to_bit_array(list.reverse([byte0, ..accumulated])))
        }
        _, _ -> Error(InvalidInputLength(message: "Invalid base64 character"))
      }
    }
    [a, b, c, "="] -> {
      case base64_value(a), base64_value(b), base64_value(c) {
        Ok(va), Ok(vb), Ok(vc) -> {
          let byte0 = int.bitwise_or(
            int.bitwise_shift_left(va, 2),
            int.bitwise_shift_right(vb, 4),
          )
          let byte1 = int.bitwise_or(
            int.bitwise_and(int.bitwise_shift_left(vb, 4), 0xF0),
            int.bitwise_shift_right(vc, 2),
          )
          Ok(list_to_bit_array(list.reverse([byte1, byte0, ..accumulated])))
        }
        _, _, _ -> Error(InvalidInputLength(message: "Invalid base64 character"))
      }
    }
    [a, b, c, d, ..rest] -> {
      case base64_value(a), base64_value(b), base64_value(c), base64_value(d) {
        Ok(va), Ok(vb), Ok(vc), Ok(vd) -> {
          let byte0 = int.bitwise_or(
            int.bitwise_shift_left(va, 2),
            int.bitwise_shift_right(vb, 4),
          )
          let byte1 = int.bitwise_or(
            int.bitwise_and(int.bitwise_shift_left(vb, 4), 0xF0),
            int.bitwise_shift_right(vc, 2),
          )
          let byte2 = int.bitwise_or(
            int.bitwise_and(int.bitwise_shift_left(vc, 6), 0xC0),
            vd,
          )
          base64_decode_chars(rest, [byte2, byte1, byte0, ..accumulated])
        }
        _, _, _, _ -> Error(InvalidInputLength(message: "Invalid base64 character"))
      }
    }
    _ -> Error(InvalidInputLength(message: "Invalid base64 input"))
  }
}

fn base64_value(char: String) -> Result(Int, Nil) {
  case char {
    "A" -> Ok(0)
    "B" -> Ok(1)
    "C" -> Ok(2)
    "D" -> Ok(3)
    "E" -> Ok(4)
    "F" -> Ok(5)
    "G" -> Ok(6)
    "H" -> Ok(7)
    "I" -> Ok(8)
    "J" -> Ok(9)
    "K" -> Ok(10)
    "L" -> Ok(11)
    "M" -> Ok(12)
    "N" -> Ok(13)
    "O" -> Ok(14)
    "P" -> Ok(15)
    "Q" -> Ok(16)
    "R" -> Ok(17)
    "S" -> Ok(18)
    "T" -> Ok(19)
    "U" -> Ok(20)
    "V" -> Ok(21)
    "W" -> Ok(22)
    "X" -> Ok(23)
    "Y" -> Ok(24)
    "Z" -> Ok(25)
    "a" -> Ok(26)
    "b" -> Ok(27)
    "c" -> Ok(28)
    "d" -> Ok(29)
    "e" -> Ok(30)
    "f" -> Ok(31)
    "g" -> Ok(32)
    "h" -> Ok(33)
    "i" -> Ok(34)
    "j" -> Ok(35)
    "k" -> Ok(36)
    "l" -> Ok(37)
    "m" -> Ok(38)
    "n" -> Ok(39)
    "o" -> Ok(40)
    "p" -> Ok(41)
    "q" -> Ok(42)
    "r" -> Ok(43)
    "s" -> Ok(44)
    "t" -> Ok(45)
    "u" -> Ok(46)
    "v" -> Ok(47)
    "w" -> Ok(48)
    "x" -> Ok(49)
    "y" -> Ok(50)
    "z" -> Ok(51)
    "0" -> Ok(52)
    "1" -> Ok(53)
    "2" -> Ok(54)
    "3" -> Ok(55)
    "4" -> Ok(56)
    "5" -> Ok(57)
    "6" -> Ok(58)
    "7" -> Ok(59)
    "8" -> Ok(60)
    "9" -> Ok(61)
    "+" -> Ok(62)
    "/" -> Ok(63)
    _ -> Error(Nil)
  }
}

/// Check if a string looks like valid base64.
pub fn is_valid_base64(input: String) -> Bool {
  case base64_decode(input) {
    Ok(_) -> True
    Error(_) -> False
  }
}
