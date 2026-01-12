# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe hexadecimal encoding and decoding operations.
#'
#' Provides hex encoding/decoding, constant-time comparison,
#' and validation functions for hexadecimal data.

#' Hex character lookup table (lowercase).
#' @keywords internal
.HEX_CHARS_LOWER <- c("0", "1", "2", "3", "4", "5", "6", "7",
                       "8", "9", "a", "b", "c", "d", "e", "f")

#' Hex character lookup table (uppercase).
#' @keywords internal
.HEX_CHARS_UPPER <- c("0", "1", "2", "3", "4", "5", "6", "7",
                       "8", "9", "A", "B", "C", "D", "E", "F")

#' Encode raw bytes to hexadecimal string.
#'
#' @param bytes Raw vector to encode
#' @param uppercase Use uppercase hex digits (default FALSE)
#' @return Hexadecimal string or NA on invalid input
#' @export
hex_encode <- function(bytes, uppercase = FALSE) {
  if (is.null(bytes)) {
    return(NA_character_)
  }

  if (!is.raw(bytes)) {
    # Try to convert character to raw
    if (is.character(bytes) && length(bytes) == 1) {
      bytes <- charToRaw(bytes)
    } else {
      return(NA_character_)
    }
  }

  if (length(bytes) == 0) {
    return("")
  }

  hex_chars <- format(as.hexmode(as.integer(bytes)), width = 2)
  result <- paste0(hex_chars, collapse = "")

  if (uppercase) {
    toupper(result)
  } else {
    tolower(result)
  }
}

#' Decode hexadecimal string to raw bytes.
#'
#' @param hex_string Hexadecimal string to decode
#' @return Raw vector or NULL on invalid input
#' @export
hex_decode <- function(hex_string) {
  if (is.na(hex_string) || is.null(hex_string)) {
    return(NULL)
  }

  # Remove any whitespace
  hex_string <- gsub("\\s", "", hex_string)

  # Remove optional 0x prefix
  if (startsWith(tolower(hex_string), "0x")) {
    hex_string <- substr(hex_string, 3, nchar(hex_string))
  }

  # Check for valid length
  if (nchar(hex_string) == 0) {
    return(raw(0))
  }

  if (nchar(hex_string) %% 2 != 0) {
    return(NULL)
  }

  # Validate hex characters
  if (!grepl("^[0-9a-fA-F]+$", hex_string)) {
    return(NULL)
  }

  tryCatch({
    n <- nchar(hex_string)
    byte_values <- sapply(seq(1, n, by = 2), function(i) {
      strtoi(substr(hex_string, i, i + 1), base = 16L)
    })
    as.raw(byte_values)
  }, error = function(e) NULL)
}

#' Encode string to hexadecimal.
#'
#' @param input String to encode
#' @param uppercase Use uppercase hex digits (default FALSE)
#' @return Hexadecimal string or NA on invalid input
#' @export
hex_encode_string <- function(input, uppercase = FALSE) {
  if (is.na(input) || is.null(input)) {
    return(NA_character_)
  }

  hex_encode(charToRaw(input), uppercase)
}

#' Decode hexadecimal string to character string.
#'
#' @param hex_string Hexadecimal string to decode
#' @return Decoded string or NA on invalid input
#' @export
hex_decode_string <- function(hex_string) {
  bytes <- hex_decode(hex_string)

  if (is.null(bytes)) {
    return(NA_character_)
  }

  tryCatch({
    rawToChar(bytes)
  }, error = function(e) NA_character_)
}

#' Constant-time comparison of two hex strings.
#'
#' Prevents timing attacks by always comparing all characters
#' regardless of where differences occur.
#'
#' @param hex1 First hex string
#' @param hex2 Second hex string
#' @return TRUE if equal, FALSE otherwise
#' @export
constant_time_equal <- function(hex1, hex2) {
  if (is.na(hex1) || is.na(hex2) || is.null(hex1) || is.null(hex2)) {
    return(FALSE)
  }

  # Normalize to lowercase for comparison
  hex1 <- tolower(hex1)
  hex2 <- tolower(hex2)

  # Different lengths means not equal
  if (nchar(hex1) != nchar(hex2)) {
    return(FALSE)
  }

  # Compare all characters to prevent timing attacks
  result <- 0L
  bytes1 <- charToRaw(hex1)
  bytes2 <- charToRaw(hex2)

  for (i in seq_along(bytes1)) {
    result <- bitwOr(result, bitwXor(as.integer(bytes1[i]), as.integer(bytes2[i])))
  }

  result == 0
}

#' Constant-time comparison of two raw byte vectors.
#'
#' @param bytes1 First raw vector
#' @param bytes2 Second raw vector
#' @return TRUE if equal, FALSE otherwise
#' @export
constant_time_equal_bytes <- function(bytes1, bytes2) {
  if (is.null(bytes1) || is.null(bytes2)) {
    return(FALSE)
  }

  if (!is.raw(bytes1) || !is.raw(bytes2)) {
    return(FALSE)
  }

  if (length(bytes1) != length(bytes2)) {
    return(FALSE)
  }

  result <- 0L
  for (i in seq_along(bytes1)) {
    result <- bitwOr(result, bitwXor(as.integer(bytes1[i]), as.integer(bytes2[i])))
  }

  result == 0
}

#' Validate a hexadecimal string.
#'
#' @param hex_string String to validate
#' @param expected_length Expected length in bytes (NULL for any length)
#' @return TRUE if valid hex string
#' @export
is_valid_hex <- function(hex_string, expected_length = NULL) {
  if (is.na(hex_string) || is.null(hex_string)) {
    return(FALSE)
  }

  # Remove optional 0x prefix for validation
  clean_hex <- hex_string
  if (startsWith(tolower(clean_hex), "0x")) {
    clean_hex <- substr(clean_hex, 3, nchar(clean_hex))
  }

  # Must have even length
  if (nchar(clean_hex) %% 2 != 0) {
    return(FALSE)
  }

  # Must only contain hex characters
  if (!grepl("^[0-9a-fA-F]*$", clean_hex)) {
    return(FALSE)
  }

  # Check expected length if specified
  if (!is.null(expected_length)) {
    if (nchar(clean_hex) / 2 != expected_length) {
      return(FALSE)
    }
  }

  TRUE
}

#' Convert integer to hexadecimal string.
#'
#' @param value Integer value to convert
#' @param width Minimum width in hex digits (default 0, no padding)
#' @param uppercase Use uppercase hex digits (default FALSE)
#' @return Hexadecimal string or NA on invalid input
#' @export
int_to_hex <- function(value, width = 0L, uppercase = FALSE) {
  if (is.na(value) || is.null(value)) {
    return(NA_character_)
  }

  if (value < 0) {
    return(NA_character_)
  }

  result <- format(as.hexmode(as.integer(value)), width = width)

  if (uppercase) {
    toupper(result)
  } else {
    tolower(result)
  }
}

#' Convert hexadecimal string to integer.
#'
#' @param hex_string Hexadecimal string
#' @return Integer value or NA on invalid input
#' @export
hex_to_int <- function(hex_string) {
  if (is.na(hex_string) || is.null(hex_string)) {
    return(NA_integer_)
  }

  # Remove optional 0x prefix
  if (startsWith(tolower(hex_string), "0x")) {
    hex_string <- substr(hex_string, 3, nchar(hex_string))
  }

  # Validate hex
  if (!grepl("^[0-9a-fA-F]+$", hex_string)) {
    return(NA_integer_)
  }

  # Check for overflow (32-bit)
  if (nchar(hex_string) > 8) {
    return(NA_integer_)
  }

  strtoi(hex_string, base = 16L)
}

#' XOR two hex strings.
#'
#' @param hex1 First hex string
#' @param hex2 Second hex string
#' @return XOR result as hex string or NA on error
#' @export
hex_xor <- function(hex1, hex2) {
  bytes1 <- hex_decode(hex1)
  bytes2 <- hex_decode(hex2)

  if (is.null(bytes1) || is.null(bytes2)) {
    return(NA_character_)
  }

  if (length(bytes1) != length(bytes2)) {
    return(NA_character_)
  }

  result <- as.raw(bitwXor(as.integer(bytes1), as.integer(bytes2)))
  hex_encode(result)
}

#' AND two hex strings.
#'
#' @param hex1 First hex string
#' @param hex2 Second hex string
#' @return AND result as hex string or NA on error
#' @export
hex_and <- function(hex1, hex2) {
  bytes1 <- hex_decode(hex1)
  bytes2 <- hex_decode(hex2)

  if (is.null(bytes1) || is.null(bytes2)) {
    return(NA_character_)
  }

  if (length(bytes1) != length(bytes2)) {
    return(NA_character_)
  }

  result <- as.raw(bitwAnd(as.integer(bytes1), as.integer(bytes2)))
  hex_encode(result)
}

#' OR two hex strings.
#'
#' @param hex1 First hex string
#' @param hex2 Second hex string
#' @return OR result as hex string or NA on error
#' @export
hex_or <- function(hex1, hex2) {
  bytes1 <- hex_decode(hex1)
  bytes2 <- hex_decode(hex2)

  if (is.null(bytes1) || is.null(bytes2)) {
    return(NA_character_)
  }

  if (length(bytes1) != length(bytes2)) {
    return(NA_character_)
  }

  result <- as.raw(bitwOr(as.integer(bytes1), as.integer(bytes2)))
  hex_encode(result)
}

#' NOT a hex string (bitwise inversion).
#'
#' @param hex_string Hex string to invert
#' @return Inverted hex string or NA on error
#' @export
hex_not <- function(hex_string) {
  bytes <- hex_decode(hex_string)

  if (is.null(bytes)) {
    return(NA_character_)
  }

  result <- as.raw(bitwAnd(bitwNot(as.integer(bytes)), 0xFF))
  hex_encode(result)
}

#' Left-pad hex string to a specific byte length.
#'
#' @param hex_string Hex string to pad
#' @param byte_length Target length in bytes
#' @return Padded hex string or NA on error
#' @export
hex_pad_left <- function(hex_string, byte_length) {
  if (is.na(hex_string) || is.null(hex_string)) {
    return(NA_character_)
  }

  # Remove 0x prefix if present
  clean_hex <- hex_string
  if (startsWith(tolower(clean_hex), "0x")) {
    clean_hex <- substr(clean_hex, 3, nchar(clean_hex))
  }

  current_bytes <- nchar(clean_hex) / 2
  if (current_bytes >= byte_length) {
    return(tolower(clean_hex))
  }

  padding <- paste0(rep("0", (byte_length - current_bytes) * 2), collapse = "")
  tolower(paste0(padding, clean_hex))
}

#' Right-pad hex string to a specific byte length.
#'
#' @param hex_string Hex string to pad
#' @param byte_length Target length in bytes
#' @return Padded hex string or NA on error
#' @export
hex_pad_right <- function(hex_string, byte_length) {
  if (is.na(hex_string) || is.null(hex_string)) {
    return(NA_character_)
  }

  # Remove 0x prefix if present
  clean_hex <- hex_string
  if (startsWith(tolower(clean_hex), "0x")) {
    clean_hex <- substr(clean_hex, 3, nchar(clean_hex))
  }

  current_bytes <- nchar(clean_hex) / 2
  if (current_bytes >= byte_length) {
    return(tolower(clean_hex))
  }

  padding <- paste0(rep("0", (byte_length - current_bytes) * 2), collapse = "")
  tolower(paste0(clean_hex, padding))
}

#' Get the length of a hex string in bytes.
#'
#' @param hex_string Hex string
#' @return Number of bytes or NA on invalid input
#' @export
hex_byte_length <- function(hex_string) {
  if (is.na(hex_string) || is.null(hex_string)) {
    return(NA_integer_)
  }

  # Remove 0x prefix if present
  clean_hex <- hex_string
  if (startsWith(tolower(clean_hex), "0x")) {
    clean_hex <- substr(clean_hex, 3, nchar(clean_hex))
  }

  if (nchar(clean_hex) %% 2 != 0) {
    return(NA_integer_)
  }

  as.integer(nchar(clean_hex) / 2)
}

#' Slice a hex string by byte positions.
#'
#' @param hex_string Hex string to slice
#' @param start Start byte position (1-indexed)
#' @param end End byte position (inclusive)
#' @return Sliced hex string or NA on error
#' @export
hex_slice <- function(hex_string, start, end) {
  if (is.na(hex_string) || is.null(hex_string)) {
    return(NA_character_)
  }

  # Remove 0x prefix if present
  clean_hex <- hex_string
  if (startsWith(tolower(clean_hex), "0x")) {
    clean_hex <- substr(clean_hex, 3, nchar(clean_hex))
  }

  byte_length <- nchar(clean_hex) / 2

  if (start < 1 || end < start || end > byte_length) {
    return(NA_character_)
  }

  # Convert byte positions to character positions
  char_start <- (start - 1) * 2 + 1
  char_end <- end * 2

  tolower(substr(clean_hex, char_start, char_end))
}

#' Concatenate multiple hex strings.
#'
#' @param ... Hex strings to concatenate
#' @return Concatenated hex string or NA on error
#' @export
hex_concat <- function(...) {
  args <- list(...)

  if (length(args) == 0) {
    return("")
  }

  results <- character(length(args))
  for (i in seq_along(args)) {
    hex_str <- args[[i]]

    if (is.na(hex_str) || is.null(hex_str)) {
      return(NA_character_)
    }

    # Remove 0x prefix if present
    if (startsWith(tolower(hex_str), "0x")) {
      hex_str <- substr(hex_str, 3, nchar(hex_str))
    }

    if (!is_valid_hex(hex_str)) {
      return(NA_character_)
    }

    results[i] <- tolower(hex_str)
  }

  paste0(results, collapse = "")
}

#' Reverse byte order of a hex string.
#'
#' @param hex_string Hex string to reverse
#' @return Reversed hex string or NA on error
#' @export
hex_reverse_bytes <- function(hex_string) {
  bytes <- hex_decode(hex_string)

  if (is.null(bytes)) {
    return(NA_character_)
  }

  hex_encode(rev(bytes))
}
