# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe cryptographic operations.

#' Constant-time byte comparison to prevent timing attacks.
#'
#' @param a First raw vector
#' @param b Second raw vector
#' @return TRUE if equal
#' @export
constant_time_equals <- function(a, b) {
  if (length(a) != length(b)) return(FALSE)

  result <- 0L
  for (i in seq_along(a)) {
    result <- bitwOr(result, bitwXor(as.integer(a[i]), as.integer(b[i])))
  }
  result == 0
}

#' Constant-time string comparison.
#'
#' @param a First string
#' @param b Second string
#' @return TRUE if equal
#' @export
constant_time_equals_string <- function(a, b) {
  if (is.na(a) || is.na(b)) return(FALSE)
  constant_time_equals(charToRaw(a), charToRaw(b))
}

#' Generate cryptographically secure random bytes.
#'
#' @param count Number of bytes
#' @return Raw vector
#' @export
random_bytes <- function(count) {
  # Use openssl for cryptographic randomness
  openssl::rand_bytes(count)
}

#' Convert bytes to hex string.
#'
#' @param bytes Raw vector
#' @return Hex string
#' @export
bytes_to_hex <- function(bytes) {
  paste0(format(as.hexmode(as.integer(bytes)), width = 2), collapse = "")
}

#' Generate random bytes as hex string.
#'
#' @param byte_count Number of bytes
#' @return Hex string
#' @export
random_hex <- function(byte_count) {
  bytes_to_hex(random_bytes(byte_count))
}

#' Generate random bytes as base64 string.
#'
#' @param byte_count Number of bytes
#' @return Base64 string
#' @export
random_base64 <- function(byte_count) {
  base64enc::base64encode(random_bytes(byte_count))
}

#' Generate URL-safe random string.
#'
#' @param byte_count Number of bytes
#' @return URL-safe string
#' @export
random_url_safe <- function(byte_count) {
  encoded <- base64enc::base64encode(random_bytes(byte_count))
  result <- gsub("+", "-", encoded, fixed = TRUE)
  result <- gsub("/", "_", result, fixed = TRUE)
  gsub("=+$", "", result)
}

#' Generate random integer in range [min, max].
#'
#' @param min_val Minimum value
#' @param max_val Maximum value
#' @return Random integer
#' @export
random_int <- function(min_val, max_val) {
  if (min_val > max_val) {
    tmp <- min_val
    min_val <- max_val
    max_val <- tmp
  }

  if (min_val == max_val) return(min_val)

  bytes <- random_bytes(4)
  value <- sum(as.integer(bytes) * c(1, 256, 65536, 16777216))
  range_size <- max_val - min_val + 1
  min_val + (value %% range_size)
}

#' Generate a secure token.
#'
#' @param length Number of bytes
#' @return Token string
#' @export
generate_token <- function(length = 32) {
  random_url_safe(length)
}

#' Generate token with default length.
#'
#' @return Token string
#' @export
generate_token_default <- function() {
  generate_token(32)
}

#' Hash a string with SHA-256.
#'
#' @param input String to hash
#' @return Hex hash
#' @export
sha256_hash <- function(input) {
  if (is.na(input)) return(NA)
  bytes_to_hex(openssl::sha256(charToRaw(input)))
}

#' Hash bytes with SHA-256.
#'
#' @param input Raw vector to hash
#' @return Hex hash
#' @export
sha256_hash_bytes <- function(input) {
  bytes_to_hex(openssl::sha256(input))
}

#' Hash a string with SHA-512.
#'
#' @param input String to hash
#' @return Hex hash
#' @export
sha512_hash <- function(input) {
  if (is.na(input)) return(NA)
  bytes_to_hex(openssl::sha512(charToRaw(input)))
}

#' Hash bytes with SHA-512.
#'
#' @param input Raw vector to hash
#' @return Hex hash
#' @export
sha512_hash_bytes <- function(input) {
  bytes_to_hex(openssl::sha512(input))
}

#' Compute HMAC-SHA256.
#'
#' @param key Key string
#' @param message Message string
#' @return Hex HMAC
#' @export
hmac_sha256 <- function(key, message) {
  if (is.na(key) || is.na(message)) return(NA)
  result <- openssl::sha256(charToRaw(message), key = charToRaw(key))
  bytes_to_hex(result)
}

#' Compute HMAC-SHA256 with bytes.
#'
#' @param key Key raw vector
#' @param message Message raw vector
#' @return Hex HMAC
#' @export
hmac_sha256_bytes <- function(key, message) {
  result <- openssl::sha256(message, key = key)
  bytes_to_hex(result)
}

#' Compute HMAC-SHA512.
#'
#' @param key Key string
#' @param message Message string
#' @return Hex HMAC
#' @export
hmac_sha512 <- function(key, message) {
  if (is.na(key) || is.na(message)) return(NA)
  result <- openssl::sha512(charToRaw(message), key = charToRaw(key))
  bytes_to_hex(result)
}

#' Verify HMAC using constant-time comparison.
#'
#' @param key Key string
#' @param message Message string
#' @param expected_mac Expected MAC
#' @return TRUE if valid
#' @export
verify_hmac_sha256 <- function(key, message, expected_mac) {
  constant_time_equals_string(hmac_sha256(key, message), expected_mac)
}

#' Verify HMAC-SHA512 using constant-time comparison.
#'
#' @param key Key string
#' @param message Message string
#' @param expected_mac Expected MAC
#' @return TRUE if valid
#' @export
verify_hmac_sha512 <- function(key, message, expected_mac) {
  constant_time_equals_string(hmac_sha512(key, message), expected_mac)
}

#' Hash a string with MD5 (NOT for security, only for checksums).
#'
#' @param input String to hash
#' @return Hex hash
#' @export
md5_hash <- function(input) {
  if (is.na(input)) return(NA)
  bytes_to_hex(openssl::md5(charToRaw(input)))
}

#' Generate a random password.
#'
#' @param length Password length
#' @param include_uppercase Include uppercase letters
#' @param include_lowercase Include lowercase letters
#' @param include_numbers Include numbers
#' @param include_symbols Include symbols
#' @return Password string
#' @export
generate_password <- function(length, include_uppercase = TRUE,
                              include_lowercase = TRUE,
                              include_numbers = TRUE,
                              include_symbols = TRUE) {
  chars <- ""
  if (include_lowercase) chars <- paste0(chars, "abcdefghijklmnopqrstuvwxyz")
  if (include_uppercase) chars <- paste0(chars, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  if (include_numbers) chars <- paste0(chars, "0123456789")
  if (include_symbols) chars <- paste0(chars, "!@#$%^&*()_+-=[]{}|;:,.<>?")

  if (nchar(chars) == 0) return("")

  bytes <- random_bytes(length)
  indices <- (as.integer(bytes) %% nchar(chars)) + 1
  paste0(substring(chars, indices, indices), collapse = "")
}

#' Generate password with defaults.
#'
#' @return Password string
#' @export
generate_password_default <- function() {
  generate_password(16, TRUE, TRUE, TRUE, TRUE)
}

#' Securely wipe a raw vector (best effort).
#'
#' @param data Raw vector reference (will be modified in place)
#' @export
secure_wipe <- function(data) {
  # Note: R doesn't support true in-place modification
  # This is best-effort only
  for (i in seq_along(data)) {
    data[i] <- as.raw(0)
  }
  invisible(data)
}
