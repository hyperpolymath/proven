/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_hex.h
 * @brief Safe hexadecimal encoding and decoding.
 *
 * This module provides hex encoding/decoding operations with proper error
 * handling, constant-time comparison for security-sensitive operations,
 * and various formatting options.
 */

#ifndef SAFE_HEX_H
#define SAFE_HEX_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Status Codes
 * ============================================================================ */

/**
 * @brief Status codes for hex operations
 */
typedef enum HexStatus {
    HEX_OK = 0,
    HEX_ERR_NULL_POINTER = -1,
    HEX_ERR_INVALID_CHAR = -2,
    HEX_ERR_ODD_LENGTH = -3,
    HEX_ERR_BUFFER_TOO_SMALL = -4,
    HEX_ERR_OVERFLOW = -5
} HexStatus;

/* ============================================================================
 * Structures
 * ============================================================================ */

/**
 * @brief Result type for hex decoding
 */
typedef struct HexDecodeResult {
    HexStatus status;
    size_t bytes_written;
} HexDecodeResult;

/**
 * @brief Result type for hex encoding
 */
typedef struct HexEncodeResult {
    HexStatus status;
    size_t chars_written;
} HexEncodeResult;

/**
 * @brief Result type for integer parsing
 */
typedef struct HexIntResult {
    HexStatus status;
    uint64_t value;
} HexIntResult;

/* ============================================================================
 * Hex Encoding
 * ============================================================================ */

/**
 * @brief Encode bytes to lowercase hex string
 * @param bytes Input bytes
 * @param bytes_len Number of bytes to encode
 * @param[out] hex Output hex string buffer
 * @param hex_size Size of output buffer (must be >= bytes_len * 2 + 1)
 * @return HexEncodeResult with status and characters written
 *
 * Output is null-terminated.
 */
HexEncodeResult hex_encode(const uint8_t* bytes, size_t bytes_len,
                           char* hex, size_t hex_size);

/**
 * @brief Encode bytes to uppercase hex string
 * @param bytes Input bytes
 * @param bytes_len Number of bytes to encode
 * @param[out] hex Output hex string buffer
 * @param hex_size Size of output buffer (must be >= bytes_len * 2 + 1)
 * @return HexEncodeResult with status and characters written
 */
HexEncodeResult hex_encode_upper(const uint8_t* bytes, size_t bytes_len,
                                 char* hex, size_t hex_size);

/**
 * @brief Encode single byte to hex (lowercase)
 * @param byte Input byte
 * @param[out] high High nibble character
 * @param[out] low Low nibble character
 */
void hex_encode_byte(uint8_t byte, char* high, char* low);

/**
 * @brief Encode single byte to hex (uppercase)
 * @param byte Input byte
 * @param[out] high High nibble character
 * @param[out] low Low nibble character
 */
void hex_encode_byte_upper(uint8_t byte, char* high, char* low);

/* ============================================================================
 * Hex Decoding
 * ============================================================================ */

/**
 * @brief Decode hex string to bytes
 * @param hex Input hex string
 * @param hex_len Length of hex string (must be even)
 * @param[out] bytes Output byte buffer
 * @param bytes_size Size of output buffer (must be >= hex_len / 2)
 * @return HexDecodeResult with status and bytes written
 */
HexDecodeResult hex_decode(const char* hex, size_t hex_len,
                           uint8_t* bytes, size_t bytes_size);

/**
 * @brief Decode null-terminated hex string to bytes
 * @param hex Null-terminated hex string
 * @param[out] bytes Output byte buffer
 * @param bytes_size Size of output buffer
 * @return HexDecodeResult with status and bytes written
 */
HexDecodeResult hex_decode_cstr(const char* hex, uint8_t* bytes, size_t bytes_size);

/**
 * @brief Decode two hex characters to a byte
 * @param high High nibble character
 * @param low Low nibble character
 * @param[out] byte Output byte
 * @return HEX_OK on success, HEX_ERR_INVALID_CHAR if invalid
 */
HexStatus hex_decode_byte(char high, char low, uint8_t* byte);

/* ============================================================================
 * Hex Validation
 * ============================================================================ */

/**
 * @brief Check if character is valid hex digit
 * @param c Character to check
 * @return true if valid hex digit (0-9, a-f, A-F)
 */
bool hex_is_valid_char(char c);

/**
 * @brief Check if string contains only valid hex characters
 * @param str String to check
 * @param len Length of string
 * @return true if all characters are valid hex
 */
bool hex_is_valid(const char* str, size_t len);

/**
 * @brief Check if string is valid hex bytes (even length, valid chars)
 * @param str String to check
 * @param len Length of string
 * @return true if valid hex byte string
 */
bool hex_is_valid_bytes(const char* str, size_t len);

/**
 * @brief Check if null-terminated string is valid hex
 * @param str Null-terminated string to check
 * @return true if all characters are valid hex
 */
bool hex_is_valid_cstr(const char* str);

/**
 * @brief Check if null-terminated string is valid hex bytes
 * @param str Null-terminated string to check
 * @return true if valid hex byte string
 */
bool hex_is_valid_bytes_cstr(const char* str);

/* ============================================================================
 * Constant-Time Operations (Timing-Safe)
 * ============================================================================ */

/**
 * @brief Constant-time hex string comparison
 * @param a First hex string
 * @param a_len Length of first string
 * @param b Second hex string
 * @param b_len Length of second string
 * @return true if equal (case-insensitive)
 *
 * This function is resistant to timing attacks. Use for comparing
 * security-sensitive values like API keys, tokens, and hashes.
 *
 * @note Both strings must be the same length to be considered equal.
 */
bool hex_constant_time_eq(const char* a, size_t a_len,
                          const char* b, size_t b_len);

/**
 * @brief Constant-time byte array comparison
 * @param a First byte array
 * @param a_len Length of first array
 * @param b Second byte array
 * @param b_len Length of second array
 * @return true if equal
 *
 * This function is resistant to timing attacks.
 *
 * @note Both arrays must be the same length to be considered equal.
 */
bool hex_constant_time_eq_bytes(const uint8_t* a, size_t a_len,
                                const uint8_t* b, size_t b_len);

/* ============================================================================
 * Hex Formatting
 * ============================================================================ */

/**
 * @brief Format hex with spaces between bytes
 * @param hex Input hex string
 * @param hex_len Length of hex string (must be even)
 * @param[out] out Output buffer
 * @param out_size Size of output buffer
 * @return HexEncodeResult with status and characters written
 *
 * Output format: "aa bb cc dd"
 */
HexEncodeResult hex_format_spaced(const char* hex, size_t hex_len,
                                  char* out, size_t out_size);

/**
 * @brief Format hex with colons between bytes
 * @param hex Input hex string
 * @param hex_len Length of hex string (must be even)
 * @param[out] out Output buffer
 * @param out_size Size of output buffer
 * @return HexEncodeResult with status and characters written
 *
 * Output format: "aa:bb:cc:dd"
 */
HexEncodeResult hex_format_colons(const char* hex, size_t hex_len,
                                  char* out, size_t out_size);

/**
 * @brief Format hex in groups (like UUID)
 * @param hex Input hex string
 * @param hex_len Length of hex string
 * @param group_size Characters per group
 * @param separator Separator character
 * @param[out] out Output buffer
 * @param out_size Size of output buffer
 * @return HexEncodeResult with status and characters written
 *
 * Example: hex_format_groups("aabbccdd", 8, 2, '-', ...) -> "aa-bb-cc-dd"
 */
HexEncodeResult hex_format_groups(const char* hex, size_t hex_len,
                                  size_t group_size, char separator,
                                  char* out, size_t out_size);

/* ============================================================================
 * Integer Conversion
 * ============================================================================ */

/**
 * @brief Convert integer to hex string
 * @param value Integer value
 * @param min_width Minimum width (0-padded if needed)
 * @param uppercase Use uppercase hex digits
 * @param[out] hex Output buffer
 * @param hex_size Size of output buffer
 * @return HexEncodeResult with status and characters written
 */
HexEncodeResult hex_from_int(uint64_t value, size_t min_width, bool uppercase,
                             char* hex, size_t hex_size);

/**
 * @brief Parse hex string to integer
 * @param hex Hex string (may have 0x prefix)
 * @param hex_len Length of hex string
 * @return HexIntResult with status and parsed value
 */
HexIntResult hex_to_int(const char* hex, size_t hex_len);

/**
 * @brief Parse null-terminated hex string to integer
 * @param hex Null-terminated hex string
 * @return HexIntResult with status and parsed value
 */
HexIntResult hex_to_int_cstr(const char* hex);

/* ============================================================================
 * Nibble Operations
 * ============================================================================ */

/**
 * @brief Convert hex character to nibble value (0-15)
 * @param c Hex character
 * @param[out] nibble Output nibble value
 * @return HEX_OK on success, HEX_ERR_INVALID_CHAR if invalid
 */
HexStatus hex_char_to_nibble(char c, uint8_t* nibble);

/**
 * @brief Convert nibble value to lowercase hex character
 * @param nibble Nibble value (0-15)
 * @return Hex character ('0'-'9' or 'a'-'f')
 */
char hex_nibble_to_char(uint8_t nibble);

/**
 * @brief Convert nibble value to uppercase hex character
 * @param nibble Nibble value (0-15)
 * @return Hex character ('0'-'9' or 'A'-'F')
 */
char hex_nibble_to_char_upper(uint8_t nibble);

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

/**
 * @brief Get status code description
 * @param status Status code
 * @return Static string describing the status
 */
const char* hex_status_string(HexStatus status);

/**
 * @brief Calculate required buffer size for encoding
 * @param bytes_len Number of bytes to encode
 * @return Required buffer size (including null terminator)
 */
size_t hex_encode_size(size_t bytes_len);

/**
 * @brief Calculate required buffer size for decoding
 * @param hex_len Length of hex string
 * @return Required buffer size for decoded bytes
 */
size_t hex_decode_size(size_t hex_len);

/**
 * @brief Strip 0x prefix if present
 * @param hex Hex string
 * @param len Length of string
 * @param[out] new_start Pointer to start after prefix
 * @param[out] new_len Length after removing prefix
 */
void hex_strip_prefix(const char* hex, size_t len,
                      const char** new_start, size_t* new_len);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_HEX_H */
