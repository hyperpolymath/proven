/* SPDX-License-Identifier: Apache-2.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_hex.c
 * @brief Safe hexadecimal encoding and decoding implementation.
 */

#include "../include/safe_hex.h"
#include <string.h>
#include <ctype.h>

/* ============================================================================
 * Lookup Tables
 * ============================================================================ */

static const char HEX_LOWER[] = "0123456789abcdef";
static const char HEX_UPPER[] = "0123456789ABCDEF";

/* ============================================================================
 * Nibble Operations
 * ============================================================================ */

HexStatus hex_char_to_nibble(char c, uint8_t* nibble) {
    if (nibble == NULL) {
        return HEX_ERR_NULL_POINTER;
    }

    if (c >= '0' && c <= '9') {
        *nibble = (uint8_t)(c - '0');
        return HEX_OK;
    }
    if (c >= 'a' && c <= 'f') {
        *nibble = (uint8_t)(c - 'a' + 10);
        return HEX_OK;
    }
    if (c >= 'A' && c <= 'F') {
        *nibble = (uint8_t)(c - 'A' + 10);
        return HEX_OK;
    }

    return HEX_ERR_INVALID_CHAR;
}

char hex_nibble_to_char(uint8_t nibble) {
    return HEX_LOWER[nibble & 0x0F];
}

char hex_nibble_to_char_upper(uint8_t nibble) {
    return HEX_UPPER[nibble & 0x0F];
}

/* ============================================================================
 * Hex Encoding
 * ============================================================================ */

void hex_encode_byte(uint8_t byte, char* high, char* low) {
    if (high) *high = HEX_LOWER[(byte >> 4) & 0x0F];
    if (low) *low = HEX_LOWER[byte & 0x0F];
}

void hex_encode_byte_upper(uint8_t byte, char* high, char* low) {
    if (high) *high = HEX_UPPER[(byte >> 4) & 0x0F];
    if (low) *low = HEX_UPPER[byte & 0x0F];
}

HexEncodeResult hex_encode(const uint8_t* bytes, size_t bytes_len,
                           char* hex, size_t hex_size) {
    HexEncodeResult result = { .status = HEX_OK, .chars_written = 0 };

    if (bytes == NULL || hex == NULL) {
        result.status = HEX_ERR_NULL_POINTER;
        return result;
    }

    size_t required = bytes_len * 2 + 1;
    if (hex_size < required) {
        result.status = HEX_ERR_BUFFER_TOO_SMALL;
        return result;
    }

    for (size_t i = 0; i < bytes_len; i++) {
        hex[i * 2] = HEX_LOWER[(bytes[i] >> 4) & 0x0F];
        hex[i * 2 + 1] = HEX_LOWER[bytes[i] & 0x0F];
    }
    hex[bytes_len * 2] = '\0';

    result.chars_written = bytes_len * 2;
    return result;
}

HexEncodeResult hex_encode_upper(const uint8_t* bytes, size_t bytes_len,
                                 char* hex, size_t hex_size) {
    HexEncodeResult result = { .status = HEX_OK, .chars_written = 0 };

    if (bytes == NULL || hex == NULL) {
        result.status = HEX_ERR_NULL_POINTER;
        return result;
    }

    size_t required = bytes_len * 2 + 1;
    if (hex_size < required) {
        result.status = HEX_ERR_BUFFER_TOO_SMALL;
        return result;
    }

    for (size_t i = 0; i < bytes_len; i++) {
        hex[i * 2] = HEX_UPPER[(bytes[i] >> 4) & 0x0F];
        hex[i * 2 + 1] = HEX_UPPER[bytes[i] & 0x0F];
    }
    hex[bytes_len * 2] = '\0';

    result.chars_written = bytes_len * 2;
    return result;
}

/* ============================================================================
 * Hex Decoding
 * ============================================================================ */

HexStatus hex_decode_byte(char high, char low, uint8_t* byte) {
    if (byte == NULL) {
        return HEX_ERR_NULL_POINTER;
    }

    uint8_t high_nibble, low_nibble;

    if (hex_char_to_nibble(high, &high_nibble) != HEX_OK) {
        return HEX_ERR_INVALID_CHAR;
    }
    if (hex_char_to_nibble(low, &low_nibble) != HEX_OK) {
        return HEX_ERR_INVALID_CHAR;
    }

    *byte = (high_nibble << 4) | low_nibble;
    return HEX_OK;
}

HexDecodeResult hex_decode(const char* hex, size_t hex_len,
                           uint8_t* bytes, size_t bytes_size) {
    HexDecodeResult result = { .status = HEX_OK, .bytes_written = 0 };

    if (hex == NULL || bytes == NULL) {
        result.status = HEX_ERR_NULL_POINTER;
        return result;
    }

    if (hex_len % 2 != 0) {
        result.status = HEX_ERR_ODD_LENGTH;
        return result;
    }

    size_t num_bytes = hex_len / 2;
    if (bytes_size < num_bytes) {
        result.status = HEX_ERR_BUFFER_TOO_SMALL;
        return result;
    }

    for (size_t i = 0; i < num_bytes; i++) {
        HexStatus status = hex_decode_byte(hex[i * 2], hex[i * 2 + 1], &bytes[i]);
        if (status != HEX_OK) {
            result.status = status;
            return result;
        }
    }

    result.bytes_written = num_bytes;
    return result;
}

HexDecodeResult hex_decode_cstr(const char* hex, uint8_t* bytes, size_t bytes_size) {
    if (hex == NULL) {
        HexDecodeResult result = { .status = HEX_ERR_NULL_POINTER, .bytes_written = 0 };
        return result;
    }
    return hex_decode(hex, strlen(hex), bytes, bytes_size);
}

/* ============================================================================
 * Hex Validation
 * ============================================================================ */

bool hex_is_valid_char(char c) {
    return (c >= '0' && c <= '9') ||
           (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}

bool hex_is_valid(const char* str, size_t len) {
    if (str == NULL) {
        return false;
    }

    for (size_t i = 0; i < len; i++) {
        if (!hex_is_valid_char(str[i])) {
            return false;
        }
    }
    return true;
}

bool hex_is_valid_bytes(const char* str, size_t len) {
    return (len % 2 == 0) && hex_is_valid(str, len);
}

bool hex_is_valid_cstr(const char* str) {
    if (str == NULL) return false;
    return hex_is_valid(str, strlen(str));
}

bool hex_is_valid_bytes_cstr(const char* str) {
    if (str == NULL) return false;
    return hex_is_valid_bytes(str, strlen(str));
}

/* ============================================================================
 * Constant-Time Operations
 * ============================================================================ */

/**
 * @brief Convert hex char to lowercase (constant-time)
 */
static char ct_tolower(char c) {
    /* Add 0x20 if uppercase letter (A-F are 0x41-0x46, a-f are 0x61-0x66) */
    char is_upper = (c >= 'A') & (c <= 'F');
    return c | (is_upper << 5);
}

bool hex_constant_time_eq(const char* a, size_t a_len,
                          const char* b, size_t b_len) {
    if (a == NULL || b == NULL) {
        return false;
    }

    /* Different lengths cannot be equal */
    if (a_len != b_len) {
        return false;
    }

    /* Constant-time comparison with case-insensitive matching */
    volatile uint8_t diff = 0;
    for (size_t i = 0; i < a_len; i++) {
        char ca = ct_tolower(a[i]);
        char cb = ct_tolower(b[i]);
        diff |= (uint8_t)(ca ^ cb);
    }

    return diff == 0;
}

bool hex_constant_time_eq_bytes(const uint8_t* a, size_t a_len,
                                const uint8_t* b, size_t b_len) {
    if (a == NULL || b == NULL) {
        return false;
    }

    /* Different lengths cannot be equal */
    if (a_len != b_len) {
        return false;
    }

    /* Constant-time comparison */
    volatile uint8_t diff = 0;
    for (size_t i = 0; i < a_len; i++) {
        diff |= a[i] ^ b[i];
    }

    return diff == 0;
}

/* ============================================================================
 * Hex Formatting
 * ============================================================================ */

HexEncodeResult hex_format_spaced(const char* hex, size_t hex_len,
                                  char* out, size_t out_size) {
    HexEncodeResult result = { .status = HEX_OK, .chars_written = 0 };

    if (hex == NULL || out == NULL) {
        result.status = HEX_ERR_NULL_POINTER;
        return result;
    }

    if (hex_len % 2 != 0) {
        result.status = HEX_ERR_ODD_LENGTH;
        return result;
    }

    if (hex_len == 0) {
        out[0] = '\0';
        return result;
    }

    /* Calculate required size: hex_len + (hex_len/2 - 1) spaces + null */
    size_t num_bytes = hex_len / 2;
    size_t required = hex_len + (num_bytes - 1) + 1;

    if (out_size < required) {
        result.status = HEX_ERR_BUFFER_TOO_SMALL;
        return result;
    }

    size_t out_pos = 0;
    for (size_t i = 0; i < num_bytes; i++) {
        if (i > 0) {
            out[out_pos++] = ' ';
        }
        out[out_pos++] = hex[i * 2];
        out[out_pos++] = hex[i * 2 + 1];
    }
    out[out_pos] = '\0';

    result.chars_written = out_pos;
    return result;
}

HexEncodeResult hex_format_colons(const char* hex, size_t hex_len,
                                  char* out, size_t out_size) {
    HexEncodeResult result = { .status = HEX_OK, .chars_written = 0 };

    if (hex == NULL || out == NULL) {
        result.status = HEX_ERR_NULL_POINTER;
        return result;
    }

    if (hex_len % 2 != 0) {
        result.status = HEX_ERR_ODD_LENGTH;
        return result;
    }

    if (hex_len == 0) {
        out[0] = '\0';
        return result;
    }

    size_t num_bytes = hex_len / 2;
    size_t required = hex_len + (num_bytes - 1) + 1;

    if (out_size < required) {
        result.status = HEX_ERR_BUFFER_TOO_SMALL;
        return result;
    }

    size_t out_pos = 0;
    for (size_t i = 0; i < num_bytes; i++) {
        if (i > 0) {
            out[out_pos++] = ':';
        }
        out[out_pos++] = hex[i * 2];
        out[out_pos++] = hex[i * 2 + 1];
    }
    out[out_pos] = '\0';

    result.chars_written = out_pos;
    return result;
}

HexEncodeResult hex_format_groups(const char* hex, size_t hex_len,
                                  size_t group_size, char separator,
                                  char* out, size_t out_size) {
    HexEncodeResult result = { .status = HEX_OK, .chars_written = 0 };

    if (hex == NULL || out == NULL) {
        result.status = HEX_ERR_NULL_POINTER;
        return result;
    }

    if (group_size == 0) {
        group_size = 2;
    }

    if (hex_len == 0) {
        out[0] = '\0';
        return result;
    }

    /* Calculate number of groups and required size */
    size_t num_groups = (hex_len + group_size - 1) / group_size;
    size_t required = hex_len + (num_groups - 1) + 1;

    if (out_size < required) {
        result.status = HEX_ERR_BUFFER_TOO_SMALL;
        return result;
    }

    size_t out_pos = 0;
    for (size_t i = 0; i < hex_len; i++) {
        if (i > 0 && i % group_size == 0) {
            out[out_pos++] = separator;
        }
        out[out_pos++] = hex[i];
    }
    out[out_pos] = '\0';

    result.chars_written = out_pos;
    return result;
}

/* ============================================================================
 * Integer Conversion
 * ============================================================================ */

HexEncodeResult hex_from_int(uint64_t value, size_t min_width, bool uppercase,
                             char* hex, size_t hex_size) {
    HexEncodeResult result = { .status = HEX_OK, .chars_written = 0 };

    if (hex == NULL) {
        result.status = HEX_ERR_NULL_POINTER;
        return result;
    }

    const char* table = uppercase ? HEX_UPPER : HEX_LOWER;

    /* First, figure out how many digits we need */
    size_t digits = 0;
    uint64_t temp = value;
    do {
        digits++;
        temp >>= 4;
    } while (temp > 0);

    /* Apply minimum width */
    if (digits < min_width) {
        digits = min_width;
    }

    /* Check buffer size */
    if (hex_size < digits + 1) {
        result.status = HEX_ERR_BUFFER_TOO_SMALL;
        return result;
    }

    /* Write digits from right to left */
    hex[digits] = '\0';
    for (size_t i = 0; i < digits; i++) {
        hex[digits - 1 - i] = table[value & 0x0F];
        value >>= 4;
    }

    result.chars_written = digits;
    return result;
}

HexIntResult hex_to_int(const char* hex, size_t hex_len) {
    HexIntResult result = { .status = HEX_OK, .value = 0 };

    if (hex == NULL) {
        result.status = HEX_ERR_NULL_POINTER;
        return result;
    }

    /* Strip 0x prefix if present */
    const char* start = hex;
    size_t len = hex_len;
    hex_strip_prefix(hex, hex_len, &start, &len);

    if (len == 0) {
        result.status = HEX_ERR_INVALID_CHAR;
        return result;
    }

    /* Maximum of 16 hex digits for uint64_t */
    if (len > 16) {
        result.status = HEX_ERR_OVERFLOW;
        return result;
    }

    uint64_t value = 0;
    for (size_t i = 0; i < len; i++) {
        uint8_t nibble;
        if (hex_char_to_nibble(start[i], &nibble) != HEX_OK) {
            result.status = HEX_ERR_INVALID_CHAR;
            return result;
        }
        value = (value << 4) | nibble;
    }

    result.value = value;
    return result;
}

HexIntResult hex_to_int_cstr(const char* hex) {
    if (hex == NULL) {
        HexIntResult result = { .status = HEX_ERR_NULL_POINTER, .value = 0 };
        return result;
    }
    return hex_to_int(hex, strlen(hex));
}

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

const char* hex_status_string(HexStatus status) {
    switch (status) {
        case HEX_OK:                  return "Success";
        case HEX_ERR_NULL_POINTER:    return "Null pointer";
        case HEX_ERR_INVALID_CHAR:    return "Invalid hex character";
        case HEX_ERR_ODD_LENGTH:      return "Odd-length hex string";
        case HEX_ERR_BUFFER_TOO_SMALL: return "Buffer too small";
        case HEX_ERR_OVERFLOW:        return "Value overflow";
        default:                      return "Unknown error";
    }
}

size_t hex_encode_size(size_t bytes_len) {
    return bytes_len * 2 + 1;
}

size_t hex_decode_size(size_t hex_len) {
    return hex_len / 2;
}

void hex_strip_prefix(const char* hex, size_t len,
                      const char** new_start, size_t* new_len) {
    if (hex == NULL || new_start == NULL || new_len == NULL) {
        if (new_start) *new_start = hex;
        if (new_len) *new_len = len;
        return;
    }

    if (len >= 2 && hex[0] == '0' && (hex[1] == 'x' || hex[1] == 'X')) {
        *new_start = hex + 2;
        *new_len = len - 2;
    } else {
        *new_start = hex;
        *new_len = len;
    }
}
