// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"
import "core:fmt"

// Hex character lookup table (lowercase).
@(private)
HEX_CHARS_LOWER :: "0123456789abcdef"

// Hex character lookup table (uppercase).
@(private)
HEX_CHARS_UPPER :: "0123456789ABCDEF"

// Encode bytes to lowercase hex string.
hex_encode_lower :: proc(data: []u8, allocator := context.allocator) -> string {
    if len(data) == 0 {
        return ""
    }

    builder := strings.builder_make_len_cap(0, len(data) * 2, allocator)

    for b in data {
        strings.write_byte(&builder, HEX_CHARS_LOWER[b >> 4])
        strings.write_byte(&builder, HEX_CHARS_LOWER[b & 0x0F])
    }

    return strings.to_string(builder)
}

// Encode bytes to uppercase hex string.
hex_encode_upper :: proc(data: []u8, allocator := context.allocator) -> string {
    if len(data) == 0 {
        return ""
    }

    builder := strings.builder_make_len_cap(0, len(data) * 2, allocator)

    for b in data {
        strings.write_byte(&builder, HEX_CHARS_UPPER[b >> 4])
        strings.write_byte(&builder, HEX_CHARS_UPPER[b & 0x0F])
    }

    return strings.to_string(builder)
}

// Default hex_encode (lowercase).
hex_encode_bytes :: proc(data: []u8, allocator := context.allocator) -> string {
    return hex_encode_lower(data, allocator)
}

// Encode string to hex.
hex_encode_string :: proc(input: string, allocator := context.allocator) -> string {
    return hex_encode_lower(transmute([]u8)input, allocator)
}

// Overloaded hex_encode procedure.
hex_encode :: proc{hex_encode_bytes, hex_encode_string}

// Decode hex string to bytes.
hex_decode :: proc(hex_string: string, allocator := context.allocator) -> (data: []u8, ok: bool) {
    s := hex_string

    // Handle optional 0x prefix
    if len(s) >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X') {
        s = s[2:]
    }

    // Must have even length
    if len(s) % 2 != 0 {
        return nil, false
    }

    if len(s) == 0 {
        return make([]u8, 0, allocator), true
    }

    result := make([]u8, len(s) / 2, allocator)

    for i := 0; i < len(s); i += 2 {
        high := hex_char_to_value(s[i])
        low := hex_char_to_value(s[i + 1])

        if high < 0 || low < 0 {
            delete(result)
            return nil, false
        }

        result[i / 2] = u8(high << 4 | low)
    }

    return result, true
}

// Decode hex string to string.
hex_decode_to_string :: proc(hex_string: string, allocator := context.allocator) -> (result: string, ok: bool) {
    bytes, decode_ok := hex_decode(hex_string, allocator)
    if !decode_ok {
        return "", false
    }
    return string(bytes), true
}

// Convert a hex character to its numeric value (0-15).
@(private)
hex_char_to_value :: proc(c: u8) -> int {
    switch c {
    case '0'..='9':
        return int(c - '0')
    case 'a'..='f':
        return int(c - 'a' + 10)
    case 'A'..='F':
        return int(c - 'A' + 10)
    }
    return -1
}

// Constant-time comparison of two byte slices.
// Returns true if slices are equal, false otherwise.
// Runs in constant time regardless of where differences occur.
constant_time_equal :: proc(a, b: []u8) -> bool {
    if len(a) != len(b) {
        return false
    }

    result: u8 = 0
    for i := 0; i < len(a); i += 1 {
        result |= a[i] ~ b[i]
    }
    return result == 0
}

// Constant-time comparison of two strings.
constant_time_equal_strings :: proc(a, b: string) -> bool {
    return constant_time_equal(transmute([]u8)a, transmute([]u8)b)
}

// Constant-time comparison of two hex strings.
// Decodes both strings and compares the underlying bytes.
constant_time_equal_hex :: proc(hex_a, hex_b: string) -> (equal: bool, ok: bool) {
    a, a_ok := hex_decode(hex_a)
    b, b_ok := hex_decode(hex_b)
    defer {
        if a_ok { delete(a) }
        if b_ok { delete(b) }
    }

    if !a_ok || !b_ok {
        return false, false
    }

    return constant_time_equal(a, b), true
}

// Validate a hex string without decoding.
is_valid_hex :: proc(input: string) -> bool {
    s := input

    // Handle optional 0x prefix
    if len(s) >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X') {
        s = s[2:]
    }

    // Must have even length
    if len(s) % 2 != 0 {
        return false
    }

    // Check all characters are valid hex digits
    for c in s {
        valid := (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
        if !valid {
            return false
        }
    }

    return true
}

// Get the byte length from a hex string length.
hex_byte_length :: proc(hex_string: string) -> int {
    s := hex_string

    // Handle optional 0x prefix
    if len(s) >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X') {
        s = s[2:]
    }

    return len(s) / 2
}

// Format hex string with separators (e.g., "aa:bb:cc:dd").
hex_format_with_separator :: proc(data: []u8, separator: string = ":", allocator := context.allocator) -> string {
    if len(data) == 0 {
        return ""
    }

    // Calculate total length: 2 chars per byte + separators
    sep_count := len(data) - 1
    total_len := len(data) * 2 + sep_count * len(separator)

    builder := strings.builder_make_len_cap(0, total_len, allocator)

    for b, i in data {
        if i > 0 {
            strings.write_string(&builder, separator)
        }
        strings.write_byte(&builder, HEX_CHARS_LOWER[b >> 4])
        strings.write_byte(&builder, HEX_CHARS_LOWER[b & 0x0F])
    }

    return strings.to_string(builder)
}

// Parse hex string with separators (e.g., "aa:bb:cc:dd" or "aa-bb-cc-dd").
hex_parse_with_separator :: proc(input: string, allocator := context.allocator) -> (data: []u8, ok: bool) {
    if len(input) == 0 {
        return make([]u8, 0, allocator), true
    }

    // Remove common separators
    cleaned := strings.replace_all(input, ":", "")
    cleaned = strings.replace_all(cleaned, "-", "")
    cleaned = strings.replace_all(cleaned, " ", "")

    return hex_decode(cleaned, allocator)
}

// Format bytes as hex dump (like xxd output).
hex_dump :: proc(data: []u8, bytes_per_line: int = 16, allocator := context.allocator) -> string {
    if len(data) == 0 {
        return ""
    }

    builder := strings.builder_make(allocator)

    for offset := 0; offset < len(data); offset += bytes_per_line {
        // Address
        fmt.sbprintf(&builder, "%08x: ", offset)

        // Hex bytes
        for i := 0; i < bytes_per_line; i += 1 {
            if offset + i < len(data) {
                fmt.sbprintf(&builder, "%02x ", data[offset + i])
            } else {
                strings.write_string(&builder, "   ")
            }

            // Extra space in the middle
            if i == 7 {
                strings.write_byte(&builder, ' ')
            }
        }

        // ASCII representation
        strings.write_string(&builder, " |")
        for i := 0; i < bytes_per_line && offset + i < len(data); i += 1 {
            b := data[offset + i]
            if b >= 32 && b < 127 {
                strings.write_byte(&builder, b)
            } else {
                strings.write_byte(&builder, '.')
            }
        }
        strings.write_string(&builder, "|\n")
    }

    return strings.to_string(builder)
}

// XOR two byte slices.
hex_xor :: proc(a, b: []u8, allocator := context.allocator) -> (result: []u8, ok: bool) {
    if len(a) != len(b) {
        return nil, false
    }

    result_slice := make([]u8, len(a), allocator)
    for i := 0; i < len(a); i += 1 {
        result_slice[i] = a[i] ~ b[i]
    }

    return result_slice, true
}

// XOR two hex strings.
hex_xor_strings :: proc(hex_a, hex_b: string, allocator := context.allocator) -> (result: string, ok: bool) {
    a, a_ok := hex_decode(hex_a, allocator)
    b, b_ok := hex_decode(hex_b, allocator)
    defer {
        if a_ok { delete(a) }
        if b_ok { delete(b) }
    }

    if !a_ok || !b_ok {
        return "", false
    }

    xored, xor_ok := hex_xor(a, b, allocator)
    if !xor_ok {
        return "", false
    }
    defer delete(xored)

    return hex_encode_lower(xored, allocator), true
}

// Reverse bytes (for endianness conversion).
hex_reverse :: proc(data: []u8, allocator := context.allocator) -> []u8 {
    result := make([]u8, len(data), allocator)
    for i := 0; i < len(data); i += 1 {
        result[i] = data[len(data) - 1 - i]
    }
    return result
}

// Convert hex string from one case to another.
hex_to_lower :: proc(hex_string: string, allocator := context.allocator) -> (result: string, ok: bool) {
    bytes, decode_ok := hex_decode(hex_string, allocator)
    if !decode_ok {
        return "", false
    }
    defer delete(bytes)

    return hex_encode_lower(bytes, allocator), true
}

// Convert hex string to uppercase.
hex_to_upper :: proc(hex_string: string, allocator := context.allocator) -> (result: string, ok: bool) {
    bytes, decode_ok := hex_decode(hex_string, allocator)
    if !decode_ok {
        return "", false
    }
    defer delete(bytes)

    return hex_encode_upper(bytes, allocator), true
}

// Pad hex string to a specific byte length.
hex_pad_left :: proc(hex_string: string, target_bytes: int, allocator := context.allocator) -> string {
    s := hex_string

    // Handle optional 0x prefix
    has_prefix := len(s) >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')
    if has_prefix {
        s = s[2:]
    }

    current_bytes := len(s) / 2
    if current_bytes >= target_bytes {
        if has_prefix {
            return strings.concatenate({"0x", s}, allocator)
        }
        return s
    }

    padding := target_bytes - current_bytes
    builder := strings.builder_make_len_cap(0, target_bytes * 2 + (has_prefix ? 2 : 0), allocator)

    if has_prefix {
        strings.write_string(&builder, "0x")
    }

    for _ in 0..<padding * 2 {
        strings.write_byte(&builder, '0')
    }
    strings.write_string(&builder, s)

    return strings.to_string(builder)
}

// Trim leading zeros from hex string.
hex_trim_left :: proc(hex_string: string, allocator := context.allocator) -> string {
    s := hex_string

    // Handle optional 0x prefix
    has_prefix := len(s) >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')
    if has_prefix {
        s = s[2:]
    }

    // Find first non-zero
    first_non_zero := 0
    for i := 0; i < len(s) - 1; i += 1 {  // Keep at least one char
        if s[i] != '0' {
            break
        }
        first_non_zero = i + 1
    }

    // Ensure even length
    if first_non_zero % 2 != 0 {
        first_non_zero -= 1
    }

    result := s[first_non_zero:]
    if has_prefix {
        return strings.concatenate({"0x", result}, allocator)
    }
    return result
}
