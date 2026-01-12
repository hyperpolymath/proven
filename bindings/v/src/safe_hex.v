// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

module proven

import strings

// Hex encoding options.
pub enum HexCase {
	lower // Lowercase hex (a-f)
	upper // Uppercase hex (A-F)
}

// Encode bytes to hexadecimal string.
pub fn hex_encode(input []u8) string {
	return hex_encode_case(input, .lower)
}

// Encode bytes to hexadecimal string with case option.
pub fn hex_encode_case(input []u8, hex_case HexCase) string {
	if input.len == 0 {
		return ''
	}

	chars := match hex_case {
		.lower { '0123456789abcdef' }
		.upper { '0123456789ABCDEF' }
	}

	mut builder := strings.new_builder(input.len * 2)
	for b in input {
		builder.write_u8(chars[b >> 4])
		builder.write_u8(chars[b & 0x0F])
	}
	return builder.str()
}

// Decode hexadecimal string to bytes.
pub fn hex_decode(input string) ?[]u8 {
	if input.len == 0 {
		return []u8{}
	}

	if input.len % 2 != 0 {
		return none // Invalid hex string length
	}

	mut result := []u8{cap: input.len / 2}
	for i := 0; i < input.len; i += 2 {
		high := hex_nibble(input[i]) or { return none }
		low := hex_nibble(input[i + 1]) or { return none }
		result << u8((high << 4) | low)
	}
	return result
}

// Helper: convert hex character to nibble value.
fn hex_nibble(c u8) ?u8 {
	if c >= `0` && c <= `9` {
		return c - `0`
	}
	if c >= `a` && c <= `f` {
		return c - `a` + 10
	}
	if c >= `A` && c <= `F` {
		return c - `A` + 10
	}
	return none
}

// Check if string is valid hexadecimal.
pub fn is_valid_hex(input string) bool {
	if input.len == 0 {
		return true // Empty string is valid
	}
	if input.len % 2 != 0 {
		return false
	}
	for c in input {
		if !is_hex_char(c) {
			return false
		}
	}
	return true
}

// Check if character is a valid hex digit.
fn is_hex_char(c u8) bool {
	return (c >= `0` && c <= `9`) || (c >= `a` && c <= `f`) || (c >= `A` && c <= `F`)
}

// Constant-time byte array comparison to prevent timing attacks.
// Returns true if arrays are equal, false otherwise.
// Time taken is proportional to length only, not content.
pub fn constant_time_equal(a []u8, b []u8) bool {
	if a.len != b.len {
		// Note: Length comparison leaks length info, but this is
		// acceptable for most use cases and standard practice.
		return false
	}

	mut result := u8(0)
	for i in 0 .. a.len {
		result |= a[i] ^ b[i]
	}
	return result == 0
}

// Constant-time string comparison.
pub fn constant_time_equal_string(a string, b string) bool {
	return constant_time_equal(a.bytes(), b.bytes())
}

// Constant-time hex string comparison.
pub fn constant_time_equal_hex(a string, b string) bool {
	// Normalize case before comparison
	a_lower := a.to_lower()
	b_lower := b.to_lower()
	return constant_time_equal(a_lower.bytes(), b_lower.bytes())
}

// Encode string to hex.
pub fn hex_encode_string(input string) string {
	return hex_encode(input.bytes())
}

// Decode hex to string.
pub fn hex_decode_string(input string) ?string {
	bytes := hex_decode(input) or { return none }
	return bytes.bytestr()
}

// Encode bytes to hex with separator.
pub fn hex_encode_separated(input []u8, separator string) string {
	if input.len == 0 {
		return ''
	}

	chars := '0123456789abcdef'
	mut parts := []string{cap: input.len}

	for b in input {
		high := chars[b >> 4]
		low := chars[b & 0x0F]
		parts << '${high}${low}'
	}

	return parts.join(separator)
}

// Decode hex with separator.
pub fn hex_decode_separated(input string, separator string) ?[]u8 {
	if input.len == 0 {
		return []u8{}
	}

	cleaned := input.replace(separator, '')
	return hex_decode(cleaned)
}

// Format bytes as hex dump (address: hex bytes | ascii).
pub fn hex_dump(input []u8, bytes_per_line int) string {
	if input.len == 0 {
		return ''
	}

	bpl := if bytes_per_line <= 0 { 16 } else { bytes_per_line }
	mut builder := strings.new_builder(input.len * 4)

	for offset := 0; offset < input.len; offset += bpl {
		// Address
		builder.write_string('${offset:08x}  ')

		// Hex bytes
		end := if offset + bpl > input.len { input.len } else { offset + bpl }
		for i := offset; i < end; i++ {
			builder.write_string('${input[i]:02x} ')
		}

		// Padding for incomplete lines
		for _ in 0 .. (bpl - (end - offset)) {
			builder.write_string('   ')
		}

		builder.write_string(' |')

		// ASCII representation
		for i := offset; i < end; i++ {
			c := input[i]
			if c >= 32 && c < 127 {
				builder.write_u8(c)
			} else {
				builder.write_u8(`.`)
			}
		}

		builder.write_string('|\n')
	}

	return builder.str()
}

// XOR two byte arrays (returns none if lengths differ).
pub fn xor_bytes(a []u8, b []u8) ?[]u8 {
	if a.len != b.len {
		return none
	}

	mut result := []u8{len: a.len}
	for i in 0 .. a.len {
		result[i] = a[i] ^ b[i]
	}
	return result
}

// XOR two hex strings (returns none if invalid or lengths differ).
pub fn xor_hex(a string, b string) ?string {
	a_bytes := hex_decode(a) or { return none }
	b_bytes := hex_decode(b) or { return none }
	result := xor_bytes(a_bytes, b_bytes) or { return none }
	return hex_encode(result)
}

// Reverse bytes in place.
pub fn reverse_bytes(mut input []u8) {
	if input.len <= 1 {
		return
	}

	mut left := 0
	mut right := input.len - 1
	for left < right {
		tmp := input[left]
		input[left] = input[right]
		input[right] = tmp
		left++
		right--
	}
}

// Create reversed copy of bytes.
pub fn reverse_bytes_copy(input []u8) []u8 {
	if input.len == 0 {
		return []u8{}
	}

	mut result := []u8{len: input.len}
	for i, b in input {
		result[input.len - 1 - i] = b
	}
	return result
}

// Pad bytes to length with specified byte value.
pub fn pad_bytes(input []u8, length int, pad_byte u8) []u8 {
	if input.len >= length {
		return input.clone()
	}

	mut result := []u8{len: length, init: pad_byte}
	for i, b in input {
		result[i] = b
	}
	return result
}

// Pad bytes on the left (prepend) to specified length.
pub fn pad_bytes_left(input []u8, length int, pad_byte u8) []u8 {
	if input.len >= length {
		return input.clone()
	}

	pad_count := length - input.len
	mut result := []u8{len: length, init: pad_byte}
	for i, b in input {
		result[pad_count + i] = b
	}
	return result
}
