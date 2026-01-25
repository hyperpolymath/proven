// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

module proven

import strings
import encoding.base64

// Safe string operations for XSS prevention and sanitization.

// Escape HTML special characters.
pub fn escape_html(input string) string {
	mut builder := strings.new_builder(input.len * 2)
	for c in input {
		match c {
			`&` { builder.write_string('&amp;') }
			`<` { builder.write_string('&lt;') }
			`>` { builder.write_string('&gt;') }
			`"` { builder.write_string('&quot;') }
			`'` { builder.write_string('&#x27;') }
			else { builder.write_u8(c) }
		}
	}
	return builder.str()
}

// Escape for SQL (single quotes).
pub fn escape_sql(input string) string {
	return input.replace("'", "''")
}

// Escape for JavaScript strings.
pub fn escape_js(input string) string {
	mut builder := strings.new_builder(input.len * 2)
	for c in input {
		match c {
			`\\` { builder.write_string('\\\\') }
			`"` { builder.write_string('\\"') }
			`'` { builder.write_string("\\'") }
			`\n` { builder.write_string('\\n') }
			`\r` { builder.write_string('\\r') }
			`\t` { builder.write_string('\\t') }
			`<` { builder.write_string('\\x3c') }
			`>` { builder.write_string('\\x3e') }
			else { builder.write_u8(c) }
		}
	}
	return builder.str()
}

// URL encode a string.
pub fn url_encode(input string) string {
	mut builder := strings.new_builder(input.len * 3)
	for c in input {
		if is_url_safe(c) {
			builder.write_u8(c)
		} else {
			builder.write_string('%')
			builder.write_string(hex_byte(c))
		}
	}
	return builder.str()
}

// Check if character is URL-safe.
fn is_url_safe(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) ||
		c == `-` || c == `_` || c == `.` || c == `~`
}

// Convert byte to hex string.
fn hex_byte(b u8) string {
	hex_chars := '0123456789ABCDEF'
	return '${hex_chars[b >> 4]}${hex_chars[b & 0x0F]}'
}

// Sanitize string to only allow safe characters.
pub fn sanitize(input string, allowed string) string {
	mut builder := strings.new_builder(input.len)
	for c in input {
		if allowed.contains(c.ascii_str()) {
			builder.write_u8(c)
		}
	}
	return builder.str()
}

// Default sanitization (alphanumeric + underscore + hyphen).
pub fn sanitize_default(input string) string {
	return sanitize(input, 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-')
}

// Convert to slug.
pub fn slugify(input string) string {
	mut result := input.to_lower()

	// Remove non-alphanumeric except spaces and hyphens
	mut builder := strings.new_builder(result.len)
	for c in result {
		if (c >= `a` && c <= `z`) || (c >= `0` && c <= `9`) || c == ` ` || c == `-` {
			builder.write_u8(c)
		}
	}
	result = builder.str()

	// Replace spaces with hyphens
	result = result.replace(' ', '-')

	// Collapse multiple hyphens
	for result.contains('--') {
		result = result.replace('--', '-')
	}

	// Trim hyphens from ends
	return result.trim('-')
}

// Truncate string safely.
pub fn truncate(input string, max_length int, suffix string) string {
	if input.len <= max_length {
		return input
	}
	if max_length <= suffix.len {
		return suffix
	}
	return input[..max_length - suffix.len] + suffix
}

// Remove control characters.
pub fn strip_control_chars(input string) string {
	mut builder := strings.new_builder(input.len)
	for c in input {
		if c >= 32 && c != 127 {
			builder.write_u8(c)
		}
	}
	return builder.str()
}

// Normalize whitespace.
pub fn normalize_whitespace(input string) string {
	mut result := input.trim_space()
	mut builder := strings.new_builder(result.len)
	mut prev_space := false
	for c in result {
		if c == ` ` || c == `\t` || c == `\n` || c == `\r` {
			if !prev_space {
				builder.write_u8(` `)
				prev_space = true
			}
		} else {
			builder.write_u8(c)
			prev_space = false
		}
	}
	return builder.str()
}

// Check if string contains only ASCII.
pub fn is_ascii_only(input string) bool {
	for c in input {
		if c >= 128 {
			return false
		}
	}
	return true
}

// Check if string contains only printable ASCII.
pub fn is_printable_ascii(input string) bool {
	for c in input {
		if c < 32 || c >= 127 {
			return false
		}
	}
	return true
}

// Base64 encode.
pub fn base64_encode(input string) string {
	return base64.encode_str(input)
}

// Base64 decode.
pub fn base64_decode(input string) ?string {
	result := base64.decode_str(input) or { return none }
	return result
}

// Hex encode bytes.
pub fn hex_encode(input []u8) string {
	mut builder := strings.new_builder(input.len * 2)
	for b in input {
		builder.write_string(hex_byte(b).to_lower())
	}
	return builder.str()
}

// Hex decode to bytes.
pub fn hex_decode(input string) ?[]u8 {
	if input.len % 2 != 0 {
		return none
	}
	mut result := []u8{cap: input.len / 2}
	for i := 0; i < input.len; i += 2 {
		high := hex_char_to_int(input[i]) or { return none }
		low := hex_char_to_int(input[i + 1]) or { return none }
		result << u8((high << 4) | low)
	}
	return result
}

// Convert hex character to integer.
fn hex_char_to_int(c u8) ?int {
	if c >= `0` && c <= `9` {
		return int(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return int(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return int(c - `A` + 10)
	}
	return none
}
