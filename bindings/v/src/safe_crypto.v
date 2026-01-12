// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

module proven

import crypto.sha256
import crypto.sha512
import crypto.hmac
import crypto.md5
import crypto.rand
import encoding.base64
import strings

// Constant-time byte array comparison to prevent timing attacks.
pub fn constant_time_equals(a []u8, b []u8) bool {
	if a.len != b.len {
		return false
	}

	mut result := u8(0)
	for i in 0 .. a.len {
		result |= a[i] ^ b[i]
	}
	return result == 0
}

// Constant-time string comparison.
pub fn constant_time_equals_string(a string, b string) bool {
	return constant_time_equals(a.bytes(), b.bytes())
}

// Generate cryptographically secure random bytes.
pub fn random_bytes(count int) []u8 {
	mut bytes := []u8{len: count}
	rand.read(mut bytes)
	return bytes
}

// Convert bytes to hex string.
pub fn bytes_to_hex(bytes []u8) string {
	mut builder := strings.new_builder(bytes.len * 2)
	for b in bytes {
		builder.write_string(hex_byte(b).to_lower())
	}
	return builder.str()
}

// Generate random bytes as hex string.
pub fn random_hex(byte_count int) string {
	return bytes_to_hex(random_bytes(byte_count))
}

// Generate random bytes as base64 string.
pub fn random_base64(byte_count int) string {
	return base64.encode(random_bytes(byte_count))
}

// Generate URL-safe random string.
pub fn random_url_safe(byte_count int) string {
	encoded := base64.encode(random_bytes(byte_count))
	mut result := encoded.replace('+', '-')
	result = result.replace('/', '_')
	return result.trim_right('=')
}

// Generate random integer in range [min, max].
pub fn random_int(min_val int, max_val int) int {
	lo, hi := if min_val <= max_val { min_val, max_val } else { max_val, min_val }

	if lo == hi {
		return lo
	}

	range_size := hi - lo + 1
	bytes := random_bytes(4)
	value := u32(bytes[0]) | (u32(bytes[1]) << 8) | (u32(bytes[2]) << 16) | (u32(bytes[3]) << 24)
	return lo + int(value % u32(range_size))
}

// Generate a secure token.
pub fn generate_token(length int) string {
	return random_url_safe(length)
}

// Generate token with default length.
pub fn generate_token_default() string {
	return generate_token(32)
}

// Hash a string with SHA-256.
pub fn sha256_hash(input string) string {
	result := sha256.sum(input.bytes())
	return bytes_to_hex(result[..])
}

// Hash bytes with SHA-256.
pub fn sha256_hash_bytes(input []u8) string {
	result := sha256.sum(input)
	return bytes_to_hex(result[..])
}

// Hash a string with SHA-512.
pub fn sha512_hash(input string) string {
	result := sha512.sum512(input.bytes())
	return bytes_to_hex(result[..])
}

// Hash bytes with SHA-512.
pub fn sha512_hash_bytes(input []u8) string {
	result := sha512.sum512(input)
	return bytes_to_hex(result[..])
}

// Compute HMAC-SHA256.
pub fn hmac_sha256(key string, message string) string {
	result := hmac.new(key.bytes(), message.bytes(), sha256.sum, sha256.block_size)
	return bytes_to_hex(result)
}

// Compute HMAC-SHA256 with bytes.
pub fn hmac_sha256_bytes(key []u8, message []u8) string {
	result := hmac.new(key, message, sha256.sum, sha256.block_size)
	return bytes_to_hex(result)
}

// Compute HMAC-SHA512.
pub fn hmac_sha512(key string, message string) string {
	result := hmac.new(key.bytes(), message.bytes(), sha512.sum512, sha512.block_size)
	return bytes_to_hex(result)
}

// Verify HMAC using constant-time comparison.
pub fn verify_hmac_sha256(key string, message string, expected_mac string) bool {
	return constant_time_equals_string(hmac_sha256(key, message), expected_mac)
}

// Verify HMAC-SHA512 using constant-time comparison.
pub fn verify_hmac_sha512(key string, message string, expected_mac string) bool {
	return constant_time_equals_string(hmac_sha512(key, message), expected_mac)
}

// Hash a string with MD5 (NOT for security, only for checksums).
pub fn md5_hash(input string) string {
	result := md5.sum(input.bytes())
	return bytes_to_hex(result[..])
}

// Generate a random password.
pub fn generate_password(length int, include_uppercase bool, include_lowercase bool, include_numbers bool, include_symbols bool) string {
	mut chars := ''
	if include_lowercase {
		chars += 'abcdefghijklmnopqrstuvwxyz'
	}
	if include_uppercase {
		chars += 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
	}
	if include_numbers {
		chars += '0123456789'
	}
	if include_symbols {
		chars += '!@#\$%^&*()_+-=[]{}|;:,.<>?'
	}

	if chars.len == 0 {
		return ''
	}

	mut builder := strings.new_builder(length)
	bytes := random_bytes(length)
	for i in 0 .. length {
		builder.write_u8(chars[int(bytes[i]) % chars.len])
	}
	return builder.str()
}

// Generate password with defaults.
pub fn generate_password_default() string {
	return generate_password(16, true, true, true, true)
}

// Securely wipe a byte array (best effort).
pub fn secure_wipe(mut data []u8) {
	for i in 0 .. data.len {
		data[i] = 0
	}
}
