// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

module proven

import strings
import crypto.rand

// UUID version types per RFC 4122.
pub enum UuidVersion {
	v1  // Time-based
	v2  // DCE Security
	v3  // Name-based (MD5)
	v4  // Random
	v5  // Name-based (SHA-1)
	nil // Nil UUID
}

// UUID variant types per RFC 4122.
pub enum UuidVariant {
	ncs       // Reserved for NCS backward compatibility
	rfc4122   // RFC 4122 variant
	microsoft // Reserved for Microsoft backward compatibility
	future    // Reserved for future use
}

// A validated UUID (128 bits).
pub struct Uuid {
pub:
	bytes [16]u8
}

// DNS namespace UUID constant.
pub const uuid_namespace_dns = Uuid{
	bytes: [u8(0x6b), 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
		0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]!
}

// URL namespace UUID constant.
pub const uuid_namespace_url = Uuid{
	bytes: [u8(0x6b), 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
		0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]!
}

// Nil UUID constant (all zeros).
pub const uuid_nil = Uuid{
	bytes: [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]!
}

// Create a UUID from raw bytes.
pub fn uuid_from_bytes(bytes [16]u8) Uuid {
	return Uuid{
		bytes: bytes
	}
}

// Get the UUID version.
pub fn (uuid Uuid) version() UuidVersion {
	version_nibble := (uuid.bytes[6] >> 4) & 0x0F
	return match version_nibble {
		1 { .v1 }
		2 { .v2 }
		3 { .v3 }
		4 { .v4 }
		5 { .v5 }
		else { .nil }
	}
}

// Get the UUID variant.
pub fn (uuid Uuid) variant() UuidVariant {
	variant_byte := uuid.bytes[8]
	if (variant_byte >> 7) == 0 {
		return .ncs
	}
	if (variant_byte >> 6) == 0b10 {
		return .rfc4122
	}
	if (variant_byte >> 5) == 0b110 {
		return .microsoft
	}
	return .future
}

// Check if this is the nil UUID.
pub fn (uuid Uuid) is_nil() bool {
	for b in uuid.bytes {
		if b != 0 {
			return false
		}
	}
	return true
}

// Format UUID as canonical string (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).
pub fn (uuid Uuid) str() string {
	mut builder := strings.new_builder(36)

	// Format: 8-4-4-4-12
	for i, b in uuid.bytes {
		builder.write_string(uuid_hex_byte(b))
		if i == 3 || i == 5 || i == 7 || i == 9 {
			builder.write_u8(`-`)
		}
	}

	return builder.str()
}

// Format UUID as URN (urn:uuid:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).
pub fn (uuid Uuid) to_urn() string {
	return 'urn:uuid:${uuid.str()}'
}

// Compare two UUIDs for equality.
pub fn (uuid Uuid) equals(other Uuid) bool {
	for i in 0 .. 16 {
		if uuid.bytes[i] != other.bytes[i] {
			return false
		}
	}
	return true
}

// Helper: convert byte to hex string.
fn uuid_hex_byte(b u8) string {
	hex_chars := '0123456789abcdef'
	return '${hex_chars[b >> 4]}${hex_chars[b & 0x0F]}'
}

// Parse UUID from canonical string format.
pub fn parse_uuid(input string) ?Uuid {
	if input.len != 36 {
		return none
	}

	// Validate hyphen positions
	if input[8] != `-` || input[13] != `-` || input[18] != `-` || input[23] != `-` {
		return none
	}

	// Extract hex characters only
	hex_str := input.replace('-', '')
	if hex_str.len != 32 {
		return none
	}

	mut bytes := [16]u8{}
	for i := 0; i < 16; i++ {
		high := uuid_hex_char_to_int(hex_str[i * 2]) or { return none }
		low := uuid_hex_char_to_int(hex_str[i * 2 + 1]) or { return none }
		bytes[i] = u8((high << 4) | low)
	}

	return Uuid{
		bytes: bytes
	}
}

// Helper: convert hex character to integer.
fn uuid_hex_char_to_int(c u8) ?int {
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

// Check if string is a valid UUID format.
pub fn is_valid_uuid(input string) bool {
	_ := parse_uuid(input) or { return false }
	return true
}

// Generate a version 4 (random) UUID.
pub fn generate_uuid_v4() Uuid {
	mut bytes := [16]u8{}
	rand.read(mut bytes)

	// Set version to 4
	bytes[6] = (bytes[6] & 0x0F) | 0x40

	// Set variant to RFC 4122
	bytes[8] = (bytes[8] & 0x3F) | 0x80

	return Uuid{
		bytes: bytes
	}
}

// Generate a version 4 UUID from provided random bytes.
pub fn uuid_v4_from_bytes(random_bytes [16]u8) Uuid {
	mut bytes := random_bytes

	// Set version to 4
	bytes[6] = (bytes[6] & 0x0F) | 0x40

	// Set variant to RFC 4122
	bytes[8] = (bytes[8] & 0x3F) | 0x80

	return Uuid{
		bytes: bytes
	}
}

// Format UUID without hyphens.
pub fn (uuid Uuid) to_hex() string {
	mut builder := strings.new_builder(32)
	for b in uuid.bytes {
		builder.write_string(uuid_hex_byte(b))
	}
	return builder.str()
}

// Parse UUID from hex string (no hyphens).
pub fn parse_uuid_hex(input string) ?Uuid {
	if input.len != 32 {
		return none
	}

	mut bytes := [16]u8{}
	for i := 0; i < 16; i++ {
		high := uuid_hex_char_to_int(input[i * 2]) or { return none }
		low := uuid_hex_char_to_int(input[i * 2 + 1]) or { return none }
		bytes[i] = u8((high << 4) | low)
	}

	return Uuid{
		bytes: bytes
	}
}
