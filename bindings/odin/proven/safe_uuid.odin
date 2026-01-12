// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:crypto"
import "core:strings"
import "core:fmt"

// UUID as a distinct type wrapping a 16-byte array.
// This provides type safety and prevents accidental mixing with raw byte arrays.
UUID :: distinct [16]u8

// UUID version numbers (bits 12-15 of time_hi_and_version).
UUID_Version :: enum u8 {
    Unknown  = 0,
    V1       = 1,  // Time-based
    V2       = 2,  // DCE Security
    V3       = 3,  // Name-based (MD5)
    V4       = 4,  // Random
    V5       = 5,  // Name-based (SHA-1)
    V6       = 6,  // Reordered time-based
    V7       = 7,  // Unix epoch time-based
    V8       = 8,  // Custom
}

// UUID variant (bits 6-7 of clock_seq_hi_and_reserved).
UUID_Variant :: enum u8 {
    NCS       = 0,  // Reserved for NCS backward compatibility
    RFC4122   = 1,  // Standard RFC 4122 variant
    Microsoft = 2,  // Reserved for Microsoft backward compatibility
    Future    = 3,  // Reserved for future definition
}

// Nil UUID (all zeros).
UUID_NIL :: UUID{}

// Max UUID (all ones).
UUID_MAX :: UUID{0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}

// Hex character lookup table.
@(private)
HEX_CHARS :: "0123456789abcdef"

// Parse a UUID from string representation.
// Accepts formats: "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx" (canonical)
// or "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" (without hyphens).
parse_uuid :: proc(input: string) -> (uuid: UUID, ok: bool) {
    s := input

    // Handle optional braces
    if len(s) >= 2 && s[0] == '{' && s[len(s) - 1] == '}' {
        s = s[1:len(s) - 1]
    }

    // Remove optional hyphens and validate length
    hex_only: [32]u8
    hex_idx := 0

    for c in s {
        if c == '-' {
            continue
        }
        if hex_idx >= 32 {
            return {}, false
        }
        hex_only[hex_idx] = u8(c)
        hex_idx += 1
    }

    if hex_idx != 32 {
        return {}, false
    }

    // Parse hex pairs into bytes
    result: UUID
    for i := 0; i < 16; i += 1 {
        high := hex_digit_value(hex_only[i * 2])
        low := hex_digit_value(hex_only[i * 2 + 1])
        if high < 0 || low < 0 {
            return {}, false
        }
        result[i] = u8(high << 4 | low)
    }

    return result, true
}

// Convert a hex character to its numeric value.
@(private)
hex_digit_value :: proc(c: u8) -> int {
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

// Format a UUID as a canonical string (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).
format_uuid :: proc(uuid: UUID, allocator := context.allocator) -> string {
    builder := strings.builder_make_len_cap(0, 36, allocator)

    // time_low (4 bytes)
    for i := 0; i < 4; i += 1 {
        strings.write_byte(&builder, HEX_CHARS[uuid[i] >> 4])
        strings.write_byte(&builder, HEX_CHARS[uuid[i] & 0x0F])
    }
    strings.write_byte(&builder, '-')

    // time_mid (2 bytes)
    for i := 4; i < 6; i += 1 {
        strings.write_byte(&builder, HEX_CHARS[uuid[i] >> 4])
        strings.write_byte(&builder, HEX_CHARS[uuid[i] & 0x0F])
    }
    strings.write_byte(&builder, '-')

    // time_hi_and_version (2 bytes)
    for i := 6; i < 8; i += 1 {
        strings.write_byte(&builder, HEX_CHARS[uuid[i] >> 4])
        strings.write_byte(&builder, HEX_CHARS[uuid[i] & 0x0F])
    }
    strings.write_byte(&builder, '-')

    // clock_seq_hi_and_reserved + clock_seq_low (2 bytes)
    for i := 8; i < 10; i += 1 {
        strings.write_byte(&builder, HEX_CHARS[uuid[i] >> 4])
        strings.write_byte(&builder, HEX_CHARS[uuid[i] & 0x0F])
    }
    strings.write_byte(&builder, '-')

    // node (6 bytes)
    for i := 10; i < 16; i += 1 {
        strings.write_byte(&builder, HEX_CHARS[uuid[i] >> 4])
        strings.write_byte(&builder, HEX_CHARS[uuid[i] & 0x0F])
    }

    return strings.to_string(builder)
}

// Format a UUID without hyphens.
format_uuid_compact :: proc(uuid: UUID, allocator := context.allocator) -> string {
    builder := strings.builder_make_len_cap(0, 32, allocator)

    for i := 0; i < 16; i += 1 {
        strings.write_byte(&builder, HEX_CHARS[uuid[i] >> 4])
        strings.write_byte(&builder, HEX_CHARS[uuid[i] & 0x0F])
    }

    return strings.to_string(builder)
}

// Format a UUID with braces (Microsoft style).
format_uuid_braced :: proc(uuid: UUID, allocator := context.allocator) -> string {
    canonical := format_uuid(uuid, allocator)
    return strings.concatenate({"{", canonical, "}"}, allocator)
}

// Generate a random UUID (version 4).
generate_uuid_v4 :: proc() -> UUID {
    uuid: UUID
    crypto.random_bytes(uuid[:])

    // Set version to 4 (bits 12-15 of time_hi_and_version)
    uuid[6] = (uuid[6] & 0x0F) | 0x40

    // Set variant to RFC 4122 (bits 6-7 of clock_seq_hi_and_reserved)
    uuid[8] = (uuid[8] & 0x3F) | 0x80

    return uuid
}

// Get the version of a UUID.
get_uuid_version :: proc(uuid: UUID) -> UUID_Version {
    version := (uuid[6] & 0xF0) >> 4
    if version >= 1 && version <= 8 {
        return UUID_Version(version)
    }
    return .Unknown
}

// Get the variant of a UUID.
get_uuid_variant :: proc(uuid: UUID) -> UUID_Variant {
    variant_bits := uuid[8]

    if (variant_bits & 0x80) == 0 {
        return .NCS
    }
    if (variant_bits & 0xC0) == 0x80 {
        return .RFC4122
    }
    if (variant_bits & 0xE0) == 0xC0 {
        return .Microsoft
    }
    return .Future
}

// Check if a UUID is nil (all zeros).
is_uuid_nil :: proc(uuid: UUID) -> bool {
    for b in uuid {
        if b != 0 {
            return false
        }
    }
    return true
}

// Check if a UUID is max (all ones).
is_uuid_max :: proc(uuid: UUID) -> bool {
    for b in uuid {
        if b != 0xFF {
            return false
        }
    }
    return true
}

// Check if a UUID is valid (RFC 4122 variant).
is_uuid_valid :: proc(uuid: UUID) -> bool {
    if is_uuid_nil(uuid) {
        return true  // Nil UUID is valid
    }
    return get_uuid_variant(uuid) == .RFC4122
}

// Compare two UUIDs.
// Returns -1 if a < b, 0 if a == b, 1 if a > b.
compare_uuid :: proc(a, b: UUID) -> int {
    for i := 0; i < 16; i += 1 {
        if a[i] < b[i] {
            return -1
        }
        if a[i] > b[i] {
            return 1
        }
    }
    return 0
}

// Check if two UUIDs are equal.
uuid_equals :: proc(a, b: UUID) -> bool {
    return compare_uuid(a, b) == 0
}

// Convert UUID to raw bytes.
uuid_to_bytes :: proc(uuid: UUID) -> [16]u8 {
    return [16]u8(uuid)
}

// Create UUID from raw bytes.
uuid_from_bytes :: proc(bytes: [16]u8) -> UUID {
    return UUID(bytes)
}

// Create UUID from byte slice (must be exactly 16 bytes).
uuid_from_slice :: proc(bytes: []u8) -> (uuid: UUID, ok: bool) {
    if len(bytes) != 16 {
        return {}, false
    }
    result: UUID
    for i := 0; i < 16; i += 1 {
        result[i] = bytes[i]
    }
    return result, true
}

// Validate a UUID string without parsing.
is_valid_uuid_string :: proc(input: string) -> bool {
    s := input

    // Handle optional braces
    if len(s) >= 2 && s[0] == '{' && s[len(s) - 1] == '}' {
        s = s[1:len(s) - 1]
    }

    // Check for canonical format with hyphens
    if len(s) == 36 {
        // Check hyphen positions: 8, 13, 18, 23
        if s[8] != '-' || s[13] != '-' || s[18] != '-' || s[23] != '-' {
            return false
        }
        // Validate hex characters
        for i := 0; i < 36; i += 1 {
            if i == 8 || i == 13 || i == 18 || i == 23 {
                continue
            }
            c := s[i]
            if !((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
                return false
            }
        }
        return true
    }

    // Check for compact format without hyphens
    if len(s) == 32 {
        for c in s {
            if !((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
                return false
            }
        }
        return true
    }

    return false
}
