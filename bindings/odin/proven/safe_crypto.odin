// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:crypto/hash"
import "core:encoding/base64"
import "core:crypto"
import "core:strings"
import "core:fmt"

// Constant-time byte comparison to prevent timing attacks.
constant_time_equals :: proc(a, b: []u8) -> bool {
    if len(a) != len(b) {
        return false
    }

    result: u8 = 0
    for i := 0; i < len(a); i += 1 {
        result |= a[i] ~ b[i]
    }
    return result == 0
}

// Constant-time string comparison.
constant_time_equals_string :: proc(a, b: string) -> bool {
    return constant_time_equals(transmute([]u8)a, transmute([]u8)b)
}

// Generate cryptographically secure random bytes.
random_bytes :: proc(count: int, allocator := context.allocator) -> []u8 {
    bytes := make([]u8, count, allocator)
    crypto.random_bytes(bytes)
    return bytes
}

// Convert bytes to hex string.
bytes_to_hex :: proc(bytes: []u8, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)
    for b in bytes {
        fmt.sbprintf(&builder, "%02x", b)
    }
    return strings.to_string(builder)
}

// Generate random bytes as hex string.
random_hex :: proc(byte_count: int, allocator := context.allocator) -> string {
    bytes := random_bytes(byte_count, allocator)
    defer delete(bytes)
    return bytes_to_hex(bytes, allocator)
}

// Generate random bytes as base64 string.
random_base64 :: proc(byte_count: int, allocator := context.allocator) -> string {
    bytes := random_bytes(byte_count, allocator)
    defer delete(bytes)
    return base64.encode(bytes, allocator)
}

// Generate URL-safe random string.
random_url_safe :: proc(byte_count: int, allocator := context.allocator) -> string {
    bytes := random_bytes(byte_count, allocator)
    defer delete(bytes)

    encoded := base64.encode(bytes, allocator)
    result := strings.replace_all(encoded, "+", "-", allocator)
    result = strings.replace_all(result, "/", "_", allocator)
    result = strings.trim_right(result, "=")
    return result
}

// Generate random integer in range [min, max].
random_int :: proc(min_val, max_val: int) -> int {
    lo, hi := min_val <= max_val ? min_val : max_val, min_val <= max_val ? max_val : min_val

    if lo == hi {
        return lo
    }

    bytes := random_bytes(4)
    defer delete(bytes)

    value := u32(bytes[0]) | (u32(bytes[1]) << 8) | (u32(bytes[2]) << 16) | (u32(bytes[3]) << 24)
    range_size := u32(hi - lo + 1)
    return lo + int(value % range_size)
}

// Generate a secure token.
generate_token :: proc(length := 32, allocator := context.allocator) -> string {
    return random_url_safe(length, allocator)
}

// Generate token with default length.
generate_token_default :: proc(allocator := context.allocator) -> string {
    return generate_token(32, allocator)
}

// Hash a string with SHA-256.
sha256_hash :: proc(input: string, allocator := context.allocator) -> string {
    h: hash.Hash
    hash.init(&h, .SHA256)
    hash.update(&h, transmute([]u8)input)
    digest := hash.final(&h)
    return bytes_to_hex(digest[:], allocator)
}

// Hash bytes with SHA-256.
sha256_hash_bytes :: proc(input: []u8, allocator := context.allocator) -> string {
    h: hash.Hash
    hash.init(&h, .SHA256)
    hash.update(&h, input)
    digest := hash.final(&h)
    return bytes_to_hex(digest[:], allocator)
}

// Hash a string with SHA-512.
sha512_hash :: proc(input: string, allocator := context.allocator) -> string {
    h: hash.Hash
    hash.init(&h, .SHA512)
    hash.update(&h, transmute([]u8)input)
    digest := hash.final(&h)
    return bytes_to_hex(digest[:], allocator)
}

// Hash bytes with SHA-512.
sha512_hash_bytes :: proc(input: []u8, allocator := context.allocator) -> string {
    h: hash.Hash
    hash.init(&h, .SHA512)
    hash.update(&h, input)
    digest := hash.final(&h)
    return bytes_to_hex(digest[:], allocator)
}

// Hash a string with MD5 (NOT for security, only for checksums).
md5_hash :: proc(input: string, allocator := context.allocator) -> string {
    h: hash.Hash
    hash.init(&h, .MD5)
    hash.update(&h, transmute([]u8)input)
    digest := hash.final(&h)
    return bytes_to_hex(digest[:], allocator)
}

// Generate a random password.
generate_password :: proc(
    length: int,
    include_uppercase := true,
    include_lowercase := true,
    include_numbers := true,
    include_symbols := true,
    allocator := context.allocator,
) -> string {
    chars: string = ""
    if include_lowercase {
        chars = strings.concatenate({chars, "abcdefghijklmnopqrstuvwxyz"})
    }
    if include_uppercase {
        chars = strings.concatenate({chars, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"})
    }
    if include_numbers {
        chars = strings.concatenate({chars, "0123456789"})
    }
    if include_symbols {
        chars = strings.concatenate({chars, "!@#$%^&*()_+-=[]{}|;:,.<>?"})
    }

    if len(chars) == 0 {
        return ""
    }

    bytes := random_bytes(length, allocator)
    defer delete(bytes)

    builder := strings.builder_make(allocator)
    for b in bytes {
        idx := int(b) % len(chars)
        strings.write_byte(&builder, chars[idx])
    }
    return strings.to_string(builder)
}

// Generate password with defaults.
generate_password_default :: proc(allocator := context.allocator) -> string {
    return generate_password(16, true, true, true, true, allocator)
}

// Securely wipe a byte array (best effort).
secure_wipe :: proc(data: []u8) {
    for i := 0; i < len(data); i += 1 {
        data[i] = 0
    }
}
