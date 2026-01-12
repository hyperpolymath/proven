// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"
import "core:unicode/utf8"
import "core:encoding/base64"
import "core:fmt"

// Safe string operations for XSS prevention and sanitization.

// Escape HTML special characters.
escape_html :: proc(input: string, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)

    for c in input {
        switch c {
        case '&':
            strings.write_string(&builder, "&amp;")
        case '<':
            strings.write_string(&builder, "&lt;")
        case '>':
            strings.write_string(&builder, "&gt;")
        case '"':
            strings.write_string(&builder, "&quot;")
        case '\'':
            strings.write_string(&builder, "&#x27;")
        case:
            strings.write_rune(&builder, c)
        }
    }

    return strings.to_string(builder)
}

// Escape for SQL (single quotes).
escape_sql :: proc(input: string, allocator := context.allocator) -> string {
    return strings.replace_all(input, "'", "''", allocator)
}

// Escape for JavaScript strings.
escape_js :: proc(input: string, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)

    for c in input {
        switch c {
        case '\\':
            strings.write_string(&builder, "\\\\")
        case '"':
            strings.write_string(&builder, "\\\"")
        case '\'':
            strings.write_string(&builder, "\\'")
        case '\n':
            strings.write_string(&builder, "\\n")
        case '\r':
            strings.write_string(&builder, "\\r")
        case '\t':
            strings.write_string(&builder, "\\t")
        case '<':
            strings.write_string(&builder, "\\x3c")
        case '>':
            strings.write_string(&builder, "\\x3e")
        case:
            strings.write_rune(&builder, c)
        }
    }

    return strings.to_string(builder)
}

// URL encode a string.
url_encode :: proc(input: string, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)

    for b in transmute([]u8)input {
        if is_url_safe(b) {
            strings.write_byte(&builder, b)
        } else {
            fmt.sbprintf(&builder, "%%%02X", b)
        }
    }

    return strings.to_string(builder)
}

// Check if character is URL-safe.
is_url_safe :: proc(c: u8) -> bool {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.' || c == '~'
}

// Sanitize string to only allow safe characters.
sanitize :: proc(input: string, allowed: string, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)

    for c in input {
        if strings.contains_rune(allowed, c) {
            strings.write_rune(&builder, c)
        }
    }

    return strings.to_string(builder)
}

// Default sanitization.
sanitize_default :: proc(input: string, allocator := context.allocator) -> string {
    return sanitize(input, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-", allocator)
}

// Convert to slug.
slugify :: proc(input: string, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)

    for c in input {
        lower := c >= 'A' && c <= 'Z' ? rune(c - 'A' + 'a') : c
        if (lower >= 'a' && lower <= 'z') || (lower >= '0' && lower <= '9') {
            strings.write_rune(&builder, lower)
        } else if lower == ' ' || lower == '-' {
            strings.write_rune(&builder, '-')
        }
    }

    result := strings.to_string(builder)

    // Collapse multiple hyphens
    for strings.contains(result, "--") {
        result = strings.replace_all(result, "--", "-", allocator)
    }

    // Trim hyphens
    result = strings.trim(result, "-")

    return result
}

// Truncate string safely.
truncate :: proc(input: string, max_length: int, suffix := "...") -> string {
    if len(input) <= max_length {
        return input
    }
    if max_length <= len(suffix) {
        return suffix
    }
    return strings.concatenate({input[:max_length - len(suffix)], suffix})
}

// Check if string contains only ASCII.
is_ascii_only :: proc(input: string) -> bool {
    for b in transmute([]u8)input {
        if b >= 128 {
            return false
        }
    }
    return true
}

// Check if string contains only printable ASCII.
is_printable_ascii :: proc(input: string) -> bool {
    for b in transmute([]u8)input {
        if b < 32 || b >= 127 {
            return false
        }
    }
    return true
}

// Base64 encode.
base64_encode :: proc(input: string, allocator := context.allocator) -> string {
    return base64.encode(transmute([]u8)input, allocator)
}

// Base64 decode.
base64_decode :: proc(input: string, allocator := context.allocator) -> (result: string, ok: bool) {
    decoded, err := base64.decode(input, allocator)
    if err != nil {
        return "", false
    }
    return string(decoded), true
}

// Hex encode bytes.
hex_encode :: proc(input: []u8, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)
    for b in input {
        fmt.sbprintf(&builder, "%02x", b)
    }
    return strings.to_string(builder)
}
