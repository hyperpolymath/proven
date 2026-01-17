// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"

// JSON value types.
JsonType :: enum {
    Null,
    Boolean,
    Number,
    String,
    Array,
    Object,
}

// Validate JSON syntax without parsing.
// Performs structural validation by checking balanced braces/brackets and string escaping.
is_valid_json :: proc(s: string) -> bool {
    depth_brace: int = 0
    depth_bracket: int = 0
    in_string := false
    escape := false

    for c in s {
        if escape {
            escape = false
            continue
        }

        switch c {
        case '\\':
            if in_string {
                escape = true
            }
        case '"':
            in_string = !in_string
        case '{':
            if !in_string {
                depth_brace += 1
            }
        case '}':
            if !in_string {
                depth_brace -= 1
            }
        case '[':
            if !in_string {
                depth_bracket += 1
            }
        case ']':
            if !in_string {
                depth_bracket -= 1
            }
        }

        if depth_brace < 0 || depth_bracket < 0 {
            return false
        }
    }

    return depth_brace == 0 && depth_bracket == 0 && !in_string
}

// Check if a string is a valid JSON number.
is_valid_json_number :: proc(s: string) -> bool {
    if len(s) == 0 {
        return false
    }

    idx := 0

    // Optional minus sign
    if s[idx] == '-' {
        idx += 1
        if idx >= len(s) {
            return false
        }
    }

    // Integer part
    if s[idx] == '0' {
        idx += 1
    } else if s[idx] >= '1' && s[idx] <= '9' {
        for idx < len(s) && s[idx] >= '0' && s[idx] <= '9' {
            idx += 1
        }
    } else {
        return false
    }

    // Fractional part
    if idx < len(s) && s[idx] == '.' {
        idx += 1
        if idx >= len(s) || s[idx] < '0' || s[idx] > '9' {
            return false
        }
        for idx < len(s) && s[idx] >= '0' && s[idx] <= '9' {
            idx += 1
        }
    }

    // Exponent part
    if idx < len(s) && (s[idx] == 'e' || s[idx] == 'E') {
        idx += 1
        if idx < len(s) && (s[idx] == '+' || s[idx] == '-') {
            idx += 1
        }
        if idx >= len(s) || s[idx] < '0' || s[idx] > '9' {
            return false
        }
        for idx < len(s) && s[idx] >= '0' && s[idx] <= '9' {
            idx += 1
        }
    }

    return idx == len(s)
}

// Escape a string for JSON.
json_escape_string :: proc(input: string, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)
    strings.write_byte(&builder, '"')

    for c in input {
        switch c {
        case '"':
            strings.write_string(&builder, "\\\"")
        case '\\':
            strings.write_string(&builder, "\\\\")
        case '\b':
            strings.write_string(&builder, "\\b")
        case '\f':
            strings.write_string(&builder, "\\f")
        case '\n':
            strings.write_string(&builder, "\\n")
        case '\r':
            strings.write_string(&builder, "\\r")
        case '\t':
            strings.write_string(&builder, "\\t")
        case:
            if c < 0x20 {
                // Control character - escape as \uXXXX
                strings.write_string(&builder, "\\u00")
                hex := "0123456789abcdef"
                strings.write_byte(&builder, hex[int(c) >> 4])
                strings.write_byte(&builder, hex[int(c) & 0xF])
            } else {
                strings.write_rune(&builder, c)
            }
        }
    }

    strings.write_byte(&builder, '"')
    return strings.to_string(builder)
}

// Unescape a JSON string.
json_unescape_string :: proc(input: string, allocator := context.allocator) -> (result: string, ok: bool) {
    s := input

    // Remove quotes if present
    if len(s) >= 2 && s[0] == '"' && s[len(s) - 1] == '"' {
        s = s[1:len(s) - 1]
    }

    builder := strings.builder_make(allocator)
    i := 0

    for i < len(s) {
        if s[i] == '\\' {
            if i + 1 >= len(s) {
                return "", false
            }
            i += 1
            switch s[i] {
            case '"':
                strings.write_byte(&builder, '"')
            case '\\':
                strings.write_byte(&builder, '\\')
            case '/':
                strings.write_byte(&builder, '/')
            case 'b':
                strings.write_byte(&builder, '\b')
            case 'f':
                strings.write_byte(&builder, '\f')
            case 'n':
                strings.write_byte(&builder, '\n')
            case 'r':
                strings.write_byte(&builder, '\r')
            case 't':
                strings.write_byte(&builder, '\t')
            case 'u':
                // Unicode escape
                if i + 4 >= len(s) {
                    return "", false
                }
                // Skip for now - would need hex parsing
                strings.write_string(&builder, "?")
                i += 4
            case:
                return "", false
            }
        } else {
            strings.write_byte(&builder, s[i])
        }
        i += 1
    }

    return strings.to_string(builder), true
}

// Check if JSON starts with an object.
json_is_object :: proc(s: string) -> bool {
    trimmed := strings.trim_space(s)
    return len(trimmed) > 0 && trimmed[0] == '{'
}

// Check if JSON starts with an array.
json_is_array :: proc(s: string) -> bool {
    trimmed := strings.trim_space(s)
    return len(trimmed) > 0 && trimmed[0] == '['
}

// Get the depth of nested structures in JSON.
json_max_depth :: proc(s: string) -> int {
    max_depth := 0
    current_depth := 0
    in_string := false
    escape := false

    for c in s {
        if escape {
            escape = false
            continue
        }

        switch c {
        case '\\':
            if in_string {
                escape = true
            }
        case '"':
            in_string = !in_string
        case '{', '[':
            if !in_string {
                current_depth += 1
                if current_depth > max_depth {
                    max_depth = current_depth
                }
            }
        case '}', ']':
            if !in_string {
                current_depth -= 1
            }
        }
    }

    return max_depth
}

// Check if JSON depth exceeds a limit (security check).
json_depth_exceeds :: proc(s: string, max_allowed: int) -> bool {
    return json_max_depth(s) > max_allowed
}

// Minify JSON by removing whitespace outside strings.
json_minify :: proc(s: string, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)
    in_string := false
    escape := false

    for c in s {
        if escape {
            escape = false
            strings.write_rune(&builder, c)
            continue
        }

        switch c {
        case '\\':
            if in_string {
                escape = true
            }
            strings.write_rune(&builder, c)
        case '"':
            in_string = !in_string
            strings.write_rune(&builder, c)
        case ' ', '\t', '\n', '\r':
            if in_string {
                strings.write_rune(&builder, c)
            }
        case:
            strings.write_rune(&builder, c)
        }
    }

    return strings.to_string(builder)
}
