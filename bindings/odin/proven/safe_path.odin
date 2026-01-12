// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"
import "core:path/filepath"

// Result type for path operations.
PathResult :: struct {
    path:  string,
    error: string,
    ok:    bool,
}

// Create a successful PathResult.
path_ok :: proc(path: string) -> PathResult {
    return PathResult{path = path, error = "", ok = true}
}

// Create an error PathResult.
path_error :: proc(message: string) -> PathResult {
    return PathResult{path = "", error = message, ok = false}
}

// Dangerous path patterns.
TRAVERSAL_PATTERNS :: []string{"..", "./", ".\\", "%2e%2e", "%2e.", ".%2e", "%00"}

// Check if path contains traversal sequences.
has_traversal :: proc(path: string) -> bool {
    normalized := strings.to_lower(path)
    for pattern in TRAVERSAL_PATTERNS {
        if strings.contains(normalized, pattern) {
            return true
        }
    }
    return false
}

// Sanitize a filename.
sanitize_filename :: proc(filename: string, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)

    skip_leading_dots := true
    for c in filename {
        switch c {
        case '/', '\\':
            strings.write_rune(&builder, '_')
            skip_leading_dots = false
        case '.':
            if !skip_leading_dots {
                strings.write_rune(&builder, c)
            }
        case '<', '>', ':', '"', '|', '?', '*':
            strings.write_rune(&builder, '_')
            skip_leading_dots = false
        case 0:
            // Skip null bytes
        case:
            strings.write_rune(&builder, c)
            skip_leading_dots = false
        }
    }

    result := strings.to_string(builder)

    // Collapse multiple underscores
    for strings.contains(result, "__") {
        result = strings.replace_all(result, "__", "_", allocator)
    }

    // Trim underscores
    result = strings.trim(result, "_")

    if len(result) == 0 {
        return "unnamed"
    }

    return result
}

// Join paths safely.
path_join :: proc(base: string, components: ..string) -> PathResult {
    for component in components {
        if has_traversal(component) {
            return path_error(strings.concatenate({"Path traversal detected in component: ", component}))
        }
    }

    result := base
    for component in components {
        // Remove leading slashes
        clean := strings.trim_left(component, "/\\")
        result = filepath.join({result, clean})
    }

    return path_ok(result)
}

// Get file extension safely.
get_extension :: proc(path: string) -> (ext: string, ok: bool) {
    if len(path) == 0 {
        return "", false
    }

    base := filepath.base(path)
    if len(base) == 0 || base[0] == '.' {
        return "", false
    }

    e := filepath.ext(path)
    if len(e) == 0 {
        return "", false
    }

    return e, true
}

// Check if extension is allowed.
extension_allowed :: proc(path: string, allowed: []string) -> bool {
    ext, ok := get_extension(path)
    if !ok {
        return false
    }

    ext_lower := strings.to_lower(ext)
    for a in allowed {
        if strings.to_lower(a) == ext_lower {
            return true
        }
    }
    return false
}

// Normalize path separators to forward slash.
normalize_separators :: proc(path: string, allocator := context.allocator) -> string {
    return strings.replace_all(path, "\\", "/", allocator)
}

// Check if path is absolute.
is_absolute :: proc(path: string) -> bool {
    if len(path) == 0 {
        return false
    }
    return path[0] == '/' || (len(path) >= 2 && path[1] == ':')
}

// Check if path is relative.
is_relative :: proc(path: string) -> bool {
    return !is_absolute(path)
}

// Get parent directory.
get_parent :: proc(path: string) -> (parent: string, ok: bool) {
    if len(path) == 0 {
        return "", false
    }
    return filepath.dir(path), true
}

// Get filename from path.
get_filename :: proc(path: string) -> string {
    return filepath.base(path)
}

// Check if filename is hidden (starts with dot).
is_hidden :: proc(path: string) -> bool {
    base := filepath.base(path)
    return len(base) > 0 && base[0] == '.'
}
