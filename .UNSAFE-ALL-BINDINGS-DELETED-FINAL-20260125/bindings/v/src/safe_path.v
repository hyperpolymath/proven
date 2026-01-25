// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

module proven

import os
import strings

// Result type for path operations.
pub struct PathResult {
pub:
	path  string
	error string
	ok    bool
}

// Create a successful PathResult.
pub fn path_ok(path string) PathResult {
	return PathResult{
		path: path
		error: ''
		ok: true
	}
}

// Create an error PathResult.
pub fn path_error(message string) PathResult {
	return PathResult{
		path: ''
		error: message
		ok: false
	}
}

// Dangerous path patterns.
const traversal_patterns = ['..', './', '.\\', '%2e%2e', '%2e.', '.%2e', '%00']

// Check if path contains traversal sequences.
pub fn has_traversal(path string) bool {
	normalized := path.to_lower()
	for pattern in proven.traversal_patterns {
		if normalized.contains(pattern) {
			return true
		}
	}
	return false
}

// Sanitize a filename.
pub fn sanitize_filename(filename string) string {
	mut result := filename

	// Remove directory separators
	result = result.replace('/', '_')
	result = result.replace('\\', '_')

	// Remove null bytes
	result = result.replace('\x00', '')

	// Remove leading dots
	for result.len > 0 && result[0] == `.` {
		result = result[1..]
	}

	// Replace dangerous chars
	for c in ['<', '>', ':', '"', '|', '?', '*'] {
		result = result.replace(c, '_')
	}

	// Collapse multiple underscores
	for result.contains('__') {
		result = result.replace('__', '_')
	}

	// Trim underscores
	result = result.trim('_')

	// Return safe default if empty
	if result.len == 0 {
		return 'unnamed'
	}
	return result
}

// Join paths safely.
pub fn path_join(base string, components ...string) PathResult {
	for component in components {
		if has_traversal(component) {
			return path_error('Path traversal detected in component: ${component}')
		}
	}

	mut result := base
	for component in components {
		// Remove leading slashes
		mut clean := component.trim_left('/')
		clean = clean.trim_left('\\')
		result = os.join_path(result, clean)
	}

	return path_ok(result)
}

// Resolve path within base directory.
pub fn resolve_within(base string, path string) PathResult {
	if has_traversal(path) {
		return path_error('Path traversal detected')
	}

	// Get absolute paths
	abs_base := os.real_path(base)

	// Clean the path
	mut clean_path := path.trim_left('/')
	clean_path = clean_path.trim_left('\\')
	full_path := os.real_path(os.join_path(abs_base, clean_path))

	// Verify it's within base
	if !full_path.starts_with(abs_base) {
		return path_error('Path escapes base directory')
	}

	return path_ok(full_path)
}

// Get file extension safely.
pub fn get_extension(path string) ?string {
	if path.len == 0 {
		return none
	}

	basename := os.base(path)
	if basename.len == 0 || basename[0] == `.` {
		return none
	}

	dot_index := basename.last_index('.') or { return none }
	if dot_index == 0 {
		return none
	}

	return basename[dot_index..]
}

// Check if extension is allowed.
pub fn extension_allowed(path string, allowed []string) bool {
	ext := get_extension(path) or { return false }
	ext_lower := ext.to_lower()
	for a in allowed {
		if a.to_lower() == ext_lower {
			return true
		}
	}
	return false
}

// Normalize path separators to forward slash.
pub fn normalize_separators(path string) string {
	return path.replace('\\', '/')
}

// Check if path is absolute.
pub fn is_absolute(path string) bool {
	if path.len == 0 {
		return false
	}
	return path[0] == `/` || (path.len >= 2 && path[1] == `:`)
}

// Check if path is relative.
pub fn is_relative(path string) bool {
	return !is_absolute(path)
}

// Get parent directory.
pub fn parent(path string) ?string {
	if path.len == 0 {
		return none
	}
	return os.dir(path)
}

// Get filename from path.
pub fn filename(path string) string {
	return os.base(path)
}

// Check if filename is hidden (starts with dot).
pub fn is_hidden(path string) bool {
	basename := os.base(path)
	return basename.len > 0 && basename[0] == `.`
}
