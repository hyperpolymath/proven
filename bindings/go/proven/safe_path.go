// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"path/filepath"
	"strings"
)

// HasTraversal checks if a path contains directory traversal sequences.
func HasTraversal(path string) bool {
	return strings.Contains(path, "..") || strings.Contains(path, "~")
}

// IsSafe checks if a path is safe (no traversal attacks).
func IsSafe(path string) bool {
	return !HasTraversal(path)
}

// SanitizeFilename sanitizes a filename by removing dangerous characters.
func SanitizeFilename(filename string) string {
	replacer := strings.NewReplacer(
		"..", "_",
		"/", "_",
		"\\", "_",
		"<", "_",
		">", "_",
		":", "_",
		"\"", "_",
		"|", "_",
		"?", "_",
		"*", "_",
		"\x00", "_",
	)
	return replacer.Replace(filename)
}

// SafeJoin safely joins path components, rejecting traversal attempts.
// Returns empty string and ok=false if any part contains traversal sequences.
func SafeJoin(base string, parts ...string) (string, bool) {
	for _, part := range parts {
		if HasTraversal(part) {
			return "", false
		}
	}

	sanitized := make([]string, len(parts))
	for i, part := range parts {
		sanitized[i] = SanitizeFilename(part)
	}

	result := base
	for _, part := range sanitized {
		result = filepath.Join(result, part)
	}

	return result, true
}

// SafeJoinSlice is like SafeJoin but takes a slice instead of variadic args.
func SafeJoinSlice(base string, parts []string) (string, bool) {
	return SafeJoin(base, parts...)
}
