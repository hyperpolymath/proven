// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"net/url"
	"strings"
)

// EscapeHTML escapes a string for safe HTML insertion.
func EscapeHTML(value string) string {
	replacer := strings.NewReplacer(
		"&", "&amp;",
		"<", "&lt;",
		">", "&gt;",
		"\"", "&quot;",
		"'", "&#x27;",
	)
	return replacer.Replace(value)
}

// EscapeSQL escapes a string for safe SQL interpolation.
// Note: Prefer parameterized queries over string interpolation.
func EscapeSQL(value string) string {
	return strings.ReplaceAll(value, "'", "''")
}

// EscapeJS escapes a string for safe JavaScript string literal insertion.
func EscapeJS(value string) string {
	replacer := strings.NewReplacer(
		"\\", "\\\\",
		"\"", "\\\"",
		"'", "\\'",
		"\n", "\\n",
		"\r", "\\r",
		"\t", "\\t",
	)
	return replacer.Replace(value)
}

// EscapeURL percent-encodes a string for safe URL inclusion.
func EscapeURL(value string) string {
	return url.QueryEscape(value)
}

// TruncateSafe safely truncates a string to a maximum length.
func TruncateSafe(value string, maxLength int, suffix string) string {
	if maxLength < 0 {
		return ""
	}

	runes := []rune(value)
	if len(runes) <= maxLength {
		return value
	}

	suffixRunes := []rune(suffix)
	suffixLen := len(suffixRunes)

	if maxLength <= suffixLen {
		return string(runes[:maxLength])
	}

	return string(runes[:maxLength-suffixLen]) + suffix
}

// TruncateSafeDefault truncates with "..." as the default suffix.
func TruncateSafeDefault(value string, maxLength int) string {
	return TruncateSafe(value, maxLength, "...")
}
