// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"strings"
)

// EmailParts represents the parts of an email address.
type EmailParts struct {
	LocalPart string
	Domain    string
}

// IsValidEmail checks if an email address is valid (basic check).
func IsValidEmail(email string) bool {
	parts := strings.Split(email, "@")
	if len(parts) != 2 {
		return false
	}

	localPart, domain := parts[0], parts[1]

	if len(localPart) == 0 {
		return false
	}
	if len(domain) < 3 {
		return false
	}
	if !strings.Contains(domain, ".") {
		return false
	}
	if strings.HasPrefix(domain, ".") {
		return false
	}
	if strings.HasSuffix(domain, ".") {
		return false
	}

	return true
}

// SplitEmail splits an email into local part and domain.
// Returns nil if the email is invalid.
func SplitEmail(email string) *EmailParts {
	if !IsValidEmail(email) {
		return nil
	}

	parts := strings.Split(email, "@")
	return &EmailParts{
		LocalPart: parts[0],
		Domain:    parts[1],
	}
}

// GetDomain extracts the domain from an email address.
func GetDomain(email string) (string, bool) {
	parts := SplitEmail(email)
	if parts == nil {
		return "", false
	}
	return parts.Domain, true
}

// GetLocalPart extracts the local part from an email address.
func GetLocalPart(email string) (string, bool) {
	parts := SplitEmail(email)
	if parts == nil {
		return "", false
	}
	return parts.LocalPart, true
}

// NormalizeEmail normalizes an email address (lowercase domain).
func NormalizeEmail(email string) (string, bool) {
	parts := SplitEmail(email)
	if parts == nil {
		return "", false
	}
	return parts.LocalPart + "@" + strings.ToLower(parts.Domain), true
}
