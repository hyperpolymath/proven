// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"regexp"
	"strings"
)

// HeaderName represents a validated HTTP header name.
type HeaderName struct {
	name string
}

// NewHeaderName creates a validated header name.
func NewHeaderName(name string) (HeaderName, bool) {
	if !isValidHeaderName(name) {
		return HeaderName{}, false
	}
	return HeaderName{name: name}, true
}

// String returns the header name.
func (h HeaderName) String() string {
	return h.name
}

// Canonical returns canonical header name (Title-Case).
func (h HeaderName) Canonical() string {
	parts := strings.Split(h.name, "-")
	for i, part := range parts {
		if len(part) > 0 {
			parts[i] = strings.ToUpper(part[:1]) + strings.ToLower(part[1:])
		}
	}
	return strings.Join(parts, "-")
}

func isValidHeaderName(name string) bool {
	if len(name) == 0 {
		return false
	}
	// Header names: token chars (no CTL, separators)
	for _, c := range name {
		if c < 33 || c > 126 {
			return false
		}
		if strings.ContainsRune("()<>@,;:\\\"/[]?={} \t", c) {
			return false
		}
	}
	return true
}

// HeaderValue represents a validated HTTP header value.
type HeaderValue struct {
	value string
}

// NewHeaderValue creates a validated header value (prevents CRLF injection).
func NewHeaderValue(value string) (HeaderValue, bool) {
	if !isValidHeaderValue(value) {
		return HeaderValue{}, false
	}
	return HeaderValue{value: value}, true
}

// String returns the header value.
func (h HeaderValue) String() string {
	return h.value
}

func isValidHeaderValue(value string) bool {
	// Reject CRLF injection attempts
	if strings.Contains(value, "\r") || strings.Contains(value, "\n") {
		return false
	}
	// Reject null bytes
	if strings.Contains(value, "\x00") {
		return false
	}
	return true
}

// SanitizeHeaderValue removes dangerous characters from header value.
func SanitizeHeaderValue(value string) string {
	// Remove CR, LF, and null bytes
	result := strings.ReplaceAll(value, "\r", "")
	result = strings.ReplaceAll(result, "\n", "")
	result = strings.ReplaceAll(result, "\x00", "")
	return result
}

// SecurityHeader represents common security headers.
type SecurityHeader string

const (
	// ContentSecurityPolicy header name.
	ContentSecurityPolicy SecurityHeader = "Content-Security-Policy"
	// XContentTypeOptions header name.
	XContentTypeOptions SecurityHeader = "X-Content-Type-Options"
	// XFrameOptions header name.
	XFrameOptions SecurityHeader = "X-Frame-Options"
	// XSSProtection header name.
	XSSProtection SecurityHeader = "X-XSS-Protection"
	// StrictTransportSecurity header name.
	StrictTransportSecurity SecurityHeader = "Strict-Transport-Security"
	// ReferrerPolicy header name.
	ReferrerPolicy SecurityHeader = "Referrer-Policy"
	// PermissionsPolicy header name.
	PermissionsPolicy SecurityHeader = "Permissions-Policy"
)

// IsSecurityHeader checks if name is a security-related header.
func IsSecurityHeader(name string) bool {
	securityHeaders := []string{
		string(ContentSecurityPolicy),
		string(XContentTypeOptions),
		string(XFrameOptions),
		string(XSSProtection),
		string(StrictTransportSecurity),
		string(ReferrerPolicy),
		string(PermissionsPolicy),
	}

	normalized := strings.ToLower(name)
	for _, h := range securityHeaders {
		if strings.ToLower(h) == normalized {
			return true
		}
	}
	return false
}

// DefaultSecurityHeaders returns recommended security headers.
func DefaultSecurityHeaders() map[string]string {
	return map[string]string{
		string(XContentTypeOptions):     "nosniff",
		string(XFrameOptions):           "DENY",
		string(XSSProtection):           "1; mode=block",
		string(ReferrerPolicy):          "strict-origin-when-cross-origin",
	}
}

// ParseCacheControl parses Cache-Control header directives.
func ParseCacheControl(value string) map[string]string {
	result := make(map[string]string)
	re := regexp.MustCompile(`([a-z-]+)(?:=([^,\s]+))?`)

	for _, match := range re.FindAllStringSubmatch(strings.ToLower(value), -1) {
		if len(match) >= 2 {
			key := match[1]
			val := ""
			if len(match) >= 3 {
				val = strings.Trim(match[2], "\"")
			}
			result[key] = val
		}
	}

	return result
}

// IsNoCache checks if Cache-Control indicates no caching.
func IsNoCache(directives map[string]string) bool {
	_, hasNoCache := directives["no-cache"]
	_, hasNoStore := directives["no-store"]
	return hasNoCache || hasNoStore
}
