// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"regexp"
	"strings"
	"time"
)

// SameSite represents cookie SameSite attribute.
type SameSite string

const (
	// SameSiteStrict prevents cross-site sending.
	SameSiteStrict SameSite = "Strict"
	// SameSiteLax allows top-level navigation.
	SameSiteLax SameSite = "Lax"
	// SameSiteNone allows cross-site with Secure.
	SameSiteNone SameSite = "None"
)

// Cookie represents an HTTP cookie.
type Cookie struct {
	Name     string
	Value    string
	Domain   string
	Path     string
	Expires  time.Time
	MaxAge   int
	Secure   bool
	HttpOnly bool
	SameSite SameSite
}

// ValidateCookieName checks if name is valid per RFC 6265.
func ValidateCookieName(name string) bool {
	if len(name) == 0 {
		return false
	}

	// Cookie names are tokens (no CTL, separators)
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

// ValidateCookieValue checks if value is valid per RFC 6265.
func ValidateCookieValue(value string) bool {
	// Cookie values: no CTL, whitespace, semicolon, comma
	for _, c := range value {
		if c < 0x21 || c > 0x7E {
			return false
		}
		if c == '"' || c == ',' || c == ';' || c == '\\' {
			return false
		}
	}
	return true
}

// NewCookie creates a validated cookie.
func NewCookie(name, value string) (Cookie, bool) {
	if !ValidateCookieName(name) || !ValidateCookieValue(value) {
		return Cookie{}, false
	}
	return Cookie{
		Name:     name,
		Value:    value,
		Path:     "/",
		SameSite: SameSiteLax,
	}, true
}

// SecureCookie creates a secure cookie with recommended settings.
func SecureCookie(name, value string) (Cookie, bool) {
	cookie, ok := NewCookie(name, value)
	if !ok {
		return Cookie{}, false
	}
	cookie.Secure = true
	cookie.HttpOnly = true
	cookie.SameSite = SameSiteStrict
	return cookie, true
}

// ToString formats cookie as Set-Cookie header value.
func (c Cookie) ToString() string {
	var parts []string
	parts = append(parts, c.Name+"="+c.Value)

	if c.Domain != "" {
		parts = append(parts, "Domain="+c.Domain)
	}
	if c.Path != "" {
		parts = append(parts, "Path="+c.Path)
	}
	if !c.Expires.IsZero() {
		parts = append(parts, "Expires="+c.Expires.UTC().Format(time.RFC1123))
	}
	if c.MaxAge > 0 {
		parts = append(parts, "Max-Age="+string(rune(c.MaxAge)))
	}
	if c.Secure {
		parts = append(parts, "Secure")
	}
	if c.HttpOnly {
		parts = append(parts, "HttpOnly")
	}
	if c.SameSite != "" {
		parts = append(parts, "SameSite="+string(c.SameSite))
	}

	return strings.Join(parts, "; ")
}

// ParseCookies parses a Cookie header value.
func ParseCookies(header string) []Cookie {
	var cookies []Cookie
	re := regexp.MustCompile(`([^=;\s]+)=([^;]*)`)

	for _, match := range re.FindAllStringSubmatch(header, -1) {
		if len(match) >= 3 {
			name := strings.TrimSpace(match[1])
			value := strings.TrimSpace(match[2])

			if ValidateCookieName(name) {
				cookies = append(cookies, Cookie{
					Name:  name,
					Value: value,
				})
			}
		}
	}

	return cookies
}

// HasSecurePrefix checks if cookie uses __Secure- prefix.
func HasSecurePrefix(name string) bool {
	return strings.HasPrefix(name, "__Secure-")
}

// HasHostPrefix checks if cookie uses __Host- prefix.
func HasHostPrefix(name string) bool {
	return strings.HasPrefix(name, "__Host-")
}

// ValidatePrefixedCookie validates cookie prefix requirements.
func ValidatePrefixedCookie(c Cookie) bool {
	if HasSecurePrefix(c.Name) {
		// __Secure- cookies must be Secure
		if !c.Secure {
			return false
		}
	}

	if HasHostPrefix(c.Name) {
		// __Host- cookies must be Secure, no Domain, Path=/
		if !c.Secure || c.Domain != "" || c.Path != "/" {
			return false
		}
	}

	return true
}

// IsSessionCookie checks if cookie is session-only (no expiry).
func (c Cookie) IsSessionCookie() bool {
	return c.Expires.IsZero() && c.MaxAge == 0
}
