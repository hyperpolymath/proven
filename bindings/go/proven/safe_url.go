// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"net/url"
	"strings"
)

// ParsedURL represents the parsed components of a URL.
type ParsedURL struct {
	Scheme   string
	Host     string
	Port     string
	Path     string
	Query    string
	Fragment string
}

// ParseURL parses a URL into its components.
func ParseURL(urlString string) *ParsedURL {
	u, err := url.Parse(urlString)
	if err != nil {
		return nil
	}

	if u.Scheme == "" || u.Host == "" {
		return nil
	}

	host := u.Hostname()
	port := u.Port()
	path := u.Path
	if path == "" {
		path = "/"
	}

	return &ParsedURL{
		Scheme:   strings.ToLower(u.Scheme),
		Host:     host,
		Port:     port,
		Path:     path,
		Query:    u.RawQuery,
		Fragment: u.Fragment,
	}
}

// IsValidURL checks if a string is a valid URL.
func IsValidURL(urlString string) bool {
	return ParseURL(urlString) != nil
}

// GetHost extracts the host from a URL.
func GetHost(urlString string) (string, bool) {
	parsed := ParseURL(urlString)
	if parsed == nil {
		return "", false
	}
	return parsed.Host, true
}

// GetPath extracts the path from a URL.
func GetPath(urlString string) (string, bool) {
	parsed := ParseURL(urlString)
	if parsed == nil {
		return "", false
	}
	return parsed.Path, true
}

// IsHTTPS checks if a URL uses HTTPS.
func IsHTTPS(urlString string) bool {
	parsed := ParseURL(urlString)
	if parsed == nil {
		return false
	}
	return parsed.Scheme == "https"
}

// IsSecure checks if a URL uses a secure scheme (https, wss).
func IsSecure(urlString string) bool {
	parsed := ParseURL(urlString)
	if parsed == nil {
		return false
	}
	return parsed.Scheme == "https" || parsed.Scheme == "wss"
}
