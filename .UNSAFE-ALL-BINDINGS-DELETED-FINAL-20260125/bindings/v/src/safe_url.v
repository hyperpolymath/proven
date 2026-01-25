// SPDX-License-Identifier: PMPL-1.0
// Safe URL validation and manipulation for V.
//
// Provides URL parsing, validation, and safe construction
// with protection against malformed URLs and injection attacks.

module proven

// URL scheme types
pub enum UrlScheme {
	http
	https
	ftp
	file
	data
	mailto
	tel
	other
}

// Parsed URL components
pub struct SafeUrl {
pub:
	scheme   string
	username string
	password string
	host     string
	port     ?u16
	path     string
	query    string
	fragment string
}

// Parse a URL string into components
pub fn parse_url(url string) ?SafeUrl {
	if url.len == 0 || url.len > 2048 {
		return none
	}

	mut working := url
	mut result := SafeUrl{}

	// Extract fragment
	if idx := working.index('#') {
		result = SafeUrl{
			...result
			fragment: working[idx + 1..]
		}
		working = working[..idx]
	}

	// Extract query string
	if idx := working.index('?') {
		result = SafeUrl{
			...result
			query: working[idx + 1..]
		}
		working = working[..idx]
	}

	// Extract scheme
	if idx := working.index('://') {
		result = SafeUrl{
			...result
			scheme: working[..idx].to_lower()
		}
		working = working[idx + 3..]
	} else {
		return none
	}

	// Extract userinfo if present
	if at_idx := working.index('@') {
		userinfo := working[..at_idx]
		if colon_idx := userinfo.index(':') {
			result = SafeUrl{
				...result
				username: userinfo[..colon_idx]
				password: userinfo[colon_idx + 1..]
			}
		} else {
			result = SafeUrl{
				...result
				username: userinfo
			}
		}
		working = working[at_idx + 1..]
	}

	// Extract path
	if slash_idx := working.index('/') {
		result = SafeUrl{
			...result
			path: working[slash_idx..]
		}
		working = working[..slash_idx]
	}

	// Extract port
	if colon_idx := working.last_index(':') {
		port_str := working[colon_idx + 1..]
		if port_str.len > 0 && is_numeric(port_str) {
			if port := port_str.u16() {
				result = SafeUrl{
					...result
					host: working[..colon_idx]
					port: port
				}
			} else {
				result = SafeUrl{
					...result
					host: working
				}
			}
		} else {
			result = SafeUrl{
				...result
				host: working
			}
		}
	} else {
		result = SafeUrl{
			...result
			host: working
		}
	}

	return result
}

// Check if string contains only numeric characters
fn is_numeric(s string) bool {
	for c in s {
		if c < `0` || c > `9` {
			return false
		}
	}
	return s.len > 0
}

// Validate URL structure
pub fn is_valid_url(url string) bool {
	return parse_url(url) != none
}

// Get scheme type from URL
pub fn get_scheme_type(url string) UrlScheme {
	parsed := parse_url(url) or { return .other }

	return match parsed.scheme {
		'http' { UrlScheme.http }
		'https' { UrlScheme.https }
		'ftp' { UrlScheme.ftp }
		'file' { UrlScheme.file }
		'data' { UrlScheme.data }
		'mailto' { UrlScheme.mailto }
		'tel' { UrlScheme.tel }
		else { UrlScheme.other }
	}
}

// Check if URL uses HTTPS
pub fn is_https(url string) bool {
	parsed := parse_url(url) or { return false }
	return parsed.scheme == 'https'
}

// Check if URL uses a secure scheme
pub fn is_secure_scheme(url string) bool {
	parsed := parse_url(url) or { return false }
	return parsed.scheme == 'https' || parsed.scheme == 'wss'
}

// Extract host from URL
pub fn extract_host(url string) ?string {
	parsed := parse_url(url)?
	if parsed.host.len == 0 {
		return none
	}
	return parsed.host
}

// Extract path from URL
pub fn extract_path(url string) string {
	parsed := parse_url(url) or { return '/' }
	if parsed.path.len == 0 {
		return '/'
	}
	return parsed.path
}

// Build URL from components
pub fn build_url(scheme string, host string, path string, query string) ?string {
	if scheme.len == 0 || host.len == 0 {
		return none
	}

	// Validate no injection characters
	if has_crlf_chars(scheme) || has_crlf_chars(host) || has_crlf_chars(path) || has_crlf_chars(query) {
		return none
	}

	mut result := '${scheme}://${host}'

	if path.len > 0 {
		if !path.starts_with('/') {
			result += '/'
		}
		result += path
	}

	if query.len > 0 {
		result += '?${query}'
	}

	return result
}

// Check for CRLF injection characters
fn has_crlf_chars(s string) bool {
	return s.contains('\r') || s.contains('\n')
}

// URL encode a string
pub fn url_encode(s string) string {
	mut result := []u8{}
	for c in s {
		if is_unreserved_char(c) {
			result << c
		} else {
			result << `%`
			hex := c.hex()
			if hex.len == 1 {
				result << `0`
			}
			for h in hex.to_upper() {
				result << h
			}
		}
	}
	return result.bytestr()
}

// Check if character is unreserved per RFC 3986
fn is_unreserved_char(c u8) bool {
	return (c >= `A` && c <= `Z`) || (c >= `a` && c <= `z`) || (c >= `0` && c <= `9`)
		|| c == `-` || c == `.` || c == `_` || c == `~`
}

// URL decode a string
pub fn url_decode(s string) ?string {
	mut result := []u8{}
	mut i := 0

	for i < s.len {
		if s[i] == `%` {
			if i + 2 >= s.len {
				return none
			}
			hex_str := s[i + 1..i + 3]
			if val := parse_hex_byte(hex_str) {
				result << val
				i += 3
			} else {
				return none
			}
		} else if s[i] == `+` {
			result << ` `
			i += 1
		} else {
			result << s[i]
			i += 1
		}
	}

	return result.bytestr()
}

// Parse a hex byte
fn parse_hex_byte(s string) ?u8 {
	if s.len != 2 {
		return none
	}

	high := hex_digit_value(s[0]) or { return none }
	low := hex_digit_value(s[1]) or { return none }

	return u8(high * 16 + low)
}

// Get numeric value of hex digit
fn hex_digit_value(c u8) ?u8 {
	if c >= `0` && c <= `9` {
		return c - `0`
	}
	if c >= `A` && c <= `F` {
		return c - `A` + 10
	}
	if c >= `a` && c <= `f` {
		return c - `a` + 10
	}
	return none
}

// Parse query string into key-value pairs
pub fn parse_query_string(query string) []struct {
	key   string
	value string
} {
	mut result := []struct {
		key   string
		value string
	}{}

	if query.len == 0 {
		return result
	}

	pairs := query.split('&')
	for pair in pairs {
		if pair.len == 0 {
			continue
		}

		if eq_idx := pair.index('=') {
			key := url_decode(pair[..eq_idx]) or { pair[..eq_idx] }
			value := url_decode(pair[eq_idx + 1..]) or { pair[eq_idx + 1..] }
			result << struct {
				key:   key
				value: value
			}
		} else {
			result << struct {
				key:   pair
				value: ''
			}
		}
	}

	return result
}

// Build query string from key-value pairs
pub fn build_query_string(params []struct {
	key   string
	value string
}) string {
	mut parts := []string{}

	for param in params {
		encoded_key := url_encode(param.key)
		encoded_value := url_encode(param.value)
		parts << '${encoded_key}=${encoded_value}'
	}

	return parts.join('&')
}

// Normalize a URL (lowercase scheme/host, remove default ports)
pub fn normalize_url(url string) ?string {
	parsed := parse_url(url)?

	mut host := parsed.host.to_lower()
	mut port_str := ''

	if port := parsed.port {
		// Remove default ports
		if !(parsed.scheme == 'http' && port == 80) && !(parsed.scheme == 'https' && port == 443) {
			port_str = ':${port}'
		}
	}

	mut result := '${parsed.scheme}://${host}${port_str}'

	if parsed.path.len > 0 {
		result += parsed.path
	} else {
		result += '/'
	}

	if parsed.query.len > 0 {
		result += '?${parsed.query}'
	}

	if parsed.fragment.len > 0 {
		result += '#${parsed.fragment}'
	}

	return result
}

// Check if URL is absolute (has scheme)
pub fn is_absolute_url(url string) bool {
	return url.contains('://')
}

// Check if URL is relative
pub fn is_relative_url(url string) bool {
	return !is_absolute_url(url)
}

// Join base URL with relative path
pub fn join_url(base string, relative string) ?string {
	if is_absolute_url(relative) {
		return relative
	}

	parsed := parse_url(base)?

	mut new_path := ''
	if relative.starts_with('/') {
		new_path = relative
	} else {
		// Remove filename from base path
		if last_slash := parsed.path.last_index('/') {
			new_path = parsed.path[..last_slash + 1] + relative
		} else {
			new_path = '/' + relative
		}
	}

	return build_url(parsed.scheme, parsed.host, new_path, '')
}
