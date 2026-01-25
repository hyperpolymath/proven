// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

module proven

import strings

// IP address classification.
pub enum IpClassification {
	loopback
	private
	reserved
	public
	invalid
}

// Parsed IPv4 address.
pub struct IPv4Address {
pub:
	a u8
	b u8
	c u8
	d u8
}

// Create a string representation of IPv4 address.
pub fn (ip IPv4Address) str() string {
	return '${ip.a}.${ip.b}.${ip.c}.${ip.d}'
}

// Convert to integer representation.
pub fn (ip IPv4Address) to_int() u32 {
	return (u32(ip.a) << 24) | (u32(ip.b) << 16) | (u32(ip.c) << 8) | u32(ip.d)
}

// Parse IPv4 address string.
pub fn parse_ipv4(address string) ?IPv4Address {
	parts := address.split('.')
	if parts.len != 4 {
		return none
	}

	mut octets := []u8{cap: 4}
	for part in parts {
		if part.len == 0 {
			return none
		}
		// Check for leading zeros
		if part.len > 1 && part[0] == `0` {
			return none
		}
		// Check all digits
		for c in part {
			if c < `0` || c > `9` {
				return none
			}
		}
		value := part.int()
		if value < 0 || value > 255 {
			return none
		}
		octets << u8(value)
	}

	return IPv4Address{
		a: octets[0]
		b: octets[1]
		c: octets[2]
		d: octets[3]
	}
}

// Check if IPv4 is a loopback address.
pub fn is_loopback(ip IPv4Address) bool {
	return ip.a == 127
}

// Check if IPv4 is a private address (RFC 1918).
pub fn is_private_ip(ip IPv4Address) bool {
	return ip.a == 10 || (ip.a == 172 && ip.b >= 16 && ip.b <= 31) ||
		(ip.a == 192 && ip.b == 168)
}

// Check if IPv4 is a reserved address.
pub fn is_reserved(ip IPv4Address) bool {
	return ip.a == 0 || (ip.a == 100 && ip.b >= 64 && ip.b <= 127) ||
		(ip.a == 169 && ip.b == 254) || (ip.a == 192 && ip.b == 0 && ip.c == 0) ||
		(ip.a == 192 && ip.b == 0 && ip.c == 2) || (ip.a == 198 && ip.b == 51 && ip.c == 100) ||
		(ip.a == 203 && ip.b == 0 && ip.c == 113) || (ip.a >= 224 && ip.a <= 239) || ip.a >= 240
}

// Check if IPv4 is a public address.
pub fn is_public_ip(ip IPv4Address) bool {
	return !is_loopback(ip) && !is_private_ip(ip) && !is_reserved(ip)
}

// Classify an IPv4 address.
pub fn classify_ip(ip IPv4Address) IpClassification {
	if is_loopback(ip) {
		return .loopback
	}
	if is_private_ip(ip) {
		return .private
	}
	if is_reserved(ip) {
		return .reserved
	}
	return .public
}

// Classify IPv4 from string.
pub fn classify_ip_string(address string) IpClassification {
	ip := parse_ipv4(address) or { return .invalid }
	return classify_ip(ip)
}

// Check if IP is in CIDR range.
pub fn ip_in_range(ip IPv4Address, network IPv4Address, prefix_length int) bool {
	if prefix_length < 0 || prefix_length > 32 {
		return false
	}

	mask := if prefix_length == 0 {
		u32(0)
	} else {
		u32(0xFFFFFFFF) << (32 - prefix_length)
	}

	return (ip.to_int() & mask) == (network.to_int() & mask)
}

// Check if string is valid IPv4.
pub fn is_valid_ipv4(address string) bool {
	_ := parse_ipv4(address) or { return false }
	return true
}

// Check if port number is valid.
pub fn is_valid_port(port int) bool {
	return port >= 1 && port <= 65535
}

// Check if port is privileged.
pub fn is_privileged_port(port int) bool {
	return port >= 1 && port < 1024
}

// Check if hostname is valid.
pub fn is_valid_hostname(hostname string) bool {
	if hostname.len == 0 || hostname.len > 253 {
		return false
	}

	labels := hostname.split('.')
	for label in labels {
		if label.len == 0 || label.len > 63 {
			return false
		}
		// Must start and end with alphanumeric
		if !is_alpha_numeric(label[0]) {
			return false
		}
		if !is_alpha_numeric(label[label.len - 1]) {
			return false
		}
		// Middle can include hyphens
		for c in label {
			if !is_alpha_numeric(c) && c != `-` {
				return false
			}
		}
	}
	return true
}

// Check if character is alphanumeric.
fn is_alpha_numeric(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`)
}

// Check if URL is valid (basic check).
pub fn is_valid_url(url string) bool {
	return url.starts_with('http://') || url.starts_with('https://')
}

// Check if URL host is a private IP (SSRF protection).
pub fn is_private_url(url string) bool {
	// Extract host from URL
	mut host := url

	// Remove protocol
	if host.starts_with('http://') {
		host = host[7..]
	} else if host.starts_with('https://') {
		host = host[8..]
	}

	// Remove path
	if slash_idx := host.index('/') {
		host = host[..slash_idx]
	}

	// Remove port
	if colon_idx := host.index(':') {
		host = host[..colon_idx]
	}

	// Check common private hosts
	if host == 'localhost' || host == '127.0.0.1' || host == '::1' {
		return true
	}

	// Check if it's a private IP
	ip := parse_ipv4(host) or { return false }
	return is_private_ip(ip) || is_loopback(ip) || is_reserved(ip)
}
