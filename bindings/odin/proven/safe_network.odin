// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"
import "core:strconv"
import "core:fmt"

// IP address classification.
IpClassification :: enum {
    Loopback,
    Private,
    Reserved,
    Public,
    Invalid,
}

// Parsed IPv4 address.
IPv4Address :: struct {
    a, b, c, d: u8,
}

// Format IPv4Address as string.
format_ipv4 :: proc(ip: IPv4Address, allocator := context.allocator) -> string {
    return fmt.aprintf("%d.%d.%d.%d", ip.a, ip.b, ip.c, ip.d)
}

// Convert to integer representation.
ipv4_to_int :: proc(ip: IPv4Address) -> u32 {
    return (u32(ip.a) << 24) | (u32(ip.b) << 16) | (u32(ip.c) << 8) | u32(ip.d)
}

// Parse IPv4 address string.
parse_ipv4 :: proc(address: string) -> (ip: IPv4Address, ok: bool) {
    parts := strings.split(address, ".")
    defer delete(parts)

    if len(parts) != 4 {
        return {}, false
    }

    octets: [4]u8
    for part, i in parts {
        if len(part) == 0 {
            return {}, false
        }
        // Check for leading zeros
        if len(part) > 1 && part[0] == '0' {
            return {}, false
        }
        // Check all digits
        for c in part {
            if c < '0' || c > '9' {
                return {}, false
            }
        }

        value, parse_ok := strconv.parse_int(part)
        if !parse_ok || value < 0 || value > 255 {
            return {}, false
        }
        octets[i] = u8(value)
    }

    return IPv4Address{octets[0], octets[1], octets[2], octets[3]}, true
}

// Check if IPv4 is a loopback address.
is_loopback :: proc(ip: IPv4Address) -> bool {
    return ip.a == 127
}

// Check if IPv4 is a private address (RFC 1918).
is_private_ip :: proc(ip: IPv4Address) -> bool {
    return ip.a == 10 ||
           (ip.a == 172 && ip.b >= 16 && ip.b <= 31) ||
           (ip.a == 192 && ip.b == 168)
}

// Check if IPv4 is a reserved address.
is_reserved :: proc(ip: IPv4Address) -> bool {
    return ip.a == 0 ||
           (ip.a == 100 && ip.b >= 64 && ip.b <= 127) ||
           (ip.a == 169 && ip.b == 254) ||
           (ip.a == 192 && ip.b == 0 && ip.c == 0) ||
           (ip.a == 192 && ip.b == 0 && ip.c == 2) ||
           (ip.a == 198 && ip.b == 51 && ip.c == 100) ||
           (ip.a == 203 && ip.b == 0 && ip.c == 113) ||
           (ip.a >= 224 && ip.a <= 239) ||
           ip.a >= 240
}

// Check if IPv4 is a public address.
is_public_ip :: proc(ip: IPv4Address) -> bool {
    return !is_loopback(ip) && !is_private_ip(ip) && !is_reserved(ip)
}

// Classify an IPv4 address.
classify_ip :: proc(ip: IPv4Address) -> IpClassification {
    if is_loopback(ip) {
        return .Loopback
    }
    if is_private_ip(ip) {
        return .Private
    }
    if is_reserved(ip) {
        return .Reserved
    }
    return .Public
}

// Classify IPv4 from string.
classify_ip_string :: proc(address: string) -> IpClassification {
    ip, ok := parse_ipv4(address)
    if !ok {
        return .Invalid
    }
    return classify_ip(ip)
}

// Check if IP is in CIDR range.
ip_in_range :: proc(ip, network: IPv4Address, prefix_length: int) -> bool {
    if prefix_length < 0 || prefix_length > 32 {
        return false
    }

    mask: u32 = prefix_length == 0 ? 0 : 0xFFFFFFFF << u32(32 - prefix_length)
    return (ipv4_to_int(ip) & mask) == (ipv4_to_int(network) & mask)
}

// Check if string is valid IPv4.
is_valid_ipv4 :: proc(address: string) -> bool {
    _, ok := parse_ipv4(address)
    return ok
}

// Check if port number is valid.
is_valid_port :: proc(port: int) -> bool {
    return port >= 1 && port <= 65535
}

// Check if port is privileged.
is_privileged_port :: proc(port: int) -> bool {
    return port >= 1 && port < 1024
}

// Check if hostname is valid.
is_valid_hostname :: proc(hostname: string) -> bool {
    if len(hostname) == 0 || len(hostname) > 253 {
        return false
    }

    labels := strings.split(hostname, ".")
    defer delete(labels)

    for label in labels {
        if len(label) == 0 || len(label) > 63 {
            return false
        }
        // Must start and end with alphanumeric
        if !is_alpha_num(label[0]) {
            return false
        }
        if !is_alpha_num(label[len(label) - 1]) {
            return false
        }
        // Middle can include hyphens
        for i := 1; i < len(label) - 1; i += 1 {
            c := label[i]
            if !is_alpha_num(c) && c != '-' {
                return false
            }
        }
    }
    return true
}

// Check if URL is valid (basic check).
is_valid_url :: proc(url: string) -> bool {
    return strings.has_prefix(url, "http://") || strings.has_prefix(url, "https://")
}

// Check if URL host is a private IP (SSRF protection).
is_private_url :: proc(url: string) -> bool {
    host := url

    // Remove protocol
    if strings.has_prefix(host, "http://") {
        host = host[7:]
    } else if strings.has_prefix(host, "https://") {
        host = host[8:]
    }

    // Remove path
    slash_idx := strings.index(host, "/")
    if slash_idx >= 0 {
        host = host[:slash_idx]
    }

    // Remove port
    colon_idx := strings.index(host, ":")
    if colon_idx >= 0 {
        host = host[:colon_idx]
    }

    // Check common private hosts
    if host == "localhost" || host == "127.0.0.1" || host == "::1" {
        return true
    }

    // Check if it's a private IP
    ip, ok := parse_ipv4(host)
    if !ok {
        return false
    }
    return is_private_ip(ip) || is_loopback(ip) || is_reserved(ip)
}
