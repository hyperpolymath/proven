// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe network operations for IP address validation and classification.
public enum SafeNetwork {
    /// Represents an IPv4 address.
    public struct IPv4: Equatable, CustomStringConvertible {
        public let a: UInt8
        public let b: UInt8
        public let c: UInt8
        public let d: UInt8

        public var description: String {
            "\(a).\(b).\(c).\(d)"
        }
    }

    /// Parse an IPv4 address string.
    public static func parseIPv4(_ address: String) -> IPv4? {
        let parts = address.split(separator: ".")
        guard parts.count == 4 else { return nil }

        var octets: [UInt8] = []
        for part in parts {
            guard let value = UInt8(part), value <= 255 else { return nil }
            octets.append(value)
        }

        return IPv4(a: octets[0], b: octets[1], c: octets[2], d: octets[3])
    }

    /// Check if a string is a valid IPv4 address.
    public static func isValidIPv4(_ address: String) -> Bool {
        parseIPv4(address) != nil
    }

    /// Check if an IPv4 address is in a private range.
    public static func isPrivate(_ address: String) -> Bool {
        guard let ip = parseIPv4(address) else { return false }

        // 10.0.0.0/8
        if ip.a == 10 { return true }
        // 172.16.0.0/12
        if ip.a == 172 && ip.b >= 16 && ip.b <= 31 { return true }
        // 192.168.0.0/16
        if ip.a == 192 && ip.b == 168 { return true }

        return false
    }

    /// Check if an IPv4 address is a loopback address (127.0.0.0/8).
    public static func isLoopback(_ address: String) -> Bool {
        guard let ip = parseIPv4(address) else { return false }
        return ip.a == 127
    }

    /// Check if an IPv4 address is public (not private or loopback).
    public static func isPublic(_ address: String) -> Bool {
        isValidIPv4(address) && !isPrivate(address) && !isLoopback(address)
    }

    /// Format an IPv4 address as a string.
    public static func formatIPv4(_ ip: IPv4) -> String {
        ip.description
    }
}
