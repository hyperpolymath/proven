// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe network operations delegated to libproven FFI.
///
/// IPv4 parsing and classification performed by the formally verified
/// Idris 2 core via the Zig FFI bridge.

import CProven

/// Represents a parsed IPv4 address.
public struct IPv4Address: Equatable, Sendable, CustomStringConvertible {
    public let octets: (UInt8, UInt8, UInt8, UInt8)

    public var description: String {
        "\(octets.0).\(octets.1).\(octets.2).\(octets.3)"
    }
}

public enum SafeNetwork {
    /// Parse an IPv4 address string.
    public static func parseIPv4(_ address: String) -> Result<IPv4Address, ProvenError> {
        withStringBytes(address) { ptr, len in
            let result = proven_network_parse_ipv4(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            let addr = result.address
            return .success(IPv4Address(octets: (
                addr.octets.0,
                addr.octets.1,
                addr.octets.2,
                addr.octets.3
            )))
        }
    }

    /// Check if an IPv4 address is private (RFC 1918).
    public static func isPrivate(_ addr: IPv4Address) -> Bool {
        var cAddr = ProvenIPv4Address()
        cAddr.octets.0 = addr.octets.0
        cAddr.octets.1 = addr.octets.1
        cAddr.octets.2 = addr.octets.2
        cAddr.octets.3 = addr.octets.3
        return proven_network_ipv4_is_private(cAddr)
    }

    /// Check if an IPv4 address is loopback (127.0.0.0/8).
    public static func isLoopback(_ addr: IPv4Address) -> Bool {
        var cAddr = ProvenIPv4Address()
        cAddr.octets.0 = addr.octets.0
        cAddr.octets.1 = addr.octets.1
        cAddr.octets.2 = addr.octets.2
        cAddr.octets.3 = addr.octets.3
        return proven_network_ipv4_is_loopback(cAddr)
    }

    /// Parse and check if an IPv4 address string is private.
    public static func isPrivate(_ address: String) -> Result<Bool, ProvenError> {
        parseIPv4(address).map { isPrivate($0) }
    }

    /// Parse and check if an IPv4 address string is loopback.
    public static func isLoopback(_ address: String) -> Result<Bool, ProvenError> {
        parseIPv4(address).map { isLoopback($0) }
    }
}
