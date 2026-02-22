// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe network validation and operations.
 *
 * Thin FFI wrapper around libproven's SafeNetwork module. IPv4 parsing
 * and classification is performed in formally verified Idris 2 code.
 * This module only marshals data to/from the C ABI.
 */
module proven.safe_network;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Parsed IPv4 address.
struct IPv4Address
{
    ubyte a, b, c, d;

    /// Format as dotted-decimal string.
    string toString() const pure @safe
    {
        import std.format : format;
        return format!"%d.%d.%d.%d"(a, b, c, d);
    }
}

/// Parse IPv4 address string (e.g., "192.168.1.1").
Nullable!IPv4Address parseIPv4(string address) @trusted nothrow
{
    if (address.length == 0)
        return Nullable!IPv4Address.init;

    auto result = proven_network_parse_ipv4(
        cast(const(ubyte)*) address.ptr, address.length
    );

    if (provenFailed(result.status))
        return Nullable!IPv4Address.init;

    return nullable(IPv4Address(
        result.address.octets[0],
        result.address.octets[1],
        result.address.octets[2],
        result.address.octets[3]
    ));
}

/// Check if IPv4 is a private address (RFC 1918).
bool isPrivateIP(IPv4Address ip) @trusted nothrow @nogc
{
    ProvenIPv4Address addr;
    addr.octets = [ip.a, ip.b, ip.c, ip.d];
    return proven_network_ipv4_is_private(addr);
}

/// Check if IPv4 is a loopback address (127.0.0.0/8).
bool isLoopback(IPv4Address ip) @trusted nothrow @nogc
{
    ProvenIPv4Address addr;
    addr.octets = [ip.a, ip.b, ip.c, ip.d];
    return proven_network_ipv4_is_loopback(addr);
}

/// Check if string is valid IPv4.
bool isValidIPv4(string address) @trusted nothrow
{
    return !parseIPv4(address).isNull;
}
