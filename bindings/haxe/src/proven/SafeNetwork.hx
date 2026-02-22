// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeNetwork - IP address parsing and classification via libproven FFI.
// All computation is performed in verified Idris 2 code. This module
// provides an idiomatic Haxe wrapper around the raw C FFI calls.

package proven;

import proven.LibProven;

/**
 * IPv4 address represented as four octets.
 */
typedef IPv4Address = Array<Int>;

/**
 * Safe network operations for IP address parsing and classification.
 *
 * All operations delegate to the formally verified Idris 2 core via
 * libproven. Returns `null` on error.
 */
class SafeNetwork {
    /**
     * Parse an IPv4 address string (e.g., "192.168.1.1").
     * @param s IPv4 address string
     * @return Array of 4 octets [a, b, c, d], or null on parse failure
     */
    public static function parseIpv4(s:String):Null<IPv4Address> {
        return LibProven.networkParseIpv4(s);
    }

    /**
     * Check if an IPv4 address is private (RFC 1918).
     * Private ranges: 10.x.x.x, 172.16-31.x.x, 192.168.x.x
     * @param addr IPv4 address as [a, b, c, d]
     * @return true if private
     */
    public static function ipv4IsPrivate(addr:IPv4Address):Bool {
        return LibProven.networkIpv4IsPrivate(addr);
    }

    /**
     * Check if an IPv4 address is loopback (127.0.0.0/8).
     * @param addr IPv4 address as [a, b, c, d]
     * @return true if loopback
     */
    public static function ipv4IsLoopback(addr:IPv4Address):Bool {
        return LibProven.networkIpv4IsLoopback(addr);
    }
}
