// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeNetwork.cs - IP address parsing and classification.
//
// Thin P/Invoke wrapper over libproven. ALL computation is performed in
// verified Idris 2 code via the Zig FFI bridge. No logic is reimplemented here.

namespace Proven
{
    /// <summary>
    /// Safe network operations backed by formally verified Idris 2 code.
    /// Provides IPv4 address parsing, private range detection, and loopback
    /// detection. All methods delegate to the libproven FFI.
    /// </summary>
    public static class SafeNetwork
    {
        /// <summary>
        /// Parse an IPv4 address string (e.g. "192.168.1.1").
        /// Delegates to proven_network_parse_ipv4 via FFI.
        /// </summary>
        /// <param name="address">IPv4 address string to parse.</param>
        /// <returns>The parsed IPv4Address, or null on parse failure.</returns>
        public static IPv4Address? ParseIPv4(string address)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(address);
            IPv4Result result = LibProven.proven_network_parse_ipv4(bytes, (nuint)bytes.Length);

            if (result.Status == 0)
            {
                return result.Address;
            }

            return null;
        }

        /// <summary>
        /// Check if a string is a valid IPv4 address.
        /// Delegates to proven_network_parse_ipv4 via FFI.
        /// </summary>
        /// <param name="address">Address string to validate.</param>
        /// <returns>true if the address is a valid IPv4 address.</returns>
        public static bool IsValidIPv4(string address)
        {
            return ParseIPv4(address) is not null;
        }

        /// <summary>
        /// Check if an IPv4 address is in a private range (RFC 1918).
        /// Private ranges: 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16.
        /// Delegates to proven_network_ipv4_is_private via FFI.
        /// </summary>
        /// <param name="address">Parsed IPv4 address to check.</param>
        /// <returns>true if the address is private.</returns>
        public static bool IsPrivate(IPv4Address address)
        {
            return LibProven.proven_network_ipv4_is_private(address);
        }

        /// <summary>
        /// Check if an IPv4 address is in a private range (RFC 1918).
        /// Convenience overload that accepts a string.
        /// Delegates to proven_network_parse_ipv4 and proven_network_ipv4_is_private via FFI.
        /// </summary>
        /// <param name="address">IPv4 address string.</param>
        /// <returns>true if the address is private, false otherwise (including parse failure).</returns>
        public static bool IsPrivate(string address)
        {
            IPv4Address? parsed = ParseIPv4(address);
            if (parsed is null)
            {
                return false;
            }
            return IsPrivate(parsed.Value);
        }

        /// <summary>
        /// Check if an IPv4 address is a loopback address (127.0.0.0/8).
        /// Delegates to proven_network_ipv4_is_loopback via FFI.
        /// </summary>
        /// <param name="address">Parsed IPv4 address to check.</param>
        /// <returns>true if the address is loopback.</returns>
        public static bool IsLoopback(IPv4Address address)
        {
            return LibProven.proven_network_ipv4_is_loopback(address);
        }

        /// <summary>
        /// Check if an IPv4 address is a loopback address (127.0.0.0/8).
        /// Convenience overload that accepts a string.
        /// Delegates to proven_network_parse_ipv4 and proven_network_ipv4_is_loopback via FFI.
        /// </summary>
        /// <param name="address">IPv4 address string.</param>
        /// <returns>true if the address is loopback, false otherwise (including parse failure).</returns>
        public static bool IsLoopback(string address)
        {
            IPv4Address? parsed = ParseIPv4(address);
            if (parsed is null)
            {
                return false;
            }
            return IsLoopback(parsed.Value);
        }
    }
}
