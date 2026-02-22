// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeNetwork.vala -- IPv4 address parsing and classification.
 *
 * Every method delegates to libproven via C extern calls declared in
 * lib_proven.vapi.  No network logic is reimplemented here.
 */
namespace Proven {

    /**
     * Parsed IPv4 address returned by {@link SafeNetwork.parse_ipv4}.
     */
    public class IPv4 : GLib.Object {
        /** The four octets of the IPv4 address. */
        public uint8 octet0 { get; private set; }
        public uint8 octet1 { get; private set; }
        public uint8 octet2 { get; private set; }
        public uint8 octet3 { get; private set; }

        /** Whether the address is in a private range (RFC 1918). */
        public bool is_private { get; private set; }

        /** Whether the address is loopback (127.0.0.0/8). */
        public bool is_loopback { get; private set; }

        internal LibProven.IPv4Address _raw;

        internal IPv4 (LibProven.IPv4Address addr) {
            _raw       = addr;
            octet0     = addr.octets[0];
            octet1     = addr.octets[1];
            octet2     = addr.octets[2];
            octet3     = addr.octets[3];
            is_private  = LibProven.network_ipv4_is_private (addr);
            is_loopback = LibProven.network_ipv4_is_loopback (addr);
        }

        /**
         * Return the address as a dotted-quad string (e.g. "192.168.1.1").
         */
        public string to_string () {
            return "%u.%u.%u.%u".printf (octet0, octet1, octet2, octet3);
        }
    }

    /**
     * Safe network address operations.
     *
     * Currently supports IPv4 parsing and classification (private,
     * loopback).
     */
    public class SafeNetwork : GLib.Object {

        /**
         * Parse an IPv4 address string (e.g. "192.168.1.1").
         *
         * @param address Address string.
         * @return Parsed IPv4 address, or null on parse failure.
         */
        public static IPv4? parse_ipv4 (string address) {
            unowned uint8[] data = (uint8[]) address.data;
            LibProven.IPv4Result r = LibProven.network_parse_ipv4 (data);
            if (r.status != 0) {
                return null;
            }
            return new IPv4 (r.address);
        }

        /**
         * Check whether an IPv4 address is in a private range (RFC 1918).
         *
         * Convenience method that parses and checks in one step.
         *
         * @param address Address string.
         * @return true if private, false if public; null on parse failure.
         */
        public static bool? is_private (string address) {
            var ip = parse_ipv4 (address);
            if (ip == null) {
                return null;
            }
            return ip.is_private;
        }

        /**
         * Check whether an IPv4 address is loopback (127.0.0.0/8).
         *
         * Convenience method that parses and checks in one step.
         *
         * @param address Address string.
         * @return true if loopback; null on parse failure.
         */
        public static bool? is_loopback (string address) {
            var ip = parse_ipv4 (address);
            if (ip == null) {
                return null;
            }
            return ip.is_loopback;
        }
    }
}
