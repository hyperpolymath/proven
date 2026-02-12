// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.nio.charset.StandardCharsets;
import java.util.regex.Pattern;

/**
 * Safe IP address parsing and validation.
 * Provides IPv4/IPv6 validation and CIDR notation handling.
 * Calls native verified code via JNI when available.
 */
public final class SafeNetwork {
    private SafeNetwork() {}

    /** IPv4 address pattern. */
    private static final Pattern IPV4_PATTERN = Pattern.compile(
        "^((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)\\.){3}(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)$"
    );

    /** CIDR notation pattern for IPv4. */
    private static final Pattern CIDR_V4_PATTERN = Pattern.compile(
        "^((25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)\\.){3}(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)/(3[0-2]|[12]?\\d)$"
    );

    /**
     * IPv4 address representation.
     */
    public record IPv4Address(byte[] octets) {
        public IPv4Address {
            if (octets == null || octets.length != 4) {
                throw new IllegalArgumentException("IPv4 address must have exactly 4 octets");
            }
            octets = octets.clone();
        }

        @Override
        public byte[] octets() {
            return octets.clone();
        }

        /**
         * Get octet at index (0-3).
         */
        public int getOctet(int index) {
            return octets[index] & 0xFF;
        }

        /**
         * Convert to dotted decimal string.
         */
        public String toString() {
            return String.format("%d.%d.%d.%d",
                octets[0] & 0xFF, octets[1] & 0xFF,
                octets[2] & 0xFF, octets[3] & 0xFF);
        }

        /**
         * Convert to 32-bit integer.
         */
        public long toInt() {
            return ((long)(octets[0] & 0xFF) << 24) |
                   ((octets[1] & 0xFF) << 16) |
                   ((octets[2] & 0xFF) << 8) |
                   (octets[3] & 0xFF);
        }

        /**
         * Create from 32-bit integer.
         */
        public static IPv4Address fromInt(long value) {
            return new IPv4Address(new byte[] {
                (byte)((value >> 24) & 0xFF),
                (byte)((value >> 16) & 0xFF),
                (byte)((value >> 8) & 0xFF),
                (byte)(value & 0xFF)
            });
        }
    }

    /**
     * CIDR notation (address with prefix length).
     */
    public record CidrNotation(IPv4Address address, int prefixLength) {
        public CidrNotation {
            if (prefixLength < 0 || prefixLength > 32) {
                throw new IllegalArgumentException("Prefix length must be 0-32");
            }
        }

        /**
         * Get subnet mask as IPv4 address.
         */
        public IPv4Address getSubnetMask() {
            if (prefixLength == 0) return IPv4Address.fromInt(0);
            long mask = 0xFFFFFFFFL << (32 - prefixLength);
            return IPv4Address.fromInt(mask);
        }

        /**
         * Get network address (first address in subnet).
         */
        public IPv4Address getNetworkAddress() {
            long mask = prefixLength == 0 ? 0 : 0xFFFFFFFFL << (32 - prefixLength);
            return IPv4Address.fromInt(address.toInt() & mask);
        }

        /**
         * Get broadcast address (last address in subnet).
         */
        public IPv4Address getBroadcastAddress() {
            long hostMask = (1L << (32 - prefixLength)) - 1;
            return IPv4Address.fromInt(address.toInt() | hostMask);
        }

        /**
         * Check if an address is within this CIDR range.
         */
        public boolean contains(IPv4Address addr) {
            long mask = prefixLength == 0 ? 0 : 0xFFFFFFFFL << (32 - prefixLength);
            return (address.toInt() & mask) == (addr.toInt() & mask);
        }

        /**
         * Get number of addresses in this range.
         */
        public long getAddressCount() {
            return 1L << (32 - prefixLength);
        }

        public String toString() {
            return address.toString() + "/" + prefixLength;
        }
    }

    /**
     * Check if string is a valid IPv4 address.
     */
    public static boolean isValidIpv4(String addr) {
        if (addr == null || addr.isEmpty()) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeNetworkIsValidIpv4(addr.getBytes(StandardCharsets.UTF_8));
        }
        return IPV4_PATTERN.matcher(addr).matches();
    }

    /**
     * Check if string is a valid IPv6 address.
     */
    public static boolean isValidIpv6(String addr) {
        if (addr == null || addr.isEmpty()) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeNetworkIsValidIpv6(addr.getBytes(StandardCharsets.UTF_8));
        }
        return isValidIpv6Pure(addr);
    }

    /**
     * Check if string is valid CIDR notation.
     */
    public static boolean isValidCidr(String cidr) {
        if (cidr == null || cidr.isEmpty()) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeNetworkIsValidCidr(cidr.getBytes(StandardCharsets.UTF_8));
        }
        return CIDR_V4_PATTERN.matcher(cidr).matches();
    }

    /**
     * Parse IPv4 address string.
     */
    public static ProvenResult<IPv4Address> parseIpv4(String addr) {
        if (addr == null || addr.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty address");
        }
        if (!isValidIpv4(addr)) {
            return ProvenResult.err(ProvenStatus.ERR_PARSE_FAILURE, "Invalid IPv4 address");
        }

        String[] parts = addr.split("\\.");
        byte[] octets = new byte[4];
        for (int i = 0; i < 4; i++) {
            octets[i] = (byte) Integer.parseInt(parts[i]);
        }

        return ProvenResult.ok(new IPv4Address(octets));
    }

    /**
     * Parse CIDR notation.
     */
    public static ProvenResult<CidrNotation> parseCidr(String cidr) {
        if (cidr == null || cidr.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty CIDR");
        }

        int slashIndex = cidr.indexOf('/');
        if (slashIndex < 0) {
            return ProvenResult.err(ProvenStatus.ERR_PARSE_FAILURE, "Missing prefix length");
        }

        String addrPart = cidr.substring(0, slashIndex);
        String prefixPart = cidr.substring(slashIndex + 1);

        var addrResult = parseIpv4(addrPart);
        if (addrResult.isErr()) {
            return ProvenResult.err(addrResult.status(), "Invalid address in CIDR");
        }

        try {
            int prefix = Integer.parseInt(prefixPart);
            if (prefix < 0 || prefix > 32) {
                return ProvenResult.err(ProvenStatus.ERR_OUT_OF_BOUNDS, "Prefix must be 0-32");
            }
            return ProvenResult.ok(new CidrNotation(addrResult.unwrap(), prefix));
        } catch (NumberFormatException e) {
            return ProvenResult.err(ProvenStatus.ERR_PARSE_FAILURE, "Invalid prefix length");
        }
    }

    /**
     * Check if IPv4 address is private (RFC 1918).
     */
    public static boolean isPrivate(IPv4Address addr) {
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeNetworkIpv4IsPrivate(addr.octets());
        }
        int first = addr.getOctet(0);
        int second = addr.getOctet(1);

        // 10.0.0.0/8
        if (first == 10) return true;
        // 172.16.0.0/12
        if (first == 172 && second >= 16 && second <= 31) return true;
        // 192.168.0.0/16
        if (first == 192 && second == 168) return true;

        return false;
    }

    /**
     * Check if IPv4 address is loopback (127.0.0.0/8).
     */
    public static boolean isLoopback(IPv4Address addr) {
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeNetworkIpv4IsLoopback(addr.octets());
        }
        return addr.getOctet(0) == 127;
    }

    /**
     * Check if IPv4 address is link-local (169.254.0.0/16).
     */
    public static boolean isLinkLocal(IPv4Address addr) {
        return addr.getOctet(0) == 169 && addr.getOctet(1) == 254;
    }

    /**
     * Check if IPv4 address is multicast (224.0.0.0/4).
     */
    public static boolean isMulticast(IPv4Address addr) {
        int first = addr.getOctet(0);
        return first >= 224 && first <= 239;
    }

    /**
     * Check if IPv4 address is broadcast (255.255.255.255).
     */
    public static boolean isBroadcast(IPv4Address addr) {
        return addr.toInt() == 0xFFFFFFFFL;
    }

    // Pure Java fallback for IPv6 validation (simplified)
    private static boolean isValidIpv6Pure(String addr) {
        if (addr.contains("::")) {
            // Only one :: allowed
            if (addr.indexOf("::") != addr.lastIndexOf("::")) return false;
        }

        // Basic structure check
        String[] parts = addr.split(":");
        if (parts.length < 2 || parts.length > 8) return false;

        for (String part : parts) {
            if (part.isEmpty()) continue; // Empty from ::
            if (part.length() > 4) return false;
            try {
                int value = Integer.parseInt(part, 16);
                if (value < 0 || value > 0xFFFF) return false;
            } catch (NumberFormatException e) {
                return false;
            }
        }

        return true;
    }
}
