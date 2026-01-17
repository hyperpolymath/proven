// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven

import groovy.transform.CompileStatic
import java.net.InetAddress
import java.net.Inet4Address
import java.net.Inet6Address
import java.util.regex.Pattern

/**
 * Safe network address operations with validation.
 * Validates IP addresses, ports, and network ranges.
 */
@CompileStatic
class SafeNetwork {

    /** Minimum valid port number. */
    static final int MIN_PORT = 0

    /** Maximum valid port number. */
    static final int MAX_PORT = 65535

    /** Well-known ports range. */
    static final int PRIVILEGED_PORT_MAX = 1023

    /** IPv4 pattern for validation. */
    private static final Pattern IPV4_PATTERN = Pattern.compile(
        /^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/
    )

    /** IPv6 patterns. */
    private static final Pattern IPV6_FULL_PATTERN = Pattern.compile(
        /^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$/
    )

    /**
     * Validate an IPv4 address.
     */
    static Result<Boolean, String> validateIpv4(String ip) {
        if (ip == null || ip.isEmpty()) {
            return Result.err("IP address is null or empty")
        }

        def matcher = IPV4_PATTERN.matcher(ip)
        if (!matcher.matches()) {
            return Result.err("Invalid IPv4 format")
        }

        for (int i = 1; i <= 4; i++) {
            int octet = Integer.parseInt(matcher.group(i))
            if (octet < 0 || octet > 255) {
                return Result.err("IPv4 octet out of range: ${octet}")
            }
        }

        return Result.ok(true)
    }

    /**
     * Check if string is valid IPv4.
     */
    static boolean isValidIpv4(String ip) {
        validateIpv4(ip).match({ Boolean v -> v }, { String e -> false })
    }

    /**
     * Validate an IPv6 address.
     */
    static Result<Boolean, String> validateIpv6(String ip) {
        if (ip == null || ip.isEmpty()) {
            return Result.err("IP address is null or empty")
        }

        try {
            InetAddress addr = InetAddress.getByName(ip)
            if (addr instanceof Inet6Address) {
                return Result.ok(true)
            }
            return Result.err("Not an IPv6 address")
        } catch (Exception e) {
            return Result.err("Invalid IPv6 address: ${e.message}")
        }
    }

    /**
     * Check if string is valid IPv6.
     */
    static boolean isValidIpv6(String ip) {
        validateIpv6(ip).match({ Boolean v -> v }, { String e -> false })
    }

    /**
     * Validate any IP address (v4 or v6).
     */
    static Result<Boolean, String> validateIp(String ip) {
        def v4 = validateIpv4(ip)
        if (v4.ok) return v4
        return validateIpv6(ip)
    }

    /**
     * Check if string is valid IP (v4 or v6).
     */
    static boolean isValidIp(String ip) {
        isValidIpv4(ip) || isValidIpv6(ip)
    }

    /**
     * Validate a port number.
     */
    static Result<Boolean, String> validatePort(int port) {
        if (port < MIN_PORT || port > MAX_PORT) {
            return Result.err("Port out of range: ${port} (valid: ${MIN_PORT}-${MAX_PORT})")
        }
        return Result.ok(true)
    }

    /**
     * Check if port is valid.
     */
    static boolean isValidPort(int port) {
        port >= MIN_PORT && port <= MAX_PORT
    }

    /**
     * Check if port is privileged (requires root).
     */
    static boolean isPrivilegedPort(int port) {
        port >= MIN_PORT && port <= PRIVILEGED_PORT_MAX
    }

    /**
     * Parse a port from string.
     */
    static Result<Integer, String> parsePort(String portStr) {
        if (portStr == null || portStr.trim().isEmpty()) {
            return Result.err("Port string is null or empty")
        }
        try {
            int port = Integer.parseInt(portStr.trim())
            def valid = validatePort(port)
            if (valid.err) return Result.err(valid.error)
            return Result.ok(port)
        } catch (NumberFormatException e) {
            return Result.err("Invalid port format: ${portStr}")
        }
    }

    /**
     * Check if IP is a loopback address.
     */
    static boolean isLoopback(String ip) {
        try {
            InetAddress addr = InetAddress.getByName(ip)
            return addr.isLoopbackAddress()
        } catch (Exception e) {
            return false
        }
    }

    /**
     * Check if IP is a private/local address.
     */
    static boolean isPrivate(String ip) {
        try {
            InetAddress addr = InetAddress.getByName(ip)
            return addr.isSiteLocalAddress() || addr.isLinkLocalAddress()
        } catch (Exception e) {
            return false
        }
    }

    /**
     * Check if IP is a multicast address.
     */
    static boolean isMulticast(String ip) {
        try {
            InetAddress addr = InetAddress.getByName(ip)
            return addr.isMulticastAddress()
        } catch (Exception e) {
            return false
        }
    }

    /**
     * Parse host:port string.
     */
    static Result<Map<String, Object>, String> parseHostPort(String hostPort) {
        if (hostPort == null || hostPort.isEmpty()) {
            return Result.err("Host:port string is null or empty")
        }

        // Handle IPv6 addresses in brackets
        if (hostPort.startsWith('[')) {
            int closeBracket = hostPort.indexOf(']')
            if (closeBracket < 0) {
                return Result.err("Unclosed bracket in IPv6 address")
            }

            String host = hostPort.substring(1, closeBracket)
            if (!isValidIpv6(host)) {
                return Result.err("Invalid IPv6 address in brackets")
            }

            if (closeBracket == hostPort.length() - 1) {
                return Result.ok([host: host, port: null] as Map<String, Object>)
            }

            if (hostPort.charAt(closeBracket + 1) != ':') {
                return Result.err("Invalid separator after IPv6 address")
            }

            def portResult = parsePort(hostPort.substring(closeBracket + 2))
            if (portResult.err) return Result.err(portResult.error)
            return Result.ok([host: host, port: portResult.value] as Map<String, Object>)
        }

        // Handle IPv4 or hostname
        int lastColon = hostPort.lastIndexOf(':')
        if (lastColon < 0) {
            return Result.ok([host: hostPort, port: null] as Map<String, Object>)
        }

        String host = hostPort.substring(0, lastColon)
        def portResult = parsePort(hostPort.substring(lastColon + 1))
        if (portResult.err) return Result.err(portResult.error)

        return Result.ok([host: host, port: portResult.value] as Map<String, Object>)
    }

    /**
     * Validate CIDR notation.
     */
    static Result<Boolean, String> validateCidr(String cidr) {
        if (cidr == null || cidr.isEmpty()) {
            return Result.err("CIDR is null or empty")
        }

        int slashIdx = cidr.indexOf('/')
        if (slashIdx < 0) {
            return Result.err("Missing prefix length in CIDR")
        }

        String ip = cidr.substring(0, slashIdx)
        String prefixStr = cidr.substring(slashIdx + 1)

        def ipValid = validateIp(ip)
        if (ipValid.err) return Result.err(ipValid.error)

        try {
            int prefix = Integer.parseInt(prefixStr)
            int maxPrefix = isValidIpv4(ip) ? 32 : 128
            if (prefix < 0 || prefix > maxPrefix) {
                return Result.err("Prefix length out of range: ${prefix}")
            }
            return Result.ok(true)
        } catch (NumberFormatException e) {
            return Result.err("Invalid prefix length: ${prefixStr}")
        }
    }

    /**
     * Check if IP is in CIDR range.
     */
    static Result<Boolean, String> isInCidr(String ip, String cidr) {
        def ipValid = validateIp(ip)
        if (ipValid.err) return Result.err("Invalid IP: ${ipValid.error}")

        def cidrValid = validateCidr(cidr)
        if (cidrValid.err) return Result.err("Invalid CIDR: ${cidrValid.error}")

        try {
            int slashIdx = cidr.indexOf('/')
            String networkIp = cidr.substring(0, slashIdx)
            int prefix = Integer.parseInt(cidr.substring(slashIdx + 1))

            InetAddress target = InetAddress.getByName(ip)
            InetAddress network = InetAddress.getByName(networkIp)

            // Different address families don't match
            if (target.class != network.class) {
                return Result.ok(false)
            }

            byte[] targetBytes = target.address
            byte[] networkBytes = network.address

            int fullBytes = prefix / 8
            int remainingBits = prefix % 8

            for (int i = 0; i < fullBytes; i++) {
                if (targetBytes[i] != networkBytes[i]) {
                    return Result.ok(false)
                }
            }

            if (remainingBits > 0 && fullBytes < targetBytes.length) {
                int mask = (0xFF << (8 - remainingBits)) & 0xFF
                if ((targetBytes[fullBytes] & mask) != (networkBytes[fullBytes] & mask)) {
                    return Result.ok(false)
                }
            }

            return Result.ok(true)
        } catch (Exception e) {
            return Result.err("CIDR check failed: ${e.message}")
        }
    }

    /**
     * Format IP and port as string.
     */
    static String formatHostPort(String host, int port) {
        if (isValidIpv6(host)) {
            return "[${host}]:${port}"
        }
        return "${host}:${port}"
    }
}
