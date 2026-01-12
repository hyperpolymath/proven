<?php
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Safe network operations for IP address validation and classification.
 */
class SafeNetwork
{
    /**
     * Parse an IPv4 address string.
     *
     * @param string $address The IPv4 address string
     * @return array{a: int, b: int, c: int, d: int}|null Octets or null if invalid
     */
    public static function parseIpv4(string $address): ?array
    {
        // Use filter_var for robust validation
        if (filter_var($address, FILTER_VALIDATE_IP, FILTER_FLAG_IPV4) === false) {
            return null;
        }

        $parts = explode('.', $address);
        if (count($parts) !== 4) {
            return null;
        }

        return [
            'a' => (int)$parts[0],
            'b' => (int)$parts[1],
            'c' => (int)$parts[2],
            'd' => (int)$parts[3],
        ];
    }

    /**
     * Check if a string is a valid IPv4 address.
     *
     * @param string $address The address to check
     * @return bool True if valid IPv4
     */
    public static function isValidIpv4(string $address): bool
    {
        return self::parseIpv4($address) !== null;
    }

    /**
     * Parse an IPv6 address string.
     *
     * @param string $address The IPv6 address string
     * @return string|null Expanded IPv6 or null if invalid
     */
    public static function parseIpv6(string $address): ?string
    {
        if (filter_var($address, FILTER_VALIDATE_IP, FILTER_FLAG_IPV6) === false) {
            return null;
        }
        // Return expanded form
        return inet_ntop(inet_pton($address));
    }

    /**
     * Check if a string is a valid IPv6 address.
     *
     * @param string $address The address to check
     * @return bool True if valid IPv6
     */
    public static function isValidIpv6(string $address): bool
    {
        return self::parseIpv6($address) !== null;
    }

    /**
     * Check if an IPv4 address is in a private range.
     *
     * @param string $address The IPv4 address
     * @return bool True if private
     */
    public static function isPrivate(string $address): bool
    {
        $ip = self::parseIpv4($address);
        if ($ip === null) {
            return false;
        }

        // 10.0.0.0/8
        if ($ip['a'] === 10) {
            return true;
        }

        // 172.16.0.0/12
        if ($ip['a'] === 172 && $ip['b'] >= 16 && $ip['b'] <= 31) {
            return true;
        }

        // 192.168.0.0/16
        if ($ip['a'] === 192 && $ip['b'] === 168) {
            return true;
        }

        return false;
    }

    /**
     * Check if an IPv4 address is a loopback address (127.0.0.0/8).
     *
     * @param string $address The IPv4 address
     * @return bool True if loopback
     */
    public static function isLoopback(string $address): bool
    {
        $ip = self::parseIpv4($address);
        if ($ip === null) {
            return false;
        }
        return $ip['a'] === 127;
    }

    /**
     * Check if an IPv4 address is public (not private or loopback).
     *
     * @param string $address The IPv4 address
     * @return bool True if public
     */
    public static function isPublic(string $address): bool
    {
        return self::isValidIpv4($address)
            && !self::isPrivate($address)
            && !self::isLoopback($address)
            && !self::isReserved($address);
    }

    /**
     * Check if an IPv4 address is in a reserved range.
     *
     * @param string $address The IPv4 address
     * @return bool True if reserved
     */
    public static function isReserved(string $address): bool
    {
        $ip = self::parseIpv4($address);
        if ($ip === null) {
            return false;
        }

        // 0.0.0.0/8 - Current network
        if ($ip['a'] === 0) {
            return true;
        }

        // 100.64.0.0/10 - Carrier-grade NAT
        if ($ip['a'] === 100 && $ip['b'] >= 64 && $ip['b'] <= 127) {
            return true;
        }

        // 169.254.0.0/16 - Link-local
        if ($ip['a'] === 169 && $ip['b'] === 254) {
            return true;
        }

        // 192.0.0.0/24 - IETF Protocol Assignments
        if ($ip['a'] === 192 && $ip['b'] === 0 && $ip['c'] === 0) {
            return true;
        }

        // 192.0.2.0/24 - TEST-NET-1
        if ($ip['a'] === 192 && $ip['b'] === 0 && $ip['c'] === 2) {
            return true;
        }

        // 198.51.100.0/24 - TEST-NET-2
        if ($ip['a'] === 198 && $ip['b'] === 51 && $ip['c'] === 100) {
            return true;
        }

        // 203.0.113.0/24 - TEST-NET-3
        if ($ip['a'] === 203 && $ip['b'] === 0 && $ip['c'] === 113) {
            return true;
        }

        // 224.0.0.0/4 - Multicast
        if ($ip['a'] >= 224 && $ip['a'] <= 239) {
            return true;
        }

        // 240.0.0.0/4 - Reserved for future use
        if ($ip['a'] >= 240) {
            return true;
        }

        return false;
    }

    /**
     * Format an IPv4 address from octets.
     *
     * @param array{a: int, b: int, c: int, d: int} $ip The octets
     * @return string Formatted address
     */
    public static function formatIpv4(array $ip): string
    {
        return sprintf('%d.%d.%d.%d', $ip['a'], $ip['b'], $ip['c'], $ip['d']);
    }

    /**
     * Check if an IP address is valid (v4 or v6).
     *
     * @param string $address The address to check
     * @return bool True if valid
     */
    public static function isValidIp(string $address): bool
    {
        return filter_var($address, FILTER_VALIDATE_IP) !== false;
    }
}
