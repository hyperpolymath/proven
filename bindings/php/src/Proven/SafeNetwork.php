<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeNetwork - FFI wrapper for proven_network_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe network/IP address operations via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeNetwork
{
    /**
     * Parse an IPv4 address string.
     * Returns an array of 4 octets on success, or null on error.
     *
     * @return int[]|null Array of 4 octets, or null on invalid input.
     */
    public static function parseIpv4(string $ip): ?array
    {
        $result = FFI::getLib()->proven_network_parse_ipv4($ip, strlen($ip));
        if ($result->status !== 0) {
            return null;
        }
        $octets = [];
        for ($i = 0; $i < 4; $i++) {
            $octets[] = $result->octets[$i];
        }
        return $octets;
    }

    /**
     * Check if an IPv4 address is in a private range.
     * Requires an IPv4Address struct (use parseIpv4 first and build the struct).
     */
    public static function ipv4IsPrivate(array $octets): bool
    {
        $addr = FFI::getLib()->new('IPv4Address');
        for ($i = 0; $i < 4; $i++) {
            $addr->octets[$i] = $octets[$i];
        }
        return FFI::getLib()->proven_network_ipv4_is_private($addr);
    }

    /**
     * Check if an IPv4 address is a loopback address.
     */
    public static function ipv4IsLoopback(array $octets): bool
    {
        $addr = FFI::getLib()->new('IPv4Address');
        for ($i = 0; $i < 4; $i++) {
            $addr->octets[$i] = $octets[$i];
        }
        return FFI::getLib()->proven_network_ipv4_is_loopback($addr);
    }
}
