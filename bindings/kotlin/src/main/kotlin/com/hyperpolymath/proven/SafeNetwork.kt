// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * Safe network operations for IP address validation and classification.
 */
object SafeNetwork {
    /**
     * Represents an IPv4 address.
     */
    data class IPv4(
        val a: UByte,
        val b: UByte,
        val c: UByte,
        val d: UByte
    ) {
        override fun toString(): String = "$a.$b.$c.$d"
    }

    /**
     * Parse an IPv4 address string.
     */
    fun parseIPv4(address: String): IPv4? {
        val parts = address.split(".")
        if (parts.size != 4) return null

        val octets = mutableListOf<UByte>()
        for (part in parts) {
            val value = part.toIntOrNull() ?: return null
            if (value < 0 || value > 255) return null
            octets.add(value.toUByte())
        }

        return IPv4(octets[0], octets[1], octets[2], octets[3])
    }

    /**
     * Check if a string is a valid IPv4 address.
     */
    fun isValidIPv4(address: String): Boolean = parseIPv4(address) != null

    /**
     * Check if an IPv4 address is in a private range.
     */
    fun isPrivate(address: String): Boolean {
        val ip = parseIPv4(address) ?: return false

        // 10.0.0.0/8
        if (ip.a.toInt() == 10) return true
        // 172.16.0.0/12
        if (ip.a.toInt() == 172 && ip.b.toInt() in 16..31) return true
        // 192.168.0.0/16
        if (ip.a.toInt() == 192 && ip.b.toInt() == 168) return true

        return false
    }

    /**
     * Check if an IPv4 address is a loopback address (127.0.0.0/8).
     */
    fun isLoopback(address: String): Boolean {
        val ip = parseIPv4(address) ?: return false
        return ip.a.toInt() == 127
    }

    /**
     * Check if an IPv4 address is public (not private or loopback).
     */
    fun isPublic(address: String): Boolean =
        isValidIPv4(address) && !isPrivate(address) && !isLoopback(address)

    /**
     * Format an IPv4 address as a string.
     */
    fun formatIPv4(ip: IPv4): String = ip.toString()
}
