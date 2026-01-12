// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.security.MessageDigest

/**
 * Cryptographic safety operations with constant-time guarantees.
 */
object SafeCrypto {
    /**
     * Compare two byte arrays in constant time to prevent timing attacks.
     */
    fun constantTimeCompare(a: ByteArray, b: ByteArray): Boolean {
        if (a.size != b.size) return false
        if (a.isEmpty()) return true

        var result: Int = 0
        for (i in a.indices) {
            result = result or (a[i].toInt() xor b[i].toInt())
        }
        return result == 0
    }

    /**
     * Compare two strings in constant time to prevent timing attacks.
     */
    fun constantTimeCompare(a: String, b: String): Boolean =
        constantTimeCompare(a.toByteArray(Charsets.UTF_8), b.toByteArray(Charsets.UTF_8))

    /**
     * Use Java's built-in constant time comparison (MessageDigest.isEqual).
     * This is the recommended approach for JVM.
     */
    fun constantTimeCompareBuiltin(a: ByteArray, b: ByteArray): Boolean =
        MessageDigest.isEqual(a, b)

    /**
     * Securely zero out a byte array to prevent data leakage.
     */
    fun secureZero(data: ByteArray) {
        java.util.Arrays.fill(data, 0.toByte())
    }

    /**
     * Securely zero out a char array to prevent data leakage.
     * Useful for password handling.
     */
    fun secureZero(data: CharArray) {
        java.util.Arrays.fill(data, '\u0000')
    }
}
