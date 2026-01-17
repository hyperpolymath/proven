// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlin.math.abs

/**
 * Checksum and hash utilities.
 */
object SafeChecksum {
    // CRC-32 lookup table
    private val CRC32_TABLE = IntArray(256) { i ->
        var crc = i
        repeat(8) {
            crc = if (crc and 1 != 0) (crc ushr 1) xor 0xEDB88320.toInt() else crc ushr 1
        }
        crc
    }

    /**
     * Calculate CRC-32 checksum.
     */
    fun crc32(data: ByteArray): Int {
        var crc = -1
        for (byte in data) {
            val index = (crc xor byte.toInt()) and 0xFF
            crc = (crc ushr 8) xor CRC32_TABLE[index]
        }
        return crc.inv()
    }

    /**
     * Calculate CRC-32 checksum for string.
     */
    fun crc32(string: String): Int = crc32(string.toByteArray(Charsets.UTF_8))

    /**
     * Calculate Adler-32 checksum.
     */
    fun adler32(data: ByteArray): Int {
        val mod = 65521
        var a = 1
        var b = 0

        for (byte in data) {
            a = (a + (byte.toInt() and 0xFF)) % mod
            b = (b + a) % mod
        }

        return (b shl 16) or a
    }

    /**
     * Calculate Adler-32 checksum for string.
     */
    fun adler32(string: String): Int = adler32(string.toByteArray(Charsets.UTF_8))

    /**
     * Calculate FNV-1a 32-bit hash.
     */
    fun fnv1a32(data: ByteArray): Int {
        var hash = 0x811c9dc5.toInt()
        for (byte in data) {
            hash = hash xor (byte.toInt() and 0xFF)
            hash = (hash * 0x01000193)
        }
        return hash
    }

    /**
     * Calculate FNV-1a 32-bit hash for string.
     */
    fun fnv1a32(string: String): Int = fnv1a32(string.toByteArray(Charsets.UTF_8))

    /**
     * Calculate FNV-1a 64-bit hash.
     */
    fun fnv1a64(data: ByteArray): Long {
        var hash = 0xcbf29ce484222325UL.toLong()
        for (byte in data) {
            hash = hash xor (byte.toLong() and 0xFF)
            hash = (hash * 0x00000100000001B3L)
        }
        return hash
    }

    /**
     * Calculate FNV-1a 64-bit hash for string.
     */
    fun fnv1a64(string: String): Long = fnv1a64(string.toByteArray(Charsets.UTF_8))

    /**
     * Validate using Luhn algorithm (credit cards, etc.).
     */
    fun luhnValidate(digits: String): Boolean {
        val chars = digits.filter { it.isDigit() }
        if (chars.isEmpty()) return false

        var sum = 0
        var alternate = false

        for (char in chars.reversed()) {
            val digit = char.digitToInt()
            if (alternate) {
                val doubled = digit * 2
                sum += if (doubled > 9) doubled - 9 else doubled
            } else {
                sum += digit
            }
            alternate = !alternate
        }

        return sum % 10 == 0
    }

    /**
     * Calculate Luhn check digit.
     */
    fun luhnCheckDigit(digits: String): Int? {
        val chars = digits.filter { it.isDigit() }
        if (chars.isEmpty()) return null

        var sum = 0
        var alternate = true

        for (char in chars.reversed()) {
            val digit = char.digitToInt()
            if (alternate) {
                val doubled = digit * 2
                sum += if (doubled > 9) doubled - 9 else doubled
            } else {
                sum += digit
            }
            alternate = !alternate
        }

        return (10 - (sum % 10)) % 10
    }

    /**
     * Validate ISBN-10.
     */
    fun isbn10Validate(isbn: String): Boolean {
        val chars = isbn.filter { it.isDigit() || it == 'X' || it == 'x' }
        if (chars.length != 10) return false

        var sum = 0
        for ((index, char) in chars.withIndex()) {
            val value = when {
                char == 'X' || char == 'x' -> {
                    if (index != 9) return false
                    10
                }
                else -> char.digitToInt()
            }
            sum += value * (10 - index)
        }

        return sum % 11 == 0
    }

    /**
     * Validate ISBN-13.
     */
    fun isbn13Validate(isbn: String): Boolean {
        val chars = isbn.filter { it.isDigit() }
        if (chars.length != 13) return false

        var sum = 0
        for ((index, char) in chars.withIndex()) {
            val digit = char.digitToInt()
            sum += digit * (if (index % 2 == 0) 1 else 3)
        }

        return sum % 10 == 0
    }

    /**
     * Calculate simple 8-bit checksum.
     */
    fun checksum8(data: ByteArray): Byte {
        var sum: Byte = 0
        for (byte in data) {
            sum = (sum + byte).toByte()
        }
        return sum
    }

    /**
     * Calculate simple 16-bit checksum.
     */
    fun checksum16(data: ByteArray): Short {
        var sum: Short = 0
        for (byte in data) {
            sum = (sum + (byte.toInt() and 0xFF)).toShort()
        }
        return sum
    }

    /**
     * Calculate XOR checksum.
     */
    fun xorChecksum(data: ByteArray): Byte {
        var result: Byte = 0
        for (byte in data) {
            result = (result.toInt() xor byte.toInt()).toByte()
        }
        return result
    }

    /**
     * Calculate Fletcher-16 checksum.
     */
    fun fletcher16(data: ByteArray): Short {
        var sum1 = 0
        var sum2 = 0

        for (byte in data) {
            sum1 = (sum1 + (byte.toInt() and 0xFF)) % 255
            sum2 = (sum2 + sum1) % 255
        }

        return ((sum2 shl 8) or sum1).toShort()
    }

    /**
     * Calculate Fletcher-32 checksum.
     */
    fun fletcher32(data: ByteArray): Int {
        var sum1 = 0
        var sum2 = 0
        var index = 0

        while (index + 1 < data.size) {
            val word = (data[index].toInt() and 0xFF) or ((data[index + 1].toInt() and 0xFF) shl 8)
            sum1 = (sum1 + word) % 65535
            sum2 = (sum2 + sum1) % 65535
            index += 2
        }

        if (index < data.size) {
            sum1 = (sum1 + (data[index].toInt() and 0xFF)) % 65535
            sum2 = (sum2 + sum1) % 65535
        }

        return (sum2 shl 16) or sum1
    }
}
