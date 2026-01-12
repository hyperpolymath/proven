// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.security.MessageDigest

/**
 * Hex encoding/decoding errors.
 */
sealed class HexError {
    data class InvalidCharacter(val character: Char, val position: Int) : HexError()
    data class OddLength(val length: Int) : HexError()
    data object EmptyInput : HexError()
    data class DecodingFailed(val message: String) : HexError()

    override fun toString(): String = when (this) {
        is InvalidCharacter -> "Invalid hex character '$character' at position $position"
        is OddLength -> "Hex string has odd length: $length"
        is EmptyInput -> "Empty input"
        is DecodingFailed -> "Hex decoding failed: $message"
    }
}

/**
 * Safe hexadecimal encoding and decoding utilities.
 */
object SafeHex {

    private val HEX_CHARS_LOWER = "0123456789abcdef".toCharArray()
    private val HEX_CHARS_UPPER = "0123456789ABCDEF".toCharArray()

    /**
     * Check if a character is a valid hexadecimal character.
     *
     * @param char The character to check.
     * @return True if the character is 0-9, a-f, or A-F.
     */
    fun isHexChar(char: Char): Boolean =
        char in '0'..'9' || char in 'a'..'f' || char in 'A'..'F'

    /**
     * Check if a string contains only valid hexadecimal characters.
     *
     * @param hexString The string to validate.
     * @return True if all characters are valid hex characters.
     */
    fun isValidHex(hexString: String): Boolean =
        hexString.isNotEmpty() && hexString.all { isHexChar(it) }

    /**
     * Check if a string is valid hex with even length (required for byte decoding).
     *
     * @param hexString The string to validate.
     * @return True if valid hex with even length.
     */
    fun isValidHexBytes(hexString: String): Boolean =
        hexString.length % 2 == 0 && isValidHex(hexString)

    /**
     * Validate a hex string.
     *
     * @param hexString The string to validate.
     * @return Result containing the validated string or an error.
     */
    fun validate(hexString: String): Result<String> = runCatching {
        if (hexString.isEmpty()) {
            throw IllegalArgumentException(HexError.EmptyInput.toString())
        }
        hexString.forEachIndexed { index, char ->
            if (!isHexChar(char)) {
                throw IllegalArgumentException(
                    HexError.InvalidCharacter(char, index).toString()
                )
            }
        }
        hexString
    }

    /**
     * Validate a hex string for byte decoding (must have even length).
     *
     * @param hexString The string to validate.
     * @return Result containing the validated string or an error.
     */
    fun validateForBytes(hexString: String): Result<String> = runCatching {
        if (hexString.length % 2 != 0) {
            throw IllegalArgumentException(HexError.OddLength(hexString.length).toString())
        }
        validate(hexString).getOrThrow()
    }

    /**
     * Encode a byte array to a lowercase hex string.
     *
     * @param bytes The bytes to encode.
     * @return The lowercase hex string.
     */
    fun encode(bytes: ByteArray): String {
        val result = CharArray(bytes.size * 2)
        bytes.forEachIndexed { index, byte ->
            val value = byte.toInt() and 0xFF
            result[index * 2] = HEX_CHARS_LOWER[value ushr 4]
            result[index * 2 + 1] = HEX_CHARS_LOWER[value and 0x0F]
        }
        return String(result)
    }

    /**
     * Encode a byte array to an uppercase hex string.
     *
     * @param bytes The bytes to encode.
     * @return The uppercase hex string.
     */
    fun encodeUpper(bytes: ByteArray): String {
        val result = CharArray(bytes.size * 2)
        bytes.forEachIndexed { index, byte ->
            val value = byte.toInt() and 0xFF
            result[index * 2] = HEX_CHARS_UPPER[value ushr 4]
            result[index * 2 + 1] = HEX_CHARS_UPPER[value and 0x0F]
        }
        return String(result)
    }

    /**
     * Encode a single byte to a two-character lowercase hex string.
     *
     * @param byte The byte to encode.
     * @return The two-character hex string.
     */
    fun encodeByte(byte: Byte): String {
        val value = byte.toInt() and 0xFF
        return "${HEX_CHARS_LOWER[value ushr 4]}${HEX_CHARS_LOWER[value and 0x0F]}"
    }

    /**
     * Decode a hex string to a byte array.
     *
     * @param hexString The hex string to decode.
     * @return Result containing the decoded bytes or an error.
     */
    fun decode(hexString: String): Result<ByteArray> = runCatching {
        if (hexString.isEmpty()) {
            throw IllegalArgumentException(HexError.EmptyInput.toString())
        }
        if (hexString.length % 2 != 0) {
            throw IllegalArgumentException(HexError.OddLength(hexString.length).toString())
        }

        val result = ByteArray(hexString.length / 2)
        for (i in result.indices) {
            val high = hexCharToNibble(hexString[i * 2], i * 2)
            val low = hexCharToNibble(hexString[i * 2 + 1], i * 2 + 1)
            result[i] = ((high shl 4) or low).toByte()
        }
        result
    }

    /**
     * Decode a hex string to a byte array, returning null on error.
     *
     * @param hexString The hex string to decode.
     * @return The decoded bytes or null if invalid.
     */
    fun decodeOrNull(hexString: String): ByteArray? = decode(hexString).getOrNull()

    /**
     * Compare two hex strings in constant time to prevent timing attacks.
     * This function is timing-attack resistant.
     *
     * @param a First hex string.
     * @param b Second hex string.
     * @return True if the strings are equal (case-insensitive).
     */
    fun constantTimeEqual(a: String, b: String): Boolean {
        val aLower = a.lowercase()
        val bLower = b.lowercase()

        if (aLower.length != bLower.length) {
            return false
        }

        var result = 0
        for (i in aLower.indices) {
            result = result or (aLower[i].code xor bLower[i].code)
        }
        return result == 0
    }

    /**
     * Compare two byte arrays in constant time to prevent timing attacks.
     * Uses Java's built-in MessageDigest.isEqual for guaranteed constant-time comparison.
     *
     * @param a First byte array.
     * @param b Second byte array.
     * @return True if the arrays are equal.
     */
    fun constantTimeEqual(a: ByteArray, b: ByteArray): Boolean =
        MessageDigest.isEqual(a, b)

    /**
     * Format hex string with spaces between bytes (e.g., "01 02 03").
     *
     * @param hexString The hex string to format.
     * @return The formatted string or null if input is invalid.
     */
    fun formatSpaced(hexString: String): String? {
        if (!isValidHexBytes(hexString)) return null
        return hexString.chunked(2).joinToString(" ")
    }

    /**
     * Format hex string with colons between bytes (e.g., "01:02:03").
     *
     * @param hexString The hex string to format.
     * @return The formatted string or null if input is invalid.
     */
    fun formatColons(hexString: String): String? {
        if (!isValidHexBytes(hexString)) return null
        return hexString.chunked(2).joinToString(":")
    }

    /**
     * Format hex string with 0x prefix (e.g., "0x0102").
     *
     * @param hexString The hex string to format.
     * @return The formatted string.
     */
    fun format0x(hexString: String): String = "0x$hexString"

    /**
     * Convert hex string to lowercase.
     *
     * @param hexString The hex string to convert.
     * @return The lowercase hex string.
     */
    fun toLower(hexString: String): String = hexString.lowercase()

    /**
     * Convert hex string to uppercase.
     *
     * @param hexString The hex string to convert.
     * @return The uppercase hex string.
     */
    fun toUpper(hexString: String): String = hexString.uppercase()

    /**
     * Get the number of bytes represented by a hex string.
     *
     * @param hexString The hex string.
     * @return The byte count.
     */
    fun byteCount(hexString: String): Int = hexString.length / 2

    /**
     * Convert an integer to a hex string with minimum width (zero-padded).
     *
     * @param value The integer value to convert.
     * @param minWidth The minimum width of the result (in hex characters).
     * @return The hex string.
     */
    fun intToHex(value: Long, minWidth: Int = 0): String {
        val absValue = kotlin.math.abs(value)
        val hex = java.lang.Long.toHexString(absValue)
        return if (hex.length < minWidth) {
            hex.padStart(minWidth, '0')
        } else {
            hex
        }
    }

    /**
     * Parse a hex string to a Long.
     *
     * @param hexString The hex string to parse.
     * @return Result containing the parsed Long or an error.
     */
    fun hexToLong(hexString: String): Result<Long> = runCatching {
        java.lang.Long.parseLong(hexString, 16)
    }

    /**
     * Parse a hex string to a Long, returning null on error.
     *
     * @param hexString The hex string to parse.
     * @return The parsed Long or null if invalid.
     */
    fun hexToLongOrNull(hexString: String): Long? = hexToLong(hexString).getOrNull()

    private fun hexCharToNibble(char: Char, position: Int): Int {
        return when (char) {
            in '0'..'9' -> char - '0'
            in 'a'..'f' -> char - 'a' + 10
            in 'A'..'F' -> char - 'A' + 10
            else -> throw IllegalArgumentException(
                HexError.InvalidCharacter(char, position).toString()
            )
        }
    }
}
