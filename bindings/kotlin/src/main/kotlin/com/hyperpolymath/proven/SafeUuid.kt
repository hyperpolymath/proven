// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * UUID version types following RFC 4122.
 */
enum class UuidVersion {
    /** Time-based UUID. */
    V1,
    /** DCE Security UUID. */
    V2,
    /** Name-based UUID using MD5 hashing. */
    V3,
    /** Random UUID. */
    V4,
    /** Name-based UUID using SHA-1 hashing. */
    V5,
    /** Nil UUID (all zeros). */
    NIL
}

/**
 * UUID variant types following RFC 4122.
 */
enum class UuidVariant {
    /** Reserved for NCS backward compatibility. */
    NCS,
    /** RFC 4122 compliant. */
    RFC4122,
    /** Reserved for Microsoft backward compatibility. */
    MICROSOFT,
    /** Reserved for future use. */
    FUTURE
}

/**
 * A validated UUID (128 bits) following RFC 4122.
 *
 * @property bytes The 16-byte representation of the UUID.
 */
data class Uuid(val bytes: ByteArray) {

    init {
        require(bytes.size == 16) { "UUID must be exactly 16 bytes" }
    }

    /**
     * Get the UUID version.
     */
    val version: UuidVersion
        get() {
            val versionNibble = (bytes[6].toInt() shr 4) and 0x0F
            return when (versionNibble) {
                1 -> UuidVersion.V1
                2 -> UuidVersion.V2
                3 -> UuidVersion.V3
                4 -> UuidVersion.V4
                5 -> UuidVersion.V5
                else -> UuidVersion.NIL
            }
        }

    /**
     * Get the UUID variant.
     */
    val variant: UuidVariant
        get() {
            val byte = bytes[8].toInt() and 0xFF
            return when {
                (byte shr 7) == 0 -> UuidVariant.NCS
                (byte shr 6) == 0b10 -> UuidVariant.RFC4122
                (byte shr 5) == 0b110 -> UuidVariant.MICROSOFT
                else -> UuidVariant.FUTURE
            }
        }

    /**
     * Check if this is the nil UUID (all zeros).
     */
    val isNil: Boolean
        get() = bytes.all { it == 0.toByte() }

    /**
     * Format as canonical string (8-4-4-4-12 format).
     */
    fun format(): String {
        val hex = bytes.joinToString("") { "%02x".format(it) }
        return "${hex.substring(0, 8)}-${hex.substring(8, 12)}-${hex.substring(12, 16)}-${hex.substring(16, 20)}-${hex.substring(20, 32)}"
    }

    /**
     * Format as URN.
     */
    fun toUrn(): String = "urn:uuid:${format()}"

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is Uuid) return false
        return bytes.contentEquals(other.bytes)
    }

    override fun hashCode(): Int = bytes.contentHashCode()

    override fun toString(): String = format()

    companion object {
        /** The nil UUID (all zeros). */
        val NIL: Uuid = Uuid(ByteArray(16))

        /** DNS namespace UUID. */
        val NAMESPACE_DNS: Uuid = Uuid(
            byteArrayOf(
                0x6b, 0xa7.toByte(), 0xb8.toByte(), 0x10,
                0x9d.toByte(), 0xad.toByte(), 0x11, 0xd1.toByte(),
                0x80.toByte(), 0xb4.toByte(), 0x00, 0xc0.toByte(),
                0x4f, 0xd4.toByte(), 0x30, 0xc8.toByte()
            )
        )

        /** URL namespace UUID. */
        val NAMESPACE_URL: Uuid = Uuid(
            byteArrayOf(
                0x6b, 0xa7.toByte(), 0xb8.toByte(), 0x11,
                0x9d.toByte(), 0xad.toByte(), 0x11, 0xd1.toByte(),
                0x80.toByte(), 0xb4.toByte(), 0x00, 0xc0.toByte(),
                0x4f, 0xd4.toByte(), 0x30, 0xc8.toByte()
            )
        )

        /**
         * Parse UUID from canonical string format (8-4-4-4-12).
         *
         * @param uuidString The UUID string to parse.
         * @return Result containing the parsed UUID or an error.
         */
        fun parse(uuidString: String): Result<Uuid> = runCatching {
            val trimmed = uuidString.trim()
            if (trimmed.length != 36) {
                throw IllegalArgumentException("UUID must be 36 characters, got ${trimmed.length}")
            }

            val dashPositions = listOf(8, 13, 18, 23)
            dashPositions.forEach { pos ->
                if (trimmed[pos] != '-') {
                    throw IllegalArgumentException("Invalid UUID format: expected dash at position $pos")
                }
            }

            val hexString = trimmed.filter { it != '-' }
            if (hexString.length != 32) {
                throw IllegalArgumentException("Invalid UUID hex length")
            }

            val bytes = ByteArray(16) { index ->
                val hexByte = hexString.substring(index * 2, index * 2 + 2)
                hexByte.toInt(16).toByte()
            }

            Uuid(bytes)
        }

        /**
         * Check if string is valid UUID format.
         *
         * @param uuidString The string to validate.
         * @return True if the string is a valid UUID format.
         */
        fun isValid(uuidString: String): Boolean = parse(uuidString).isSuccess

        /**
         * Create UUID from 16 bytes.
         *
         * @param bytes The 16-byte array.
         * @return Result containing the UUID or an error.
         */
        fun fromBytes(bytes: ByteArray): Result<Uuid> = runCatching {
            require(bytes.size == 16) { "UUID must be exactly 16 bytes" }
            Uuid(bytes.copyOf())
        }

        /**
         * Generate a v4 (random) UUID from provided random bytes.
         * Sets the appropriate version and variant bits.
         *
         * @param randomBytes 16 random bytes to use as the UUID base.
         * @return A v4 UUID with correct version and variant bits.
         */
        fun v4FromBytes(randomBytes: ByteArray): Result<Uuid> = runCatching {
            require(randomBytes.size == 16) { "Random bytes must be exactly 16 bytes" }
            val bytes = randomBytes.copyOf()
            // Set version to 4
            bytes[6] = ((bytes[6].toInt() and 0x0F) or 0x40).toByte()
            // Set variant to RFC 4122
            bytes[8] = ((bytes[8].toInt() and 0x3F) or 0x80).toByte()
            Uuid(bytes)
        }
    }
}

/**
 * Safe UUID operations.
 */
object SafeUuid {
    /**
     * Parse UUID from canonical string format.
     *
     * @param uuidString The UUID string to parse.
     * @return The parsed UUID or null if invalid.
     */
    fun parse(uuidString: String): Uuid? = Uuid.parse(uuidString).getOrNull()

    /**
     * Parse UUID from canonical string format, returning Result.
     *
     * @param uuidString The UUID string to parse.
     * @return Result containing the parsed UUID or an error.
     */
    fun parseResult(uuidString: String): Result<Uuid> = Uuid.parse(uuidString)

    /**
     * Check if string is valid UUID format.
     *
     * @param uuidString The string to validate.
     * @return True if the string is a valid UUID format.
     */
    fun isValid(uuidString: String): Boolean = Uuid.isValid(uuidString)

    /**
     * Create UUID from bytes.
     *
     * @param bytes The 16-byte array.
     * @return The UUID or null if invalid.
     */
    fun fromBytes(bytes: ByteArray): Uuid? = Uuid.fromBytes(bytes).getOrNull()

    /**
     * Generate a v4 (random) UUID from provided random bytes.
     *
     * @param randomBytes 16 random bytes to use as the UUID base.
     * @return A v4 UUID or null if invalid.
     */
    fun v4FromBytes(randomBytes: ByteArray): Uuid? = Uuid.v4FromBytes(randomBytes).getOrNull()

    /**
     * Format UUID as canonical string.
     *
     * @param uuid The UUID to format.
     * @return The formatted string (8-4-4-4-12 format).
     */
    fun format(uuid: Uuid): String = uuid.format()

    /**
     * Format UUID as URN.
     *
     * @param uuid The UUID to format.
     * @return The URN string.
     */
    fun toUrn(uuid: Uuid): String = uuid.toUrn()
}
