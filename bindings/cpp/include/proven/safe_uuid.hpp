// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file safe_uuid.hpp
 * @brief Safe UUID generation and validation following RFC 4122
 *
 * Provides type-safe UUID handling with version/variant detection,
 * parsing from canonical string format, and formatting options.
 *
 * @example
 * @code
 * #include <proven/safe_uuid.hpp>
 * #include <iostream>
 *
 * int main() {
 *     // Parse a UUID string
 *     auto uuid = proven::SafeUuid::parse("550e8400-e29b-41d4-a716-446655440000");
 *     if (uuid) {
 *         std::cout << "Version: " << static_cast<int>(uuid->version()) << "\n";
 *         std::cout << "String: " << uuid->to_string() << "\n";
 *     }
 *
 *     // Create from bytes
 *     std::array<uint8_t, 16> randomBytes = { /* random data */ };
 *     auto v4Uuid = proven::SafeUuid::v4FromBytes(randomBytes);
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_SAFE_UUID_HPP
#define PROVEN_SAFE_UUID_HPP

#include <array>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <algorithm>
#include <cctype>

namespace proven {

// ============================================================================
// UUID Version
// ============================================================================

/**
 * @brief UUID version types as defined in RFC 4122
 */
enum class UuidVersion : uint8_t {
    Nil = 0,  ///< Nil UUID (all zeros)
    V1 = 1,   ///< Time-based version
    V2 = 2,   ///< DCE Security version
    V3 = 3,   ///< Name-based (MD5)
    V4 = 4,   ///< Random
    V5 = 5,   ///< Name-based (SHA-1)
    V6 = 6,   ///< Reordered time-based (draft)
    V7 = 7,   ///< Unix epoch time-based (draft)
    V8 = 8,   ///< Custom (draft)
    Unknown = 255
};

/**
 * @brief Get string representation of UUID version
 */
[[nodiscard]] constexpr const char* uuidVersionToString(UuidVersion version) noexcept {
    switch (version) {
        case UuidVersion::Nil: return "nil";
        case UuidVersion::V1: return "v1";
        case UuidVersion::V2: return "v2";
        case UuidVersion::V3: return "v3";
        case UuidVersion::V4: return "v4";
        case UuidVersion::V5: return "v5";
        case UuidVersion::V6: return "v6";
        case UuidVersion::V7: return "v7";
        case UuidVersion::V8: return "v8";
        default: return "unknown";
    }
}

// ============================================================================
// UUID Variant
// ============================================================================

/**
 * @brief UUID variant types as defined in RFC 4122
 */
enum class UuidVariant : uint8_t {
    Ncs = 0,       ///< NCS backward compatibility
    Rfc4122 = 1,   ///< RFC 4122 (standard)
    Microsoft = 2, ///< Microsoft backward compatibility
    Future = 3     ///< Reserved for future definition
};

/**
 * @brief Get string representation of UUID variant
 */
[[nodiscard]] constexpr const char* uuidVariantToString(UuidVariant variant) noexcept {
    switch (variant) {
        case UuidVariant::Ncs: return "ncs";
        case UuidVariant::Rfc4122: return "rfc4122";
        case UuidVariant::Microsoft: return "microsoft";
        case UuidVariant::Future: return "future";
        default: return "unknown";
    }
}

// ============================================================================
// UUID Class
// ============================================================================

/**
 * @brief A validated UUID (128 bits)
 *
 * Stores UUID as a 16-byte array and provides version/variant detection,
 * formatting, and comparison operations.
 */
class Uuid {
public:
    /// Size of UUID in bytes
    static constexpr size_t ByteSize = 16;

    /// Size of canonical string representation
    static constexpr size_t StringSize = 36;

    /// The nil UUID (all zeros)
    static const Uuid Nil;

    /// DNS namespace UUID (6ba7b810-9dad-11d1-80b4-00c04fd430c8)
    static const Uuid NamespaceDns;

    /// URL namespace UUID (6ba7b811-9dad-11d1-80b4-00c04fd430c8)
    static const Uuid NamespaceUrl;

    /// OID namespace UUID (6ba7b812-9dad-11d1-80b4-00c04fd430c8)
    static const Uuid NamespaceOid;

    /// X.500 DN namespace UUID (6ba7b814-9dad-11d1-80b4-00c04fd430c8)
    static const Uuid NamespaceX500;

    /**
     * @brief Default constructor creates nil UUID
     */
    constexpr Uuid() noexcept : bytes_{{}} {}

    /**
     * @brief Construct from byte array
     */
    constexpr explicit Uuid(const std::array<uint8_t, ByteSize>& bytes) noexcept
        : bytes_(bytes) {}

    /**
     * @brief Construct from individual bytes
     */
    constexpr Uuid(
        uint8_t b0, uint8_t b1, uint8_t b2, uint8_t b3,
        uint8_t b4, uint8_t b5, uint8_t b6, uint8_t b7,
        uint8_t b8, uint8_t b9, uint8_t b10, uint8_t b11,
        uint8_t b12, uint8_t b13, uint8_t b14, uint8_t b15
    ) noexcept
        : bytes_{{b0, b1, b2, b3, b4, b5, b6, b7,
                  b8, b9, b10, b11, b12, b13, b14, b15}} {}

    /**
     * @brief Get the underlying bytes
     */
    [[nodiscard]] constexpr const std::array<uint8_t, ByteSize>& bytes() const noexcept {
        return bytes_;
    }

    /**
     * @brief Get pointer to underlying data
     */
    [[nodiscard]] constexpr const uint8_t* data() const noexcept {
        return bytes_.data();
    }

    /**
     * @brief Access individual byte
     */
    [[nodiscard]] constexpr uint8_t operator[](size_t index) const noexcept {
        return bytes_[index];
    }

    /**
     * @brief Get the UUID version
     */
    [[nodiscard]] constexpr UuidVersion version() const noexcept {
        const uint8_t versionNibble = (bytes_[6] >> 4) & 0x0F;
        switch (versionNibble) {
            case 0: return isNil() ? UuidVersion::Nil : UuidVersion::Unknown;
            case 1: return UuidVersion::V1;
            case 2: return UuidVersion::V2;
            case 3: return UuidVersion::V3;
            case 4: return UuidVersion::V4;
            case 5: return UuidVersion::V5;
            case 6: return UuidVersion::V6;
            case 7: return UuidVersion::V7;
            case 8: return UuidVersion::V8;
            default: return UuidVersion::Unknown;
        }
    }

    /**
     * @brief Get the UUID variant
     */
    [[nodiscard]] constexpr UuidVariant variant() const noexcept {
        const uint8_t variantByte = bytes_[8];
        if ((variantByte >> 7) == 0) {
            return UuidVariant::Ncs;
        } else if ((variantByte >> 6) == 0b10) {
            return UuidVariant::Rfc4122;
        } else if ((variantByte >> 5) == 0b110) {
            return UuidVariant::Microsoft;
        } else {
            return UuidVariant::Future;
        }
    }

    /**
     * @brief Check if this is the nil UUID
     */
    [[nodiscard]] constexpr bool isNil() const noexcept {
        for (const auto& byte : bytes_) {
            if (byte != 0) return false;
        }
        return true;
    }

    /**
     * @brief Check if this is an RFC 4122 compliant UUID
     */
    [[nodiscard]] constexpr bool isRfc4122() const noexcept {
        return variant() == UuidVariant::Rfc4122;
    }

    /**
     * @brief Format as canonical string (lowercase)
     *
     * Format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
     */
    [[nodiscard]] std::string toString() const {
        static constexpr char hexChars[] = "0123456789abcdef";
        std::string result;
        result.reserve(StringSize);

        for (size_t i = 0; i < ByteSize; ++i) {
            if (i == 4 || i == 6 || i == 8 || i == 10) {
                result += '-';
            }
            result += hexChars[(bytes_[i] >> 4) & 0x0F];
            result += hexChars[bytes_[i] & 0x0F];
        }

        return result;
    }

    /**
     * @brief Format as uppercase string
     */
    [[nodiscard]] std::string toStringUpper() const {
        static constexpr char hexChars[] = "0123456789ABCDEF";
        std::string result;
        result.reserve(StringSize);

        for (size_t i = 0; i < ByteSize; ++i) {
            if (i == 4 || i == 6 || i == 8 || i == 10) {
                result += '-';
            }
            result += hexChars[(bytes_[i] >> 4) & 0x0F];
            result += hexChars[bytes_[i] & 0x0F];
        }

        return result;
    }

    /**
     * @brief Format as URN (urn:uuid:...)
     */
    [[nodiscard]] std::string toUrn() const {
        return "urn:uuid:" + toString();
    }

    /**
     * @brief Format as hex string without dashes
     */
    [[nodiscard]] std::string toHex() const {
        static constexpr char hexChars[] = "0123456789abcdef";
        std::string result;
        result.reserve(ByteSize * 2);

        for (const auto& byte : bytes_) {
            result += hexChars[(byte >> 4) & 0x0F];
            result += hexChars[byte & 0x0F];
        }

        return result;
    }

    // Comparison operators
    [[nodiscard]] constexpr bool operator==(const Uuid& other) const noexcept {
        return bytes_ == other.bytes_;
    }

    [[nodiscard]] constexpr bool operator!=(const Uuid& other) const noexcept {
        return bytes_ != other.bytes_;
    }

    [[nodiscard]] constexpr bool operator<(const Uuid& other) const noexcept {
        return bytes_ < other.bytes_;
    }

    [[nodiscard]] constexpr bool operator<=(const Uuid& other) const noexcept {
        return bytes_ <= other.bytes_;
    }

    [[nodiscard]] constexpr bool operator>(const Uuid& other) const noexcept {
        return bytes_ > other.bytes_;
    }

    [[nodiscard]] constexpr bool operator>=(const Uuid& other) const noexcept {
        return bytes_ >= other.bytes_;
    }

private:
    std::array<uint8_t, ByteSize> bytes_;
};

// Static member definitions
inline const Uuid Uuid::Nil = Uuid();

inline const Uuid Uuid::NamespaceDns = Uuid(
    0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
);

inline const Uuid Uuid::NamespaceUrl = Uuid(
    0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
);

inline const Uuid Uuid::NamespaceOid = Uuid(
    0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
);

inline const Uuid Uuid::NamespaceX500 = Uuid(
    0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
);

// ============================================================================
// SafeUuid Operations
// ============================================================================

/**
 * @brief Safe UUID operations
 *
 * Provides parsing, validation, and generation helpers for UUIDs.
 */
class SafeUuid {
public:
    /**
     * @brief Parse UUID from canonical string format
     *
     * Accepts format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
     * Case-insensitive.
     *
     * @param input The string to parse
     * @return The parsed UUID, or std::nullopt if invalid
     */
    [[nodiscard]] static std::optional<Uuid> parse(std::string_view input) noexcept {
        if (input.size() != Uuid::StringSize) {
            return std::nullopt;
        }

        // Validate dash positions
        if (input[8] != '-' || input[13] != '-' ||
            input[18] != '-' || input[23] != '-') {
            return std::nullopt;
        }

        std::array<uint8_t, Uuid::ByteSize> bytes{};
        size_t byteIndex = 0;

        for (size_t i = 0; i < input.size(); ++i) {
            if (input[i] == '-') continue;

            if (byteIndex >= Uuid::ByteSize) {
                return std::nullopt;
            }

            const auto highNibble = hexCharToNibble(input[i]);
            if (!highNibble) return std::nullopt;

            ++i;
            if (i >= input.size()) return std::nullopt;

            const auto lowNibble = hexCharToNibble(input[i]);
            if (!lowNibble) return std::nullopt;

            bytes[byteIndex++] = (*highNibble << 4) | *lowNibble;
        }

        if (byteIndex != Uuid::ByteSize) {
            return std::nullopt;
        }

        return Uuid(bytes);
    }

    /**
     * @brief Parse UUID from hex string (no dashes)
     *
     * @param hex 32-character hex string
     * @return The parsed UUID, or std::nullopt if invalid
     */
    [[nodiscard]] static std::optional<Uuid> parseHex(std::string_view hex) noexcept {
        if (hex.size() != Uuid::ByteSize * 2) {
            return std::nullopt;
        }

        std::array<uint8_t, Uuid::ByteSize> bytes{};

        for (size_t i = 0; i < Uuid::ByteSize; ++i) {
            const auto highNibble = hexCharToNibble(hex[i * 2]);
            if (!highNibble) return std::nullopt;

            const auto lowNibble = hexCharToNibble(hex[i * 2 + 1]);
            if (!lowNibble) return std::nullopt;

            bytes[i] = (*highNibble << 4) | *lowNibble;
        }

        return Uuid(bytes);
    }

    /**
     * @brief Check if string is valid UUID format
     */
    [[nodiscard]] static bool isValid(std::string_view input) noexcept {
        return parse(input).has_value();
    }

    /**
     * @brief Create a version 4 (random) UUID from provided random bytes
     *
     * Sets the version and variant bits appropriately.
     *
     * @param randomBytes 16 bytes of random data
     * @return A valid v4 UUID
     */
    [[nodiscard]] static Uuid v4FromBytes(std::array<uint8_t, Uuid::ByteSize> randomBytes) noexcept {
        // Set version to 4 (bits 12-15 of time_hi_and_version)
        randomBytes[6] = (randomBytes[6] & 0x0F) | 0x40;
        // Set variant to RFC 4122 (bits 6-7 of clock_seq_hi_and_reserved)
        randomBytes[8] = (randomBytes[8] & 0x3F) | 0x80;

        return Uuid(randomBytes);
    }

    /**
     * @brief Create a version 7 (Unix epoch time-based) UUID from timestamp and random bytes
     *
     * @param unixTimestampMs Unix timestamp in milliseconds
     * @param randomBytes 10 bytes of random data (only 74 bits used)
     * @return A valid v7 UUID
     */
    [[nodiscard]] static Uuid v7FromTimestamp(
        uint64_t unixTimestampMs,
        const std::array<uint8_t, 10>& randomBytes
    ) noexcept {
        std::array<uint8_t, Uuid::ByteSize> bytes{};

        // First 48 bits: Unix timestamp in milliseconds (big-endian)
        bytes[0] = static_cast<uint8_t>((unixTimestampMs >> 40) & 0xFF);
        bytes[1] = static_cast<uint8_t>((unixTimestampMs >> 32) & 0xFF);
        bytes[2] = static_cast<uint8_t>((unixTimestampMs >> 24) & 0xFF);
        bytes[3] = static_cast<uint8_t>((unixTimestampMs >> 16) & 0xFF);
        bytes[4] = static_cast<uint8_t>((unixTimestampMs >> 8) & 0xFF);
        bytes[5] = static_cast<uint8_t>(unixTimestampMs & 0xFF);

        // Version 7 and random bits
        bytes[6] = 0x70 | (randomBytes[0] & 0x0F);
        bytes[7] = randomBytes[1];

        // Variant (RFC 4122) and random bits
        bytes[8] = 0x80 | (randomBytes[2] & 0x3F);

        // Remaining random bytes
        for (size_t i = 3; i < 10; ++i) {
            bytes[6 + i] = randomBytes[i];
        }

        return Uuid(bytes);
    }

    /**
     * @brief Get the nil UUID
     */
    [[nodiscard]] static constexpr Uuid nil() noexcept {
        return Uuid();
    }

private:
    /**
     * @brief Convert hex character to nibble value
     */
    [[nodiscard]] static std::optional<uint8_t> hexCharToNibble(char c) noexcept {
        if (c >= '0' && c <= '9') return static_cast<uint8_t>(c - '0');
        if (c >= 'a' && c <= 'f') return static_cast<uint8_t>(c - 'a' + 10);
        if (c >= 'A' && c <= 'F') return static_cast<uint8_t>(c - 'A' + 10);
        return std::nullopt;
    }
};

} // namespace proven

// Hash support for std::unordered_map/set
namespace std {
template <>
struct hash<proven::Uuid> {
    size_t operator()(const proven::Uuid& uuid) const noexcept {
        // FNV-1a hash
        size_t hash = 14695981039346656037ULL;
        for (const auto& byte : uuid.bytes()) {
            hash ^= static_cast<size_t>(byte);
            hash *= 1099511628211ULL;
        }
        return hash;
    }
};
} // namespace std

#endif // PROVEN_SAFE_UUID_HPP
