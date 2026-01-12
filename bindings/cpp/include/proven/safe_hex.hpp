// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file safe_hex.hpp
 * @brief Safe hexadecimal encoding and decoding
 *
 * Provides type-safe hex encoding/decoding with validation,
 * constant-time comparison for cryptographic use cases,
 * and various formatting options.
 *
 * @example
 * @code
 * #include <proven/safe_hex.hpp>
 * #include <iostream>
 * #include <vector>
 *
 * int main() {
 *     using namespace proven;
 *
 *     // Encode bytes to hex
 *     std::vector<uint8_t> data = {0xFF, 0x00, 0xAB};
 *     std::string hex = SafeHex::encode(data);
 *     std::cout << "Hex: " << hex << "\n";  // "ff00ab"
 *
 *     // Decode hex to bytes
 *     auto decoded = SafeHex::decode("deadbeef");
 *     if (decoded) {
 *         std::cout << "Decoded " << decoded->size() << " bytes\n";
 *     }
 *
 *     // Constant-time comparison (for cryptographic use)
 *     if (SafeHex::constantTimeEqual("abc123", "ABC123")) {
 *         std::cout << "Match!\n";
 *     }
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_SAFE_HEX_HPP
#define PROVEN_SAFE_HEX_HPP

#include <array>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>
#include <algorithm>

namespace proven {

/**
 * @brief Safe hexadecimal encoding and decoding utilities
 *
 * All operations are designed to be safe against common errors
 * and provide clear validation of input.
 */
namespace SafeHex {

// ============================================================================
// Character Validation and Conversion
// ============================================================================

/**
 * @brief Check if character is a valid hex digit
 */
[[nodiscard]] constexpr bool isHexChar(char c) noexcept {
    return (c >= '0' && c <= '9') ||
           (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}

/**
 * @brief Convert hex character to nibble value (0-15)
 *
 * @param c The hex character
 * @return The nibble value, or std::nullopt if invalid
 */
[[nodiscard]] constexpr std::optional<uint8_t> hexCharToNibble(char c) noexcept {
    if (c >= '0' && c <= '9') return static_cast<uint8_t>(c - '0');
    if (c >= 'a' && c <= 'f') return static_cast<uint8_t>(c - 'a' + 10);
    if (c >= 'A' && c <= 'F') return static_cast<uint8_t>(c - 'A' + 10);
    return std::nullopt;
}

/**
 * @brief Convert nibble value to lowercase hex character
 */
[[nodiscard]] constexpr char nibbleToHexChar(uint8_t nibble) noexcept {
    return (nibble < 10) ? static_cast<char>('0' + nibble)
                         : static_cast<char>('a' + nibble - 10);
}

/**
 * @brief Convert nibble value to uppercase hex character
 */
[[nodiscard]] constexpr char nibbleToHexCharUpper(uint8_t nibble) noexcept {
    return (nibble < 10) ? static_cast<char>('0' + nibble)
                         : static_cast<char>('A' + nibble - 10);
}

// ============================================================================
// Encoding
// ============================================================================

/**
 * @brief Encode bytes to lowercase hex string
 *
 * @param data Pointer to byte data
 * @param length Number of bytes to encode
 * @return Lowercase hex string
 */
[[nodiscard]] inline std::string encode(const uint8_t* data, size_t length) {
    std::string result;
    result.reserve(length * 2);

    for (size_t i = 0; i < length; ++i) {
        result += nibbleToHexChar((data[i] >> 4) & 0x0F);
        result += nibbleToHexChar(data[i] & 0x0F);
    }

    return result;
}

/**
 * @brief Encode bytes to lowercase hex string
 *
 * @param data Vector of bytes
 * @return Lowercase hex string
 */
[[nodiscard]] inline std::string encode(const std::vector<uint8_t>& data) {
    return encode(data.data(), data.size());
}

/**
 * @brief Encode fixed-size array to lowercase hex string
 *
 * @tparam N Size of the array
 * @param data Array of bytes
 * @return Lowercase hex string
 */
template <size_t N>
[[nodiscard]] std::string encode(const std::array<uint8_t, N>& data) {
    return encode(data.data(), N);
}

/**
 * @brief Encode bytes to uppercase hex string
 *
 * @param data Pointer to byte data
 * @param length Number of bytes to encode
 * @return Uppercase hex string
 */
[[nodiscard]] inline std::string encodeUpper(const uint8_t* data, size_t length) {
    std::string result;
    result.reserve(length * 2);

    for (size_t i = 0; i < length; ++i) {
        result += nibbleToHexCharUpper((data[i] >> 4) & 0x0F);
        result += nibbleToHexCharUpper(data[i] & 0x0F);
    }

    return result;
}

/**
 * @brief Encode bytes to uppercase hex string
 *
 * @param data Vector of bytes
 * @return Uppercase hex string
 */
[[nodiscard]] inline std::string encodeUpper(const std::vector<uint8_t>& data) {
    return encodeUpper(data.data(), data.size());
}

/**
 * @brief Encode fixed-size array to uppercase hex string
 *
 * @tparam N Size of the array
 * @param data Array of bytes
 * @return Uppercase hex string
 */
template <size_t N>
[[nodiscard]] std::string encodeUpper(const std::array<uint8_t, N>& data) {
    return encodeUpper(data.data(), N);
}

// ============================================================================
// Decoding
// ============================================================================

/**
 * @brief Decode hex string to bytes
 *
 * @param hex The hex string to decode
 * @return Vector of bytes, or std::nullopt if invalid
 */
[[nodiscard]] inline std::optional<std::vector<uint8_t>> decode(std::string_view hex) {
    // Must have even length
    if (hex.size() % 2 != 0) {
        return std::nullopt;
    }

    std::vector<uint8_t> result;
    result.reserve(hex.size() / 2);

    for (size_t i = 0; i < hex.size(); i += 2) {
        const auto highNibble = hexCharToNibble(hex[i]);
        if (!highNibble) return std::nullopt;

        const auto lowNibble = hexCharToNibble(hex[i + 1]);
        if (!lowNibble) return std::nullopt;

        result.push_back((*highNibble << 4) | *lowNibble);
    }

    return result;
}

/**
 * @brief Decode hex string into fixed-size array
 *
 * @tparam N Expected number of output bytes
 * @param hex The hex string to decode
 * @return Array of bytes, or std::nullopt if invalid or wrong size
 */
template <size_t N>
[[nodiscard]] std::optional<std::array<uint8_t, N>> decodeArray(std::string_view hex) {
    if (hex.size() != N * 2) {
        return std::nullopt;
    }

    std::array<uint8_t, N> result{};

    for (size_t i = 0; i < N; ++i) {
        const auto highNibble = hexCharToNibble(hex[i * 2]);
        if (!highNibble) return std::nullopt;

        const auto lowNibble = hexCharToNibble(hex[i * 2 + 1]);
        if (!lowNibble) return std::nullopt;

        result[i] = (*highNibble << 4) | *lowNibble;
    }

    return result;
}

/**
 * @brief Decode hex string into existing buffer
 *
 * @param hex The hex string to decode
 * @param output Pointer to output buffer
 * @param outputSize Size of output buffer
 * @return Number of bytes written, or std::nullopt if invalid
 */
[[nodiscard]] inline std::optional<size_t> decodeInto(
    std::string_view hex,
    uint8_t* output,
    size_t outputSize
) {
    if (hex.size() % 2 != 0) {
        return std::nullopt;
    }

    const size_t byteCount = hex.size() / 2;
    if (byteCount > outputSize) {
        return std::nullopt;
    }

    for (size_t i = 0; i < byteCount; ++i) {
        const auto highNibble = hexCharToNibble(hex[i * 2]);
        if (!highNibble) return std::nullopt;

        const auto lowNibble = hexCharToNibble(hex[i * 2 + 1]);
        if (!lowNibble) return std::nullopt;

        output[i] = (*highNibble << 4) | *lowNibble;
    }

    return byteCount;
}

// ============================================================================
// Validation
// ============================================================================

/**
 * @brief Check if string contains only valid hex characters
 */
[[nodiscard]] inline bool isValid(std::string_view hex) noexcept {
    for (char c : hex) {
        if (!isHexChar(c)) return false;
    }
    return true;
}

/**
 * @brief Check if string is valid hex with even length (decodable to bytes)
 */
[[nodiscard]] inline bool isValidBytes(std::string_view hex) noexcept {
    return hex.size() % 2 == 0 && isValid(hex);
}

/**
 * @brief Check if string is valid hex with specific byte count
 *
 * @param hex The hex string to check
 * @param byteCount Expected number of bytes when decoded
 */
[[nodiscard]] inline bool isValidWithLength(std::string_view hex, size_t byteCount) noexcept {
    return hex.size() == byteCount * 2 && isValid(hex);
}

// ============================================================================
// Constant-Time Operations
// ============================================================================

/**
 * @brief Constant-time comparison of hex strings
 *
 * This function compares hex strings in constant time to prevent
 * timing attacks. Case-insensitive comparison.
 *
 * @param a First hex string
 * @param b Second hex string
 * @return true if equal, false otherwise
 */
[[nodiscard]] inline bool constantTimeEqual(std::string_view a, std::string_view b) noexcept {
    if (a.size() != b.size()) {
        return false;
    }

    uint8_t diff = 0;

    for (size_t i = 0; i < a.size(); ++i) {
        // Convert to lowercase for comparison
        char ca = a[i];
        char cb = b[i];

        if (ca >= 'A' && ca <= 'Z') ca = ca - 'A' + 'a';
        if (cb >= 'A' && cb <= 'Z') cb = cb - 'A' + 'a';

        diff |= static_cast<uint8_t>(ca ^ cb);
    }

    return diff == 0;
}

/**
 * @brief Constant-time comparison of byte arrays
 *
 * @param a First byte array
 * @param b Second byte array
 * @return true if equal, false otherwise
 */
[[nodiscard]] inline bool constantTimeEqualBytes(
    const std::vector<uint8_t>& a,
    const std::vector<uint8_t>& b
) noexcept {
    if (a.size() != b.size()) {
        return false;
    }

    uint8_t diff = 0;

    for (size_t i = 0; i < a.size(); ++i) {
        diff |= a[i] ^ b[i];
    }

    return diff == 0;
}

/**
 * @brief Constant-time comparison of fixed-size byte arrays
 *
 * @tparam N Size of the arrays
 * @param a First array
 * @param b Second array
 * @return true if equal, false otherwise
 */
template <size_t N>
[[nodiscard]] constexpr bool constantTimeEqualBytes(
    const std::array<uint8_t, N>& a,
    const std::array<uint8_t, N>& b
) noexcept {
    uint8_t diff = 0;

    for (size_t i = 0; i < N; ++i) {
        diff |= a[i] ^ b[i];
    }

    return diff == 0;
}

// ============================================================================
// Formatting
// ============================================================================

/**
 * @brief Format hex string with spaces between bytes
 *
 * @param hex The hex string to format (must have even length)
 * @return Formatted string, or std::nullopt if invalid
 */
[[nodiscard]] inline std::optional<std::string> formatSpaced(std::string_view hex) {
    if (hex.size() % 2 != 0) {
        return std::nullopt;
    }

    if (hex.empty()) {
        return std::string{};
    }

    std::string result;
    result.reserve(hex.size() + hex.size() / 2 - 1);

    for (size_t i = 0; i < hex.size(); i += 2) {
        if (i > 0) {
            result += ' ';
        }
        result += hex[i];
        result += hex[i + 1];
    }

    return result;
}

/**
 * @brief Format hex string with colons between bytes (MAC address style)
 *
 * @param hex The hex string to format (must have even length)
 * @return Formatted string, or std::nullopt if invalid
 */
[[nodiscard]] inline std::optional<std::string> formatColons(std::string_view hex) {
    if (hex.size() % 2 != 0) {
        return std::nullopt;
    }

    if (hex.empty()) {
        return std::string{};
    }

    std::string result;
    result.reserve(hex.size() + hex.size() / 2 - 1);

    for (size_t i = 0; i < hex.size(); i += 2) {
        if (i > 0) {
            result += ':';
        }
        result += hex[i];
        result += hex[i + 1];
    }

    return result;
}

/**
 * @brief Format hex string with custom separator between bytes
 *
 * @param hex The hex string to format (must have even length)
 * @param separator The separator string to use
 * @return Formatted string, or std::nullopt if invalid
 */
[[nodiscard]] inline std::optional<std::string> formatWithSeparator(
    std::string_view hex,
    std::string_view separator
) {
    if (hex.size() % 2 != 0) {
        return std::nullopt;
    }

    if (hex.empty()) {
        return std::string{};
    }

    const size_t byteCount = hex.size() / 2;
    std::string result;
    result.reserve(hex.size() + separator.size() * (byteCount - 1));

    for (size_t i = 0; i < hex.size(); i += 2) {
        if (i > 0) {
            result += separator;
        }
        result += hex[i];
        result += hex[i + 1];
    }

    return result;
}

/**
 * @brief Strip separator characters from hex string
 *
 * Removes spaces, colons, dashes, and other common separators.
 *
 * @param hex The hex string with separators
 * @return Clean hex string with separators removed
 */
[[nodiscard]] inline std::string stripSeparators(std::string_view hex) {
    std::string result;
    result.reserve(hex.size());

    for (char c : hex) {
        if (isHexChar(c)) {
            result += c;
        }
        // Skip separators: space, colon, dash, underscore
    }

    return result;
}

// ============================================================================
// Integer Conversion
// ============================================================================

/**
 * @brief Convert unsigned integer to hex string
 *
 * @param value The integer value
 * @param minWidth Minimum width (left-padded with zeros)
 * @return Hex string representation
 */
[[nodiscard]] inline std::string intToHex(uint64_t value, size_t minWidth = 0) {
    std::string result;

    // Handle zero case
    if (value == 0) {
        if (minWidth == 0) minWidth = 1;
        return std::string(minWidth, '0');
    }

    // Build hex string in reverse
    while (value > 0) {
        result = nibbleToHexChar(value & 0x0F) + result;
        value >>= 4;
    }

    // Pad if needed
    while (result.size() < minWidth) {
        result = '0' + result;
    }

    return result;
}

/**
 * @brief Convert unsigned integer to uppercase hex string
 *
 * @param value The integer value
 * @param minWidth Minimum width (left-padded with zeros)
 * @return Uppercase hex string representation
 */
[[nodiscard]] inline std::string intToHexUpper(uint64_t value, size_t minWidth = 0) {
    std::string result;

    if (value == 0) {
        if (minWidth == 0) minWidth = 1;
        return std::string(minWidth, '0');
    }

    while (value > 0) {
        result = nibbleToHexCharUpper(value & 0x0F) + result;
        value >>= 4;
    }

    while (result.size() < minWidth) {
        result = '0' + result;
    }

    return result;
}

/**
 * @brief Parse hex string to unsigned integer
 *
 * @param hex The hex string to parse
 * @return The integer value, or std::nullopt if invalid
 */
[[nodiscard]] inline std::optional<uint64_t> hexToInt(std::string_view hex) noexcept {
    if (hex.empty() || hex.size() > 16) {
        return std::nullopt;
    }

    uint64_t result = 0;

    for (char c : hex) {
        const auto nibble = hexCharToNibble(c);
        if (!nibble) return std::nullopt;

        result = (result << 4) | *nibble;
    }

    return result;
}

/**
 * @brief Parse hex string to unsigned 32-bit integer
 *
 * @param hex The hex string to parse
 * @return The integer value, or std::nullopt if invalid or too large
 */
[[nodiscard]] inline std::optional<uint32_t> hexToU32(std::string_view hex) noexcept {
    if (hex.empty() || hex.size() > 8) {
        return std::nullopt;
    }

    const auto result = hexToInt(hex);
    if (!result || *result > UINT32_MAX) {
        return std::nullopt;
    }

    return static_cast<uint32_t>(*result);
}

// ============================================================================
// Hex Dump Formatting
// ============================================================================

/**
 * @brief Create a hex dump of bytes (similar to `hexdump -C`)
 *
 * @param data Pointer to byte data
 * @param length Number of bytes
 * @param bytesPerLine Number of bytes per line (default 16)
 * @return Formatted hex dump string
 */
[[nodiscard]] inline std::string hexDump(
    const uint8_t* data,
    size_t length,
    size_t bytesPerLine = 16
) {
    std::string result;
    result.reserve(length * 4 + (length / bytesPerLine) * 20);

    for (size_t offset = 0; offset < length; offset += bytesPerLine) {
        // Address
        result += intToHex(offset, 8) + "  ";

        // Hex bytes
        for (size_t i = 0; i < bytesPerLine; ++i) {
            if (offset + i < length) {
                result += nibbleToHexChar((data[offset + i] >> 4) & 0x0F);
                result += nibbleToHexChar(data[offset + i] & 0x0F);
                result += ' ';
            } else {
                result += "   ";
            }

            // Extra space in middle
            if (i == 7) result += ' ';
        }

        // ASCII representation
        result += " |";
        for (size_t i = 0; i < bytesPerLine && offset + i < length; ++i) {
            uint8_t byte = data[offset + i];
            if (byte >= 32 && byte < 127) {
                result += static_cast<char>(byte);
            } else {
                result += '.';
            }
        }
        result += "|\n";
    }

    return result;
}

/**
 * @brief Create a hex dump of bytes
 *
 * @param data Vector of bytes
 * @param bytesPerLine Number of bytes per line (default 16)
 * @return Formatted hex dump string
 */
[[nodiscard]] inline std::string hexDump(
    const std::vector<uint8_t>& data,
    size_t bytesPerLine = 16
) {
    return hexDump(data.data(), data.size(), bytesPerLine);
}

} // namespace SafeHex

} // namespace proven

#endif // PROVEN_SAFE_HEX_HPP
