<?php
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Safe hexadecimal encoding/decoding operations.
 *
 * Provides constant-time operations where security-relevant and
 * safe handling of invalid input.
 */
class SafeHex
{
    /** @var string Lowercase hex alphabet */
    private const HEX_LOWER = '0123456789abcdef';

    /** @var string Uppercase hex alphabet */
    private const HEX_UPPER = '0123456789ABCDEF';

    /**
     * Encode binary data to lowercase hexadecimal.
     *
     * @param string $data The binary data to encode
     * @return string Lowercase hex string
     */
    public static function encode(string $data): string
    {
        return bin2hex($data);
    }

    /**
     * Encode binary data to uppercase hexadecimal.
     *
     * @param string $data The binary data to encode
     * @return string Uppercase hex string
     */
    public static function encodeUpper(string $data): string
    {
        return strtoupper(bin2hex($data));
    }

    /**
     * Decode hexadecimal string to binary data.
     *
     * @param string $hex The hex string to decode
     * @return string|null Decoded binary data or null if invalid
     */
    public static function decode(string $hex): ?string
    {
        // Remove optional 0x prefix
        if (str_starts_with($hex, '0x') || str_starts_with($hex, '0X')) {
            $hex = substr($hex, 2);
        }

        // Check for valid hex string (even length, hex characters only)
        if (strlen($hex) % 2 !== 0) {
            return null;
        }

        if (!ctype_xdigit($hex) && $hex !== '') {
            return null;
        }

        if ($hex === '') {
            return '';
        }

        $result = @hex2bin($hex);
        return $result === false ? null : $result;
    }

    /**
     * Decode hexadecimal with a fallback value for invalid input.
     *
     * @param string $hex The hex string to decode
     * @param string $fallback Value to return if invalid
     * @return string Decoded data or fallback
     */
    public static function decodeOr(string $hex, string $fallback): string
    {
        return self::decode($hex) ?? $fallback;
    }

    /**
     * Compare two hex strings in constant time.
     *
     * This prevents timing attacks when comparing secrets like API keys,
     * tokens, or other sensitive hex-encoded data.
     *
     * @param string $hexA First hex string
     * @param string $hexB Second hex string
     * @return bool True if equal (case-insensitive)
     */
    public static function constantTimeEqual(string $hexA, string $hexB): bool
    {
        // Normalize both to lowercase for comparison
        $normalizedA = strtolower($hexA);
        $normalizedB = strtolower($hexB);

        // Remove 0x prefix if present
        if (str_starts_with($normalizedA, '0x')) {
            $normalizedA = substr($normalizedA, 2);
        }
        if (str_starts_with($normalizedB, '0x')) {
            $normalizedB = substr($normalizedB, 2);
        }

        return hash_equals($normalizedA, $normalizedB);
    }

    /**
     * Check if a string is valid hexadecimal.
     *
     * @param string $hex The string to validate
     * @param bool $allowPrefix Whether to allow 0x prefix
     * @return bool True if valid hex
     */
    public static function isValid(string $hex, bool $allowPrefix = true): bool
    {
        $toCheck = $hex;

        if ($allowPrefix && (str_starts_with($hex, '0x') || str_starts_with($hex, '0X'))) {
            $toCheck = substr($hex, 2);
        }

        // Empty string after removing prefix is not valid hex
        if ($toCheck === '') {
            return false;
        }

        return ctype_xdigit($toCheck);
    }

    /**
     * Check if a string is valid hexadecimal with even length (can be decoded).
     *
     * @param string $hex The string to validate
     * @param bool $allowPrefix Whether to allow 0x prefix
     * @return bool True if valid and decodable
     */
    public static function isDecodable(string $hex, bool $allowPrefix = true): bool
    {
        $toCheck = $hex;

        if ($allowPrefix && (str_starts_with($hex, '0x') || str_starts_with($hex, '0X'))) {
            $toCheck = substr($hex, 2);
        }

        if ($toCheck === '') {
            return true; // Empty hex decodes to empty string
        }

        return ctype_xdigit($toCheck) && strlen($toCheck) % 2 === 0;
    }

    /**
     * Normalize a hex string (lowercase, no prefix).
     *
     * @param string $hex The hex string to normalize
     * @return string|null Normalized hex or null if invalid
     */
    public static function normalize(string $hex): ?string
    {
        $toNormalize = $hex;

        if (str_starts_with($hex, '0x') || str_starts_with($hex, '0X')) {
            $toNormalize = substr($hex, 2);
        }

        if ($toNormalize !== '' && !ctype_xdigit($toNormalize)) {
            return null;
        }

        return strtolower($toNormalize);
    }

    /**
     * Add 0x prefix to hex string if not present.
     *
     * @param string $hex The hex string
     * @return string|null Hex with 0x prefix or null if invalid
     */
    public static function withPrefix(string $hex): ?string
    {
        $normalized = self::normalize($hex);
        if ($normalized === null) {
            return null;
        }
        return '0x' . $normalized;
    }

    /**
     * Remove 0x prefix from hex string if present.
     *
     * @param string $hex The hex string
     * @return string|null Hex without prefix or null if invalid
     */
    public static function withoutPrefix(string $hex): ?string
    {
        return self::normalize($hex);
    }

    /**
     * Encode a single byte to two hex characters.
     *
     * @param int $byte Byte value (0-255)
     * @return string Two hex characters
     * @throws \InvalidArgumentException If byte is out of range
     */
    public static function encodeByte(int $byte): string
    {
        if ($byte < 0 || $byte > 255) {
            throw new \InvalidArgumentException('Byte must be in range 0-255');
        }
        return sprintf('%02x', $byte);
    }

    /**
     * Decode two hex characters to a byte value.
     *
     * @param string $hex Two hex characters
     * @return int|null Byte value (0-255) or null if invalid
     */
    public static function decodeByte(string $hex): ?int
    {
        if (strlen($hex) !== 2 || !ctype_xdigit($hex)) {
            return null;
        }
        return (int)hexdec($hex);
    }

    /**
     * Encode an integer to hexadecimal.
     *
     * @param int $value The integer to encode
     * @param int $minWidth Minimum width (will be zero-padded)
     * @param bool $prefix Whether to include 0x prefix
     * @return string Hex representation
     */
    public static function encodeInt(int $value, int $minWidth = 0, bool $prefix = false): string
    {
        $hex = dechex($value);

        if ($minWidth > 0 && strlen($hex) < $minWidth) {
            $hex = str_pad($hex, $minWidth, '0', STR_PAD_LEFT);
        }

        return $prefix ? '0x' . $hex : $hex;
    }

    /**
     * Decode hexadecimal to an integer.
     *
     * @param string $hex The hex string
     * @return int|null The integer value or null if invalid
     */
    public static function decodeInt(string $hex): ?int
    {
        $normalized = self::normalize($hex);
        if ($normalized === null || $normalized === '') {
            return null;
        }

        // Prevent overflow on 64-bit systems (max 16 hex chars = 8 bytes)
        if (strlen($normalized) > 16) {
            return null;
        }

        return (int)hexdec($normalized);
    }

    /**
     * Format hex string with separators for readability.
     *
     * @param string $hex The hex string
     * @param string $separator Separator character (default space)
     * @param int $groupSize Number of chars per group (default 2)
     * @return string|null Formatted hex or null if invalid
     */
    public static function format(string $hex, string $separator = ' ', int $groupSize = 2): ?string
    {
        $normalized = self::normalize($hex);
        if ($normalized === null) {
            return null;
        }

        if ($normalized === '') {
            return '';
        }

        if ($groupSize <= 0) {
            return $normalized;
        }

        return implode($separator, str_split($normalized, $groupSize));
    }

    /**
     * Remove all whitespace and separators from hex string.
     *
     * @param string $hex The hex string (may contain spaces, colons, etc.)
     * @return string|null Clean hex or null if invalid after cleaning
     */
    public static function clean(string $hex): ?string
    {
        // Remove common separators: space, colon, dash, dot
        $cleaned = preg_replace('/[\s:\-.]/', '', $hex);
        if ($cleaned === null) {
            return null;
        }

        return self::normalize($cleaned);
    }

    /**
     * Generate a random hex string.
     *
     * @param int $byteLength Number of random bytes (output will be 2x length)
     * @return string Random hex string
     * @throws \Exception If secure random generation fails
     */
    public static function random(int $byteLength): string
    {
        if ($byteLength < 0) {
            throw new \InvalidArgumentException('Byte length must be non-negative');
        }
        if ($byteLength === 0) {
            return '';
        }
        return bin2hex(random_bytes($byteLength));
    }

    /**
     * XOR two hex strings of equal length.
     *
     * @param string $hexA First hex string
     * @param string $hexB Second hex string
     * @return string|null XOR result or null if lengths differ or invalid
     */
    public static function xor(string $hexA, string $hexB): ?string
    {
        $bytesA = self::decode($hexA);
        $bytesB = self::decode($hexB);

        if ($bytesA === null || $bytesB === null) {
            return null;
        }

        if (strlen($bytesA) !== strlen($bytesB)) {
            return null;
        }

        $result = $bytesA ^ $bytesB;
        return bin2hex($result);
    }

    /**
     * Check if two hex strings are equal (case-insensitive, ignores prefix).
     *
     * This is NOT constant-time - use constantTimeEqual for secrets.
     *
     * @param string $hexA First hex string
     * @param string $hexB Second hex string
     * @return bool True if equal
     */
    public static function equals(string $hexA, string $hexB): bool
    {
        $normalizedA = self::normalize($hexA);
        $normalizedB = self::normalize($hexB);

        if ($normalizedA === null || $normalizedB === null) {
            return false;
        }

        return $normalizedA === $normalizedB;
    }

    /**
     * Reverse the bytes in a hex string (for endianness conversion).
     *
     * @param string $hex The hex string
     * @return string|null Reversed hex or null if invalid/odd length
     */
    public static function reverseBytes(string $hex): ?string
    {
        $normalized = self::normalize($hex);
        if ($normalized === null || strlen($normalized) % 2 !== 0) {
            return null;
        }

        if ($normalized === '') {
            return '';
        }

        $bytes = str_split($normalized, 2);
        return implode('', array_reverse($bytes));
    }

    /**
     * Pad a hex string to a minimum length.
     *
     * @param string $hex The hex string
     * @param int $minLength Minimum length (in hex chars)
     * @param bool $padLeft Whether to pad on left (true) or right (false)
     * @return string|null Padded hex or null if invalid
     */
    public static function pad(string $hex, int $minLength, bool $padLeft = true): ?string
    {
        $normalized = self::normalize($hex);
        if ($normalized === null) {
            return null;
        }

        if (strlen($normalized) >= $minLength) {
            return $normalized;
        }

        $padType = $padLeft ? STR_PAD_LEFT : STR_PAD_RIGHT;
        return str_pad($normalized, $minLength, '0', $padType);
    }

    /**
     * Truncate a hex string to a maximum length.
     *
     * @param string $hex The hex string
     * @param int $maxLength Maximum length (in hex chars)
     * @param bool $fromLeft Whether to keep left (true) or right (false) portion
     * @return string|null Truncated hex or null if invalid
     */
    public static function truncate(string $hex, int $maxLength, bool $fromLeft = true): ?string
    {
        $normalized = self::normalize($hex);
        if ($normalized === null) {
            return null;
        }

        if (strlen($normalized) <= $maxLength) {
            return $normalized;
        }

        if ($fromLeft) {
            return substr($normalized, 0, $maxLength);
        }
        return substr($normalized, -$maxLength);
    }
}
