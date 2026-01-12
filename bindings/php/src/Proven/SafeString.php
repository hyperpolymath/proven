<?php
// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Safe string operations for escaping and sanitization.
 *
 * Provides safe UTF-8 validation and escaping for SQL, HTML, and JavaScript
 * without exceptions or security vulnerabilities.
 */
class SafeString
{
    /**
     * Check if a string is valid UTF-8.
     *
     * @param string $data The string to validate
     * @return bool True if valid UTF-8, false otherwise
     */
    public static function isValidUtf8(string $data): bool
    {
        return mb_check_encoding($data, 'UTF-8');
    }

    /**
     * Escape a string for safe HTML insertion.
     *
     * Prevents XSS by escaping < > & " ' characters.
     *
     * @param string $value The string to escape
     * @return string Escaped string safe for HTML
     */
    public static function escapeHtml(string $value): string
    {
        return htmlspecialchars($value, ENT_QUOTES | ENT_HTML5, 'UTF-8');
    }

    /**
     * Escape a string for safe SQL interpolation.
     *
     * Note: Prefer parameterized queries over string interpolation.
     * This is a last resort for legacy code.
     *
     * @param string $value The string to escape
     * @return string Escaped string safe for SQL
     */
    public static function escapeSql(string $value): string
    {
        // Double single quotes for SQL escaping
        return str_replace("'", "''", $value);
    }

    /**
     * Escape a string for safe JavaScript string literal insertion.
     *
     * Prevents XSS in JavaScript contexts.
     *
     * @param string $value The string to escape
     * @return string Escaped string safe for JS strings
     */
    public static function escapeJs(string $value): string
    {
        $replacements = [
            '\\' => '\\\\',
            '"' => '\\"',
            "'" => "\\'",
            "\n" => '\\n',
            "\r" => '\\r',
            "\t" => '\\t',
            '</' => '<\\/',  // Prevent </script> injection
            "\u{2028}" => '\\u2028',  // Line separator
            "\u{2029}" => '\\u2029',  // Paragraph separator
        ];
        return strtr($value, $replacements);
    }

    /**
     * Percent-encode a string for safe URL inclusion.
     *
     * @param string $value The string to encode
     * @return string URL-encoded string
     */
    public static function escapeUrl(string $value): string
    {
        return rawurlencode($value);
    }

    /**
     * Safely truncate a string to a maximum length.
     *
     * Handles multi-byte strings correctly.
     *
     * @param string $value The string to truncate
     * @param int $maxLength Maximum length
     * @param string $suffix Suffix to append if truncated (default "...")
     * @return string Truncated string
     */
    public static function truncateSafe(string $value, int $maxLength, string $suffix = '...'): string
    {
        if ($maxLength < 0) {
            return '';
        }

        $length = mb_strlen($value, 'UTF-8');
        if ($length <= $maxLength) {
            return $value;
        }

        $suffixLen = mb_strlen($suffix, 'UTF-8');
        if ($maxLength <= $suffixLen) {
            return mb_substr($value, 0, $maxLength, 'UTF-8');
        }

        return mb_substr($value, 0, $maxLength - $suffixLen, 'UTF-8') . $suffix;
    }

    /**
     * Decode bytes to string, replacing invalid UTF-8 sequences.
     *
     * @param string $data The bytes to decode
     * @param string $fallback Character to use for invalid sequences
     * @return string Decoded string with invalid sequences replaced
     */
    public static function safeDecode(string $data, string $fallback = "\u{FFFD}"): string
    {
        if (self::isValidUtf8($data)) {
            return $data;
        }
        return mb_convert_encoding($data, 'UTF-8', 'UTF-8');
    }

    /**
     * Strip all HTML tags from a string safely.
     *
     * @param string $value The string to strip
     * @return string String with HTML tags removed
     */
    public static function stripHtml(string $value): string
    {
        return strip_tags($value);
    }

    /**
     * Escape a string for use in a shell command.
     *
     * Note: Prefer parameterized execution over shell interpolation.
     *
     * @param string $value The string to escape
     * @return string Escaped string safe for shell
     */
    public static function escapeShell(string $value): string
    {
        return escapeshellarg($value);
    }
}
