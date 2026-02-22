<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeString - FFI wrapper for proven_string_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe string operations for escaping and sanitization.
 *
 * Every method calls the corresponding libproven FFI function.
 * Returns null on error.
 */
final class SafeString
{
    /**
     * Check if a byte string is valid UTF-8.
     */
    public static function isValidUtf8(string $data): ?bool
    {
        $result = FFI::getLib()->proven_string_is_valid_utf8($data, strlen($data));
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Escape string for SQL (single quotes).
     */
    public static function escapeSql(string $value): ?string
    {
        return self::extractString(
            FFI::getLib()->proven_string_escape_sql($value, strlen($value))
        );
    }

    /**
     * Escape string for HTML (< > & " ').
     */
    public static function escapeHtml(string $value): ?string
    {
        return self::extractString(
            FFI::getLib()->proven_string_escape_html($value, strlen($value))
        );
    }

    /**
     * Escape string for JavaScript (within quotes).
     */
    public static function escapeJs(string $value): ?string
    {
        return self::extractString(
            FFI::getLib()->proven_string_escape_js($value, strlen($value))
        );
    }

    /**
     * Extract string from StringResult, free C memory, return null on error.
     */
    private static function extractString(mixed $result): ?string
    {
        if ($result->status !== 0 || $result->value === null) {
            return null;
        }
        $str = \FFI::string($result->value, $result->length);
        FFI::getLib()->proven_free_string($result->value);
        return $str;
    }
}
