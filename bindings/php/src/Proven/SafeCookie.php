<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCookie - FFI wrapper for proven_cookie_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe HTTP cookie operations via libproven FFI.
 *
 * Provides injection detection, name/value validation, prefix detection,
 * and delete-cookie construction.
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeCookie
{
    /**
     * Check if a cookie value contains injection characters.
     */
    public static function hasInjection(string $value): ?bool
    {
        $result = FFI::getLib()->proven_cookie_has_injection($value, strlen($value));
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Validate a cookie name.
     */
    public static function validateName(string $name): ?bool
    {
        $result = FFI::getLib()->proven_cookie_validate_name($name, strlen($name));
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Validate a cookie value.
     */
    public static function validateValue(string $value): ?bool
    {
        $result = FFI::getLib()->proven_cookie_validate_value($value, strlen($value));
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Get the cookie prefix type (__Host-, __Secure-, or none).
     * Returns a prefix type code (integer), or null on error.
     */
    public static function getPrefix(string $name): ?int
    {
        $result = FFI::getLib()->proven_cookie_get_prefix($name, strlen($name));
        if ($result->status !== 0) {
            return null;
        }
        return (int) $result->value;
    }

    /**
     * Build a Set-Cookie header that deletes the named cookie.
     */
    public static function buildDelete(string $name): ?string
    {
        return self::extractString(
            FFI::getLib()->proven_cookie_build_delete($name, strlen($name))
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
