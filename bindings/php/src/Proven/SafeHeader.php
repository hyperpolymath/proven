<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeHeader - FFI wrapper for proven_header_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe HTTP header operations via libproven FFI.
 *
 * Provides CRLF injection detection, header name validation,
 * dangerous header detection, header rendering, and security header building.
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeHeader
{
    /**
     * Check if a header value contains CRLF injection sequences.
     */
    public static function hasCrlf(string $value): ?bool
    {
        $result = FFI::getLib()->proven_header_has_crlf($value, strlen($value));
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Check if a header name is valid per HTTP spec.
     */
    public static function isValidName(string $name): ?bool
    {
        $result = FFI::getLib()->proven_header_is_valid_name($name, strlen($name));
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Check if a header name is security-dangerous.
     */
    public static function isDangerous(string $name): ?bool
    {
        $result = FFI::getLib()->proven_header_is_dangerous($name, strlen($name));
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Render a header name-value pair as a proper HTTP header line.
     */
    public static function render(string $name, string $value): ?string
    {
        return self::extractString(
            FFI::getLib()->proven_header_render(
                $name, strlen($name), $value, strlen($value)
            )
        );
    }

    /**
     * Build a Content-Security-Policy header from a JSON policy specification.
     */
    public static function buildCsp(string $json): ?string
    {
        return self::extractString(
            FFI::getLib()->proven_header_build_csp($json, strlen($json))
        );
    }

    /**
     * Build an HSTS (HTTP Strict Transport Security) header value.
     *
     * @param int  $maxAge            max-age in seconds.
     * @param bool $includeSubdomains Whether to include subdomains.
     * @param bool $preload           Whether to include the preload directive.
     */
    public static function buildHsts(int $maxAge, bool $includeSubdomains, bool $preload): ?string
    {
        return self::extractString(
            FFI::getLib()->proven_header_build_hsts($maxAge, $includeSubdomains, $preload)
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
