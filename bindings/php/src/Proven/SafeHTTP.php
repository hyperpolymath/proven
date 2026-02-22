<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeHTTP - FFI wrapper for proven_http_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe HTTP URL encoding/decoding via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeHTTP
{
    /**
     * URL-encode a string (percent-encoding).
     */
    public static function urlEncode(string $input): ?string
    {
        return self::extractString(
            FFI::getLib()->proven_http_url_encode($input, strlen($input))
        );
    }

    /**
     * URL-decode a percent-encoded string.
     */
    public static function urlDecode(string $input): ?string
    {
        return self::extractString(
            FFI::getLib()->proven_http_url_decode($input, strlen($input))
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
