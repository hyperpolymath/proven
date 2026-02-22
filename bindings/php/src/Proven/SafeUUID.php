<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUUID - FFI wrapper for proven_uuid_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe UUID operations via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeUUID
{
    /**
     * Generate a new UUID v4.
     *
     * @return string|null The UUID as a string, or null on error.
     */
    public static function v4(): ?string
    {
        $result = FFI::getLib()->proven_uuid_v4();
        if ($result->status !== 0) {
            return null;
        }
        return self::uuidToString($result->uuid);
    }

    /**
     * Parse a UUID string into a UUID struct and return as a normalized string.
     *
     * @return string|null The normalized UUID string, or null on invalid input.
     */
    public static function parse(string $uuid): ?string
    {
        $result = FFI::getLib()->proven_uuid_parse($uuid, strlen($uuid));
        if ($result->status !== 0) {
            return null;
        }
        return self::uuidToString($result->uuid);
    }

    /**
     * Check if a UUID is the nil UUID (all zeros).
     *
     * @param string $uuid A UUID string to check.
     * @return bool|null True if nil, false if not, null on parse error.
     */
    public static function isNil(string $uuid): ?bool
    {
        $parsed = FFI::getLib()->proven_uuid_parse($uuid, strlen($uuid));
        if ($parsed->status !== 0) {
            return null;
        }
        return FFI::getLib()->proven_uuid_is_nil($parsed->uuid);
    }

    /**
     * Get the version number of a UUID.
     *
     * @param string $uuid A UUID string.
     * @return int|null The version (1-5), or null on parse error.
     */
    public static function version(string $uuid): ?int
    {
        $parsed = FFI::getLib()->proven_uuid_parse($uuid, strlen($uuid));
        if ($parsed->status !== 0) {
            return null;
        }
        return FFI::getLib()->proven_uuid_version($parsed->uuid);
    }

    /**
     * Convert a UUID struct to its string representation via FFI.
     */
    private static function uuidToString(mixed $uuid): ?string
    {
        $result = FFI::getLib()->proven_uuid_to_string($uuid);
        if ($result->status !== 0 || $result->value === null) {
            return null;
        }
        $str = \FFI::string($result->value, $result->length);
        FFI::getLib()->proven_free_string($result->value);
        return $str;
    }
}
