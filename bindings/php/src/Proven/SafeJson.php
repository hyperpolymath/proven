<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeJson - FFI wrapper for proven_json_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe JSON validation via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeJson
{
    /**
     * Check if a string contains valid JSON.
     */
    public static function isValid(string $json): ?bool
    {
        $result = FFI::getLib()->proven_json_is_valid($json, strlen($json));
        if ($result->status !== 0) {
            return null;
        }
        return (bool) $result->value;
    }

    /**
     * Get the JSON value type.
     *
     * Returns an integer type code, or null on error.
     * Type codes are defined by the libproven C ABI.
     */
    public static function getType(string $json): ?int
    {
        return FFI::getLib()->proven_json_get_type($json, strlen($json));
    }
}
