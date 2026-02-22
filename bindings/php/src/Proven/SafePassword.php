<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafePassword - FFI wrapper for proven_password_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe password validation via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafePassword
{
    /**
     * Validate a password and return strength analysis.
     *
     * Returns an associative array with strength, has_lowercase, has_uppercase,
     * has_digit, has_special, length on success.
     *
     * @return array{strength: int, has_lowercase: bool, has_uppercase: bool,
     *               has_digit: bool, has_special: bool, length: int}
     */
    public static function validate(string $password): array
    {
        $result = FFI::getLib()->proven_password_validate($password, strlen($password));
        return [
            'strength' => (int) $result->strength,
            'has_lowercase' => (bool) $result->has_lowercase,
            'has_uppercase' => (bool) $result->has_uppercase,
            'has_digit' => (bool) $result->has_digit,
            'has_special' => (bool) $result->has_special,
            'length' => (int) $result->length,
        ];
    }

    /**
     * Check if a password is in the common passwords list.
     */
    public static function isCommon(string $password): bool
    {
        return FFI::getLib()->proven_password_is_common($password, strlen($password));
    }
}
