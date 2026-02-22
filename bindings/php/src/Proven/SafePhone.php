<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafePhone - FFI wrapper for proven_phone_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe phone number parsing and formatting via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafePhone
{
    /**
     * Parse a phone number string.
     *
     * Returns an associative array with country_code, national_number, is_valid
     * on success, or null on error.
     *
     * @return array{country_code: int, national_number: int, is_valid: bool}|null
     */
    public static function parse(string $phone): ?array
    {
        $result = FFI::getLib()->proven_phone_parse($phone, strlen($phone));
        if ($result->status !== 0) {
            return null;
        }
        return [
            'country_code' => (int) $result->country_code,
            'national_number' => (int) $result->national_number,
            'is_valid' => (bool) $result->is_valid,
        ];
    }

    /**
     * Format a phone number in E.164 format.
     *
     * @param int $countryCode    The country code.
     * @param int $nationalNumber The national number.
     * @return string|null The E.164 formatted string, or null on error.
     */
    public static function formatE164(int $countryCode, int $nationalNumber): ?string
    {
        $result = FFI::getLib()->proven_phone_format_e164($countryCode, $nationalNumber);
        if ($result->status !== 0 || $result->value === null) {
            return null;
        }
        $str = \FFI::string($result->value, $result->length);
        FFI::getLib()->proven_free_string($result->value);
        return $str;
    }
}
