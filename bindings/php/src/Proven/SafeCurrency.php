<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCurrency - FFI wrapper for proven_currency_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe ISO 4217 currency operations via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeCurrency
{
    /**
     * Parse a currency string (e.g. "$12.50", "USD 12.50").
     *
     * Returns an associative array with amount_minor, currency_code, decimal_places
     * on success, or null on error.
     *
     * @return array{amount_minor: int, currency_code: string, decimal_places: int}|null
     */
    public static function parse(string $input): ?array
    {
        $result = FFI::getLib()->proven_currency_parse($input, strlen($input));
        if ($result->status !== 0) {
            return null;
        }
        $code = '';
        for ($i = 0; $i < 3; $i++) {
            $code .= chr($result->currency_code[$i]);
        }
        return [
            'amount_minor' => (int) $result->amount_minor,
            'currency_code' => $code,
            'decimal_places' => (int) $result->decimal_places,
        ];
    }

    /**
     * Format a currency amount in minor units to a display string.
     *
     * @param int    $amountMinor   The amount in minor units (e.g. cents).
     * @param string $currencyCode  The 3-letter ISO 4217 code.
     * @param int    $decimalPlaces Number of decimal places.
     * @return string|null The formatted string, or null on error.
     */
    public static function format(int $amountMinor, string $currencyCode, int $decimalPlaces): ?string
    {
        $code = array_map('ord', str_split(substr($currencyCode, 0, 3)));
        while (count($code) < 3) {
            $code[] = 0;
        }
        $codeArr = FFI::getLib()->new('uint8_t[3]');
        for ($i = 0; $i < 3; $i++) {
            $codeArr[$i] = $code[$i];
        }
        $result = FFI::getLib()->proven_currency_format(
            $amountMinor, $codeArr, $decimalPlaces
        );
        if ($result->status !== 0 || $result->value === null) {
            return null;
        }
        $str = \FFI::string($result->value, $result->length);
        FFI::getLib()->proven_free_string($result->value);
        return $str;
    }
}
