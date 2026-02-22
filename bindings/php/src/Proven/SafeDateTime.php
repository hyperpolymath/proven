<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeDateTime - FFI wrapper for proven_datetime_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe date/time operations via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeDateTime
{
    /**
     * Parse an ISO 8601 date/time string.
     *
     * Returns an associative array with year, month, day, hour, minute, second,
     * nanosecond, tz_offset_minutes on success, or null on error.
     *
     * @return array{year: int, month: int, day: int, hour: int, minute: int,
     *               second: int, nanosecond: int, tz_offset_minutes: int}|null
     */
    public static function parse(string $input): ?array
    {
        $result = FFI::getLib()->proven_datetime_parse($input, strlen($input));
        if ($result->status !== 0) {
            return null;
        }
        $dt = $result->datetime;
        return [
            'year' => (int) $dt->year,
            'month' => (int) $dt->month,
            'day' => (int) $dt->day,
            'hour' => (int) $dt->hour,
            'minute' => (int) $dt->minute,
            'second' => (int) $dt->second,
            'nanosecond' => (int) $dt->nanosecond,
            'tz_offset_minutes' => (int) $dt->tz_offset_minutes,
        ];
    }

    /**
     * Format a DateTime struct as an ISO 8601 string.
     *
     * @param array $dt An associative array matching the DateTime struct fields.
     * @return string|null The formatted string, or null on error.
     */
    public static function formatIso8601(array $dt): ?string
    {
        $cdt = FFI::getLib()->new('DateTime');
        $cdt->year = $dt['year'];
        $cdt->month = $dt['month'];
        $cdt->day = $dt['day'];
        $cdt->hour = $dt['hour'];
        $cdt->minute = $dt['minute'];
        $cdt->second = $dt['second'];
        $cdt->nanosecond = $dt['nanosecond'] ?? 0;
        $cdt->tz_offset_minutes = $dt['tz_offset_minutes'] ?? 0;

        $result = FFI::getLib()->proven_datetime_format_iso8601($cdt);
        if ($result->status !== 0 || $result->value === null) {
            return null;
        }
        $str = \FFI::string($result->value, $result->length);
        FFI::getLib()->proven_free_string($result->value);
        return $str;
    }

    /**
     * Check if a year is a leap year.
     */
    public static function isLeapYear(int $year): bool
    {
        return FFI::getLib()->proven_datetime_is_leap_year($year);
    }

    /**
     * Get the number of days in a given month.
     *
     * @param int $year  The year (needed for February in leap years).
     * @param int $month The month (1-12).
     * @return int The number of days.
     */
    public static function daysInMonth(int $year, int $month): int
    {
        return FFI::getLib()->proven_datetime_days_in_month($year, $month);
    }
}
