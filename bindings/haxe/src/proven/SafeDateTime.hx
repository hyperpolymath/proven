// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeDateTime - ISO 8601 date/time handling via libproven FFI.
// All computation is performed in verified Idris 2 code. This module
// provides an idiomatic Haxe wrapper around the raw C FFI calls.

package proven;

import proven.LibProven;

/**
 * Safe date/time operations for ISO 8601 parsing, formatting, and
 * calendar calculations.
 *
 * All operations delegate to the formally verified Idris 2 core via
 * libproven. Returns `null` on error.
 */
class SafeDateTime {
    /**
     * Check if a year is a leap year.
     * @param year Year to check
     * @return true if leap year
     */
    public static function isLeapYear(year:Int):Bool {
        return LibProven.datetimeIsLeapYear(year);
    }

    /**
     * Get the number of days in a given month.
     * @param year Year (for leap year calculation)
     * @param month Month (1-12)
     * @return Number of days, or 0 if month is invalid
     */
    public static function daysInMonth(year:Int, month:Int):Int {
        return LibProven.datetimeDaysInMonth(year, month);
    }
}
