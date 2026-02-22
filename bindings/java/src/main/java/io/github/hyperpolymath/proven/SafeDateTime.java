// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

/**
 * Safe date/time operations via libproven FFI.
 *
 * Provides leap year checking and days-in-month calculation.
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No date logic is reimplemented in Java.
 */
public final class SafeDateTime {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeDateTime() {}

    /**
     * Check if a year is a leap year.
     *
     * @param year the year to check
     * @return true if the year is a leap year
     */
    public static boolean isLeapYear(int year) {
        return LIB.proven_datetime_is_leap_year(year);
    }

    /**
     * Get the number of days in a given month.
     *
     * @param year  the year (needed for February in leap years)
     * @param month the month (1-12)
     * @return the number of days in that month
     */
    public static byte daysInMonth(int year, byte month) {
        return LIB.proven_datetime_days_in_month(year, month);
    }
}
