// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeDateTime - JNA wrapper for proven_datetime_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import groovy.transform.CompileStatic

/**
 * Safe date/time operations via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No datetime logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeDateTime {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Check if a year is a leap year. */
    static boolean isLeapYear(int year) {
        LIB.proven_datetime_is_leap_year(year)
    }

    /** Get the number of days in a given month. */
    static byte daysInMonth(int year, byte month) {
        LIB.proven_datetime_days_in_month(year, month)
    }
}
