# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeDatetime - FFI wrapper for proven_datetime_* functions.
# All computation delegated to libproven; no logic reimplemented here.

package provide proven::datetime 1.0.0
package require Tcl 8.6
package require proven

namespace eval ::proven::datetime {
    namespace export parse format_iso8601 is_leap_year days_in_month
    namespace ensemble create

    # Parse ISO 8601 datetime string.
    # Returns dict {year <int> month <int> day <int> hour <int> minute <int>
    #               second <int> nanosecond <int> tz_offset_minutes <int>
    #               ok <bool> error <string>}.
    proc parse {datetimeString} {
        set data [encoding convertto utf-8 $datetimeString]
        set result [::proven::ffi::datetime_parse $data [string length $data]]
        return $result
    }

    # Format datetime as ISO 8601 string.
    # Takes a datetime dict as returned by parse.
    # Returns dict {value <string> ok <bool> error <string>}.
    proc format_iso8601 {datetimeDict} {
        set result [::proven::ffi::datetime_format_iso8601 $datetimeDict]
        return $result
    }

    # Check if a year is a leap year.
    proc is_leap_year {year} {
        return [::proven::ffi::datetime_is_leap_year [expr {int($year)}]]
    }

    # Get the number of days in a given month.
    proc days_in_month {year month} {
        return [::proven::ffi::datetime_days_in_month [expr {int($year)}] [expr {int($month)}]]
    }
}
