# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeDatetime - Safe datetime parsing and validation.
#

package provide proven::datetime 0.4.0
package require Tcl 8.6

namespace eval ::proven::datetime {
    namespace export is_leap_year days_in_month validate_date validate_time
    namespace export parse_iso_date parse_iso_time parse_iso_datetime
    namespace export format_iso_date format_iso_time format_iso_datetime
    namespace export add_days add_months compare_dates day_of_week
    namespace export is_valid_date is_valid_time
    namespace ensemble create

    # Check if a year is a leap year
    proc is_leap_year {year} {
        return [expr {($year % 4 == 0 && $year % 100 != 0) || ($year % 400 == 0)}]
    }

    # Get number of days in a month
    proc days_in_month {year month} {
        switch -- $month {
            1 - 3 - 5 - 7 - 8 - 10 - 12 { return 31 }
            4 - 6 - 9 - 11 { return 30 }
            2 { return [expr {[is_leap_year $year] ? 29 : 28}] }
            default { return 0 }
        }
    }

    # Validate date components
    # Returns dict with {year month day ok error}
    proc validate_date {year month day} {
        if {![string is integer -strict $year]} {
            return [dict create year 0 month 0 day 0 ok 0 error "Invalid year"]
        }

        if {![string is integer -strict $month] || $month < 1 || $month > 12} {
            return [dict create year 0 month 0 day 0 ok 0 error "Invalid month: $month"]
        }

        set maxDay [days_in_month $year $month]
        if {![string is integer -strict $day] || $day < 1 || $day > $maxDay} {
            return [dict create year 0 month 0 day 0 ok 0 error "Invalid day $day for month $month"]
        }

        return [dict create year $year month $month day $day ok 1 error ""]
    }

    # Validate time components
    # Returns dict with {hour minute second nanos ok error}
    proc validate_time {hour minute second {nanos 0}} {
        if {![string is integer -strict $hour] || $hour < 0 || $hour > 23} {
            return [dict create hour 0 minute 0 second 0 nanos 0 ok 0 error "Invalid hour: $hour"]
        }

        if {![string is integer -strict $minute] || $minute < 0 || $minute > 59} {
            return [dict create hour 0 minute 0 second 0 nanos 0 ok 0 error "Invalid minute: $minute"]
        }

        if {![string is integer -strict $second] || $second < 0 || $second > 59} {
            return [dict create hour 0 minute 0 second 0 nanos 0 ok 0 error "Invalid second: $second"]
        }

        return [dict create hour $hour minute $minute second $second nanos $nanos ok 1 error ""]
    }

    # Parse ISO 8601 date (YYYY-MM-DD)
    proc parse_iso_date {dateString} {
        set trimmed [string trim $dateString]

        if {![regexp {^(\d{4})-(\d{2})-(\d{2})$} $trimmed -> year month day]} {
            return [dict create year 0 month 0 day 0 ok 0 error "Invalid date format"]
        }

        # Remove leading zeros for integer comparison
        set month [scan $month %d]
        set day [scan $day %d]
        set year [scan $year %d]

        return [validate_date $year $month $day]
    }

    # Parse ISO 8601 time (HH:MM:SS or HH:MM)
    proc parse_iso_time {timeString} {
        set trimmed [string trim $timeString]

        if {[regexp {^(\d{2}):(\d{2}):(\d{2})(?:\.(\d+))?$} $trimmed -> hour minute second frac]} {
            set hour [scan $hour %d]
            set minute [scan $minute %d]
            set second [scan $second %d]
            set nanos 0
            if {$frac ne ""} {
                # Pad to 9 digits for nanoseconds
                set frac [format "%-9s" $frac]
                regsub -all { } $frac {0} frac
                set nanos [scan [string range $frac 0 8] %d]
            }
            return [validate_time $hour $minute $second $nanos]
        }

        if {[regexp {^(\d{2}):(\d{2})$} $trimmed -> hour minute]} {
            set hour [scan $hour %d]
            set minute [scan $minute %d]
            return [validate_time $hour $minute 0 0]
        }

        return [dict create hour 0 minute 0 second 0 nanos 0 ok 0 error "Invalid time format"]
    }

    # Parse ISO 8601 datetime (YYYY-MM-DDTHH:MM:SS)
    proc parse_iso_datetime {datetimeString} {
        set trimmed [string trim $datetimeString]

        # Handle Z suffix
        set tz ""
        if {[string index $trimmed end] eq "Z"} {
            set tz "Z"
            set trimmed [string range $trimmed 0 end-1]
        }

        # Split by T
        set tPos [string first "T" $trimmed]
        if {$tPos < 0} {
            set tPos [string first " " $trimmed]
        }

        if {$tPos < 0} {
            return [dict create year 0 month 0 day 0 hour 0 minute 0 second 0 ok 0 error "Missing time separator"]
        }

        set datePart [string range $trimmed 0 [expr {$tPos - 1}]]
        set timePart [string range $trimmed [expr {$tPos + 1}] end]

        set dateResult [parse_iso_date $datePart]
        if {![dict get $dateResult ok]} {
            return [dict create year 0 month 0 day 0 hour 0 minute 0 second 0 ok 0 \
                error [dict get $dateResult error]]
        }

        set timeResult [parse_iso_time $timePart]
        if {![dict get $timeResult ok]} {
            return [dict create year 0 month 0 day 0 hour 0 minute 0 second 0 ok 0 \
                error [dict get $timeResult error]]
        }

        return [dict create \
            year [dict get $dateResult year] \
            month [dict get $dateResult month] \
            day [dict get $dateResult day] \
            hour [dict get $timeResult hour] \
            minute [dict get $timeResult minute] \
            second [dict get $timeResult second] \
            nanos [dict get $timeResult nanos] \
            timezone $tz \
            ok 1 \
            error ""]
    }

    # Format date as ISO 8601
    proc format_iso_date {year month day} {
        return [format "%04d-%02d-%02d" $year $month $day]
    }

    # Format time as ISO 8601
    proc format_iso_time {hour minute second {includeSeconds 1}} {
        if {$includeSeconds} {
            return [format "%02d:%02d:%02d" $hour $minute $second]
        }
        return [format "%02d:%02d" $hour $minute]
    }

    # Format datetime as ISO 8601
    proc format_iso_datetime {year month day hour minute second {timezone ""}} {
        set result [format "%04d-%02d-%02dT%02d:%02d:%02d" $year $month $day $hour $minute $second]
        if {$timezone ne ""} {
            append result $timezone
        }
        return $result
    }

    # Add days to a date
    # Returns dict with {year month day ok error}
    proc add_days {year month day daysToAdd} {
        set dateResult [validate_date $year $month $day]
        if {![dict get $dateResult ok]} {
            return $dateResult
        }

        set julianDay [_to_julian $year $month $day]
        set julianDay [expr {$julianDay + $daysToAdd}]
        return [_from_julian $julianDay]
    }

    # Add months to a date
    proc add_months {year month day monthsToAdd} {
        set dateResult [validate_date $year $month $day]
        if {![dict get $dateResult ok]} {
            return $dateResult
        }

        set totalMonths [expr {($year * 12) + $month - 1 + $monthsToAdd}]
        set newYear [expr {$totalMonths / 12}]
        set newMonth [expr {($totalMonths % 12) + 1}]

        # Clamp day to valid range for new month
        set maxDay [days_in_month $newYear $newMonth]
        set newDay [expr {min($day, $maxDay)}]

        return [dict create year $newYear month $newMonth day $newDay ok 1 error ""]
    }

    # Compare two dates
    # Returns -1 if date1 < date2, 0 if equal, 1 if date1 > date2
    proc compare_dates {year1 month1 day1 year2 month2 day2} {
        if {$year1 < $year2} { return -1 }
        if {$year1 > $year2} { return 1 }
        if {$month1 < $month2} { return -1 }
        if {$month1 > $month2} { return 1 }
        if {$day1 < $day2} { return -1 }
        if {$day1 > $day2} { return 1 }
        return 0
    }

    # Get day of week (0 = Sunday, 6 = Saturday)
    proc day_of_week {year month day} {
        set julianDay [_to_julian $year $month $day]
        return [expr {($julianDay + 1) % 7}]
    }

    # Simple date validation
    proc is_valid_date {year month day} {
        set result [validate_date $year $month $day]
        return [dict get $result ok]
    }

    # Simple time validation
    proc is_valid_time {hour minute second} {
        set result [validate_time $hour $minute $second]
        return [dict get $result ok]
    }

    # Convert date to Julian day number (internal)
    proc _to_julian {year month day} {
        set a [expr {(14 - $month) / 12}]
        set y [expr {$year + 4800 - $a}]
        set m [expr {$month + 12 * $a - 3}]
        return [expr {$day + (153 * $m + 2) / 5 + 365 * $y + $y / 4 - $y / 100 + $y / 400 - 32045}]
    }

    # Convert Julian day number to date (internal)
    proc _from_julian {julianDay} {
        set a [expr {$julianDay + 32044}]
        set b [expr {(4 * $a + 3) / 146097}]
        set c [expr {$a - (146097 * $b) / 4}]
        set d [expr {(4 * $c + 3) / 1461}]
        set e [expr {$c - (1461 * $d) / 4}]
        set m [expr {(5 * $e + 2) / 153}]

        set day [expr {$e - (153 * $m + 2) / 5 + 1}]
        set month [expr {$m + 3 - 12 * ($m / 10)}]
        set year [expr {100 * $b + $d - 4800 + $m / 10}]

        return [dict create year $year month $month day $day ok 1 error ""]
    }
}
