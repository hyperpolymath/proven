# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven::SafeDateTime - ISO 8601 date/time handling.
#
# Thin wrapper over libproven's SafeDateTime module.  All date/time parsing,
# formatting, and calendar calculations are performed in formally verified
# Idris 2 code.  This module does NOT reimplement any date/time logic.

unit module Proven::SafeDateTime;

use NativeCall;
use Proven::LibProven;

# ============================================================================
# ISO 8601 parsing
# ============================================================================

#| Parse an ISO 8601 date/time string.
#|
#| Supported formats:
#|   YYYY-MM-DD
#|   YYYY-MM-DDTHH:MM:SS
#|   YYYY-MM-DDTHH:MM:SSZ
#|   YYYY-MM-DDTHH:MM:SS+HH:MM
#|
#| Returns a hash with keys:
#|   year, month, day, hour, minute, second, nanosecond, tz-offset-minutes
#| or Nil on parse failure.
sub datetime-parse(Str:D $s --> Hash) is export {
    my ($buf, $len) = str-to-buf($s);
    my DateTimeResult $r = proven_datetime_parse(nativecast(Pointer, $buf), $len);
    return Nil unless $r.status == 0;
    return {
        year              => $r.year,
        month             => $r.month,
        day               => $r.day,
        hour              => $r.hour,
        minute            => $r.minute,
        second            => $r.second,
        nanosecond        => $r.nanosecond,
        tz-offset-minutes => $r.tz-offset-minutes,
    };
}

# ============================================================================
# ISO 8601 formatting
# ============================================================================

#| Format a datetime hash (as returned by datetime-parse) to an ISO 8601 string.
#| Returns the formatted string, or Nil on error.
sub datetime-format-iso8601(
    Int:D :$year,
    Int:D :$month,
    Int:D :$day,
    Int:D :$hour = 0,
    Int:D :$minute = 0,
    Int:D :$second = 0,
    Int:D :$nanosecond = 0,
    Int:D :$tz-offset-minutes = 0,
    --> Str
) is export {
    my ProvenDateTime $dt .= new(
        :$year, :$month, :$day, :$hour, :$minute, :$second,
        :_pad0(0), :_pad1(0), :_pad2(0),
        :$nanosecond, :$tz-offset-minutes,
    );
    my StringResult $r = proven_datetime_format_iso8601($dt);
    return extract-string($r);
}

# ============================================================================
# Calendar utilities
# ============================================================================

#| Check if a year is a leap year.
sub is-leap-year(Int:D $year --> Bool) is export {
    return proven_datetime_is_leap_year($year);
}

#| Get the number of days in a given month of a given year.
#| Returns 0 if the month is invalid.
sub days-in-month(Int:D $year, Int:D $month --> Int) is export {
    return proven_datetime_days_in_month($year, $month);
}
