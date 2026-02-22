# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeDateTime -- ISO 8601 date/time parsing and formatting.

Parses ISO 8601 date strings into structured components and formats DateTime
values back to ISO 8601 strings. Also provides leap year detection and
days-in-month queries. All computation is delegated to the formally verified
Idris 2 core via C FFI. No logic is reimplemented here.

Functions:
    parse_datetime    -- Parse an ISO 8601 date/time string
    format_iso8601    -- Format a DateTime as ISO 8601 string
    is_leap_year      -- Check if a year is a leap year
    days_in_month     -- Get number of days in a given month/year
"""

from memory import UnsafePointer

from .lib_proven import (
    DateTime,
    DateTimeResult,
    StringResult,
    proven_datetime_parse,
    proven_datetime_format_iso8601,
    proven_datetime_is_leap_year,
    proven_datetime_days_in_month,
    proven_free_string,
    string_result_to_string,
    PROVEN_OK,
)


fn _str_to_ptr(s: String) -> (UnsafePointer[UInt8], Int):
    """Extract a raw byte pointer and length from a Mojo String."""
    var byte_slice = s.as_bytes()
    var length = len(byte_slice)
    var ptr = byte_slice.unsafe_ptr()
    return (ptr, length)


fn parse_datetime(iso_string: String) -> Optional[DateTime]:
    """Parse an ISO 8601 date/time string.

    Supported formats:
        YYYY-MM-DD
        YYYY-MM-DDTHH:MM:SS
        YYYY-MM-DDTHH:MM:SSZ
        YYYY-MM-DDTHH:MM:SS+HH:MM

    Returns None if the string cannot be parsed.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(iso_string)
    var result = proven_datetime_parse(pair[0], pair[1])
    if result.succeeded():
        return result.datetime
    return None


fn format_iso8601(dt: DateTime) -> Optional[String]:
    """Format a DateTime as an ISO 8601 string.

    Returns None on error.
    All computation performed in formally verified Idris 2 code.
    """
    var result = proven_datetime_format_iso8601(dt)
    return string_result_to_string(result)


fn is_leap_year(year: Int32) -> Bool:
    """Check if a year is a leap year.

    All computation performed in formally verified Idris 2 code.
    """
    return proven_datetime_is_leap_year(year)


fn days_in_month(year: Int32, month: UInt8) -> UInt8:
    """Get the number of days in a given month and year.

    Returns 0 if the month is invalid (not 1-12).
    All computation performed in formally verified Idris 2 code.
    """
    return proven_datetime_days_in_month(year, month)


fn make_datetime(
    year: Int32,
    month: UInt8,
    day: UInt8,
    hour: UInt8 = 0,
    minute: UInt8 = 0,
    second: UInt8 = 0,
    nanosecond: UInt32 = 0,
    tz_offset_minutes: Int16 = 0,
) -> DateTime:
    """Create a DateTime struct from individual components.

    This is a convenience constructor for building DateTime values to pass
    to format_iso8601. No validation is performed here; validation happens
    in the Idris 2 core when the DateTime is used.
    """
    return DateTime(
        year, month, day, hour, minute, second, nanosecond, tz_offset_minutes
    )
