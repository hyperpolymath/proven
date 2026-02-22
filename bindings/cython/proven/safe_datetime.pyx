# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# cython: language_level=3

"""
SafeDateTime - ISO 8601 date/time handling.

All functions delegate to libproven via direct C FFI calls.
Returns None on error. NEVER reimplements logic.
"""

from libc.stdint cimport uint8_t, int32_t
from libc.stddef cimport size_t

cimport proven.lib_proven as c


cdef _decode_string_result(c.ProvenStringResult result):
    """Extract a Python string from a StringResult, freeing the C memory."""
    if result.status != c.PROVEN_OK:
        return None
    if result.value == NULL:
        return None
    cdef bytes py_bytes = result.value[:result.length]
    c.proven_free_string(result.value)
    return py_bytes.decode("utf-8", errors="replace")


def parse_datetime(str s):
    """Parse an ISO 8601 date/time string.
    Returns a dict with keys: year, month, day, hour, minute, second,
    nanosecond, tz_offset_minutes. Returns None on parse failure.

    >>> result = parse_datetime("2026-01-15T10:30:00Z")
    >>> result["year"]
    2026
    >>> result["month"]
    1
    """
    cdef bytes b = s.encode("utf-8")
    cdef const uint8_t* ptr = <const uint8_t*><char*>b
    cdef size_t length = len(b)
    cdef c.ProvenDateTimeResult result
    with nogil:
        result = c.proven_datetime_parse(ptr, length)
    if result.status != c.PROVEN_OK:
        return None

    cdef c.ProvenDateTime dt = result.datetime
    return {
        "year":              int(dt.year),
        "month":             int(dt.month),
        "day":               int(dt.day),
        "hour":              int(dt.hour),
        "minute":            int(dt.minute),
        "second":            int(dt.second),
        "nanosecond":        int(dt.nanosecond),
        "tz_offset_minutes": int(dt.tz_offset_minutes),
    }


def format_iso8601(dict dt_dict):
    """Format a datetime dict as ISO 8601 string. Returns None on error.

    >>> format_iso8601({"year": 2026, "month": 1, "day": 15, "hour": 10,
    ...                 "minute": 30, "second": 0, "nanosecond": 0,
    ...                 "tz_offset_minutes": 0})
    '2026-01-15T10:30:00Z'
    """
    cdef c.ProvenDateTime dt
    dt.year = dt_dict.get("year", 0)
    dt.month = dt_dict.get("month", 1)
    dt.day = dt_dict.get("day", 1)
    dt.hour = dt_dict.get("hour", 0)
    dt.minute = dt_dict.get("minute", 0)
    dt.second = dt_dict.get("second", 0)
    dt.nanosecond = dt_dict.get("nanosecond", 0)
    dt.tz_offset_minutes = dt_dict.get("tz_offset_minutes", 0)

    cdef c.ProvenStringResult result
    with nogil:
        result = c.proven_datetime_format_iso8601(dt)
    return _decode_string_result(result)


def is_leap_year(int year):
    """Check if a year is a leap year.

    >>> is_leap_year(2024)
    True
    >>> is_leap_year(2023)
    False
    """
    cdef bint result
    with nogil:
        result = c.proven_datetime_is_leap_year(year)
    return bool(result)


def days_in_month(int year, int month):
    """Get number of days in a month. Returns 0 if invalid month.

    >>> days_in_month(2024, 2)
    29
    >>> days_in_month(2023, 2)
    28
    """
    cdef unsigned char result
    with nogil:
        result = c.proven_datetime_days_in_month(year, month)
    return int(result)
