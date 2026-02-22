// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe datetime parsing and validation operations.
 *
 * Thin FFI wrapper around libproven's SafeDateTime module. ISO 8601
 * date/time parsing and leap year logic are performed in formally
 * verified Idris 2 code. This module only marshals data to/from the
 * C ABI.
 */
module proven.safe_datetime;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Parsed date/time components.
struct DateTime
{
    int year;
    ubyte month;
    ubyte day;
    ubyte hour;
    ubyte minute;
    ubyte second;
    uint nanosecond;
    short tzOffsetMinutes;

    /// Format as ISO 8601 date string (YYYY-MM-DD).
    string toIsoDateString() const pure @safe
    {
        import std.format : format;
        return format!"%04d-%02d-%02d"(year, month, day);
    }

    /// Format as ISO 8601 datetime string (YYYY-MM-DDTHH:MM:SS).
    string toIsoString() const pure @safe
    {
        import std.format : format;
        return format!"%04d-%02d-%02dT%02d:%02d:%02d"(
            year, month, day, hour, minute, second
        );
    }
}

/// DateTime parsing result.
struct DateTimeResult
{
    DateTime dateTime;
    string error;
    bool ok;

    static DateTimeResult success(DateTime dt)
    {
        return DateTimeResult(dt, "", true);
    }

    static DateTimeResult failure(string error)
    {
        return DateTimeResult(DateTime.init, error, false);
    }
}

/// Parse ISO 8601 date/time string.
/// Supports: YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS, YYYY-MM-DDTHH:MM:SSZ,
/// YYYY-MM-DDTHH:MM:SS+HH:MM
DateTimeResult parseIsoDateTime(string dateTimeString) @trusted nothrow
{
    if (dateTimeString.length == 0)
        return DateTimeResult.failure("Empty input");

    auto result = proven_datetime_parse(
        cast(const(ubyte)*) dateTimeString.ptr, dateTimeString.length
    );

    if (provenFailed(result.status))
        return DateTimeResult.failure("Parse failed");

    auto dt = result.datetime;
    return DateTimeResult.success(DateTime(
        dt.year, dt.month, dt.day,
        dt.hour, dt.minute, dt.second,
        dt.nanosecond, dt.tz_offset_minutes
    ));
}

/// Format DateTime as ISO 8601 string via libproven.
string formatIso8601(DateTime dt) @trusted nothrow
{
    ProvenDateTime pdt;
    pdt.year = dt.year;
    pdt.month = dt.month;
    pdt.day = dt.day;
    pdt.hour = dt.hour;
    pdt.minute = dt.minute;
    pdt.second = dt.second;
    pdt.nanosecond = dt.nanosecond;
    pdt.tz_offset_minutes = dt.tzOffsetMinutes;

    auto result = proven_datetime_format_iso8601(pdt);
    return provenStringToD(result);
}

/// Check if a year is a leap year.
bool isLeapYear(int year) @trusted nothrow @nogc
{
    return proven_datetime_is_leap_year(year);
}

/// Get the number of days in a month. Returns 0 if invalid month.
ubyte daysInMonth(int year, ubyte month) @trusted nothrow @nogc
{
    return proven_datetime_days_in_month(year, month);
}
