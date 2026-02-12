// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe datetime parsing and validation operations.
 * Provides ISO 8601 date/time parsing with validation.
 */
module proven.safe_datetime;

import std.array : split;
import std.conv : to, ConvException;
import std.typecons : Nullable, nullable;

/// Parsed date components.
struct Date
{
    /// Year (e.g., 2025)
    int year;
    /// Month (1-12)
    ubyte month;
    /// Day (1-31)
    ubyte day;

    /// Format as ISO 8601 date string (YYYY-MM-DD).
    string toIsoString() const pure @safe
    {
        import std.format : format;
        return format!"%04d-%02d-%02d"(year, month, day);
    }
}

/// Parsed time components.
struct Time
{
    /// Hour (0-23)
    ubyte hour;
    /// Minute (0-59)
    ubyte minute;
    /// Second (0-59)
    ubyte second;
    /// Milliseconds (0-999)
    ushort millis;

    /// Format as ISO 8601 time string (HH:MM:SS).
    string toIsoString() const pure @safe
    {
        import std.format : format;
        return format!"%02d:%02d:%02d"(hour, minute, second);
    }

    /// Format with milliseconds.
    string toIsoStringWithMillis() const pure @safe
    {
        import std.format : format;
        return format!"%02d:%02d:%02d.%03d"(hour, minute, second, millis);
    }
}

/// Combined date and time.
struct DateTime
{
    Date date;
    Time time;

    /// Format as ISO 8601 datetime string.
    string toIsoString() const pure @safe
    {
        return date.toIsoString() ~ "T" ~ time.toIsoString();
    }
}

/// Date parsing result.
struct DateResult
{
    Date date;
    string error;
    bool ok;

    static DateResult success(Date date)
    {
        return DateResult(date, "", true);
    }

    static DateResult failure(string error)
    {
        return DateResult(Date.init, error, false);
    }
}

/// Time parsing result.
struct TimeResult
{
    Time time;
    string error;
    bool ok;

    static TimeResult success(Time time)
    {
        return TimeResult(time, "", true);
    }

    static TimeResult failure(string error)
    {
        return TimeResult(Time.init, error, false);
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

/// Check if a year is a leap year.
bool isLeapYear(int year) pure nothrow @safe @nogc
{
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

/// Get the number of days in a month.
Nullable!ubyte daysInMonth(int year, ubyte month) pure nothrow @safe @nogc
{
    if (month < 1 || month > 12)
        return Nullable!ubyte.init;

    immutable ubyte[12] daysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    if (month == 2 && isLeapYear(year))
        return nullable(cast(ubyte) 29);
    return nullable(daysPerMonth[month - 1]);
}

/// Validate a date.
DateResult validateDate(int year, ubyte month, ubyte day) pure @safe
{
    if (month < 1 || month > 12)
        return DateResult.failure("Invalid month (must be 1-12)");

    auto maxDaysNullable = daysInMonth(year, month);
    if (maxDaysNullable.isNull)
        return DateResult.failure("Invalid month");

    immutable maxDays = maxDaysNullable.get;
    if (day < 1 || day > maxDays)
        return DateResult.failure("Invalid day for month");

    return DateResult.success(Date(year, month, day));
}

/// Validate a time.
TimeResult validateTime(ubyte hour, ubyte minute, ubyte second, ushort millis = 0) pure @safe
{
    if (hour > 23)
        return TimeResult.failure("Invalid hour (must be 0-23)");
    if (minute > 59)
        return TimeResult.failure("Invalid minute (must be 0-59)");
    if (second > 59)
        return TimeResult.failure("Invalid second (must be 0-59)");
    if (millis > 999)
        return TimeResult.failure("Invalid milliseconds (must be 0-999)");

    return TimeResult.success(Time(hour, minute, second, millis));
}

/// Parse ISO 8601 date (YYYY-MM-DD).
DateResult parseIsoDate(string dateString) pure @safe
{
    auto parts = dateString.split('-');
    if (parts.length != 3)
        return DateResult.failure("Invalid date format (expected YYYY-MM-DD)");

    try
    {
        immutable year = parts[0].to!int;
        immutable month = parts[1].to!ubyte;
        immutable day = parts[2].to!ubyte;
        return validateDate(year, month, day);
    }
    catch (ConvException)
    {
        return DateResult.failure("Invalid numeric values in date");
    }
}

/// Parse ISO 8601 time (HH:MM:SS or HH:MM:SS.mmm).
TimeResult parseIsoTime(string timeString) pure @safe
{
    // Handle timezone suffix
    auto cleanTime = timeString;
    if (cleanTime.length > 0 && cleanTime[$ - 1] == 'Z')
        cleanTime = cleanTime[0 .. $ - 1];

    // Split off milliseconds
    ushort millis = 0;
    auto dotIdx = cleanTime.indexOf('.');
    if (dotIdx >= 0)
    {
        try
        {
            auto millisPart = cleanTime[dotIdx + 1 .. $];
            // Pad or truncate to 3 digits
            if (millisPart.length > 3)
                millisPart = millisPart[0 .. 3];
            while (millisPart.length < 3)
                millisPart ~= '0';
            millis = millisPart.to!ushort;
        }
        catch (ConvException)
        {
            return TimeResult.failure("Invalid milliseconds");
        }
        cleanTime = cleanTime[0 .. dotIdx];
    }

    auto parts = cleanTime.split(':');
    if (parts.length < 2 || parts.length > 3)
        return TimeResult.failure("Invalid time format (expected HH:MM or HH:MM:SS)");

    try
    {
        immutable hour = parts[0].to!ubyte;
        immutable minute = parts[1].to!ubyte;
        immutable second = parts.length > 2 ? parts[2].to!ubyte : cast(ubyte) 0;
        return validateTime(hour, minute, second, millis);
    }
    catch (ConvException)
    {
        return TimeResult.failure("Invalid numeric values in time");
    }
}

/// Parse ISO 8601 datetime (YYYY-MM-DDTHH:MM:SS).
DateTimeResult parseIsoDateTime(string dateTimeString) pure @safe
{
    auto tIdx = dateTimeString.indexOf('T');
    if (tIdx < 0)
        tIdx = dateTimeString.indexOf(' ');
    if (tIdx < 0)
        return DateTimeResult.failure("Invalid datetime format (missing T or space separator)");

    auto dateResult = parseIsoDate(dateTimeString[0 .. tIdx]);
    if (!dateResult.ok)
        return DateTimeResult.failure(dateResult.error);

    auto timeResult = parseIsoTime(dateTimeString[tIdx + 1 .. $]);
    if (!timeResult.ok)
        return DateTimeResult.failure(timeResult.error);

    return DateTimeResult.success(DateTime(dateResult.date, timeResult.time));
}

/// Get the day of the week (0=Sunday, 6=Saturday).
ubyte dayOfWeek(Date date) pure nothrow @safe @nogc
{
    // Zeller's congruence for Gregorian calendar
    int year = date.year;
    int month = date.month;
    int day = date.day;

    if (month < 3)
    {
        month += 12;
        year--;
    }

    immutable k = year % 100;
    immutable j = year / 100;
    immutable h = (day + (13 * (month + 1)) / 5 + k + k / 4 + j / 4 - 2 * j) % 7;
    return cast(ubyte)((h + 6) % 7);
}

/// Get the name of the day of the week.
string dayOfWeekName(ubyte dow) pure nothrow @safe
{
    immutable string[7] names = [
        "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
    ];
    return dow < 7 ? names[dow] : "Unknown";
}

/// Get the name of a month.
string monthName(ubyte month) pure nothrow @safe
{
    immutable string[12] names = [
        "January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December"
    ];
    return (month >= 1 && month <= 12) ? names[month - 1] : "Unknown";
}

/// Calculate difference in days between two dates.
long daysBetween(Date d1, Date d2) pure nothrow @safe
{
    // Convert to Julian Day Number for easy calculation
    long jdn1 = toJulianDayNumber(d1);
    long jdn2 = toJulianDayNumber(d2);
    return jdn2 - jdn1;
}

/// Convert date to Julian Day Number.
private long toJulianDayNumber(Date date) pure nothrow @safe @nogc
{
    immutable a = (14 - date.month) / 12;
    immutable y = date.year + 4800 - a;
    immutable m = date.month + 12 * a - 3;
    return date.day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045;
}

/// Find index of character in string.
private ptrdiff_t indexOf(string s, char c) pure nothrow @safe @nogc
{
    foreach (i, ch; s)
    {
        if (ch == c)
            return i;
    }
    return -1;
}

// Unit tests
unittest
{
    // Test leap year detection
    assert(isLeapYear(2000));
    assert(isLeapYear(2024));
    assert(!isLeapYear(1900));
    assert(!isLeapYear(2023));

    // Test days in month
    assert(daysInMonth(2024, 2).get == 29);
    assert(daysInMonth(2023, 2).get == 28);
    assert(daysInMonth(2024, 1).get == 31);

    // Test date parsing
    auto date = parseIsoDate("2025-01-15");
    assert(date.ok);
    assert(date.date.year == 2025);
    assert(date.date.month == 1);
    assert(date.date.day == 15);

    // Test time parsing
    auto time = parseIsoTime("14:30:45");
    assert(time.ok);
    assert(time.time.hour == 14);
    assert(time.time.minute == 30);
    assert(time.time.second == 45);

    // Test datetime parsing
    auto dt = parseIsoDateTime("2025-01-15T14:30:45");
    assert(dt.ok);

    // Test invalid dates
    assert(!parseIsoDate("2025-13-01").ok);
    assert(!parseIsoDate("2025-02-30").ok);

    // Test day of week
    auto testDate = Date(2025, 1, 15); // Wednesday
    assert(dayOfWeek(testDate) == 3);
}
