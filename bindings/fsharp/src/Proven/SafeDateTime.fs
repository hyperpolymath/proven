// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe date/time operations with timezone awareness and validation.
module SafeDateTime =
    open System
    open System.Globalization

    /// DateTime parsing errors.
    type DateTimeError =
        | InvalidFormat of string
        | OutOfRange of string
        | InvalidTimezone of string
        | EmptyInput
        | AmbiguousTime of string
        | OverflowError

    /// Duration representation (avoiding TimeSpan overflow issues).
    type Duration = {
        TotalMilliseconds: int64
    }

    /// Create duration from milliseconds.
    let milliseconds (ms: int64) : Duration = { TotalMilliseconds = ms }

    /// Create duration from seconds.
    let seconds (s: int64) : Duration = { TotalMilliseconds = s * 1000L }

    /// Create duration from minutes.
    let minutes (m: int64) : Duration = { TotalMilliseconds = m * 60L * 1000L }

    /// Create duration from hours.
    let hours (h: int64) : Duration = { TotalMilliseconds = h * 60L * 60L * 1000L }

    /// Create duration from days.
    let days (d: int64) : Duration = { TotalMilliseconds = d * 24L * 60L * 60L * 1000L }

    /// Create duration from weeks.
    let weeks (w: int64) : Duration = { TotalMilliseconds = w * 7L * 24L * 60L * 60L * 1000L }

    /// Get total seconds from duration.
    let getTotalSeconds (duration: Duration) : int64 =
        duration.TotalMilliseconds / 1000L

    /// Get total minutes from duration.
    let getTotalMinutes (duration: Duration) : int64 =
        duration.TotalMilliseconds / 60000L

    /// Get total hours from duration.
    let getTotalHours (duration: Duration) : int64 =
        duration.TotalMilliseconds / 3600000L

    /// Get total days from duration.
    let getTotalDays (duration: Duration) : int64 =
        duration.TotalMilliseconds / 86400000L

    /// Add two durations.
    let addDuration (a: Duration) (b: Duration) : Duration option =
        try
            Some { TotalMilliseconds = Checked.(+) a.TotalMilliseconds b.TotalMilliseconds }
        with
        | :? OverflowException -> None

    /// Subtract durations.
    let subtractDuration (a: Duration) (b: Duration) : Duration option =
        try
            Some { TotalMilliseconds = Checked.(-) a.TotalMilliseconds b.TotalMilliseconds }
        with
        | :? OverflowException -> None

    /// Multiply duration by scalar.
    let multiplyDuration (duration: Duration) (scalar: int64) : Duration option =
        try
            Some { TotalMilliseconds = Checked.(*) duration.TotalMilliseconds scalar }
        with
        | :? OverflowException -> None

    /// Divide duration by scalar.
    let divideDuration (duration: Duration) (divisor: int64) : Duration option =
        if divisor = 0L then None
        else Some { TotalMilliseconds = duration.TotalMilliseconds / divisor }

    /// Negate duration.
    let negateDuration (duration: Duration) : Duration =
        { TotalMilliseconds = -duration.TotalMilliseconds }

    /// Get absolute duration.
    let absDuration (duration: Duration) : Duration =
        { TotalMilliseconds = Math.Abs(duration.TotalMilliseconds) }

    /// Compare durations.
    let compareDuration (a: Duration) (b: Duration) : int =
        compare a.TotalMilliseconds b.TotalMilliseconds

    /// Parse ISO 8601 datetime string.
    let parseIso8601 (input: string) : Result<DateTimeOffset, DateTimeError> =
        if String.IsNullOrWhiteSpace(input) then
            Error EmptyInput
        else
            try
                Ok(DateTimeOffset.Parse(input, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind))
            with
            | :? FormatException -> Error(InvalidFormat input)
            | _ -> Error(InvalidFormat input)

    /// Parse ISO 8601 datetime, returning Option.
    let tryParseIso8601 (input: string) : DateTimeOffset option =
        match parseIso8601 input with
        | Ok dt -> Some dt
        | Error _ -> None

    /// Parse RFC 2822 datetime string (email format).
    let parseRfc2822 (input: string) : Result<DateTimeOffset, DateTimeError> =
        if String.IsNullOrWhiteSpace(input) then
            Error EmptyInput
        else
            try
                let formats = [|
                    "ddd, dd MMM yyyy HH:mm:ss zzz"
                    "ddd, d MMM yyyy HH:mm:ss zzz"
                    "dd MMM yyyy HH:mm:ss zzz"
                    "d MMM yyyy HH:mm:ss zzz"
                |]
                Ok(DateTimeOffset.ParseExact(input.Trim(), formats, CultureInfo.InvariantCulture, DateTimeStyles.None))
            with
            | :? FormatException -> Error(InvalidFormat input)
            | _ -> Error(InvalidFormat input)

    /// Parse Unix timestamp (seconds since epoch).
    let fromUnixSeconds (seconds: int64) : Result<DateTimeOffset, DateTimeError> =
        try
            Ok(DateTimeOffset.FromUnixTimeSeconds(seconds))
        with
        | :? ArgumentOutOfRangeException -> Error(OutOfRange(sprintf "%d" seconds))

    /// Parse Unix timestamp (milliseconds since epoch).
    let fromUnixMilliseconds (milliseconds: int64) : Result<DateTimeOffset, DateTimeError> =
        try
            Ok(DateTimeOffset.FromUnixTimeMilliseconds(milliseconds))
        with
        | :? ArgumentOutOfRangeException -> Error(OutOfRange(sprintf "%d" milliseconds))

    /// Get Unix timestamp in seconds.
    let toUnixSeconds (dt: DateTimeOffset) : int64 =
        dt.ToUnixTimeSeconds()

    /// Get Unix timestamp in milliseconds.
    let toUnixMilliseconds (dt: DateTimeOffset) : int64 =
        dt.ToUnixTimeMilliseconds()

    /// Format as ISO 8601.
    let formatIso8601 (dt: DateTimeOffset) : string =
        dt.ToString("o", CultureInfo.InvariantCulture)

    /// Format as ISO 8601 date only.
    let formatIso8601Date (dt: DateTimeOffset) : string =
        dt.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)

    /// Format as ISO 8601 time only.
    let formatIso8601Time (dt: DateTimeOffset) : string =
        dt.ToString("HH:mm:ss", CultureInfo.InvariantCulture)

    /// Format as RFC 2822.
    let formatRfc2822 (dt: DateTimeOffset) : string =
        dt.ToString("ddd, dd MMM yyyy HH:mm:ss zzz", CultureInfo.InvariantCulture)

    /// Format with custom format string.
    let formatCustom (format: string) (dt: DateTimeOffset) : string =
        dt.ToString(format, CultureInfo.InvariantCulture)

    /// Get current UTC time.
    let nowUtc () : DateTimeOffset = DateTimeOffset.UtcNow

    /// Get current local time.
    let nowLocal () : DateTimeOffset = DateTimeOffset.Now

    /// Create datetime from components (UTC).
    let createUtc (year: int) (month: int) (day: int) (hour: int) (minute: int) (second: int) : Result<DateTimeOffset, DateTimeError> =
        try
            Ok(DateTimeOffset(year, month, day, hour, minute, second, TimeSpan.Zero))
        with
        | :? ArgumentOutOfRangeException as ex -> Error(OutOfRange ex.Message)

    /// Create date only (midnight UTC).
    let createDateUtc (year: int) (month: int) (day: int) : Result<DateTimeOffset, DateTimeError> =
        createUtc year month day 0 0 0

    /// Add duration to datetime.
    let add (duration: Duration) (dt: DateTimeOffset) : Result<DateTimeOffset, DateTimeError> =
        try
            Ok(dt.AddMilliseconds(float duration.TotalMilliseconds))
        with
        | :? ArgumentOutOfRangeException -> Error OverflowError

    /// Subtract duration from datetime.
    let subtract (duration: Duration) (dt: DateTimeOffset) : Result<DateTimeOffset, DateTimeError> =
        add (negateDuration duration) dt

    /// Get difference between two datetimes as duration.
    let diff (a: DateTimeOffset) (b: DateTimeOffset) : Duration =
        let diffMs = (a - b).TotalMilliseconds
        { TotalMilliseconds = int64 diffMs }

    /// Add years (handles leap years).
    let addYears (years: int) (dt: DateTimeOffset) : Result<DateTimeOffset, DateTimeError> =
        try
            Ok(dt.AddYears(years))
        with
        | :? ArgumentOutOfRangeException -> Error OverflowError

    /// Add months.
    let addMonths (months: int) (dt: DateTimeOffset) : Result<DateTimeOffset, DateTimeError> =
        try
            Ok(dt.AddMonths(months))
        with
        | :? ArgumentOutOfRangeException -> Error OverflowError

    /// Add days.
    let addDays (d: float) (dt: DateTimeOffset) : Result<DateTimeOffset, DateTimeError> =
        try
            Ok(dt.AddDays(d))
        with
        | :? ArgumentOutOfRangeException -> Error OverflowError

    /// Add hours.
    let addHoursToDateTime (h: float) (dt: DateTimeOffset) : Result<DateTimeOffset, DateTimeError> =
        try
            Ok(dt.AddHours(h))
        with
        | :? ArgumentOutOfRangeException -> Error OverflowError

    /// Add minutes.
    let addMinutesToDateTime (m: float) (dt: DateTimeOffset) : Result<DateTimeOffset, DateTimeError> =
        try
            Ok(dt.AddMinutes(m))
        with
        | :? ArgumentOutOfRangeException -> Error OverflowError

    /// Add seconds.
    let addSecondsToDateTime (s: float) (dt: DateTimeOffset) : Result<DateTimeOffset, DateTimeError> =
        try
            Ok(dt.AddSeconds(s))
        with
        | :? ArgumentOutOfRangeException -> Error OverflowError

    /// Get start of day.
    let startOfDay (dt: DateTimeOffset) : DateTimeOffset =
        DateTimeOffset(dt.Date, dt.Offset)

    /// Get end of day.
    let endOfDay (dt: DateTimeOffset) : DateTimeOffset =
        DateTimeOffset(dt.Date.AddDays(1.0).AddTicks(-1L), dt.Offset)

    /// Get start of month.
    let startOfMonth (dt: DateTimeOffset) : DateTimeOffset =
        DateTimeOffset(dt.Year, dt.Month, 1, 0, 0, 0, dt.Offset)

    /// Get end of month.
    let endOfMonth (dt: DateTimeOffset) : DateTimeOffset =
        let daysInMonth = DateTime.DaysInMonth(dt.Year, dt.Month)
        DateTimeOffset(dt.Year, dt.Month, daysInMonth, 23, 59, 59, 999, dt.Offset)

    /// Get start of year.
    let startOfYear (dt: DateTimeOffset) : DateTimeOffset =
        DateTimeOffset(dt.Year, 1, 1, 0, 0, 0, dt.Offset)

    /// Get end of year.
    let endOfYear (dt: DateTimeOffset) : DateTimeOffset =
        DateTimeOffset(dt.Year, 12, 31, 23, 59, 59, 999, dt.Offset)

    /// Check if datetime is in the past.
    let isPast (dt: DateTimeOffset) : bool =
        dt < DateTimeOffset.UtcNow

    /// Check if datetime is in the future.
    let isFuture (dt: DateTimeOffset) : bool =
        dt > DateTimeOffset.UtcNow

    /// Check if datetime is today.
    let isToday (dt: DateTimeOffset) : bool =
        dt.Date = DateTimeOffset.Now.Date

    /// Check if year is a leap year.
    let isLeapYear (year: int) : bool =
        DateTime.IsLeapYear(year)

    /// Get day of week (0 = Sunday, 6 = Saturday).
    let dayOfWeek (dt: DateTimeOffset) : int =
        int dt.DayOfWeek

    /// Get day of year (1-366).
    let dayOfYear (dt: DateTimeOffset) : int =
        dt.DayOfYear

    /// Get week of year (ISO 8601).
    let weekOfYear (dt: DateTimeOffset) : int =
        CultureInfo.InvariantCulture.Calendar.GetWeekOfYear(
            dt.DateTime,
            CalendarWeekRule.FirstFourDayWeek,
            DayOfWeek.Monday)

    /// Check if datetime is a weekend.
    let isWeekend (dt: DateTimeOffset) : bool =
        let dow = dt.DayOfWeek
        dow = DayOfWeek.Saturday || dow = DayOfWeek.Sunday

    /// Check if datetime is a weekday.
    let isWeekday (dt: DateTimeOffset) : bool =
        not (isWeekend dt)

    /// Convert to UTC.
    let toUtc (dt: DateTimeOffset) : DateTimeOffset =
        dt.ToUniversalTime()

    /// Convert to timezone.
    let toTimezone (timezoneId: string) (dt: DateTimeOffset) : Result<DateTimeOffset, DateTimeError> =
        try
            let tz = TimeZoneInfo.FindSystemTimeZoneById(timezoneId)
            Ok(TimeZoneInfo.ConvertTime(dt, tz))
        with
        | :? TimeZoneNotFoundException -> Error(InvalidTimezone timezoneId)
        | :? InvalidTimeZoneException -> Error(InvalidTimezone timezoneId)

    /// Get timezone offset as string (+HH:mm or -HH:mm).
    let getTimezoneOffset (dt: DateTimeOffset) : string =
        let offset = dt.Offset
        let sign = if offset < TimeSpan.Zero then "-" else "+"
        sprintf "%s%02d:%02d" sign (Math.Abs(offset.Hours)) (Math.Abs(offset.Minutes))

    /// Compare two datetimes.
    let compareDateTimes (a: DateTimeOffset) (b: DateTimeOffset) : int =
        DateTimeOffset.Compare(a, b)

    /// Check if datetime is between two others.
    let isBetween (start: DateTimeOffset) (endDt: DateTimeOffset) (dt: DateTimeOffset) : bool =
        dt >= start && dt <= endDt

    /// Get minimum of two datetimes.
    let minDateTime (a: DateTimeOffset) (b: DateTimeOffset) : DateTimeOffset =
        if a <= b then a else b

    /// Get maximum of two datetimes.
    let maxDateTime (a: DateTimeOffset) (b: DateTimeOffset) : DateTimeOffset =
        if a >= b then a else b

    /// Clamp datetime to range.
    let clampDateTime (minDt: DateTimeOffset) (maxDt: DateTimeOffset) (dt: DateTimeOffset) : DateTimeOffset =
        if dt < minDt then minDt
        elif dt > maxDt then maxDt
        else dt
