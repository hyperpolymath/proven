// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe datetime operations delegated to libproven FFI.
///
/// ISO 8601 parsing and formatting via the formally verified
/// Idris 2 core.

import CProven

/// Parsed datetime components returned by SafeDatetime.parse.
public struct DateTimeComponents: Equatable, Sendable {
    public let year: Int32
    public let month: UInt8
    public let day: UInt8
    public let hour: UInt8
    public let minute: UInt8
    public let second: UInt8
    public let nanosecond: UInt32
    public let tzOffsetMinutes: Int16
}

public enum SafeDatetime {
    /// Parse an ISO 8601 date string.
    public static func parse(_ dateString: String) -> Result<DateTimeComponents, ProvenError> {
        withStringBytes(dateString) { ptr, len in
            let result = proven_datetime_parse(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            let dt = result.datetime
            return .success(DateTimeComponents(
                year: dt.year,
                month: dt.month,
                day: dt.day,
                hour: dt.hour,
                minute: dt.minute,
                second: dt.second,
                nanosecond: dt.nanosecond,
                tzOffsetMinutes: dt.tz_offset_minutes
            ))
        }
    }

    /// Format a DateTimeComponents as an ISO 8601 string.
    public static func formatISO8601(_ dt: DateTimeComponents) -> Result<String, ProvenError> {
        var cDt = ProvenDateTime()
        cDt.year = dt.year
        cDt.month = dt.month
        cDt.day = dt.day
        cDt.hour = dt.hour
        cDt.minute = dt.minute
        cDt.second = dt.second
        cDt.nanosecond = dt.nanosecond
        cDt.tz_offset_minutes = dt.tzOffsetMinutes
        return consumeStringResult(proven_datetime_format_iso8601(cDt))
    }

    /// Check if a year is a leap year.
    public static func isLeapYear(_ year: Int32) -> Bool {
        proven_datetime_is_leap_year(year)
    }

    /// Get the number of days in a month.
    public static func daysInMonth(year: Int32, month: UInt8) -> UInt8 {
        proven_datetime_days_in_month(year, month)
    }
}
