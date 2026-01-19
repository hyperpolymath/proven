// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe datetime parsing and manipulation that cannot crash.

const std = @import("std");

/// Error types for datetime operations.
pub const DateTimeError = error{
    InvalidFormat,
    InvalidDate,
    InvalidTime,
    InvalidTimezone,
    Overflow,
};

/// A safe datetime representation
pub const DateTime = struct {
    year: i32,
    month: u8,    // 1-12
    day: u8,      // 1-31
    hour: u8,     // 0-23
    minute: u8,   // 0-59
    second: u8,   // 0-59
    nanosecond: u32 = 0, // 0-999999999
    utc_offset_minutes: i16 = 0,

    /// Check if the datetime is valid
    pub fn isValid(self: DateTime) bool {
        if (self.month < 1 or self.month > 12) return false;
        if (self.day < 1 or self.day > daysInMonth(self.year, self.month)) return false;
        if (self.hour > 23) return false;
        if (self.minute > 59) return false;
        if (self.second > 59) return false;
        if (self.nanosecond > 999999999) return false;
        if (self.utc_offset_minutes < -720 or self.utc_offset_minutes > 840) return false;
        return true;
    }

    /// Convert to Unix timestamp (seconds since 1970-01-01 00:00:00 UTC)
    pub fn toUnixTimestamp(self: DateTime) DateTimeError!i64 {
        if (!self.isValid()) return error.InvalidDate;

        // Days from year
        var days: i64 = 0;
        var y: i32 = 1970;
        while (y < self.year) : (y += 1) {
            days += if (isLeapYear(y)) 366 else 365;
        }
        while (y > self.year) : (y -= 1) {
            days -= if (isLeapYear(y - 1)) 366 else 365;
        }

        // Days from month
        var m: u8 = 1;
        while (m < self.month) : (m += 1) {
            days += daysInMonth(self.year, m);
        }

        // Days in month
        days += self.day - 1;

        // Convert to seconds
        var seconds: i64 = days * 86400;
        seconds += @as(i64, self.hour) * 3600;
        seconds += @as(i64, self.minute) * 60;
        seconds += self.second;

        // Apply timezone offset
        seconds -= @as(i64, self.utc_offset_minutes) * 60;

        return seconds;
    }

    /// Create from Unix timestamp
    pub fn fromUnixTimestamp(timestamp: i64) DateTime {
        var remaining = timestamp;

        // Calculate year
        var year: i32 = 1970;
        while (remaining >= daysInYear(year) * 86400) {
            remaining -= daysInYear(year) * 86400;
            year += 1;
        }
        while (remaining < 0) {
            year -= 1;
            remaining += daysInYear(year) * 86400;
        }

        // Calculate month and day
        var month: u8 = 1;
        while (remaining >= @as(i64, daysInMonth(year, month)) * 86400) {
            remaining -= @as(i64, daysInMonth(year, month)) * 86400;
            month += 1;
        }

        const day: u8 = @intCast(@divTrunc(remaining, 86400) + 1);
        remaining = @mod(remaining, 86400);

        const hour: u8 = @intCast(@divTrunc(remaining, 3600));
        remaining = @mod(remaining, 3600);

        const minute: u8 = @intCast(@divTrunc(remaining, 60));
        const second: u8 = @intCast(@mod(remaining, 60));

        return DateTime{
            .year = year,
            .month = month,
            .day = day,
            .hour = hour,
            .minute = minute,
            .second = second,
        };
    }

    /// Format as ISO 8601 string
    pub fn toIso8601(self: DateTime, buffer: []u8) DateTimeError![]u8 {
        if (!self.isValid()) return error.InvalidDate;
        if (buffer.len < 25) return error.InvalidFormat;

        const written = std.fmt.bufPrint(buffer, "{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}", .{
            self.year, self.month, self.day,
            self.hour, self.minute, self.second,
        }) catch return error.Overflow;

        return written;
    }

    /// Get day of week (0 = Sunday, 6 = Saturday)
    pub fn dayOfWeek(self: DateTime) u8 {
        // Zeller's congruence
        var y = self.year;
        var m = self.month;
        if (m < 3) {
            m += 12;
            y -= 1;
        }
        const q = self.day;
        const k: i32 = @mod(y, 100);
        const j: i32 = @divTrunc(y, 100);

        const h = @mod(q + @divTrunc(13 * (m + 1), 5) + k + @divTrunc(k, 4) + @divTrunc(j, 4) - 2 * j, 7);
        return @intCast(@mod(h + 6, 7)); // Convert to Sunday = 0
    }

    /// Check if date is in a leap year
    pub fn isInLeapYear(self: DateTime) bool {
        return isLeapYear(self.year);
    }
};

/// Check if a year is a leap year
pub fn isLeapYear(year: i32) bool {
    return (@mod(year, 4) == 0 and @mod(year, 100) != 0) or @mod(year, 400) == 0;
}

/// Get the number of days in a year
pub fn daysInYear(year: i32) i64 {
    return if (isLeapYear(year)) 366 else 365;
}

/// Get the number of days in a month
pub fn daysInMonth(year: i32, month: u8) u8 {
    const days = [_]u8{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    if (month < 1 or month > 12) return 0;
    if (month == 2 and isLeapYear(year)) return 29;
    return days[month - 1];
}

/// Parse ISO 8601 date string (YYYY-MM-DD)
pub fn parseDate(input: []const u8) DateTimeError!DateTime {
    if (input.len < 10) return error.InvalidFormat;

    const year = std.fmt.parseInt(i32, input[0..4], 10) catch return error.InvalidFormat;
    if (input[4] != '-') return error.InvalidFormat;
    const month = std.fmt.parseInt(u8, input[5..7], 10) catch return error.InvalidFormat;
    if (input[7] != '-') return error.InvalidFormat;
    const day = std.fmt.parseInt(u8, input[8..10], 10) catch return error.InvalidFormat;

    const dt = DateTime{
        .year = year,
        .month = month,
        .day = day,
        .hour = 0,
        .minute = 0,
        .second = 0,
    };

    if (!dt.isValid()) return error.InvalidDate;
    return dt;
}

/// Get current Unix timestamp in seconds.
/// This is a cross-platform helper for getting the current time.
pub fn getTimestamp() i64 {
    const ts = std.posix.clock_gettime(.REALTIME) catch return 0;
    return ts.sec;
}

/// Get current UTC time
pub fn now() DateTime {
    const timestamp = getTimestamp();
    return DateTime.fromUnixTimestamp(timestamp);
}

/// Create a date from components
pub fn date(year: i32, month: u8, day: u8) DateTimeError!DateTime {
    const dt = DateTime{
        .year = year,
        .month = month,
        .day = day,
        .hour = 0,
        .minute = 0,
        .second = 0,
    };
    if (!dt.isValid()) return error.InvalidDate;
    return dt;
}

/// Create a datetime from components
pub fn datetime(year: i32, month: u8, day: u8, hour: u8, minute: u8, second: u8) DateTimeError!DateTime {
    const dt = DateTime{
        .year = year,
        .month = month,
        .day = day,
        .hour = hour,
        .minute = minute,
        .second = second,
    };
    if (!dt.isValid()) return error.InvalidDate;
    return dt;
}

test "isLeapYear" {
    try std.testing.expect(isLeapYear(2000));
    try std.testing.expect(isLeapYear(2024));
    try std.testing.expect(!isLeapYear(1900));
    try std.testing.expect(!isLeapYear(2023));
}

test "daysInMonth" {
    try std.testing.expectEqual(@as(u8, 31), daysInMonth(2024, 1));
    try std.testing.expectEqual(@as(u8, 29), daysInMonth(2024, 2));
    try std.testing.expectEqual(@as(u8, 28), daysInMonth(2023, 2));
}

test "parseDate" {
    const dt = try parseDate("2024-12-15");
    try std.testing.expectEqual(@as(i32, 2024), dt.year);
    try std.testing.expectEqual(@as(u8, 12), dt.month);
    try std.testing.expectEqual(@as(u8, 15), dt.day);
}

test "toUnixTimestamp" {
    const dt = DateTime{ .year = 1970, .month = 1, .day = 1, .hour = 0, .minute = 0, .second = 0 };
    try std.testing.expectEqual(@as(i64, 0), try dt.toUnixTimestamp());
}
