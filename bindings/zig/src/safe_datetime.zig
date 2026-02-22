// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeDateTime - FFI bindings to libproven datetime operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for datetime operations.
pub const DateTimeError = error{
    ParseFailure,
    FormatError,
    ProvenError,
};

/// DateTime components.
pub const DateTime = struct {
    year: i32,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
    nanosecond: u32,
    tz_offset_minutes: i16,
};

/// Managed string from libproven.
pub const ProvenString = struct {
    ptr: [*]u8,
    len: usize,

    pub fn slice(self: ProvenString) []const u8 {
        return self.ptr[0..self.len];
    }

    pub fn deinit(self: ProvenString) void {
        c.proven_free_string(self.ptr);
    }
};

/// Parse ISO 8601 date string via libproven.
pub fn parse(input: []const u8) DateTimeError!DateTime {
    const result = c.proven_datetime_parse(input.ptr, input.len);
    if (result.status != c.PROVEN_OK) return error.ParseFailure;
    const dt = result.datetime;
    return DateTime{
        .year = dt.year,
        .month = dt.month,
        .day = dt.day,
        .hour = dt.hour,
        .minute = dt.minute,
        .second = dt.second,
        .nanosecond = dt.nanosecond,
        .tz_offset_minutes = dt.tz_offset_minutes,
    };
}

/// Format DateTime as ISO 8601 string via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn formatIso8601(dt: DateTime) DateTimeError!ProvenString {
    const c_dt = c.ProvenDateTime{
        .year = dt.year,
        .month = dt.month,
        .day = dt.day,
        .hour = dt.hour,
        .minute = dt.minute,
        .second = dt.second,
        .nanosecond = dt.nanosecond,
        .tz_offset_minutes = dt.tz_offset_minutes,
    };
    const result = c.proven_datetime_format_iso8601(c_dt);
    if (result.status != c.PROVEN_OK) return error.FormatError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

/// Check if year is a leap year via libproven.
pub fn isLeapYear(year: i32) bool {
    return c.proven_datetime_is_leap_year(year);
}

/// Get number of days in a month via libproven.
/// Returns 0 if invalid month.
pub fn daysInMonth(year: i32, month: u8) u8 {
    return c.proven_datetime_days_in_month(year, month);
}

test "parse" {
    const dt = try parse("2026-01-15");
    try std.testing.expectEqual(@as(i32, 2026), dt.year);
    try std.testing.expectEqual(@as(u8, 1), dt.month);
    try std.testing.expectEqual(@as(u8, 15), dt.day);
}

test "isLeapYear" {
    try std.testing.expect(isLeapYear(2024));
    try std.testing.expect(!isLeapYear(2025));
}

test "daysInMonth" {
    try std.testing.expectEqual(@as(u8, 29), daysInMonth(2024, 2));
    try std.testing.expectEqual(@as(u8, 28), daysInMonth(2025, 2));
    try std.testing.expectEqual(@as(u8, 31), daysInMonth(2026, 1));
}
