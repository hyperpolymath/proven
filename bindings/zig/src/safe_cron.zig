// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe cron expression parsing and validation.
//!
//! Provides functions for parsing, validating, and working with cron expressions.
//! Supports standard 5-field cron format: minute hour day-of-month month day-of-week.
//! All operations are bounds-checked and cannot crash.

const std = @import("std");

/// Error types for cron operations.
pub const CronError = error{
    /// Wrong number of fields in cron expression
    InvalidFieldCount,
    /// Field value out of allowed range
    ValueOutOfRange,
    /// Invalid step value (must be > 0)
    InvalidStep,
    /// Invalid range (start > end)
    InvalidRange,
    /// Malformed field syntax
    MalformedField,
    /// Invalid day-of-week name
    InvalidDayName,
    /// Invalid month name
    InvalidMonthName,
    /// Buffer too small for output
    BufferTooSmall,
};

/// Cron field ranges
pub const FieldRange = struct {
    min: u8,
    max: u8,
};

/// Standard cron field definitions
pub const CronFields = struct {
    pub const minute = FieldRange{ .min = 0, .max = 59 };
    pub const hour = FieldRange{ .min = 0, .max = 23 };
    pub const day_of_month = FieldRange{ .min = 1, .max = 31 };
    pub const month = FieldRange{ .min = 1, .max = 12 };
    pub const day_of_week = FieldRange{ .min = 0, .max = 6 }; // 0 = Sunday
};

/// Parsed cron expression
pub const CronExpression = struct {
    /// Bitmask of allowed minutes (0-59)
    minutes: u64,
    /// Bitmask of allowed hours (0-23)
    hours: u32,
    /// Bitmask of allowed days of month (1-31)
    days_of_month: u32,
    /// Bitmask of allowed months (1-12)
    months: u16,
    /// Bitmask of allowed days of week (0-6, Sunday = 0)
    days_of_week: u8,

    /// Check if a specific time matches this cron expression.
    pub fn matches(self: CronExpression, minute: u8, hour: u8, day: u8, month: u8, day_of_week: u8) bool {
        if (minute > 59 or hour > 23 or day < 1 or day > 31 or month < 1 or month > 12 or day_of_week > 6) {
            return false;
        }

        const minute_match = (self.minutes & (@as(u64, 1) << @intCast(minute))) != 0;
        const hour_match = (self.hours & (@as(u32, 1) << @intCast(hour))) != 0;
        const day_match = (self.days_of_month & (@as(u32, 1) << @intCast(day))) != 0;
        const month_match = (self.months & (@as(u16, 1) << @intCast(month))) != 0;
        const dow_match = (self.days_of_week & (@as(u8, 1) << @intCast(day_of_week))) != 0;

        return minute_match and hour_match and day_match and month_match and dow_match;
    }

    /// Check if expression runs every minute
    pub fn isEveryMinute(self: CronExpression) bool {
        return self.minutes == 0x0FFFFFFFFFFFFFFF and // all 60 minutes
            self.hours == 0x00FFFFFF and // all 24 hours
            self.days_of_month == 0xFFFFFFFE and // all 31 days (bit 0 unused)
            self.months == 0x1FFE and // all 12 months (bit 0 unused)
            self.days_of_week == 0x7F; // all 7 days
    }

    /// Get count of matching minutes
    pub fn countMinutes(self: CronExpression) u8 {
        return @popCount(self.minutes);
    }

    /// Get count of matching hours
    pub fn countHours(self: CronExpression) u8 {
        return @popCount(self.hours);
    }
};

/// Day-of-week names for parsing
const day_names = [_][]const u8{ "sun", "mon", "tue", "wed", "thu", "fri", "sat" };

/// Month names for parsing
const month_names = [_][]const u8{ "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec" };

/// Parse a cron expression string.
///
/// Supports standard 5-field format: minute hour day-of-month month day-of-week
/// Each field can contain:
/// - `*` - all values
/// - `N` - specific value
/// - `N-M` - range
/// - `*/N` - step from start
/// - `N-M/S` - step within range
/// - `N,M,O` - list of values
pub fn parse(expression: []const u8) CronError!CronExpression {
    var fields: [5][]const u8 = undefined;
    var field_count: usize = 0;

    var iter = std.mem.splitScalar(u8, expression, ' ');
    while (iter.next()) |field| {
        if (field.len == 0) continue;
        if (field_count >= 5) return error.InvalidFieldCount;
        fields[field_count] = field;
        field_count += 1;
    }

    if (field_count != 5) return error.InvalidFieldCount;

    return CronExpression{
        .minutes = try parseField(u64, fields[0], CronFields.minute, null),
        .hours = try parseField(u32, fields[1], CronFields.hour, null),
        .days_of_month = try parseField(u32, fields[2], CronFields.day_of_month, null),
        .months = try parseField(u16, fields[3], CronFields.month, &month_names),
        .days_of_week = try parseField(u8, fields[4], CronFields.day_of_week, &day_names),
    };
}

/// Parse a single cron field into a bitmask.
fn parseField(comptime T: type, field: []const u8, range: FieldRange, names: ?[]const []const u8) CronError!T {
    var result: T = 0;

    // Handle comma-separated list
    var list_iter = std.mem.splitScalar(u8, field, ',');
    while (list_iter.next()) |item| {
        const mask = try parseFieldItem(T, item, range, names);
        result |= mask;
    }

    return result;
}

/// Parse a single item in a cron field (not comma-separated).
fn parseFieldItem(comptime T: type, item: []const u8, range: FieldRange, names: ?[]const []const u8) CronError!T {
    if (item.len == 0) return error.MalformedField;

    // Check for step
    var step: u8 = 1;
    var main_part = item;

    if (std.mem.indexOfScalar(u8, item, '/')) |slash_pos| {
        main_part = item[0..slash_pos];
        const step_part = item[slash_pos + 1 ..];
        step = std.fmt.parseInt(u8, step_part, 10) catch return error.InvalidStep;
        if (step == 0) return error.InvalidStep;
    }

    var start: u8 = range.min;
    var end: u8 = range.max;

    if (std.mem.eql(u8, main_part, "*")) {
        // Wildcard - use full range
    } else if (std.mem.indexOfScalar(u8, main_part, '-')) |dash_pos| {
        // Range
        const start_part = main_part[0..dash_pos];
        const end_part = main_part[dash_pos + 1 ..];
        start = try parseValue(start_part, range, names);
        end = try parseValue(end_part, range, names);
        if (start > end) return error.InvalidRange;
    } else {
        // Single value
        start = try parseValue(main_part, range, names);
        end = start;
    }

    // Build bitmask
    var result: T = 0;
    var value = start;
    while (value <= end) : (value += step) {
        result |= @as(T, 1) << @intCast(value);
        if (step > end - value) break; // Prevent overflow
    }

    return result;
}

/// Parse a value (number or name) within range.
fn parseValue(value: []const u8, range: FieldRange, names: ?[]const []const u8) CronError!u8 {
    // Try to parse as number
    if (std.fmt.parseInt(u8, value, 10)) |num| {
        if (num < range.min or num > range.max) return error.ValueOutOfRange;
        return num;
    } else |_| {}

    // Try to parse as name
    if (names) |name_list| {
        var lowercase_buf: [3]u8 = undefined;
        if (value.len >= 3) {
            for (value[0..3], 0..) |char, index| {
                lowercase_buf[index] = std.ascii.toLower(char);
            }
            for (name_list, 0..) |name, index| {
                if (std.mem.eql(u8, &lowercase_buf, name)) {
                    const result_value = @as(u8, @intCast(index));
                    // Adjust for 1-indexed months
                    if (range.min == 1) {
                        return result_value + 1;
                    }
                    return result_value;
                }
            }
        }
        return if (range.min == 1) error.InvalidMonthName else error.InvalidDayName;
    }

    return error.MalformedField;
}

/// Validate a cron expression without fully parsing it.
pub fn isValid(expression: []const u8) bool {
    _ = parse(expression) catch return false;
    return true;
}

/// Common cron expression presets.
pub const Presets = struct {
    /// Every minute: `* * * * *`
    pub const every_minute = "* * * * *";
    /// Every hour at minute 0: `0 * * * *`
    pub const hourly = "0 * * * *";
    /// Every day at midnight: `0 0 * * *`
    pub const daily = "0 0 * * *";
    /// Every Sunday at midnight: `0 0 * * 0`
    pub const weekly = "0 0 * * 0";
    /// First of every month at midnight: `0 0 1 * *`
    pub const monthly = "0 0 1 * *";
    /// First of January at midnight: `0 0 1 1 *`
    pub const yearly = "0 0 1 1 *";
};

/// Parse a preset or custom expression.
pub fn parseOrPreset(expression: []const u8) CronError!CronExpression {
    // Handle @-style presets
    if (expression.len > 0 and expression[0] == '@') {
        const preset = expression[1..];
        if (std.mem.eql(u8, preset, "yearly") or std.mem.eql(u8, preset, "annually")) {
            return parse(Presets.yearly);
        } else if (std.mem.eql(u8, preset, "monthly")) {
            return parse(Presets.monthly);
        } else if (std.mem.eql(u8, preset, "weekly")) {
            return parse(Presets.weekly);
        } else if (std.mem.eql(u8, preset, "daily") or std.mem.eql(u8, preset, "midnight")) {
            return parse(Presets.daily);
        } else if (std.mem.eql(u8, preset, "hourly")) {
            return parse(Presets.hourly);
        }
        return error.MalformedField;
    }

    return parse(expression);
}

/// Get human-readable description of a cron expression.
pub fn describe(expression: CronExpression, buffer: []u8) CronError![]u8 {
    // Simple descriptions for common patterns
    if (expression.isEveryMinute()) {
        const description = "every minute";
        if (buffer.len < description.len) return error.BufferTooSmall;
        @memcpy(buffer[0..description.len], description);
        return buffer[0..description.len];
    }

    // Count set bits for simple descriptions
    const minute_count = expression.countMinutes();
    const hour_count = expression.countHours();

    if (minute_count == 1 and hour_count == 24) {
        const description = "once per hour";
        if (buffer.len < description.len) return error.BufferTooSmall;
        @memcpy(buffer[0..description.len], description);
        return buffer[0..description.len];
    }

    const description = "custom schedule";
    if (buffer.len < description.len) return error.BufferTooSmall;
    @memcpy(buffer[0..description.len], description);
    return buffer[0..description.len];
}

// ============================================================================
// Tests
// ============================================================================

test "parse every minute" {
    const cron_expression = try parse("* * * * *");
    try std.testing.expect(cron_expression.isEveryMinute());
}

test "parse specific time" {
    const cron_expression = try parse("30 9 * * *");
    try std.testing.expect(cron_expression.matches(30, 9, 15, 6, 1));
    try std.testing.expect(!cron_expression.matches(0, 9, 15, 6, 1));
    try std.testing.expect(!cron_expression.matches(30, 10, 15, 6, 1));
}

test "parse range" {
    const cron_expression = try parse("0-30 * * * *");
    try std.testing.expect(cron_expression.matches(0, 0, 1, 1, 0));
    try std.testing.expect(cron_expression.matches(15, 0, 1, 1, 0));
    try std.testing.expect(cron_expression.matches(30, 0, 1, 1, 0));
    try std.testing.expect(!cron_expression.matches(31, 0, 1, 1, 0));
}

test "parse step" {
    const cron_expression = try parse("*/15 * * * *");
    try std.testing.expect(cron_expression.matches(0, 0, 1, 1, 0));
    try std.testing.expect(cron_expression.matches(15, 0, 1, 1, 0));
    try std.testing.expect(cron_expression.matches(30, 0, 1, 1, 0));
    try std.testing.expect(cron_expression.matches(45, 0, 1, 1, 0));
    try std.testing.expect(!cron_expression.matches(10, 0, 1, 1, 0));
}

test "parse list" {
    const cron_expression = try parse("0,15,30,45 * * * *");
    try std.testing.expect(cron_expression.matches(0, 0, 1, 1, 0));
    try std.testing.expect(cron_expression.matches(15, 0, 1, 1, 0));
    try std.testing.expect(!cron_expression.matches(10, 0, 1, 1, 0));
}

test "parse day names" {
    const cron_expression = try parse("0 0 * * mon");
    try std.testing.expect(cron_expression.matches(0, 0, 1, 1, 1)); // Monday = 1
    try std.testing.expect(!cron_expression.matches(0, 0, 1, 1, 0)); // Sunday = 0
}

test "parse month names" {
    const cron_expression = try parse("0 0 1 jan *");
    try std.testing.expect(cron_expression.matches(0, 0, 1, 1, 0)); // January = 1
    try std.testing.expect(!cron_expression.matches(0, 0, 1, 2, 0)); // February = 2
}

test "invalid expressions" {
    try std.testing.expectError(error.InvalidFieldCount, parse("* * *"));
    try std.testing.expectError(error.InvalidFieldCount, parse("* * * * * *"));
    try std.testing.expectError(error.ValueOutOfRange, parse("60 * * * *"));
    try std.testing.expectError(error.ValueOutOfRange, parse("* 24 * * *"));
    try std.testing.expectError(error.InvalidRange, parse("30-10 * * * *"));
    try std.testing.expectError(error.InvalidStep, parse("*/0 * * * *"));
}

test "isValid" {
    try std.testing.expect(isValid("* * * * *"));
    try std.testing.expect(isValid("0 0 1 1 *"));
    try std.testing.expect(!isValid("invalid"));
    try std.testing.expect(!isValid(""));
}

test "parseOrPreset" {
    const yearly = try parseOrPreset("@yearly");
    try std.testing.expect(yearly.matches(0, 0, 1, 1, 0));

    const daily = try parseOrPreset("@daily");
    try std.testing.expect(daily.matches(0, 0, 15, 6, 3));
}
