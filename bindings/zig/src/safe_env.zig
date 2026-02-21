// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe environment variable handling with validation and sanitization.
//!
//! Provides safe access to environment variables with:
//! - Validation of variable names and values
//! - Protection against injection attacks
//! - Type-safe parsing of common value types
//! - Default value handling without crashes

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for environment variable operations.
pub const EnvError = error{
    InvalidName,
    InvalidValue,
    NotFound,
    ParseError,
    OutOfMemory,
    ValueTooLong,
    EmptyValue,
};

/// Maximum allowed length for environment variable names.
pub const MAX_NAME_LENGTH: usize = 255;

/// Maximum allowed length for environment variable values.
pub const MAX_VALUE_LENGTH: usize = 32767;

/// Characters that are forbidden in environment variable names.
const FORBIDDEN_NAME_CHARS = "=\x00";

/// Check if an environment variable name is valid.
/// Names must be non-empty, not contain '=' or null bytes, and be within length limits.
pub fn isValidName(name: []const u8) bool {
    if (name.len == 0 or name.len > MAX_NAME_LENGTH) return false;

    for (name) |char| {
        for (FORBIDDEN_NAME_CHARS) |forbidden| {
            if (char == forbidden) return false;
        }
    }

    return true;
}

/// Check if an environment variable value is valid.
/// Values must not contain null bytes and be within length limits.
pub fn isValidValue(value: []const u8) bool {
    if (value.len > MAX_VALUE_LENGTH) return false;

    for (value) |char| {
        if (char == 0) return false;
    }

    return true;
}

/// Safely get an environment variable, returning null if not found or invalid.
pub fn get(name: []const u8) ?[]const u8 {
    if (!isValidName(name)) return null;

    // Use std.posix.getenv for safe access
    return std.posix.getenv(name);
}

/// Get an environment variable with a default value if not found.
pub fn getOrDefault(name: []const u8, default: []const u8) []const u8 {
    return get(name) orelse default;
}

/// Get an environment variable, returning error if not found.
pub fn getRequired(name: []const u8) EnvError![]const u8 {
    if (!isValidName(name)) return error.InvalidName;
    return get(name) orelse error.NotFound;
}

/// Parse an environment variable as an integer.
pub fn getInt(comptime T: type, name: []const u8) EnvError!T {
    const value = get(name) orelse return error.NotFound;
    return std.fmt.parseInt(T, value, 10) catch error.ParseError;
}

/// Parse an environment variable as an integer with a default value.
pub fn getIntOrDefault(comptime T: type, name: []const u8, default: T) T {
    return getInt(T, name) catch default;
}

/// Parse an environment variable as a boolean.
/// Accepts: "true", "false", "1", "0", "yes", "no" (case-insensitive).
pub fn getBool(name: []const u8) EnvError!bool {
    const value = get(name) orelse return error.NotFound;

    const lower = blk: {
        var buf: [16]u8 = undefined;
        if (value.len > buf.len) return error.ParseError;
        for (value, 0..) |char, i| {
            buf[i] = std.ascii.toLower(char);
        }
        break :blk buf[0..value.len];
    };

    if (std.mem.eql(u8, lower, "true") or
        std.mem.eql(u8, lower, "1") or
        std.mem.eql(u8, lower, "yes"))
    {
        return true;
    }

    if (std.mem.eql(u8, lower, "false") or
        std.mem.eql(u8, lower, "0") or
        std.mem.eql(u8, lower, "no"))
    {
        return false;
    }

    return error.ParseError;
}

/// Parse an environment variable as a boolean with a default value.
pub fn getBoolOrDefault(name: []const u8, default: bool) bool {
    return getBool(name) catch default;
}

/// Parse an environment variable as a float.
pub fn getFloat(comptime T: type, name: []const u8) EnvError!T {
    const value = get(name) orelse return error.NotFound;
    return std.fmt.parseFloat(T, value) catch error.ParseError;
}

/// Parse an environment variable as a float with a default value.
pub fn getFloatOrDefault(comptime T: type, name: []const u8, default: T) T {
    return getFloat(T, name) catch default;
}

/// Split an environment variable value by a delimiter.
/// Caller owns the returned slice and must free it.
pub fn getSplit(allocator: Allocator, name: []const u8, delimiter: u8) EnvError![][]const u8 {
    const value = get(name) orelse return error.NotFound;

    var list = std.array_list.Managed([]const u8).init(allocator);
    errdefer list.deinit();

    var iter = std.mem.splitScalar(u8, value, delimiter);
    while (iter.next()) |part| {
        list.append(part) catch return error.OutOfMemory;
    }

    return list.toOwnedSlice() catch error.OutOfMemory;
}

/// Sanitize an environment variable value by removing control characters.
/// Returns a newly allocated string that must be freed by the caller.
pub fn sanitizeValue(allocator: Allocator, value: []const u8) EnvError![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (value) |char| {
        // Skip control characters except tab, newline, carriage return
        if (char < 32 and char != '\t' and char != '\n' and char != '\r') {
            continue;
        }
        // Skip DEL character
        if (char == 127) {
            continue;
        }
        result.append(char) catch return error.OutOfMemory;
    }

    return result.toOwnedSlice() catch error.OutOfMemory;
}

/// Check if an environment variable exists.
pub fn exists(name: []const u8) bool {
    return get(name) != null;
}

/// Check if an environment variable exists and has a non-empty value.
pub fn existsNonEmpty(name: []const u8) bool {
    const value = get(name) orelse return false;
    return value.len > 0;
}

/// Get an environment variable, returning error if empty.
pub fn getNonEmpty(name: []const u8) EnvError![]const u8 {
    const value = getRequired(name) catch |err| return err;
    if (value.len == 0) return error.EmptyValue;
    return value;
}

test "isValidName" {
    try std.testing.expect(isValidName("PATH"));
    try std.testing.expect(isValidName("MY_VAR"));
    try std.testing.expect(isValidName("var123"));
    try std.testing.expect(!isValidName(""));
    try std.testing.expect(!isValidName("VAR=VALUE"));
    try std.testing.expect(!isValidName("VAR\x00NAME"));
}

test "isValidValue" {
    try std.testing.expect(isValidValue("some value"));
    try std.testing.expect(isValidValue("/usr/bin:/usr/local/bin"));
    try std.testing.expect(isValidValue(""));
    try std.testing.expect(!isValidValue("value\x00with\x00nulls"));
}

test "getOrDefault" {
    // This test uses a non-existent variable
    const result = getOrDefault("PROVEN_TEST_NONEXISTENT_VAR_12345", "default_value");
    try std.testing.expectEqualStrings("default_value", result);
}

test "sanitizeValue" {
    const allocator = std.testing.allocator;

    const result = try sanitizeValue(allocator, "hello\x01world\x7Ftest");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("helloworldtest", result);

    const result2 = try sanitizeValue(allocator, "line1\nline2\ttab");
    defer allocator.free(result2);
    try std.testing.expectEqualStrings("line1\nline2\ttab", result2);
}

test "exists" {
    // PATH should exist on most systems
    // Use a definitely non-existent variable for the false case
    try std.testing.expect(!exists("PROVEN_TEST_NONEXISTENT_VAR_67890"));
}
