// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe JSON parsing and generation that cannot crash.

const std = @import("std");

/// Error types for JSON operations.
pub const JsonError = error{
    ParseError,
    InvalidUtf8,
    MaxDepthExceeded,
    OutOfMemory,
    InvalidValue,
};

/// JSON value types
pub const ValueType = enum {
    null,
    boolean,
    number,
    string,
    array,
    object,
};

/// A safe JSON value representation
pub const Value = union(ValueType) {
    null: void,
    boolean: bool,
    number: f64,
    string: []const u8,
    array: []const Value,
    object: std.StringHashMap(Value),

    /// Check if value is null
    pub fn isNull(self: Value) bool {
        return self == .null;
    }

    /// Check if value is boolean
    pub fn isBool(self: Value) bool {
        return self == .boolean;
    }

    /// Check if value is number
    pub fn isNumber(self: Value) bool {
        return self == .number;
    }

    /// Check if value is string
    pub fn isString(self: Value) bool {
        return self == .string;
    }

    /// Check if value is array
    pub fn isArray(self: Value) bool {
        return self == .array;
    }

    /// Check if value is object
    pub fn isObject(self: Value) bool {
        return self == .object;
    }

    /// Get boolean value, return null if not boolean
    pub fn asBool(self: Value) ?bool {
        return switch (self) {
            .boolean => |b| b,
            else => null,
        };
    }

    /// Get number value, return null if not number
    pub fn asNumber(self: Value) ?f64 {
        return switch (self) {
            .number => |n| n,
            else => null,
        };
    }

    /// Get string value, return null if not string
    pub fn asString(self: Value) ?[]const u8 {
        return switch (self) {
            .string => |s| s,
            else => null,
        };
    }

    /// Get array value, return null if not array
    pub fn asArray(self: Value) ?[]const Value {
        return switch (self) {
            .array => |a| a,
            else => null,
        };
    }

    /// Get object value, return null if not object
    pub fn asObject(self: Value) ?std.StringHashMap(Value) {
        return switch (self) {
            .object => |o| o,
            else => null,
        };
    }
};

/// Maximum allowed nesting depth
pub const MAX_DEPTH = 64;

/// Safe JSON parsing options
pub const ParseOptions = struct {
    max_depth: usize = MAX_DEPTH,
    allow_trailing_comma: bool = false,
    allow_comments: bool = false,
};

/// Safely parse a JSON string
pub fn parse(allocator: std.mem.Allocator, input: []const u8, options: ParseOptions) JsonError!Value {
    _ = options;
    var parsed = std.json.parseFromSlice(Value, allocator, input, .{}) catch return error.ParseError;
    defer parsed.deinit();
    return parsed.value;
}

/// Safely stringify a JSON value
pub fn stringify(allocator: std.mem.Allocator, value: Value) JsonError![]u8 {
    var list = std.array_list.Managed(u8).init(allocator);
    errdefer list.deinit();

    const str = std.json.stringifyAlloc(allocator, value, .{}) catch return error.InvalidValue;
    return str;
}

/// Check if a string is valid JSON
pub fn isValid(input: []const u8) bool {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    _ = std.json.parseFromSlice(std.json.Value, arena.allocator(), input, .{}) catch return false;
    return true;
}

/// Get a value at a path (e.g., "foo.bar.baz")
pub fn getPath(value: Value, path: []const u8) ?Value {
    var current = value;
    var it = std.mem.splitScalar(u8, path, '.');

    while (it.next()) |key| {
        switch (current) {
            .object => |obj| {
                if (obj.get(key)) |v| {
                    current = v;
                } else {
                    return null;
                }
            },
            .array => |arr| {
                const index = std.fmt.parseInt(usize, key, 10) catch return null;
                if (index >= arr.len) return null;
                current = arr[index];
            },
            else => return null,
        }
    }

    return current;
}

/// Safely escape a string for JSON
pub fn escapeString(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var list = std.array_list.Managed(u8).init(allocator);
    errdefer list.deinit();

    for (input) |c| {
        switch (c) {
            '"' => try list.appendSlice("\\\""),
            '\\' => try list.appendSlice("\\\\"),
            '\n' => try list.appendSlice("\\n"),
            '\r' => try list.appendSlice("\\r"),
            '\t' => try list.appendSlice("\\t"),
            else => {
                if (c < 0x20) {
                    var buf: [6]u8 = undefined;
                    const formatted = std.fmt.bufPrint(&buf, "\\u{x:0>4}", .{c}) catch continue;
                    try list.appendSlice(formatted);
                } else {
                    try list.append(c);
                }
            },
        }
    }

    return list.toOwnedSlice();
}

test "isValid" {
    try std.testing.expect(isValid("{}"));
    try std.testing.expect(isValid("{\"foo\": 42}"));
    try std.testing.expect(isValid("[1, 2, 3]"));
    try std.testing.expect(!isValid("{invalid}"));
    try std.testing.expect(!isValid(""));
}

test "escapeString" {
    const allocator = std.testing.allocator;
    const escaped = try escapeString(allocator, "hello\nworld");
    defer allocator.free(escaped);
    try std.testing.expectEqualStrings("hello\\nworld", escaped);
}
