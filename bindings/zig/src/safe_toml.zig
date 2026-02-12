// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe TOML parsing and validation that cannot crash.
//!
//! Provides bounded TOML parsing with depth limits, key validation,
//! and type-safe value extraction. All operations return errors
//! rather than crashing on malformed input.

const std = @import("std");

/// Error types for TOML operations.
pub const TomlError = error{
    ParseError,
    InvalidUtf8,
    MaxDepthExceeded,
    MaxKeysExceeded,
    InvalidKey,
    InvalidValue,
    DuplicateKey,
    OutOfMemory,
    TypeMismatch,
    KeyNotFound,
};

/// TOML value types
pub const ValueType = enum {
    string,
    integer,
    float,
    boolean,
    datetime,
    array,
    table,
};

/// Maximum allowed nesting depth for tables
pub const MAX_DEPTH = 32;

/// Maximum keys per table
pub const MAX_KEYS_PER_TABLE = 256;

/// A safe TOML value representation
/// Note: recursive types use slices/pointers to avoid compile-time dependency issues
pub const Value = union(ValueType) {
    string: []const u8,
    integer: i64,
    float: f64,
    boolean: bool,
    datetime: []const u8,
    array: []const u8, // Serialized array (simplified to avoid recursion)
    table: []const u8, // Serialized table (simplified to avoid recursion)

    /// Check if value is a string
    pub fn isString(self: Value) bool {
        return self == .string;
    }

    /// Check if value is an integer
    pub fn isInteger(self: Value) bool {
        return self == .integer;
    }

    /// Check if value is a float
    pub fn isFloat(self: Value) bool {
        return self == .float;
    }

    /// Check if value is a boolean
    pub fn isBoolean(self: Value) bool {
        return self == .boolean;
    }

    /// Check if value is a datetime
    pub fn isDatetime(self: Value) bool {
        return self == .datetime;
    }

    /// Check if value is an array
    pub fn isArray(self: Value) bool {
        return self == .array;
    }

    /// Check if value is a table
    pub fn isTable(self: Value) bool {
        return self == .table;
    }

    /// Get string value, return null if not string
    pub fn asString(self: Value) ?[]const u8 {
        return switch (self) {
            .string => |s| s,
            else => null,
        };
    }

    /// Get integer value, return null if not integer
    pub fn asInteger(self: Value) ?i64 {
        return switch (self) {
            .integer => |i| i,
            else => null,
        };
    }

    /// Get float value, return null if not float
    pub fn asFloat(self: Value) ?f64 {
        return switch (self) {
            .float => |f| f,
            else => null,
        };
    }

    /// Get boolean value, return null if not boolean
    pub fn asBool(self: Value) ?bool {
        return switch (self) {
            .boolean => |b| b,
            else => null,
        };
    }

    /// Get array value as serialized string, return null if not array
    pub fn asArray(self: Value) ?[]const u8 {
        return switch (self) {
            .array => |a| a,
            else => null,
        };
    }

    /// Get table value as serialized string, return null if not table
    pub fn asTable(self: Value) ?[]const u8 {
        return switch (self) {
            .table => |t| t,
            else => null,
        };
    }
};

/// A TOML table (key-value mapping) with bounded capacity.
pub const Table = struct {
    keys: [MAX_KEYS_PER_TABLE]?[]const u8 = [_]?[]const u8{null} ** MAX_KEYS_PER_TABLE,
    values: [MAX_KEYS_PER_TABLE]?Value = [_]?Value{null} ** MAX_KEYS_PER_TABLE,
    count: usize = 0,

    /// Initialize an empty table.
    pub fn init() Table {
        return .{};
    }

    /// Get the number of entries.
    pub fn len(self: *const Table) usize {
        return self.count;
    }

    /// Check if table is empty.
    pub fn isEmpty(self: *const Table) bool {
        return self.count == 0;
    }

    /// Get a value by key.
    pub fn get(self: *const Table, key: []const u8) ?Value {
        for (self.keys[0..self.count], self.values[0..self.count]) |k_opt, v_opt| {
            if (k_opt) |k| {
                if (std.mem.eql(u8, k, key)) {
                    return v_opt;
                }
            }
        }
        return null;
    }

    /// Set a value by key.
    pub fn set(self: *Table, key: []const u8, value: Value) TomlError!void {
        // Check for existing key
        for (self.keys[0..self.count], 0..) |k_opt, i| {
            if (k_opt) |k| {
                if (std.mem.eql(u8, k, key)) {
                    self.values[i] = value;
                    return;
                }
            }
        }

        // Add new key
        if (self.count >= MAX_KEYS_PER_TABLE) return error.MaxKeysExceeded;
        self.keys[self.count] = key;
        self.values[self.count] = value;
        self.count += 1;
    }

    /// Check if key exists.
    pub fn contains(self: *const Table, key: []const u8) bool {
        return self.get(key) != null;
    }
};

/// Safe TOML parsing options
pub const ParseOptions = struct {
    max_depth: usize = MAX_DEPTH,
    max_keys: usize = MAX_KEYS_PER_TABLE,
    allow_duplicate_keys: bool = false,
};

/// Validate a TOML key.
pub fn isValidKey(key: []const u8) bool {
    if (key.len == 0) return false;

    // Bare keys can contain A-Za-z0-9_-
    for (key) |c| {
        const is_valid = (c >= 'A' and c <= 'Z') or
            (c >= 'a' and c <= 'z') or
            (c >= '0' and c <= '9') or
            c == '_' or c == '-';
        if (!is_valid) return false;
    }
    return true;
}

/// Validate a TOML string is valid UTF-8.
pub fn isValidString(input: []const u8) bool {
    return std.unicode.utf8ValidateSlice(input);
}

/// Parse a simple TOML key-value line (e.g., "key = value").
pub fn parseKeyValue(line: []const u8) TomlError!struct { key: []const u8, value: Value } {
    // Find the equals sign
    const eq_pos = std.mem.indexOf(u8, line, "=") orelse return error.ParseError;

    // Extract and trim key
    var key = line[0..eq_pos];
    key = std.mem.trim(u8, key, " \t");

    if (key.len == 0) return error.InvalidKey;

    // Extract and trim value
    var value_str = line[eq_pos + 1 ..];
    value_str = std.mem.trim(u8, value_str, " \t");

    if (value_str.len == 0) return error.InvalidValue;

    // Parse the value
    const value = try parseValue(value_str);

    return .{ .key = key, .value = value };
}

/// Parse a TOML value string into a Value.
pub fn parseValue(input: []const u8) TomlError!Value {
    if (input.len == 0) return error.InvalidValue;

    // Boolean
    if (std.mem.eql(u8, input, "true")) {
        return Value{ .boolean = true };
    }
    if (std.mem.eql(u8, input, "false")) {
        return Value{ .boolean = false };
    }

    // String (quoted)
    if (input[0] == '"' and input.len >= 2 and input[input.len - 1] == '"') {
        return Value{ .string = input[1 .. input.len - 1] };
    }

    // Single-quoted string
    if (input[0] == '\'' and input.len >= 2 and input[input.len - 1] == '\'') {
        return Value{ .string = input[1 .. input.len - 1] };
    }

    // Try integer
    if (std.fmt.parseInt(i64, input, 10)) |int_val| {
        return Value{ .integer = int_val };
    } else |_| {}

    // Try float
    if (std.fmt.parseFloat(f64, input)) |float_val| {
        return Value{ .float = float_val };
    } else |_| {}

    return error.InvalidValue;
}

/// Check if a string looks like valid TOML syntax.
pub fn isValidToml(input: []const u8) bool {
    if (!isValidString(input)) return false;

    var depth: usize = 0;
    var lines = std.mem.splitScalar(u8, input, '\n');

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Skip empty lines and comments
        if (trimmed.len == 0 or trimmed[0] == '#') continue;

        // Table header
        if (trimmed[0] == '[') {
            // Check for array of tables
            if (trimmed.len > 1 and trimmed[1] == '[') {
                depth += 1;
                if (depth > MAX_DEPTH) return false;
            } else {
                depth = 1;
            }
            continue;
        }

        // Key-value pair
        if (std.mem.indexOf(u8, trimmed, "=") == null) {
            return false;
        }
    }

    return true;
}

/// Safely escape a string for TOML output.
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

/// Format a Value as a TOML string.
pub fn formatValue(allocator: std.mem.Allocator, value: Value) ![]u8 {
    var list = std.array_list.Managed(u8).init(allocator);
    errdefer list.deinit();

    switch (value) {
        .string => |s| {
            try list.append('"');
            const escaped = try escapeString(allocator, s);
            defer allocator.free(escaped);
            try list.appendSlice(escaped);
            try list.append('"');
        },
        .integer => |i| {
            var buf: [32]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{d}", .{i}) catch return error.OutOfMemory;
            try list.appendSlice(formatted);
        },
        .float => |f| {
            var buf: [64]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{d}", .{f}) catch return error.OutOfMemory;
            try list.appendSlice(formatted);
        },
        .boolean => |b| {
            try list.appendSlice(if (b) "true" else "false");
        },
        .datetime => |d| {
            try list.appendSlice(d);
        },
        .array => |arr| {
            // Array is stored as serialized string
            try list.appendSlice(arr);
        },
        .table => |t| {
            // Table is stored as serialized string
            try list.appendSlice(t);
        },
    }

    return list.toOwnedSlice();
}

/// Get a value at a dotted path (e.g., "server.host").
/// Note: Nested table traversal is simplified since tables are now serialized strings.
pub fn getPath(table: Table, path: []const u8) ?Value {
    // Simple single-level lookup
    var it = std.mem.splitScalar(u8, path, '.');
    if (it.next()) |key| {
        if (it.peek() == null) {
            // Single key lookup
            return table.get(key);
        }
        // Nested lookups not supported in simplified implementation
        return null;
    }

    return null;
}

test "isValidKey" {
    try std.testing.expect(isValidKey("name"));
    try std.testing.expect(isValidKey("server-name"));
    try std.testing.expect(isValidKey("key_123"));
    try std.testing.expect(!isValidKey(""));
    try std.testing.expect(!isValidKey("key with space"));
    try std.testing.expect(!isValidKey("key.with.dots"));
}

test "parseValue" {
    const str_val = try parseValue("\"hello\"");
    try std.testing.expectEqualStrings("hello", str_val.asString().?);

    const int_val = try parseValue("42");
    try std.testing.expectEqual(@as(i64, 42), int_val.asInteger().?);

    const float_val = try parseValue("3.14");
    try std.testing.expectApproxEqAbs(@as(f64, 3.14), float_val.asFloat().?, 0.001);

    const bool_val = try parseValue("true");
    try std.testing.expect(bool_val.asBool().?);
}

test "parseKeyValue" {
    const result = try parseKeyValue("name = \"test\"");
    try std.testing.expectEqualStrings("name", result.key);
    try std.testing.expectEqualStrings("test", result.value.asString().?);

    const int_result = try parseKeyValue("port = 8080");
    try std.testing.expectEqualStrings("port", int_result.key);
    try std.testing.expectEqual(@as(i64, 8080), int_result.value.asInteger().?);
}

test "Table operations" {
    var table = Table.init();
    try table.set("name", Value{ .string = "test" });
    try table.set("port", Value{ .integer = 8080 });

    try std.testing.expectEqual(@as(usize, 2), table.len());
    try std.testing.expect(table.contains("name"));
    try std.testing.expectEqualStrings("test", table.get("name").?.asString().?);
    try std.testing.expectEqual(@as(i64, 8080), table.get("port").?.asInteger().?);
}

test "isValidToml" {
    try std.testing.expect(isValidToml("name = \"test\""));
    try std.testing.expect(isValidToml("[server]\nhost = \"localhost\""));
    try std.testing.expect(isValidToml("# comment\nkey = 42"));
    try std.testing.expect(!isValidToml("invalid line without equals"));
}

test "escapeString" {
    const allocator = std.testing.allocator;
    const escaped = try escapeString(allocator, "hello\nworld");
    defer allocator.free(escaped);
    try std.testing.expectEqualStrings("hello\\nworld", escaped);
}
