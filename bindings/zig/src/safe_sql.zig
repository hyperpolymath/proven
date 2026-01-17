// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe SQL query sanitization and parameterization that cannot crash.
//!
//! Provides utilities for constructing safe SQL queries with proper escaping
//! and parameterized query building. Always prefer parameterized queries over
//! string interpolation for security.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for SQL operations.
pub const SqlError = error{
    InvalidIdentifier,
    InvalidParameterIndex,
    TooManyParameters,
    EmptyQuery,
    UnbalancedQuotes,
    DangerousKeyword,
    OutOfMemory,
};

/// Maximum number of parameters allowed in a single query.
pub const MAX_PARAMETERS = 1000;

/// SQL value types for parameterized queries.
pub const ValueType = enum {
    null,
    integer,
    float,
    text,
    blob,
    boolean,
};

/// A safe SQL parameter value.
pub const Value = union(ValueType) {
    null: void,
    integer: i64,
    float: f64,
    text: []const u8,
    blob: []const u8,
    boolean: bool,

    /// Check if value is null.
    pub fn isNull(self: Value) bool {
        return self == .null;
    }

    /// Format value as SQL literal string.
    pub fn toSqlLiteral(self: Value, allocator: Allocator) ![]u8 {
        return switch (self) {
            .null => try allocator.dupe(u8, "NULL"),
            .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
            .float => |f| try std.fmt.allocPrint(allocator, "{d}", .{f}),
            .text => |t| try escapeStringLiteral(allocator, t),
            .blob => |b| try escapeBlobLiteral(allocator, b),
            .boolean => |bl| try allocator.dupe(u8, if (bl) "TRUE" else "FALSE"),
        };
    }
};

/// Dangerous SQL keywords that should trigger warnings.
const dangerous_keywords = [_][]const u8{
    "DROP",
    "DELETE",
    "TRUNCATE",
    "ALTER",
    "GRANT",
    "REVOKE",
    "EXEC",
    "EXECUTE",
    "SHUTDOWN",
    "CREATE USER",
    "DROP USER",
};

/// Escape a string for safe SQL string literal insertion.
/// Doubles single quotes and handles special characters.
pub fn escapeStringLiteral(allocator: Allocator, value: []const u8) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    try result.append('\'');

    for (value) |c| {
        switch (c) {
            '\'' => try result.appendSlice("''"),
            '\\' => try result.appendSlice("\\\\"),
            '\x00' => try result.appendSlice("\\0"),
            '\n' => try result.appendSlice("\\n"),
            '\r' => try result.appendSlice("\\r"),
            '\x1a' => try result.appendSlice("\\Z"),
            else => try result.append(c),
        }
    }

    try result.append('\'');

    return result.toOwnedSlice();
}

/// Escape a blob/binary value as hex literal.
pub fn escapeBlobLiteral(allocator: Allocator, value: []const u8) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    try result.appendSlice("X'");

    for (value) |byte| {
        const hex_chars = "0123456789ABCDEF";
        try result.append(hex_chars[byte >> 4]);
        try result.append(hex_chars[byte & 0x0F]);
    }

    try result.append('\'');

    return result.toOwnedSlice();
}

/// Validate and escape a SQL identifier (table name, column name).
/// Only allows alphanumeric characters and underscores.
pub fn escapeIdentifier(allocator: Allocator, identifier: []const u8) SqlError![]u8 {
    if (identifier.len == 0) return error.InvalidIdentifier;

    // First character must be letter or underscore
    if (!std.ascii.isAlphabetic(identifier[0]) and identifier[0] != '_') {
        return error.InvalidIdentifier;
    }

    // Rest must be alphanumeric or underscore
    for (identifier) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_') {
            return error.InvalidIdentifier;
        }
    }

    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    try result.append('"');
    try result.appendSlice(identifier);
    try result.append('"');

    return result.toOwnedSlice() catch return error.OutOfMemory;
}

/// Check if a string contains dangerous SQL keywords.
pub fn containsDangerousKeyword(query: []const u8) bool {
    // Convert to uppercase for comparison
    var upper_buffer: [4096]u8 = undefined;
    const check_len = @min(query.len, upper_buffer.len);

    for (0..check_len) |i| {
        upper_buffer[i] = std.ascii.toUpper(query[i]);
    }

    const upper = upper_buffer[0..check_len];

    for (dangerous_keywords) |keyword| {
        if (std.mem.indexOf(u8, upper, keyword) != null) {
            return true;
        }
    }

    return false;
}

/// Builder for parameterized SQL queries.
pub const QueryBuilder = struct {
    allocator: Allocator,
    query_parts: std.ArrayList([]const u8),
    parameters: std.ArrayList(Value),
    placeholder_style: PlaceholderStyle,

    /// Placeholder style for different database systems.
    pub const PlaceholderStyle = enum {
        question_mark, // ? (SQLite, MySQL)
        dollar_number, // $1, $2 (PostgreSQL)
        at_name, // @name (SQL Server)
        colon_number, // :1, :2 (Oracle)
    };

    /// Initialize a new query builder.
    pub fn init(allocator: Allocator, style: PlaceholderStyle) QueryBuilder {
        return QueryBuilder{
            .allocator = allocator,
            .query_parts = std.ArrayList([]const u8).init(allocator),
            .parameters = std.ArrayList(Value).init(allocator),
            .placeholder_style = style,
        };
    }

    /// Deinitialize and free resources.
    pub fn deinit(self: *QueryBuilder) void {
        self.query_parts.deinit();
        self.parameters.deinit();
    }

    /// Add a literal SQL fragment.
    pub fn addLiteral(self: *QueryBuilder, sql: []const u8) !void {
        try self.query_parts.append(sql);
    }

    /// Add a parameter placeholder and value.
    pub fn addParam(self: *QueryBuilder, value: Value) SqlError!void {
        if (self.parameters.items.len >= MAX_PARAMETERS) {
            return error.TooManyParameters;
        }
        try self.parameters.append(value);
        try self.query_parts.append("\x00"); // Marker for placeholder
    }

    /// Build the final query string with placeholders.
    pub fn build(self: *QueryBuilder) ![]u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        errdefer result.deinit();

        var param_index: usize = 1;

        for (self.query_parts.items) |part| {
            if (part.len == 1 and part[0] == '\x00') {
                // Replace marker with placeholder
                switch (self.placeholder_style) {
                    .question_mark => try result.append('?'),
                    .dollar_number => try result.writer().print("${d}", .{param_index}),
                    .at_name => try result.writer().print("@p{d}", .{param_index}),
                    .colon_number => try result.writer().print(":{d}", .{param_index}),
                }
                param_index += 1;
            } else {
                try result.appendSlice(part);
            }
        }

        return result.toOwnedSlice();
    }

    /// Get the parameters list.
    pub fn getParameters(self: *QueryBuilder) []const Value {
        return self.parameters.items;
    }
};

/// Sanitize a value for use in a LIKE pattern.
/// Escapes %, _, and \ characters.
pub fn escapeLikePattern(allocator: Allocator, pattern: []const u8, escape_char: u8) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    for (pattern) |c| {
        if (c == '%' or c == '_' or c == escape_char) {
            try result.append(escape_char);
        }
        try result.append(c);
    }

    return result.toOwnedSlice();
}

/// Check if a query string has balanced quotes.
pub fn hasBalancedQuotes(query: []const u8) bool {
    var single_quote_count: usize = 0;
    var double_quote_count: usize = 0;
    var in_single: bool = false;
    var in_double: bool = false;
    var prev_char: u8 = 0;

    for (query) |c| {
        if (c == '\'' and prev_char != '\\' and !in_double) {
            in_single = !in_single;
            single_quote_count += 1;
        } else if (c == '"' and prev_char != '\\' and !in_single) {
            in_double = !in_double;
            double_quote_count += 1;
        }
        prev_char = c;
    }

    return single_quote_count % 2 == 0 and double_quote_count % 2 == 0;
}

/// Strip SQL comments from a query (both -- and /* */ style).
pub fn stripComments(allocator: Allocator, query: []const u8) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    var i: usize = 0;
    var in_string = false;
    var string_char: u8 = 0;

    while (i < query.len) {
        const c = query[i];

        // Track string literals to avoid stripping inside them
        if (!in_string and (c == '\'' or c == '"')) {
            in_string = true;
            string_char = c;
            try result.append(c);
            i += 1;
            continue;
        }

        if (in_string) {
            try result.append(c);
            if (c == string_char and (i == 0 or query[i - 1] != '\\')) {
                in_string = false;
            }
            i += 1;
            continue;
        }

        // Check for -- comment
        if (i + 1 < query.len and c == '-' and query[i + 1] == '-') {
            // Skip until end of line
            while (i < query.len and query[i] != '\n') {
                i += 1;
            }
            continue;
        }

        // Check for /* */ comment
        if (i + 1 < query.len and c == '/' and query[i + 1] == '*') {
            i += 2;
            while (i + 1 < query.len) {
                if (query[i] == '*' and query[i + 1] == '/') {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }

        try result.append(c);
        i += 1;
    }

    return result.toOwnedSlice();
}

/// Validate that a query is safe for execution (basic checks).
pub fn validateQuery(query: []const u8) SqlError!void {
    if (query.len == 0) return error.EmptyQuery;
    if (!hasBalancedQuotes(query)) return error.UnbalancedQuotes;
}

/// Create a simple SELECT query with safe identifiers.
pub fn selectQuery(allocator: Allocator, table: []const u8, columns: []const []const u8, where_clause: ?[]const u8) SqlError![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    try result.appendSlice("SELECT ");

    if (columns.len == 0) {
        try result.append('*');
    } else {
        for (columns, 0..) |col, idx| {
            if (idx > 0) try result.appendSlice(", ");
            const escaped_col = try escapeIdentifier(allocator, col);
            defer allocator.free(escaped_col);
            try result.appendSlice(escaped_col);
        }
    }

    try result.appendSlice(" FROM ");
    const escaped_table = try escapeIdentifier(allocator, table);
    defer allocator.free(escaped_table);
    try result.appendSlice(escaped_table);

    if (where_clause) |clause| {
        try result.appendSlice(" WHERE ");
        try result.appendSlice(clause);
    }

    return result.toOwnedSlice() catch return error.OutOfMemory;
}

test "escapeStringLiteral" {
    const allocator = std.testing.allocator;
    const result = try escapeStringLiteral(allocator, "it's a test");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("'it''s a test'", result);
}

test "escapeStringLiteral with special chars" {
    const allocator = std.testing.allocator;
    const result = try escapeStringLiteral(allocator, "line\nbreak");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("'line\\nbreak'", result);
}

test "escapeBlobLiteral" {
    const allocator = std.testing.allocator;
    const result = try escapeBlobLiteral(allocator, &[_]u8{ 0xDE, 0xAD, 0xBE, 0xEF });
    defer allocator.free(result);
    try std.testing.expectEqualStrings("X'DEADBEEF'", result);
}

test "escapeIdentifier valid" {
    const allocator = std.testing.allocator;
    const result = try escapeIdentifier(allocator, "users");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("\"users\"", result);
}

test "escapeIdentifier invalid" {
    const allocator = std.testing.allocator;
    try std.testing.expectError(error.InvalidIdentifier, escapeIdentifier(allocator, "123invalid"));
    try std.testing.expectError(error.InvalidIdentifier, escapeIdentifier(allocator, "user-name"));
    try std.testing.expectError(error.InvalidIdentifier, escapeIdentifier(allocator, ""));
}

test "containsDangerousKeyword" {
    try std.testing.expect(containsDangerousKeyword("DROP TABLE users"));
    try std.testing.expect(containsDangerousKeyword("delete from users"));
    try std.testing.expect(!containsDangerousKeyword("SELECT * FROM users"));
}

test "hasBalancedQuotes" {
    try std.testing.expect(hasBalancedQuotes("SELECT * FROM users WHERE name = 'John'"));
    try std.testing.expect(!hasBalancedQuotes("SELECT * FROM users WHERE name = 'John"));
    try std.testing.expect(hasBalancedQuotes(""));
}

test "escapeLikePattern" {
    const allocator = std.testing.allocator;
    const result = try escapeLikePattern(allocator, "100%", '\\');
    defer allocator.free(result);
    try std.testing.expectEqualStrings("100\\%", result);
}

test "QueryBuilder postgres style" {
    const allocator = std.testing.allocator;
    var builder = QueryBuilder.init(allocator, .dollar_number);
    defer builder.deinit();

    try builder.addLiteral("SELECT * FROM users WHERE id = ");
    try builder.addParam(.{ .integer = 42 });
    try builder.addLiteral(" AND name = ");
    try builder.addParam(.{ .text = "John" });

    const query = try builder.build();
    defer allocator.free(query);

    try std.testing.expectEqualStrings("SELECT * FROM users WHERE id = $1 AND name = $2", query);
}

test "QueryBuilder sqlite style" {
    const allocator = std.testing.allocator;
    var builder = QueryBuilder.init(allocator, .question_mark);
    defer builder.deinit();

    try builder.addLiteral("INSERT INTO users (name, age) VALUES (");
    try builder.addParam(.{ .text = "Alice" });
    try builder.addLiteral(", ");
    try builder.addParam(.{ .integer = 30 });
    try builder.addLiteral(")");

    const query = try builder.build();
    defer allocator.free(query);

    try std.testing.expectEqualStrings("INSERT INTO users (name, age) VALUES (?, ?)", query);
}

test "stripComments" {
    const allocator = std.testing.allocator;
    const result = try stripComments(allocator, "SELECT * -- this is a comment\nFROM users");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("SELECT * \nFROM users", result);
}

test "selectQuery" {
    const allocator = std.testing.allocator;
    const columns = [_][]const u8{ "id", "name", "email" };
    const result = try selectQuery(allocator, "users", &columns, "id = 1");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("SELECT \"id\", \"name\", \"email\" FROM \"users\" WHERE id = 1", result);
}

test "Value.toSqlLiteral" {
    const allocator = std.testing.allocator;

    const null_val = Value{ .null = {} };
    const null_result = try null_val.toSqlLiteral(allocator);
    defer allocator.free(null_result);
    try std.testing.expectEqualStrings("NULL", null_result);

    const int_val = Value{ .integer = 42 };
    const int_result = try int_val.toSqlLiteral(allocator);
    defer allocator.free(int_result);
    try std.testing.expectEqualStrings("42", int_result);

    const bool_val = Value{ .boolean = true };
    const bool_result = try bool_val.toSqlLiteral(allocator);
    defer allocator.free(bool_result);
    try std.testing.expectEqualStrings("TRUE", bool_result);
}
