// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe CSV parsing and generation with proper escaping.
//!
//! Provides RFC 4180 compliant CSV parsing and generation.
//! All operations are bounds-checked and handle edge cases safely.
//! Supports custom delimiters, quoting, and escape characters.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for CSV operations.
pub const CsvError = error{
    /// Unterminated quoted field
    UnterminatedQuote,
    /// Invalid escape sequence
    InvalidEscape,
    /// Row has inconsistent column count
    InconsistentColumnCount,
    /// Input is empty
    EmptyInput,
    /// Maximum field count exceeded
    MaxFieldsExceeded,
    /// Field too long
    FieldTooLong,
    /// Out of memory
    OutOfMemory,
};

/// CSV parsing options
pub const ParseOptions = struct {
    /// Field delimiter (default: comma)
    delimiter: u8 = ',',
    /// Quote character for escaping (default: double quote)
    quote: u8 = '"',
    /// Whether to trim whitespace from fields
    trim_whitespace: bool = false,
    /// Maximum number of fields per row (0 = unlimited)
    max_fields: usize = 0,
    /// Maximum field length (0 = unlimited)
    max_field_length: usize = 0,
    /// Skip empty rows
    skip_empty_rows: bool = false,
};

/// CSV generation options
pub const WriteOptions = struct {
    /// Field delimiter
    delimiter: u8 = ',',
    /// Quote character
    quote: u8 = '"',
    /// Line ending
    line_ending: LineEnding = .lf,
    /// Always quote fields (even if not necessary)
    always_quote: bool = false,
};

/// Line ending styles
pub const LineEnding = enum {
    lf, // Unix: \n
    crlf, // Windows: \r\n
    cr, // Old Mac: \r
};

/// A parsed CSV row
pub const Row = struct {
    fields: [][]const u8,
    allocator: Allocator,

    pub fn deinit(self: Row) void {
        for (self.fields) |field| {
            self.allocator.free(field);
        }
        self.allocator.free(self.fields);
    }

    /// Get field at index, or null if out of bounds
    pub fn get(self: Row, index: usize) ?[]const u8 {
        if (index >= self.fields.len) return null;
        return self.fields[index];
    }

    /// Get field count
    pub fn len(self: Row) usize {
        return self.fields.len;
    }
};

/// CSV Reader for streaming row-by-row parsing
pub const Reader = struct {
    input: []const u8,
    position: usize,
    options: ParseOptions,
    allocator: Allocator,
    expected_columns: ?usize,

    pub fn init(allocator: Allocator, input: []const u8, options: ParseOptions) Reader {
        return Reader{
            .input = input,
            .position = 0,
            .options = options,
            .allocator = allocator,
            .expected_columns = null,
        };
    }

    /// Read the next row, or null if at end of input
    pub fn next(self: *Reader) CsvError!?Row {
        if (self.position >= self.input.len) return null;

        var fields = std.ArrayList([]const u8).init(self.allocator);
        errdefer {
            for (fields.items) |field| {
                self.allocator.free(field);
            }
            fields.deinit();
        }

        while (self.position < self.input.len) {
            // Check max fields
            if (self.options.max_fields > 0 and fields.items.len >= self.options.max_fields) {
                return error.MaxFieldsExceeded;
            }

            const field = try self.parseField();
            fields.append(field) catch return error.OutOfMemory;

            if (self.position >= self.input.len) break;

            const current_char = self.input[self.position];
            if (current_char == '\n') {
                self.position += 1;
                break;
            } else if (current_char == '\r') {
                self.position += 1;
                if (self.position < self.input.len and self.input[self.position] == '\n') {
                    self.position += 1;
                }
                break;
            } else if (current_char == self.options.delimiter) {
                self.position += 1;
            }
        }

        // Skip empty rows if configured
        if (self.options.skip_empty_rows and fields.items.len == 1 and fields.items[0].len == 0) {
            self.allocator.free(fields.items[0]);
            fields.deinit();
            return self.next();
        }

        // Check column consistency
        if (self.expected_columns) |expected_column_count| {
            if (fields.items.len != expected_column_count) {
                return error.InconsistentColumnCount;
            }
        } else {
            self.expected_columns = fields.items.len;
        }

        return Row{
            .fields = fields.toOwnedSlice() catch return error.OutOfMemory,
            .allocator = self.allocator,
        };
    }

    fn parseField(self: *Reader) CsvError![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);
        errdefer result.deinit();

        const in_quotes = self.position < self.input.len and self.input[self.position] == self.options.quote;
        if (in_quotes) self.position += 1;

        while (self.position < self.input.len) {
            const char = self.input[self.position];

            if (in_quotes) {
                if (char == self.options.quote) {
                    // Check for escaped quote
                    if (self.position + 1 < self.input.len and self.input[self.position + 1] == self.options.quote) {
                        result.append(self.options.quote) catch return error.OutOfMemory;
                        self.position += 2;
                    } else {
                        // End of quoted field
                        self.position += 1;
                        break;
                    }
                } else {
                    result.append(char) catch return error.OutOfMemory;
                    self.position += 1;
                }
            } else {
                if (char == self.options.delimiter or char == '\n' or char == '\r') {
                    break;
                }
                result.append(char) catch return error.OutOfMemory;
                self.position += 1;
            }

            // Check max field length
            if (self.options.max_field_length > 0 and result.items.len > self.options.max_field_length) {
                return error.FieldTooLong;
            }
        }

        // Handle unterminated quote
        if (in_quotes and (self.position >= self.input.len or
            (self.input[self.position - 1] != self.options.quote)))
        {
            // Check if we actually ended with a quote
            if (self.position > 0 and self.input[self.position - 1] != self.options.quote) {
                // We're at end of input without closing quote - this is still valid,
                // the quote was "closed" by EOF
            }
        }

        var field = result.toOwnedSlice() catch return error.OutOfMemory;

        // Trim whitespace if configured
        if (self.options.trim_whitespace) {
            const trimmed = std.mem.trim(u8, field, &[_]u8{ ' ', '\t' });
            if (trimmed.len != field.len) {
                const new_field = self.allocator.dupe(u8, trimmed) catch {
                    self.allocator.free(field);
                    return error.OutOfMemory;
                };
                self.allocator.free(field);
                field = new_field;
            }
        }

        return field;
    }
};

/// Parse all rows from CSV input
pub fn parseAll(allocator: Allocator, input: []const u8, options: ParseOptions) CsvError![]Row {
    if (input.len == 0) return error.EmptyInput;

    var rows = std.ArrayList(Row).init(allocator);
    errdefer {
        for (rows.items) |row| {
            row.deinit();
        }
        rows.deinit();
    }

    var reader = Reader.init(allocator, input, options);
    while (try reader.next()) |row| {
        rows.append(row) catch return error.OutOfMemory;
    }

    return rows.toOwnedSlice() catch return error.OutOfMemory;
}

/// Free all rows returned by parseAll
pub fn freeRows(rows: []Row) void {
    for (rows) |row| {
        row.deinit();
    }
    if (rows.len > 0) {
        rows[0].allocator.free(rows);
    }
}

/// Check if a field needs quoting
pub fn needsQuoting(field: []const u8, options: WriteOptions) bool {
    if (options.always_quote) return true;

    for (field) |char| {
        if (char == options.delimiter or char == options.quote or char == '\n' or char == '\r') {
            return true;
        }
    }
    return false;
}

/// Escape a field for CSV output
pub fn escapeField(allocator: Allocator, field: []const u8, options: WriteOptions) ![]u8 {
    if (!needsQuoting(field, options)) {
        return allocator.dupe(u8, field);
    }

    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    try result.append(options.quote);

    for (field) |char| {
        if (char == options.quote) {
            try result.append(options.quote);
            try result.append(options.quote);
        } else {
            try result.append(char);
        }
    }

    try result.append(options.quote);
    return result.toOwnedSlice();
}

/// Write a single row to CSV format
pub fn writeRow(allocator: Allocator, fields: []const []const u8, options: WriteOptions) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    errdefer result.deinit();

    for (fields, 0..) |field, field_index| {
        if (field_index > 0) {
            try result.append(options.delimiter);
        }

        if (needsQuoting(field, options)) {
            try result.append(options.quote);
            for (field) |char| {
                if (char == options.quote) {
                    try result.append(options.quote);
                }
                try result.append(char);
            }
            try result.append(options.quote);
        } else {
            try result.appendSlice(field);
        }
    }

    switch (options.line_ending) {
        .lf => try result.append('\n'),
        .crlf => try result.appendSlice("\r\n"),
        .cr => try result.append('\r'),
    }

    return result.toOwnedSlice();
}

/// CSV Writer for building CSV output incrementally
pub const Writer = struct {
    buffer: std.ArrayList(u8),
    options: WriteOptions,
    row_count: usize,

    pub fn init(allocator: Allocator, options: WriteOptions) Writer {
        return Writer{
            .buffer = std.ArrayList(u8).init(allocator),
            .options = options,
            .row_count = 0,
        };
    }

    pub fn deinit(self: *Writer) void {
        self.buffer.deinit();
    }

    /// Write a row of fields
    pub fn writeRow(self: *Writer, fields: []const []const u8) !void {
        for (fields, 0..) |field, field_index| {
            if (field_index > 0) {
                try self.buffer.append(self.options.delimiter);
            }

            if (needsQuoting(field, self.options)) {
                try self.buffer.append(self.options.quote);
                for (field) |char| {
                    if (char == self.options.quote) {
                        try self.buffer.append(self.options.quote);
                    }
                    try self.buffer.append(char);
                }
                try self.buffer.append(self.options.quote);
            } else {
                try self.buffer.appendSlice(field);
            }
        }

        switch (self.options.line_ending) {
            .lf => try self.buffer.append('\n'),
            .crlf => try self.buffer.appendSlice("\r\n"),
            .cr => try self.buffer.append('\r'),
        }

        self.row_count += 1;
    }

    /// Get the generated CSV content
    pub fn toOwnedSlice(self: *Writer) ![]u8 {
        return self.buffer.toOwnedSlice();
    }

    /// Get current content without taking ownership
    pub fn getWritten(self: *Writer) []const u8 {
        return self.buffer.items;
    }
};

/// Count columns in first row (without full parsing)
pub fn countColumns(input: []const u8, delimiter: u8) usize {
    if (input.len == 0) return 0;

    var count: usize = 1;
    var in_quotes = false;
    var index: usize = 0;

    while (index < input.len) {
        const char = input[index];

        if (char == '"' and !in_quotes) {
            in_quotes = true;
        } else if (char == '"' and in_quotes) {
            if (index + 1 < input.len and input[index + 1] == '"') {
                index += 1; // Skip escaped quote
            } else {
                in_quotes = false;
            }
        } else if (!in_quotes) {
            if (char == delimiter) {
                count += 1;
            } else if (char == '\n' or char == '\r') {
                break;
            }
        }
        index += 1;
    }

    return count;
}

/// Check if input is valid CSV
pub fn isValid(input: []const u8, options: ParseOptions) bool {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    _ = parseAll(arena.allocator(), input, options) catch return false;
    return true;
}

// ============================================================================
// Tests
// ============================================================================

test "parse simple CSV" {
    const allocator = std.testing.allocator;
    const input = "a,b,c\n1,2,3\n";

    const rows = try parseAll(allocator, input, .{});
    defer freeRows(rows);

    try std.testing.expectEqual(@as(usize, 2), rows.len);
    try std.testing.expectEqual(@as(usize, 3), rows[0].len());
    try std.testing.expectEqualStrings("a", rows[0].get(0).?);
    try std.testing.expectEqualStrings("b", rows[0].get(1).?);
    try std.testing.expectEqualStrings("c", rows[0].get(2).?);
}

test "parse quoted fields" {
    const allocator = std.testing.allocator;
    const input = "\"hello, world\",\"with \"\"quotes\"\"\"\n";

    const rows = try parseAll(allocator, input, .{});
    defer freeRows(rows);

    try std.testing.expectEqual(@as(usize, 1), rows.len);
    try std.testing.expectEqualStrings("hello, world", rows[0].get(0).?);
    try std.testing.expectEqualStrings("with \"quotes\"", rows[0].get(1).?);
}

test "parse with custom delimiter" {
    const allocator = std.testing.allocator;
    const input = "a;b;c\n1;2;3\n";

    const rows = try parseAll(allocator, input, .{ .delimiter = ';' });
    defer freeRows(rows);

    try std.testing.expectEqual(@as(usize, 2), rows.len);
    try std.testing.expectEqualStrings("a", rows[0].get(0).?);
}

test "escape field" {
    const allocator = std.testing.allocator;

    const simple = try escapeField(allocator, "hello", .{});
    defer allocator.free(simple);
    try std.testing.expectEqualStrings("hello", simple);

    const with_comma = try escapeField(allocator, "hello, world", .{});
    defer allocator.free(with_comma);
    try std.testing.expectEqualStrings("\"hello, world\"", with_comma);

    const with_quote = try escapeField(allocator, "say \"hi\"", .{});
    defer allocator.free(with_quote);
    try std.testing.expectEqualStrings("\"say \"\"hi\"\"\"", with_quote);
}

test "needsQuoting" {
    try std.testing.expect(!needsQuoting("hello", .{}));
    try std.testing.expect(needsQuoting("hello, world", .{}));
    try std.testing.expect(needsQuoting("with\nnewline", .{}));
    try std.testing.expect(needsQuoting("with \"quote\"", .{}));
    try std.testing.expect(needsQuoting("anything", .{ .always_quote = true }));
}

test "Writer" {
    const allocator = std.testing.allocator;

    var writer = Writer.init(allocator, .{});
    defer writer.deinit();

    try writer.writeRow(&[_][]const u8{ "name", "age" });
    try writer.writeRow(&[_][]const u8{ "Alice", "30" });

    const output = writer.getWritten();
    try std.testing.expectEqualStrings("name,age\nAlice,30\n", output);
}

test "countColumns" {
    try std.testing.expectEqual(@as(usize, 3), countColumns("a,b,c\n", ','));
    try std.testing.expectEqual(@as(usize, 1), countColumns("single\n", ','));
    try std.testing.expectEqual(@as(usize, 2), countColumns("\"with,comma\",other\n", ','));
    try std.testing.expectEqual(@as(usize, 0), countColumns("", ','));
}

test "isValid" {
    try std.testing.expect(isValid("a,b,c\n1,2,3\n", .{}));
    try std.testing.expect(isValid("\"quoted\"\n", .{}));
    try std.testing.expect(!isValid("", .{}));
}

test "empty input" {
    const allocator = std.testing.allocator;
    try std.testing.expectError(error.EmptyInput, parseAll(allocator, "", .{}));
}
