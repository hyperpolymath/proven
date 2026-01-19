// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe YAML parsing and validation that cannot crash.
//!
//! Provides secure YAML handling with protection against common YAML
//! vulnerabilities including billion laughs attacks, arbitrary code execution
//! via tags, and deeply nested structures. Implements a safe subset of YAML
//! suitable for configuration files.

const std = @import("std");

/// Error types for YAML operations.
pub const YamlError = error{
    /// Invalid YAML syntax.
    ParseError,
    /// Invalid indentation.
    InvalidIndentation,
    /// Maximum depth exceeded.
    MaxDepthExceeded,
    /// Maximum document size exceeded.
    MaxSizeExceeded,
    /// Invalid character in YAML content.
    InvalidCharacter,
    /// Duplicate key in mapping.
    DuplicateKey,
    /// Invalid anchor or alias.
    InvalidReference,
    /// Unsupported YAML feature (tags, directives, etc.).
    UnsupportedFeature,
    /// Out of memory.
    OutOfMemory,
};

/// YAML value types (safe subset).
pub const ValueType = enum {
    null,
    boolean,
    integer,
    float,
    string,
    sequence,
    mapping,
};

/// A safe YAML value representation.
pub const Value = union(ValueType) {
    null: void,
    boolean: bool,
    integer: i64,
    float: f64,
    string: []const u8,
    sequence: []const Value,
    mapping: std.StringHashMap(Value),

    /// Check if value is null.
    pub fn isNull(self: Value) bool {
        return self == .null;
    }

    /// Check if value is boolean.
    pub fn isBool(self: Value) bool {
        return self == .boolean;
    }

    /// Check if value is integer.
    pub fn isInteger(self: Value) bool {
        return self == .integer;
    }

    /// Check if value is float.
    pub fn isFloat(self: Value) bool {
        return self == .float;
    }

    /// Check if value is string.
    pub fn isString(self: Value) bool {
        return self == .string;
    }

    /// Check if value is sequence.
    pub fn isSequence(self: Value) bool {
        return self == .sequence;
    }

    /// Check if value is mapping.
    pub fn isMapping(self: Value) bool {
        return self == .mapping;
    }

    /// Get boolean value, return null if not boolean.
    pub fn asBool(self: Value) ?bool {
        return switch (self) {
            .boolean => |boolean_value| boolean_value,
            else => null,
        };
    }

    /// Get integer value, return null if not integer.
    pub fn asInteger(self: Value) ?i64 {
        return switch (self) {
            .integer => |integer_value| integer_value,
            else => null,
        };
    }

    /// Get float value, return null if not float.
    pub fn asFloat(self: Value) ?f64 {
        return switch (self) {
            .float => |float_value| float_value,
            .integer => |integer_value| @as(f64, @floatFromInt(integer_value)),
            else => null,
        };
    }

    /// Get string value, return null if not string.
    pub fn asString(self: Value) ?[]const u8 {
        return switch (self) {
            .string => |string_value| string_value,
            else => null,
        };
    }

    /// Get sequence value, return null if not sequence.
    pub fn asSequence(self: Value) ?[]const Value {
        return switch (self) {
            .sequence => |sequence_value| sequence_value,
            else => null,
        };
    }

    /// Get mapping value, return null if not mapping.
    pub fn asMapping(self: Value) ?std.StringHashMap(Value) {
        return switch (self) {
            .mapping => |mapping_value| mapping_value,
            else => null,
        };
    }

    /// Get a value from a mapping by key.
    pub fn get(self: Value, key: []const u8) ?Value {
        return switch (self) {
            .mapping => |mapping_value| mapping_value.get(key),
            else => null,
        };
    }

    /// Get a value at a path (e.g., "foo.bar.baz").
    pub fn getPath(self: Value, path: []const u8) ?Value {
        var current_value = self;
        var path_iterator = std.mem.splitScalar(u8, path, '.');

        while (path_iterator.next()) |key| {
            switch (current_value) {
                .mapping => |mapping_value| {
                    if (mapping_value.get(key)) |next_value| {
                        current_value = next_value;
                    } else {
                        return null;
                    }
                },
                .sequence => |sequence_value| {
                    const index = std.fmt.parseInt(usize, key, 10) catch return null;
                    if (index >= sequence_value.len) return null;
                    current_value = sequence_value[index];
                },
                else => return null,
            }
        }

        return current_value;
    }
};

/// Maximum allowed nesting depth.
pub const MAX_DEPTH = 32;

/// Maximum document size in bytes.
pub const MAX_SIZE = 1024 * 1024; // 1 MB

/// Safe YAML parsing options.
pub const ParseOptions = struct {
    max_depth: usize = MAX_DEPTH,
    max_size: usize = MAX_SIZE,
    allow_anchors: bool = false, // Disabled by default to prevent billion laughs
    allow_tags: bool = false, // Disabled by default to prevent arbitrary code
    strict_indentation: bool = true,
};

/// Check if a string represents a YAML null value.
pub fn isNullValue(input: []const u8) bool {
    const trimmed = std.mem.trim(u8, input, " \t");
    return trimmed.len == 0 or
        std.mem.eql(u8, trimmed, "null") or
        std.mem.eql(u8, trimmed, "Null") or
        std.mem.eql(u8, trimmed, "NULL") or
        std.mem.eql(u8, trimmed, "~");
}

/// Check if a string represents a YAML boolean value.
pub fn isBoolValue(input: []const u8) bool {
    const trimmed = std.mem.trim(u8, input, " \t");
    return std.mem.eql(u8, trimmed, "true") or
        std.mem.eql(u8, trimmed, "True") or
        std.mem.eql(u8, trimmed, "TRUE") or
        std.mem.eql(u8, trimmed, "false") or
        std.mem.eql(u8, trimmed, "False") or
        std.mem.eql(u8, trimmed, "FALSE") or
        std.mem.eql(u8, trimmed, "yes") or
        std.mem.eql(u8, trimmed, "Yes") or
        std.mem.eql(u8, trimmed, "YES") or
        std.mem.eql(u8, trimmed, "no") or
        std.mem.eql(u8, trimmed, "No") or
        std.mem.eql(u8, trimmed, "NO") or
        std.mem.eql(u8, trimmed, "on") or
        std.mem.eql(u8, trimmed, "On") or
        std.mem.eql(u8, trimmed, "ON") or
        std.mem.eql(u8, trimmed, "off") or
        std.mem.eql(u8, trimmed, "Off") or
        std.mem.eql(u8, trimmed, "OFF");
}

/// Parse a boolean from a YAML string.
pub fn parseBool(input: []const u8) ?bool {
    const trimmed = std.mem.trim(u8, input, " \t");

    if (std.mem.eql(u8, trimmed, "true") or
        std.mem.eql(u8, trimmed, "True") or
        std.mem.eql(u8, trimmed, "TRUE") or
        std.mem.eql(u8, trimmed, "yes") or
        std.mem.eql(u8, trimmed, "Yes") or
        std.mem.eql(u8, trimmed, "YES") or
        std.mem.eql(u8, trimmed, "on") or
        std.mem.eql(u8, trimmed, "On") or
        std.mem.eql(u8, trimmed, "ON"))
    {
        return true;
    }

    if (std.mem.eql(u8, trimmed, "false") or
        std.mem.eql(u8, trimmed, "False") or
        std.mem.eql(u8, trimmed, "FALSE") or
        std.mem.eql(u8, trimmed, "no") or
        std.mem.eql(u8, trimmed, "No") or
        std.mem.eql(u8, trimmed, "NO") or
        std.mem.eql(u8, trimmed, "off") or
        std.mem.eql(u8, trimmed, "Off") or
        std.mem.eql(u8, trimmed, "OFF"))
    {
        return false;
    }

    return null;
}

/// Check if a string represents a YAML integer.
pub fn isIntegerValue(input: []const u8) bool {
    const trimmed = std.mem.trim(u8, input, " \t");
    if (trimmed.len == 0) return false;

    _ = std.fmt.parseInt(i64, trimmed, 0) catch return false;
    return true;
}

/// Parse an integer from a YAML string.
pub fn parseInteger(input: []const u8) ?i64 {
    const trimmed = std.mem.trim(u8, input, " \t");
    return std.fmt.parseInt(i64, trimmed, 0) catch null;
}

/// Check if a string represents a YAML float.
pub fn isFloatValue(input: []const u8) bool {
    const trimmed = std.mem.trim(u8, input, " \t");
    if (trimmed.len == 0) return false;

    // Check for special values
    if (std.mem.eql(u8, trimmed, ".inf") or
        std.mem.eql(u8, trimmed, ".Inf") or
        std.mem.eql(u8, trimmed, ".INF") or
        std.mem.eql(u8, trimmed, "-.inf") or
        std.mem.eql(u8, trimmed, "-.Inf") or
        std.mem.eql(u8, trimmed, "-.INF") or
        std.mem.eql(u8, trimmed, ".nan") or
        std.mem.eql(u8, trimmed, ".NaN") or
        std.mem.eql(u8, trimmed, ".NAN"))
    {
        return true;
    }

    _ = std.fmt.parseFloat(f64, trimmed) catch return false;
    return true;
}

/// Parse a float from a YAML string.
pub fn parseFloat(input: []const u8) ?f64 {
    const trimmed = std.mem.trim(u8, input, " \t");

    // Handle special values
    if (std.mem.eql(u8, trimmed, ".inf") or
        std.mem.eql(u8, trimmed, ".Inf") or
        std.mem.eql(u8, trimmed, ".INF"))
    {
        return std.math.inf(f64);
    }
    if (std.mem.eql(u8, trimmed, "-.inf") or
        std.mem.eql(u8, trimmed, "-.Inf") or
        std.mem.eql(u8, trimmed, "-.INF"))
    {
        return -std.math.inf(f64);
    }
    if (std.mem.eql(u8, trimmed, ".nan") or
        std.mem.eql(u8, trimmed, ".NaN") or
        std.mem.eql(u8, trimmed, ".NAN"))
    {
        return std.math.nan(f64);
    }

    return std.fmt.parseFloat(f64, trimmed) catch null;
}

/// Escape a string for safe inclusion in YAML.
pub fn escapeString(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    // Check if quoting is needed
    var needs_quoting = input.len == 0;
    var needs_escaping = false;

    for (input, 0..) |character, character_index| {
        switch (character) {
            ':', '#', '{', '}', '[', ']', ',', '&', '*', '!', '|', '>', '\'', '"', '%', '@', '`' => {
                needs_quoting = true;
            },
            '\n', '\r', '\t', '\\' => {
                needs_escaping = true;
                needs_quoting = true;
            },
            ' ' => {
                if (character_index == 0 or character_index == input.len - 1) {
                    needs_quoting = true;
                }
            },
            else => {},
        }
    }

    // Check for special values that need quoting
    if (isNullValue(input) or isBoolValue(input) or isIntegerValue(input) or isFloatValue(input)) {
        needs_quoting = true;
    }

    if (!needs_quoting) {
        return allocator.dupe(u8, input);
    }

    var output_list = std.array_list.Managed(u8).init(allocator);
    errdefer output_list.deinit();

    try output_list.append('"');

    for (input) |character| {
        if (needs_escaping) {
            switch (character) {
                '\n' => try output_list.appendSlice("\\n"),
                '\r' => try output_list.appendSlice("\\r"),
                '\t' => try output_list.appendSlice("\\t"),
                '\\' => try output_list.appendSlice("\\\\"),
                '"' => try output_list.appendSlice("\\\""),
                else => try output_list.append(character),
            }
        } else {
            if (character == '"') {
                try output_list.appendSlice("\\\"");
            } else {
                try output_list.append(character);
            }
        }
    }

    try output_list.append('"');

    return output_list.toOwnedSlice();
}

/// Unquote and unescape a YAML string.
pub fn unescapeString(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    const trimmed = std.mem.trim(u8, input, " \t");
    if (trimmed.len == 0) return allocator.dupe(u8, "");

    // Check for quoted string
    const is_double_quoted = trimmed[0] == '"' and trimmed.len > 1 and trimmed[trimmed.len - 1] == '"';
    const is_single_quoted = trimmed[0] == '\'' and trimmed.len > 1 and trimmed[trimmed.len - 1] == '\'';

    if (is_single_quoted) {
        // Single-quoted strings only escape '' as '
        const content = trimmed[1 .. trimmed.len - 1];
        var output_list = std.array_list.Managed(u8).init(allocator);
        errdefer output_list.deinit();

        var read_index: usize = 0;
        while (read_index < content.len) {
            if (read_index + 1 < content.len and content[read_index] == '\'' and content[read_index + 1] == '\'') {
                try output_list.append('\'');
                read_index += 2;
            } else {
                try output_list.append(content[read_index]);
                read_index += 1;
            }
        }

        return output_list.toOwnedSlice();
    }

    if (is_double_quoted) {
        // Double-quoted strings support escape sequences
        const content = trimmed[1 .. trimmed.len - 1];
        var output_list = std.array_list.Managed(u8).init(allocator);
        errdefer output_list.deinit();

        var read_index: usize = 0;
        while (read_index < content.len) {
            if (content[read_index] == '\\' and read_index + 1 < content.len) {
                switch (content[read_index + 1]) {
                    'n' => try output_list.append('\n'),
                    'r' => try output_list.append('\r'),
                    't' => try output_list.append('\t'),
                    '\\' => try output_list.append('\\'),
                    '"' => try output_list.append('"'),
                    '0' => try output_list.append(0),
                    else => {
                        try output_list.append('\\');
                        try output_list.append(content[read_index + 1]);
                    },
                }
                read_index += 2;
            } else {
                try output_list.append(content[read_index]);
                read_index += 1;
            }
        }

        return output_list.toOwnedSlice();
    }

    // Unquoted string
    return allocator.dupe(u8, trimmed);
}

/// Validate a string for safe YAML key.
pub fn isValidKey(key: []const u8) bool {
    if (key.len == 0) return false;

    // Keys cannot start with certain characters
    if (key[0] == '-' or key[0] == '?' or key[0] == ':' or key[0] == '[' or key[0] == '{') {
        return false;
    }

    // Check for invalid characters
    for (key) |character| {
        if (character == '\n' or character == '\r' or character == 0) {
            return false;
        }
    }

    return true;
}

/// Check if content contains potentially dangerous YAML features.
pub fn hasDangerousContent(input: []const u8) bool {
    // Check for anchors and aliases (billion laughs attack vector)
    if (std.mem.indexOf(u8, input, "&") != null or std.mem.indexOf(u8, input, "*") != null) {
        return true;
    }

    // Check for tags (code execution attack vector)
    if (std.mem.indexOf(u8, input, "!") != null) {
        return true;
    }

    // Check for directives
    if (std.mem.indexOf(u8, input, "%") != null) {
        return true;
    }

    return false;
}

/// Validate YAML indentation.
pub fn validateIndentation(input: []const u8) YamlError!void {
    var line_iterator = std.mem.splitScalar(u8, input, '\n');
    var expected_indent: ?usize = null;

    while (line_iterator.next()) |line| {
        if (line.len == 0) continue;

        // Count leading spaces
        var space_count: usize = 0;
        for (line) |character| {
            if (character == ' ') {
                space_count += 1;
            } else if (character == '\t') {
                return error.InvalidIndentation; // Tabs not allowed
            } else {
                break;
            }
        }

        // Skip comment-only lines
        if (space_count < line.len and line[space_count] == '#') continue;

        // Determine indent size
        if (expected_indent == null and space_count > 0) {
            expected_indent = space_count;
        }

        // Validate indent is multiple of base
        if (expected_indent) |base_indent| {
            if (space_count > 0 and space_count % base_indent != 0) {
                return error.InvalidIndentation;
            }
        }
    }
}

/// Calculate depth of nested structure.
pub fn calculateDepth(input: []const u8) usize {
    var max_depth: usize = 0;
    var current_depth: usize = 0;

    for (input) |character| {
        switch (character) {
            '{', '[' => {
                current_depth += 1;
                if (current_depth > max_depth) max_depth = current_depth;
            },
            '}', ']' => {
                if (current_depth > 0) current_depth -= 1;
            },
            else => {},
        }
    }

    return max_depth;
}

/// Check if a string is valid YAML (basic structural validation).
pub fn isValid(input: []const u8, options: ParseOptions) bool {
    if (input.len > options.max_size) return false;

    // Check for dangerous content
    if (!options.allow_anchors and !options.allow_tags) {
        if (hasDangerousContent(input)) return false;
    }

    // Validate depth
    if (calculateDepth(input) > options.max_depth) return false;

    // Validate indentation
    if (options.strict_indentation) {
        validateIndentation(input) catch return false;
    }

    return true;
}

test "isNullValue" {
    try std.testing.expect(isNullValue("null"));
    try std.testing.expect(isNullValue("Null"));
    try std.testing.expect(isNullValue("NULL"));
    try std.testing.expect(isNullValue("~"));
    try std.testing.expect(isNullValue("  "));
    try std.testing.expect(!isNullValue("notnull"));
}

test "parseBool" {
    try std.testing.expectEqual(@as(?bool, true), parseBool("true"));
    try std.testing.expectEqual(@as(?bool, true), parseBool("yes"));
    try std.testing.expectEqual(@as(?bool, true), parseBool("on"));
    try std.testing.expectEqual(@as(?bool, false), parseBool("false"));
    try std.testing.expectEqual(@as(?bool, false), parseBool("no"));
    try std.testing.expectEqual(@as(?bool, false), parseBool("off"));
    try std.testing.expectEqual(@as(?bool, null), parseBool("maybe"));
}

test "parseInteger" {
    try std.testing.expectEqual(@as(?i64, 42), parseInteger("42"));
    try std.testing.expectEqual(@as(?i64, -10), parseInteger("-10"));
    try std.testing.expectEqual(@as(?i64, 255), parseInteger("0xff"));
    try std.testing.expectEqual(@as(?i64, 8), parseInteger("0o10"));
    try std.testing.expectEqual(@as(?i64, null), parseInteger("not a number"));
}

test "parseFloat" {
    try std.testing.expectEqual(@as(?f64, 3.14), parseFloat("3.14"));
    try std.testing.expectEqual(@as(?f64, -2.5), parseFloat("-2.5"));
    try std.testing.expect(std.math.isInf(parseFloat(".inf").?));
    try std.testing.expect(std.math.isInf(parseFloat("-.Inf").?));
    try std.testing.expect(std.math.isNan(parseFloat(".nan").?));
}

test "escapeString" {
    const allocator = std.testing.allocator;

    // String that needs quoting
    const escaped = try escapeString(allocator, "value: with colon");
    defer allocator.free(escaped);
    try std.testing.expectEqualStrings("\"value: with colon\"", escaped);

    // String with escapes
    const with_newline = try escapeString(allocator, "line1\nline2");
    defer allocator.free(with_newline);
    try std.testing.expectEqualStrings("\"line1\\nline2\"", with_newline);

    // Simple string - no quoting needed
    const simple = try escapeString(allocator, "simple");
    defer allocator.free(simple);
    try std.testing.expectEqualStrings("simple", simple);
}

test "unescapeString" {
    const allocator = std.testing.allocator;

    // Double-quoted
    const double_quoted = try unescapeString(allocator, "\"line1\\nline2\"");
    defer allocator.free(double_quoted);
    try std.testing.expectEqualStrings("line1\nline2", double_quoted);

    // Single-quoted
    const single_quoted = try unescapeString(allocator, "'it''s'");
    defer allocator.free(single_quoted);
    try std.testing.expectEqualStrings("it's", single_quoted);
}

test "isValidKey" {
    try std.testing.expect(isValidKey("simple_key"));
    try std.testing.expect(isValidKey("key123"));
    try std.testing.expect(!isValidKey("-invalid"));
    try std.testing.expect(!isValidKey(""));
    try std.testing.expect(!isValidKey("key\nwith\nnewlines"));
}

test "hasDangerousContent" {
    try std.testing.expect(hasDangerousContent("&anchor"));
    try std.testing.expect(hasDangerousContent("*alias"));
    try std.testing.expect(hasDangerousContent("!tag"));
    try std.testing.expect(hasDangerousContent("%directive"));
    try std.testing.expect(!hasDangerousContent("safe: content"));
}

test "validateIndentation" {
    try validateIndentation("key: value");
    try validateIndentation("parent:\n  child: value");
    try std.testing.expectError(error.InvalidIndentation, validateIndentation("key:\n\tchild: value"));
}

test "isValid" {
    try std.testing.expect(isValid("key: value", .{}));
    try std.testing.expect(isValid("list:\n  - item1\n  - item2", .{}));
    try std.testing.expect(!isValid("&anchor value", .{}));
    try std.testing.expect(!isValid("!tag value", .{}));
}
