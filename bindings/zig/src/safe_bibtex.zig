// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe BibTeX entry parsing and validation operations that cannot crash.
//!
//! This module provides safe parsing of BibTeX entries commonly used in
//! academic citations and bibliography management. All operations return
//! errors on invalid input rather than panicking.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for BibTeX operations.
pub const BibTeXError = error{
    InvalidEntryType,
    InvalidCiteKey,
    MissingField,
    UnterminatedString,
    UnterminatedBrace,
    InvalidCharacter,
    MalformedEntry,
    DuplicateField,
    EmptyInput,
    OutOfMemory,
};

/// BibTeX entry types.
pub const EntryType = enum {
    article,
    book,
    booklet,
    conference,
    inbook,
    incollection,
    inproceedings,
    manual,
    mastersthesis,
    misc,
    phdthesis,
    proceedings,
    techreport,
    unpublished,
    online,
    software,
    dataset,
    unknown,

    /// Parse entry type from string (case-insensitive).
    pub fn fromString(str: []const u8) EntryType {
        const entry_type_map = .{
            .{ "article", EntryType.article },
            .{ "book", EntryType.book },
            .{ "booklet", EntryType.booklet },
            .{ "conference", EntryType.conference },
            .{ "inbook", EntryType.inbook },
            .{ "incollection", EntryType.incollection },
            .{ "inproceedings", EntryType.inproceedings },
            .{ "manual", EntryType.manual },
            .{ "mastersthesis", EntryType.mastersthesis },
            .{ "misc", EntryType.misc },
            .{ "phdthesis", EntryType.phdthesis },
            .{ "proceedings", EntryType.proceedings },
            .{ "techreport", EntryType.techreport },
            .{ "unpublished", EntryType.unpublished },
            .{ "online", EntryType.online },
            .{ "software", EntryType.software },
            .{ "dataset", EntryType.dataset },
        };

        inline for (entry_type_map) |pair| {
            if (std.ascii.eqlIgnoreCase(str, pair[0])) {
                return pair[1];
            }
        }
        return .unknown;
    }

    /// Convert to canonical string representation.
    pub fn toString(self: EntryType) []const u8 {
        return switch (self) {
            .article => "article",
            .book => "book",
            .booklet => "booklet",
            .conference => "conference",
            .inbook => "inbook",
            .incollection => "incollection",
            .inproceedings => "inproceedings",
            .manual => "manual",
            .mastersthesis => "mastersthesis",
            .misc => "misc",
            .phdthesis => "phdthesis",
            .proceedings => "proceedings",
            .techreport => "techreport",
            .unpublished => "unpublished",
            .online => "online",
            .software => "software",
            .dataset => "dataset",
            .unknown => "unknown",
        };
    }
};

/// A single BibTeX field (key-value pair).
pub const Field = struct {
    key: []const u8,
    value: []const u8,
};

/// A parsed BibTeX entry.
pub const Entry = struct {
    entry_type: EntryType,
    cite_key: []const u8,
    fields: []Field,

    /// Get a field value by key (case-insensitive).
    pub fn getField(self: *const Entry, key: []const u8) ?[]const u8 {
        for (self.fields) |field| {
            if (std.ascii.eqlIgnoreCase(field.key, key)) {
                return field.value;
            }
        }
        return null;
    }

    /// Check if entry has a required field.
    pub fn hasField(self: *const Entry, key: []const u8) bool {
        return self.getField(key) != null;
    }

    /// Get author field.
    pub fn getAuthor(self: *const Entry) ?[]const u8 {
        return self.getField("author");
    }

    /// Get title field.
    pub fn getTitle(self: *const Entry) ?[]const u8 {
        return self.getField("title");
    }

    /// Get year field.
    pub fn getYear(self: *const Entry) ?[]const u8 {
        return self.getField("year");
    }

    /// Get journal field.
    pub fn getJournal(self: *const Entry) ?[]const u8 {
        return self.getField("journal");
    }

    /// Get DOI field.
    pub fn getDoi(self: *const Entry) ?[]const u8 {
        return self.getField("doi");
    }

    /// Get URL field.
    pub fn getUrl(self: *const Entry) ?[]const u8 {
        return self.getField("url");
    }

    /// Free allocated memory.
    pub fn deinit(self: *Entry, allocator: Allocator) void {
        allocator.free(self.cite_key);
        for (self.fields) |field| {
            allocator.free(field.key);
            allocator.free(field.value);
        }
        allocator.free(self.fields);
    }
};

/// Required fields per entry type.
pub const RequiredFields = struct {
    pub fn forType(entry_type: EntryType) []const []const u8 {
        return switch (entry_type) {
            .article => &[_][]const u8{ "author", "title", "journal", "year" },
            .book => &[_][]const u8{ "author", "title", "publisher", "year" },
            .inproceedings => &[_][]const u8{ "author", "title", "booktitle", "year" },
            .phdthesis => &[_][]const u8{ "author", "title", "school", "year" },
            .mastersthesis => &[_][]const u8{ "author", "title", "school", "year" },
            .techreport => &[_][]const u8{ "author", "title", "institution", "year" },
            .misc => &[_][]const u8{},
            else => &[_][]const u8{},
        };
    }
};

/// Check if a cite key is valid.
/// Valid keys: alphanumeric, underscore, hyphen, colon, period.
pub fn isValidCiteKey(key: []const u8) bool {
    if (key.len == 0) return false;
    if (key.len > 256) return false;

    // First character must be alphanumeric
    if (!std.ascii.isAlphanumeric(key[0])) return false;

    for (key) |char| {
        const is_valid_char = std.ascii.isAlphanumeric(char) or
            char == '_' or char == '-' or char == ':' or char == '.';
        if (!is_valid_char) return false;
    }

    return true;
}

/// Validate that an entry has all required fields for its type.
pub fn validateRequiredFields(entry: *const Entry) BibTeXError!void {
    const required = RequiredFields.forType(entry.entry_type);
    for (required) |field_name| {
        if (!entry.hasField(field_name)) {
            return error.MissingField;
        }
    }
}

/// Parse a single BibTeX entry from input string.
pub fn parseEntry(allocator: Allocator, input: []const u8) BibTeXError!Entry {
    if (input.len == 0) return error.EmptyInput;

    var cursor: usize = 0;

    // Skip leading whitespace
    while (cursor < input.len and std.ascii.isWhitespace(input[cursor])) {
        cursor += 1;
    }

    // Expect '@'
    if (cursor >= input.len or input[cursor] != '@') {
        return error.MalformedEntry;
    }
    cursor += 1;

    // Parse entry type
    const entry_type_start = cursor;
    while (cursor < input.len and std.ascii.isAlphabetic(input[cursor])) {
        cursor += 1;
    }
    if (cursor == entry_type_start) return error.InvalidEntryType;

    const entry_type_str = input[entry_type_start..cursor];
    const entry_type = EntryType.fromString(entry_type_str);

    // Skip whitespace
    while (cursor < input.len and std.ascii.isWhitespace(input[cursor])) {
        cursor += 1;
    }

    // Expect '{' or '('
    if (cursor >= input.len) return error.MalformedEntry;
    const opening_brace = input[cursor];
    if (opening_brace != '{' and opening_brace != '(') {
        return error.MalformedEntry;
    }
    const closing_brace: u8 = if (opening_brace == '{') '}' else ')';
    cursor += 1;

    // Skip whitespace
    while (cursor < input.len and std.ascii.isWhitespace(input[cursor])) {
        cursor += 1;
    }

    // Parse cite key
    const cite_key_start = cursor;
    while (cursor < input.len and input[cursor] != ',' and !std.ascii.isWhitespace(input[cursor])) {
        cursor += 1;
    }
    if (cursor == cite_key_start) return error.InvalidCiteKey;

    const cite_key_raw = input[cite_key_start..cursor];
    if (!isValidCiteKey(cite_key_raw)) return error.InvalidCiteKey;

    const cite_key = allocator.dupe(u8, cite_key_raw) catch return error.OutOfMemory;
    errdefer allocator.free(cite_key);

    // Skip to comma or end
    while (cursor < input.len and input[cursor] != ',' and input[cursor] != closing_brace) {
        cursor += 1;
    }
    if (cursor < input.len and input[cursor] == ',') {
        cursor += 1;
    }

    // Parse fields
    var fields_list = std.array_list.Managed(Field).init(allocator);
    errdefer {
        for (fields_list.items) |field| {
            allocator.free(field.key);
            allocator.free(field.value);
        }
        fields_list.deinit();
    }

    while (cursor < input.len) {
        // Skip whitespace
        while (cursor < input.len and std.ascii.isWhitespace(input[cursor])) {
            cursor += 1;
        }

        // Check for end
        if (cursor >= input.len or input[cursor] == closing_brace) {
            break;
        }

        // Parse field name
        const field_name_start = cursor;
        while (cursor < input.len and (std.ascii.isAlphanumeric(input[cursor]) or input[cursor] == '_' or input[cursor] == '-')) {
            cursor += 1;
        }
        if (cursor == field_name_start) {
            cursor += 1; // Skip invalid character
            continue;
        }

        const field_name = input[field_name_start..cursor];

        // Skip whitespace and '='
        while (cursor < input.len and (std.ascii.isWhitespace(input[cursor]) or input[cursor] == '=')) {
            cursor += 1;
        }

        // Parse field value
        const field_value = try parseFieldValue(input, &cursor);

        // Store field
        const key_copy = allocator.dupe(u8, field_name) catch return error.OutOfMemory;
        errdefer allocator.free(key_copy);
        const value_copy = allocator.dupe(u8, field_value) catch return error.OutOfMemory;

        fields_list.append(.{ .key = key_copy, .value = value_copy }) catch return error.OutOfMemory;

        // Skip comma
        while (cursor < input.len and (std.ascii.isWhitespace(input[cursor]) or input[cursor] == ',')) {
            cursor += 1;
        }
    }

    return Entry{
        .entry_type = entry_type,
        .cite_key = cite_key,
        .fields = fields_list.toOwnedSlice() catch return error.OutOfMemory,
    };
}

/// Parse a field value (handles braces, quotes, and numbers).
fn parseFieldValue(input: []const u8, cursor: *usize) BibTeXError![]const u8 {
    while (cursor.* < input.len and std.ascii.isWhitespace(input[cursor.*])) {
        cursor.* += 1;
    }

    if (cursor.* >= input.len) return error.MalformedEntry;

    const first_char = input[cursor.*];

    if (first_char == '{') {
        // Braced value
        cursor.* += 1;
        const value_start = cursor.*;
        var brace_depth: usize = 1;

        while (cursor.* < input.len and brace_depth > 0) {
            if (input[cursor.*] == '{') {
                brace_depth += 1;
            } else if (input[cursor.*] == '}') {
                brace_depth -= 1;
            }
            if (brace_depth > 0) cursor.* += 1;
        }

        if (brace_depth != 0) return error.UnterminatedBrace;

        const value = input[value_start..cursor.*];
        cursor.* += 1; // Skip closing brace
        return value;
    } else if (first_char == '"') {
        // Quoted value
        cursor.* += 1;
        const value_start = cursor.*;

        while (cursor.* < input.len and input[cursor.*] != '"') {
            if (input[cursor.*] == '\\' and cursor.* + 1 < input.len) {
                cursor.* += 2; // Skip escaped character
            } else {
                cursor.* += 1;
            }
        }

        if (cursor.* >= input.len) return error.UnterminatedString;

        const value = input[value_start..cursor.*];
        cursor.* += 1; // Skip closing quote
        return value;
    } else if (std.ascii.isDigit(first_char)) {
        // Numeric value
        const value_start = cursor.*;
        while (cursor.* < input.len and std.ascii.isDigit(input[cursor.*])) {
            cursor.* += 1;
        }
        return input[value_start..cursor.*];
    } else {
        // Bare word (string variable reference)
        const value_start = cursor.*;
        while (cursor.* < input.len and std.ascii.isAlphanumeric(input[cursor.*])) {
            cursor.* += 1;
        }
        if (cursor.* == value_start) return error.MalformedEntry;
        return input[value_start..cursor.*];
    }
}

/// Check if input appears to be a valid BibTeX entry.
pub fn isValidEntry(input: []const u8) bool {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const entry = parseEntry(arena.allocator(), input) catch return false;
    _ = entry;
    return true;
}

/// Format an entry to canonical BibTeX string.
pub fn formatEntry(allocator: Allocator, entry: *const Entry) BibTeXError![]u8 {
    var buffer = std.array_list.Managed(u8).init(allocator);
    errdefer buffer.deinit();

    // Format entry header
    buffer.appendSlice("@") catch return error.OutOfMemory;
    buffer.appendSlice(entry.entry_type.toString()) catch return error.OutOfMemory;
    buffer.appendSlice("{") catch return error.OutOfMemory;
    buffer.appendSlice(entry.cite_key) catch return error.OutOfMemory;
    buffer.appendSlice(",\n") catch return error.OutOfMemory;

    for (entry.fields, 0..) |field, index| {
        buffer.appendSlice("  ") catch return error.OutOfMemory;
        buffer.appendSlice(field.key) catch return error.OutOfMemory;
        buffer.appendSlice(" = {") catch return error.OutOfMemory;
        buffer.appendSlice(field.value) catch return error.OutOfMemory;
        buffer.appendSlice("}") catch return error.OutOfMemory;
        if (index < entry.fields.len - 1) {
            buffer.append(',') catch return error.OutOfMemory;
        }
        buffer.append('\n') catch return error.OutOfMemory;
    }

    buffer.appendSlice("}\n") catch return error.OutOfMemory;

    return buffer.toOwnedSlice() catch return error.OutOfMemory;
}

/// Escape special BibTeX characters in a string.
pub fn escapeString(allocator: Allocator, input: []const u8) BibTeXError![]u8 {
    var buffer = std.array_list.Managed(u8).init(allocator);
    errdefer buffer.deinit();

    for (input) |char| {
        switch (char) {
            '{', '}' => {
                buffer.append('\\') catch return error.OutOfMemory;
                buffer.append(char) catch return error.OutOfMemory;
            },
            '&' => buffer.appendSlice("\\&") catch return error.OutOfMemory,
            '%' => buffer.appendSlice("\\%") catch return error.OutOfMemory,
            '$' => buffer.appendSlice("\\$") catch return error.OutOfMemory,
            '#' => buffer.appendSlice("\\#") catch return error.OutOfMemory,
            '_' => buffer.appendSlice("\\_") catch return error.OutOfMemory,
            '~' => buffer.appendSlice("\\textasciitilde{}") catch return error.OutOfMemory,
            '^' => buffer.appendSlice("\\textasciicircum{}") catch return error.OutOfMemory,
            else => buffer.append(char) catch return error.OutOfMemory,
        }
    }

    return buffer.toOwnedSlice() catch return error.OutOfMemory;
}

test "EntryType.fromString" {
    try std.testing.expectEqual(EntryType.article, EntryType.fromString("article"));
    try std.testing.expectEqual(EntryType.article, EntryType.fromString("ARTICLE"));
    try std.testing.expectEqual(EntryType.article, EntryType.fromString("Article"));
    try std.testing.expectEqual(EntryType.book, EntryType.fromString("book"));
    try std.testing.expectEqual(EntryType.unknown, EntryType.fromString("invalid"));
}

test "isValidCiteKey" {
    try std.testing.expect(isValidCiteKey("knuth1984"));
    try std.testing.expect(isValidCiteKey("Knuth_1984"));
    try std.testing.expect(isValidCiteKey("author:2024"));
    try std.testing.expect(isValidCiteKey("cite-key.1"));
    try std.testing.expect(!isValidCiteKey(""));
    try std.testing.expect(!isValidCiteKey("invalid key"));
    try std.testing.expect(!isValidCiteKey("_invalid"));
}

test "parseEntry basic" {
    const allocator = std.testing.allocator;
    const input =
        \\@article{knuth1984,
        \\  author = {Donald E. Knuth},
        \\  title = {Literate Programming},
        \\  journal = {The Computer Journal},
        \\  year = 1984
        \\}
    ;

    var entry = try parseEntry(allocator, input);
    defer entry.deinit(allocator);

    try std.testing.expectEqual(EntryType.article, entry.entry_type);
    try std.testing.expectEqualStrings("knuth1984", entry.cite_key);
    try std.testing.expectEqualStrings("Donald E. Knuth", entry.getAuthor().?);
    try std.testing.expectEqualStrings("Literate Programming", entry.getTitle().?);
    try std.testing.expectEqualStrings("1984", entry.getYear().?);
}

test "parseEntry with quotes" {
    const allocator = std.testing.allocator;
    const input =
        \\@book{test2020,
        \\  author = "John Doe",
        \\  title = "Test Book"
        \\}
    ;

    var entry = try parseEntry(allocator, input);
    defer entry.deinit(allocator);

    try std.testing.expectEqual(EntryType.book, entry.entry_type);
    try std.testing.expectEqualStrings("John Doe", entry.getAuthor().?);
}

test "isValidEntry" {
    try std.testing.expect(isValidEntry("@article{key, author={Test}}"));
    try std.testing.expect(!isValidEntry("not bibtex"));
    try std.testing.expect(!isValidEntry("@article{"));
}

test "escapeString" {
    const allocator = std.testing.allocator;
    const escaped = try escapeString(allocator, "10% & 20$");
    defer allocator.free(escaped);
    try std.testing.expectEqualStrings("10\\% \\& 20\\$", escaped);
}
