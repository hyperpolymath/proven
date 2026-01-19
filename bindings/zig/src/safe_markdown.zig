// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe Markdown escaping and validation that cannot crash.
//!
//! Provides utilities for escaping special Markdown characters,
//! sanitizing user input for safe Markdown rendering, and validating
//! Markdown structure. All operations are memory-safe.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for Markdown operations.
pub const MarkdownError = error{
    InvalidUtf8,
    NestingTooDeep,
    UnclosedElement,
    InvalidLinkUrl,
    InvalidImageUrl,
    OutOfMemory,
};

/// Maximum nesting depth for elements like lists and blockquotes.
pub const MAX_NESTING_DEPTH = 16;

/// Markdown special characters that may need escaping.
pub const SpecialChars = struct {
    pub const BACKSLASH = '\\';
    pub const BACKTICK = '`';
    pub const ASTERISK = '*';
    pub const UNDERSCORE = '_';
    pub const HASH = '#';
    pub const PLUS = '+';
    pub const MINUS = '-';
    pub const DOT = '.';
    pub const EXCLAMATION = '!';
    pub const OPEN_BRACKET = '[';
    pub const CLOSE_BRACKET = ']';
    pub const OPEN_PAREN = '(';
    pub const CLOSE_PAREN = ')';
    pub const OPEN_ANGLE = '<';
    pub const CLOSE_ANGLE = '>';
    pub const PIPE = '|';
    pub const TILDE = '~';
};

/// Characters that need escaping in inline text.
const INLINE_ESCAPE_CHARS = [_]u8{
    SpecialChars.BACKSLASH,
    SpecialChars.BACKTICK,
    SpecialChars.ASTERISK,
    SpecialChars.UNDERSCORE,
    SpecialChars.OPEN_BRACKET,
    SpecialChars.CLOSE_BRACKET,
    SpecialChars.OPEN_PAREN,
    SpecialChars.CLOSE_PAREN,
    SpecialChars.OPEN_ANGLE,
    SpecialChars.CLOSE_ANGLE,
    SpecialChars.PIPE,
    SpecialChars.TILDE,
};

/// Characters that need escaping at the start of a line.
const LINE_START_ESCAPE_CHARS = [_]u8{
    SpecialChars.HASH,
    SpecialChars.PLUS,
    SpecialChars.MINUS,
    SpecialChars.DOT,
    SpecialChars.EXCLAMATION,
};

/// Escape special characters in inline Markdown text.
/// This prevents user input from being interpreted as Markdown formatting.
pub fn escapeInline(allocator: Allocator, input: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (input) |char| {
        var should_escape = false;
        for (INLINE_ESCAPE_CHARS) |escape_char| {
            if (char == escape_char) {
                should_escape = true;
                break;
            }
        }

        if (should_escape) {
            try result.append('\\');
        }
        try result.append(char);
    }

    return result.toOwnedSlice();
}

/// Escape special characters at the start of lines.
/// This prevents user input from creating headers, lists, etc.
pub fn escapeLineStart(allocator: Allocator, input: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    var at_line_start = true;

    for (input) |char| {
        if (at_line_start) {
            // Check if this character needs escaping at line start
            var should_escape = false;

            // Check for list markers (digits followed by dot)
            if (char >= '0' and char <= '9') {
                // Don't escape numbers, they're handled with the dot
                at_line_start = false;
                try result.append(char);
                continue;
            }

            for (LINE_START_ESCAPE_CHARS) |escape_char| {
                if (char == escape_char) {
                    should_escape = true;
                    break;
                }
            }

            if (should_escape) {
                try result.append('\\');
            }
        }

        try result.append(char);
        at_line_start = (char == '\n');
    }

    return result.toOwnedSlice();
}

/// Fully escape text for safe inclusion in Markdown.
/// Combines inline and line-start escaping.
pub fn escapeText(allocator: Allocator, input: []const u8) ![]u8 {
    const inline_escaped = try escapeInline(allocator, input);
    defer allocator.free(inline_escaped);

    return escapeLineStart(allocator, inline_escaped);
}

/// Escape text for use in a Markdown link title.
pub fn escapeLinkTitle(allocator: Allocator, title: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (title) |char| {
        switch (char) {
            '"' => try result.appendSlice("\\\""),
            '\\' => try result.appendSlice("\\\\"),
            '\n' => try result.appendSlice(" "),
            else => try result.append(char),
        }
    }

    return result.toOwnedSlice();
}

/// Escape text for use in a code span (backticks).
pub fn escapeCodeSpan(allocator: Allocator, code: []const u8) ![]u8 {
    // Count maximum consecutive backticks in the input
    var max_backticks: usize = 0;
    var current_backticks: usize = 0;

    for (code) |char| {
        if (char == '`') {
            current_backticks += 1;
            if (current_backticks > max_backticks) {
                max_backticks = current_backticks;
            }
        } else {
            current_backticks = 0;
        }
    }

    // Use one more backtick than the maximum found
    const fence_count = max_backticks + 1;

    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    // Opening fence
    for (0..fence_count) |_| {
        try result.append('`');
    }

    // Space if code starts with backtick
    if (code.len > 0 and code[0] == '`') {
        try result.append(' ');
    }

    try result.appendSlice(code);

    // Space if code ends with backtick
    if (code.len > 0 and code[code.len - 1] == '`') {
        try result.append(' ');
    }

    // Closing fence
    for (0..fence_count) |_| {
        try result.append('`');
    }

    return result.toOwnedSlice();
}

/// Check if a URL is safe for use in Markdown links.
pub fn isValidLinkUrl(url: []const u8) bool {
    if (url.len == 0) return false;

    // Check for dangerous protocols
    const dangerous_protocols = [_][]const u8{
        "javascript:",
        "vbscript:",
        "data:",
        "file:",
    };

    var lower_url: [256]u8 = undefined;
    const check_len = @min(url.len, lower_url.len);

    for (url[0..check_len], 0..) |char, index| {
        lower_url[index] = std.ascii.toLower(char);
    }

    for (dangerous_protocols) |protocol| {
        if (check_len >= protocol.len and std.mem.startsWith(u8, lower_url[0..check_len], protocol)) {
            return false;
        }
    }

    // Check for control characters
    for (url) |char| {
        if (char < 0x20 or char == 0x7f) {
            return false;
        }
    }

    return true;
}

/// Check if a URL is safe for use in Markdown images.
pub fn isValidImageUrl(url: []const u8) bool {
    // Images have the same restrictions as links
    return isValidLinkUrl(url);
}

/// Sanitize a URL for safe use in Markdown.
pub fn sanitizeUrl(allocator: Allocator, url: []const u8) ![]u8 {
    if (!isValidLinkUrl(url)) {
        // Return empty string for invalid URLs
        return try allocator.dupe(u8, "");
    }

    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (url) |char| {
        switch (char) {
            // Escape parentheses
            '(' => try result.appendSlice("%28"),
            ')' => try result.appendSlice("%29"),
            // Escape spaces
            ' ' => try result.appendSlice("%20"),
            // Pass through safe characters
            else => try result.append(char),
        }
    }

    return result.toOwnedSlice();
}

/// Create a safe Markdown link.
pub fn createLink(allocator: Allocator, text: []const u8, url: []const u8, title: ?[]const u8) ![]u8 {
    if (!isValidLinkUrl(url)) {
        return error.InvalidLinkUrl;
    }

    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    try result.append('[');

    // Escape link text
    const escaped_text = try escapeInline(allocator, text);
    defer allocator.free(escaped_text);
    try result.appendSlice(escaped_text);

    try result.appendSlice("](");

    // Sanitize URL
    const sanitized_url = try sanitizeUrl(allocator, url);
    defer allocator.free(sanitized_url);
    try result.appendSlice(sanitized_url);

    // Add title if present
    if (title) |t| {
        try result.appendSlice(" \"");
        const escaped_title = try escapeLinkTitle(allocator, t);
        defer allocator.free(escaped_title);
        try result.appendSlice(escaped_title);
        try result.append('"');
    }

    try result.append(')');

    return result.toOwnedSlice();
}

/// Create a safe Markdown image.
pub fn createImage(allocator: Allocator, alt_text: []const u8, url: []const u8, title: ?[]const u8) ![]u8 {
    if (!isValidImageUrl(url)) {
        return error.InvalidImageUrl;
    }

    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    try result.appendSlice("![");

    // Escape alt text
    const escaped_alt = try escapeInline(allocator, alt_text);
    defer allocator.free(escaped_alt);
    try result.appendSlice(escaped_alt);

    try result.appendSlice("](");

    // Sanitize URL
    const sanitized_url = try sanitizeUrl(allocator, url);
    defer allocator.free(sanitized_url);
    try result.appendSlice(sanitized_url);

    // Add title if present
    if (title) |t| {
        try result.appendSlice(" \"");
        const escaped_title = try escapeLinkTitle(allocator, t);
        defer allocator.free(escaped_title);
        try result.appendSlice(escaped_title);
        try result.append('"');
    }

    try result.append(')');

    return result.toOwnedSlice();
}

/// Strip all Markdown formatting from text.
pub fn stripFormatting(allocator: Allocator, input: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    var index: usize = 0;
    while (index < input.len) {
        const char = input[index];

        // Handle escape sequences
        if (char == '\\' and index + 1 < input.len) {
            try result.append(input[index + 1]);
            index += 2;
            continue;
        }

        // Skip formatting characters
        const skip = switch (char) {
            '*', '_', '~', '`' => true,
            else => false,
        };

        if (!skip) {
            try result.append(char);
        }

        index += 1;
    }

    return result.toOwnedSlice();
}

/// Check if text contains any Markdown formatting.
pub fn hasFormatting(input: []const u8) bool {
    var index: usize = 0;
    while (index < input.len) {
        const char = input[index];

        // Skip escape sequences
        if (char == '\\' and index + 1 < input.len) {
            index += 2;
            continue;
        }

        // Check for formatting characters
        switch (char) {
            '*', '_', '~', '`', '#', '[', ']', '!', '|' => return true,
            else => {},
        }

        index += 1;
    }

    return false;
}

/// Validate UTF-8 encoding of Markdown text.
pub fn isValidUtf8(input: []const u8) bool {
    return std.unicode.utf8ValidateSlice(input);
}

/// Element types for block-level Markdown.
pub const BlockType = enum {
    paragraph,
    heading,
    code_block,
    blockquote,
    unordered_list,
    ordered_list,
    horizontal_rule,
    table,
};

/// Detect the type of a Markdown block from its first line.
pub fn detectBlockType(line: []const u8) BlockType {
    const trimmed = std.mem.trimStart(u8, line, " \t");

    if (trimmed.len == 0) return .paragraph;

    // Heading
    if (trimmed[0] == '#') return .heading;

    // Code block (fenced)
    if (trimmed.len >= 3 and std.mem.startsWith(u8, trimmed, "```")) return .code_block;
    if (trimmed.len >= 3 and std.mem.startsWith(u8, trimmed, "~~~")) return .code_block;

    // Blockquote
    if (trimmed[0] == '>') return .blockquote;

    // Unordered list
    if (trimmed.len >= 2 and (trimmed[0] == '-' or trimmed[0] == '*' or trimmed[0] == '+') and trimmed[1] == ' ') {
        return .unordered_list;
    }

    // Ordered list (digit followed by dot and space)
    if (trimmed.len >= 3 and trimmed[0] >= '0' and trimmed[0] <= '9') {
        var dot_index: usize = 1;
        while (dot_index < trimmed.len and trimmed[dot_index] >= '0' and trimmed[dot_index] <= '9') {
            dot_index += 1;
        }
        if (dot_index < trimmed.len - 1 and trimmed[dot_index] == '.' and trimmed[dot_index + 1] == ' ') {
            return .ordered_list;
        }
    }

    // Horizontal rule
    if (trimmed.len >= 3) {
        var hr_char: u8 = 0;
        var hr_count: usize = 0;
        var valid_hr = true;

        for (trimmed) |char| {
            if (char == ' ') continue;
            if (hr_char == 0 and (char == '-' or char == '*' or char == '_')) {
                hr_char = char;
                hr_count = 1;
            } else if (char == hr_char) {
                hr_count += 1;
            } else {
                valid_hr = false;
                break;
            }
        }

        if (valid_hr and hr_count >= 3) return .horizontal_rule;
    }

    // Table (starts with |)
    if (trimmed[0] == '|') return .table;

    return .paragraph;
}

test "escapeInline" {
    const allocator = std.testing.allocator;

    const result = try escapeInline(allocator, "Hello *world* [test]");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("Hello \\*world\\* \\[test\\]", result);
}

test "escapeLineStart" {
    const allocator = std.testing.allocator;

    const result = try escapeLineStart(allocator, "# Heading\n- List item");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("\\# Heading\n\\- List item", result);
}

test "escapeCodeSpan" {
    const allocator = std.testing.allocator;

    // Simple code
    const simple = try escapeCodeSpan(allocator, "code");
    defer allocator.free(simple);
    try std.testing.expectEqualStrings("`code`", simple);

    // Code with backtick
    const with_backtick = try escapeCodeSpan(allocator, "code`with`backticks");
    defer allocator.free(with_backtick);
    try std.testing.expectEqualStrings("``code`with`backticks``", with_backtick);
}

test "isValidLinkUrl" {
    try std.testing.expect(isValidLinkUrl("https://example.com"));
    try std.testing.expect(isValidLinkUrl("http://example.com/path"));
    try std.testing.expect(isValidLinkUrl("/relative/path"));
    try std.testing.expect(!isValidLinkUrl("javascript:alert(1)"));
    try std.testing.expect(!isValidLinkUrl("data:text/html,<script>"));
    try std.testing.expect(!isValidLinkUrl(""));
}

test "createLink" {
    const allocator = std.testing.allocator;

    const link = try createLink(allocator, "Click here", "https://example.com", null);
    defer allocator.free(link);
    try std.testing.expectEqualStrings("[Click here](https://example.com)", link);

    const link_with_title = try createLink(allocator, "Test", "https://example.com", "Title");
    defer allocator.free(link_with_title);
    try std.testing.expectEqualStrings("[Test](https://example.com \"Title\")", link_with_title);
}

test "createImage" {
    const allocator = std.testing.allocator;

    const image = try createImage(allocator, "Alt text", "https://example.com/image.png", null);
    defer allocator.free(image);
    try std.testing.expectEqualStrings("![Alt text](https://example.com/image.png)", image);
}

test "stripFormatting" {
    const allocator = std.testing.allocator;

    const stripped = try stripFormatting(allocator, "Hello **world** and *text*");
    defer allocator.free(stripped);
    try std.testing.expectEqualStrings("Hello world and text", stripped);
}

test "hasFormatting" {
    try std.testing.expect(hasFormatting("Hello **world**"));
    try std.testing.expect(hasFormatting("# Heading"));
    try std.testing.expect(hasFormatting("[link](url)"));
    try std.testing.expect(!hasFormatting("Plain text"));
    try std.testing.expect(!hasFormatting("Escaped \\*text\\*"));
}

test "detectBlockType" {
    try std.testing.expectEqual(BlockType.heading, detectBlockType("# Heading"));
    try std.testing.expectEqual(BlockType.code_block, detectBlockType("```rust"));
    try std.testing.expectEqual(BlockType.blockquote, detectBlockType("> Quote"));
    try std.testing.expectEqual(BlockType.unordered_list, detectBlockType("- Item"));
    try std.testing.expectEqual(BlockType.ordered_list, detectBlockType("1. Item"));
    try std.testing.expectEqual(BlockType.horizontal_rule, detectBlockType("---"));
    try std.testing.expectEqual(BlockType.table, detectBlockType("| A | B |"));
    try std.testing.expectEqual(BlockType.paragraph, detectBlockType("Normal text"));
}

test "isValidUtf8" {
    try std.testing.expect(isValidUtf8("Hello, World!"));
    try std.testing.expect(isValidUtf8("Unicode: \u{1F600}"));
    try std.testing.expect(!isValidUtf8(&[_]u8{ 0xFF, 0xFE }));
}
