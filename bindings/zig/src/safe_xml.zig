// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe XML escaping and validation that cannot crash.
//!
//! Provides secure XML handling including entity escaping, attribute value
//! encoding, and validation of XML names and content. All operations are
//! designed to prevent XML injection attacks and produce well-formed output.

const std = @import("std");

/// Error types for XML operations.
pub const XmlError = error{
    /// Invalid XML character encountered.
    InvalidCharacter,
    /// Invalid XML name (element or attribute name).
    InvalidName,
    /// Malformed XML structure.
    MalformedXml,
    /// Maximum depth exceeded.
    MaxDepthExceeded,
    /// Invalid entity reference.
    InvalidEntity,
    /// Unclosed tag or element.
    UnclosedElement,
    /// Out of memory.
    OutOfMemory,
};

/// XML escaping mode.
pub const EscapeMode = enum {
    /// Escape for text content (inside elements).
    text,
    /// Escape for attribute values (inside quotes).
    attribute,
    /// Escape for CDATA sections.
    cdata,
};

/// Escape a string for safe inclusion in XML text content.
/// Escapes: < > & ' "
pub fn escapeText(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    return escapeWithMode(allocator, input, .text);
}

/// Escape a string for safe inclusion in XML attribute values.
/// Escapes: < > & ' " and also control characters.
pub fn escapeAttribute(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    return escapeWithMode(allocator, input, .attribute);
}

/// Escape a string with the specified mode.
pub fn escapeWithMode(allocator: std.mem.Allocator, input: []const u8, escape_mode: EscapeMode) ![]u8 {
    var output_list = std.ArrayList(u8).init(allocator);
    errdefer output_list.deinit();

    for (input) |character| {
        switch (character) {
            '<' => try output_list.appendSlice("&lt;"),
            '>' => try output_list.appendSlice("&gt;"),
            '&' => try output_list.appendSlice("&amp;"),
            '\'' => try output_list.appendSlice("&apos;"),
            '"' => try output_list.appendSlice("&quot;"),
            else => {
                if (escape_mode == .attribute and character < 0x20 and character != '\t' and character != '\n' and character != '\r') {
                    // Escape control characters in attributes as numeric entities
                    try output_list.writer().print("&#x{X:0>2};", .{character});
                } else if (escape_mode == .cdata and character == ']') {
                    // Check for CDATA end sequence
                    try output_list.append(character);
                } else {
                    try output_list.append(character);
                }
            },
        }
    }

    return output_list.toOwnedSlice();
}

/// Unescape XML entities in a string.
pub fn unescape(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var output_list = std.ArrayList(u8).init(allocator);
    errdefer output_list.deinit();

    var read_index: usize = 0;
    while (read_index < input.len) {
        if (input[read_index] == '&') {
            // Find end of entity
            const entity_end = std.mem.indexOfScalarPos(u8, input, read_index, ';') orelse {
                try output_list.append(input[read_index]);
                read_index += 1;
                continue;
            };

            const entity_name = input[read_index + 1 .. entity_end];

            if (std.mem.eql(u8, entity_name, "lt")) {
                try output_list.append('<');
            } else if (std.mem.eql(u8, entity_name, "gt")) {
                try output_list.append('>');
            } else if (std.mem.eql(u8, entity_name, "amp")) {
                try output_list.append('&');
            } else if (std.mem.eql(u8, entity_name, "apos")) {
                try output_list.append('\'');
            } else if (std.mem.eql(u8, entity_name, "quot")) {
                try output_list.append('"');
            } else if (entity_name.len > 1 and entity_name[0] == '#') {
                // Numeric entity
                const numeric_value = if (entity_name[1] == 'x' or entity_name[1] == 'X')
                    std.fmt.parseInt(u21, entity_name[2..], 16) catch {
                        try output_list.appendSlice(input[read_index .. entity_end + 1]);
                        read_index = entity_end + 1;
                        continue;
                    }
                else
                    std.fmt.parseInt(u21, entity_name[1..], 10) catch {
                        try output_list.appendSlice(input[read_index .. entity_end + 1]);
                        read_index = entity_end + 1;
                        continue;
                    };

                // Encode as UTF-8
                var utf8_buffer: [4]u8 = undefined;
                const utf8_length = std.unicode.utf8Encode(numeric_value, &utf8_buffer) catch {
                    try output_list.appendSlice(input[read_index .. entity_end + 1]);
                    read_index = entity_end + 1;
                    continue;
                };
                try output_list.appendSlice(utf8_buffer[0..utf8_length]);
            } else {
                // Unknown entity, keep as-is
                try output_list.appendSlice(input[read_index .. entity_end + 1]);
            }

            read_index = entity_end + 1;
        } else {
            try output_list.append(input[read_index]);
            read_index += 1;
        }
    }

    return output_list.toOwnedSlice();
}

/// Check if a character is valid in XML content (XML 1.0).
pub fn isValidXmlChar(codepoint: u21) bool {
    return switch (codepoint) {
        0x9, 0xA, 0xD => true,
        0x20...0xD7FF => true,
        0xE000...0xFFFD => true,
        0x10000...0x10FFFF => true,
        else => false,
    };
}

/// Check if a character is valid as the start of an XML name.
pub fn isValidNameStartChar(codepoint: u21) bool {
    return switch (codepoint) {
        ':' => true,
        'A'...'Z' => true,
        '_' => true,
        'a'...'z' => true,
        0xC0...0xD6 => true,
        0xD8...0xF6 => true,
        0xF8...0x2FF => true,
        0x370...0x37D => true,
        0x37F...0x1FFF => true,
        0x200C...0x200D => true,
        0x2070...0x218F => true,
        0x2C00...0x2FEF => true,
        0x3001...0xD7FF => true,
        0xF900...0xFDCF => true,
        0xFDF0...0xFFFD => true,
        0x10000...0xEFFFF => true,
        else => false,
    };
}

/// Check if a character is valid within an XML name (after the first character).
pub fn isValidNameChar(codepoint: u21) bool {
    if (isValidNameStartChar(codepoint)) return true;
    return switch (codepoint) {
        '-' => true,
        '.' => true,
        '0'...'9' => true,
        0xB7 => true,
        0x0300...0x036F => true,
        0x203F...0x2040 => true,
        else => false,
    };
}

/// Validate an XML name (element name or attribute name).
pub fn isValidName(name: []const u8) bool {
    if (name.len == 0) return false;

    var iterator = std.unicode.Utf8Iterator{ .bytes = name, .i = 0 };

    // Check first character
    const first_codepoint = iterator.nextCodepoint() orelse return false;
    if (!isValidNameStartChar(first_codepoint)) return false;

    // Check remaining characters
    while (iterator.nextCodepoint()) |codepoint| {
        if (!isValidNameChar(codepoint)) return false;
    }

    return true;
}

/// Validate that a string contains only valid XML characters.
pub fn isValidContent(content: []const u8) bool {
    var iterator = std.unicode.Utf8Iterator{ .bytes = content, .i = 0 };
    while (iterator.nextCodepoint()) |codepoint| {
        if (!isValidXmlChar(codepoint)) return false;
    }
    return true;
}

/// Remove invalid XML characters from a string.
pub fn sanitizeContent(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var output_list = std.ArrayList(u8).init(allocator);
    errdefer output_list.deinit();

    var iterator = std.unicode.Utf8Iterator{ .bytes = input, .i = 0 };
    var previous_index: usize = 0;

    while (iterator.nextCodepoint()) |codepoint| {
        if (isValidXmlChar(codepoint)) {
            const current_index = iterator.i;
            try output_list.appendSlice(input[previous_index..current_index]);
        }
        previous_index = iterator.i;
    }

    return output_list.toOwnedSlice();
}

/// Create a safe XML element string.
pub fn createElement(
    allocator: std.mem.Allocator,
    tag_name: []const u8,
    content: ?[]const u8,
    attributes: ?[]const struct { name: []const u8, value: []const u8 },
) ![]u8 {
    if (!isValidName(tag_name)) return error.InvalidName;

    var output_list = std.ArrayList(u8).init(allocator);
    errdefer output_list.deinit();

    // Opening tag
    try output_list.append('<');
    try output_list.appendSlice(tag_name);

    // Attributes
    if (attributes) |attribute_list| {
        for (attribute_list) |attribute| {
            if (!isValidName(attribute.name)) return error.InvalidName;
            try output_list.append(' ');
            try output_list.appendSlice(attribute.name);
            try output_list.appendSlice("=\"");
            const escaped_value = try escapeAttribute(allocator, attribute.value);
            defer allocator.free(escaped_value);
            try output_list.appendSlice(escaped_value);
            try output_list.append('"');
        }
    }

    // Content or self-closing
    if (content) |inner_content| {
        try output_list.append('>');
        const escaped_content = try escapeText(allocator, inner_content);
        defer allocator.free(escaped_content);
        try output_list.appendSlice(escaped_content);
        try output_list.appendSlice("</");
        try output_list.appendSlice(tag_name);
        try output_list.append('>');
    } else {
        try output_list.appendSlice("/>");
    }

    return output_list.toOwnedSlice();
}

/// Create a CDATA section.
pub fn createCDATA(allocator: std.mem.Allocator, content: []const u8) ![]u8 {
    // CDATA cannot contain "]]>"
    if (std.mem.indexOf(u8, content, "]]>") != null) {
        return error.InvalidCharacter;
    }

    var output_list = std.ArrayList(u8).init(allocator);
    errdefer output_list.deinit();

    try output_list.appendSlice("<![CDATA[");
    try output_list.appendSlice(content);
    try output_list.appendSlice("]]>");

    return output_list.toOwnedSlice();
}

/// Create an XML comment.
pub fn createComment(allocator: std.mem.Allocator, content: []const u8) ![]u8 {
    // Comments cannot contain "--" or end with "-"
    if (std.mem.indexOf(u8, content, "--") != null) {
        return error.InvalidCharacter;
    }
    if (content.len > 0 and content[content.len - 1] == '-') {
        return error.InvalidCharacter;
    }

    var output_list = std.ArrayList(u8).init(allocator);
    errdefer output_list.deinit();

    try output_list.appendSlice("<!-- ");
    try output_list.appendSlice(content);
    try output_list.appendSlice(" -->");

    return output_list.toOwnedSlice();
}

/// Check if a string looks like well-formed XML (basic validation).
pub fn isWellFormed(input: []const u8) bool {
    if (input.len == 0) return false;

    // Find first non-whitespace
    var start_position: usize = 0;
    while (start_position < input.len and std.ascii.isWhitespace(input[start_position])) {
        start_position += 1;
    }

    if (start_position >= input.len) return false;

    // Check for XML declaration or root element
    if (input[start_position] != '<') return false;

    // Basic tag matching
    var depth: usize = 0;
    var read_index = start_position;

    while (read_index < input.len) {
        if (input[read_index] == '<') {
            if (read_index + 1 >= input.len) return false;

            if (input[read_index + 1] == '/') {
                // Closing tag
                if (depth == 0) return false;
                depth -= 1;
            } else if (input[read_index + 1] == '?' or input[read_index + 1] == '!') {
                // Declaration, comment, or CDATA - skip
            } else {
                // Opening tag - check for self-closing
                const tag_end = std.mem.indexOfScalarPos(u8, input, read_index, '>') orelse return false;
                if (input[tag_end - 1] != '/') {
                    depth += 1;
                }
            }
        }
        read_index += 1;
    }

    return depth == 0;
}

test "escapeText" {
    const allocator = std.testing.allocator;
    const escaped = try escapeText(allocator, "<script>alert('xss')</script>");
    defer allocator.free(escaped);
    try std.testing.expectEqualStrings("&lt;script&gt;alert(&apos;xss&apos;)&lt;/script&gt;", escaped);
}

test "escapeAttribute" {
    const allocator = std.testing.allocator;
    const escaped = try escapeAttribute(allocator, "value with \"quotes\" and <brackets>");
    defer allocator.free(escaped);
    try std.testing.expectEqualStrings("value with &quot;quotes&quot; and &lt;brackets&gt;", escaped);
}

test "unescape" {
    const allocator = std.testing.allocator;
    const unescaped = try unescape(allocator, "&lt;tag&gt;content&lt;/tag&gt;");
    defer allocator.free(unescaped);
    try std.testing.expectEqualStrings("<tag>content</tag>", unescaped);
}

test "unescape numeric entities" {
    const allocator = std.testing.allocator;
    const unescaped = try unescape(allocator, "&#65;&#x42;");
    defer allocator.free(unescaped);
    try std.testing.expectEqualStrings("AB", unescaped);
}

test "isValidName" {
    try std.testing.expect(isValidName("element"));
    try std.testing.expect(isValidName("_private"));
    try std.testing.expect(isValidName("ns:element"));
    try std.testing.expect(isValidName("element123"));
    try std.testing.expect(!isValidName("123element"));
    try std.testing.expect(!isValidName("-invalid"));
    try std.testing.expect(!isValidName(""));
}

test "isValidContent" {
    try std.testing.expect(isValidContent("Hello, World!"));
    try std.testing.expect(isValidContent("Line1\nLine2"));
    try std.testing.expect(!isValidContent("Invalid\x00null"));
}

test "createElement" {
    const allocator = std.testing.allocator;

    // Simple element
    const simple = try createElement(allocator, "div", "content", null);
    defer allocator.free(simple);
    try std.testing.expectEqualStrings("<div>content</div>", simple);

    // Self-closing
    const selfclosing = try createElement(allocator, "br", null, null);
    defer allocator.free(selfclosing);
    try std.testing.expectEqualStrings("<br/>", selfclosing);

    // With attributes
    const attrs = [_]struct { name: []const u8, value: []const u8 }{
        .{ .name = "class", .value = "test" },
        .{ .name = "id", .value = "main" },
    };
    const with_attrs = try createElement(allocator, "div", "text", &attrs);
    defer allocator.free(with_attrs);
    try std.testing.expectEqualStrings("<div class=\"test\" id=\"main\">text</div>", with_attrs);
}

test "createCDATA" {
    const allocator = std.testing.allocator;
    const cdata = try createCDATA(allocator, "raw <content> & stuff");
    defer allocator.free(cdata);
    try std.testing.expectEqualStrings("<![CDATA[raw <content> & stuff]]>", cdata);
}

test "createCDATA rejects invalid content" {
    const allocator = std.testing.allocator;
    try std.testing.expectError(error.InvalidCharacter, createCDATA(allocator, "contains ]]> sequence"));
}

test "createComment" {
    const allocator = std.testing.allocator;
    const comment = try createComment(allocator, "This is a comment");
    defer allocator.free(comment);
    try std.testing.expectEqualStrings("<!-- This is a comment -->", comment);
}

test "createComment rejects invalid content" {
    const allocator = std.testing.allocator;
    try std.testing.expectError(error.InvalidCharacter, createComment(allocator, "contains -- sequence"));
    try std.testing.expectError(error.InvalidCharacter, createComment(allocator, "ends with dash-"));
}

test "isWellFormed" {
    try std.testing.expect(isWellFormed("<root></root>"));
    try std.testing.expect(isWellFormed("<root><child/></root>"));
    try std.testing.expect(isWellFormed("<?xml version=\"1.0\"?><root/>"));
    try std.testing.expect(!isWellFormed("<root>"));
    try std.testing.expect(!isWellFormed("not xml"));
}
