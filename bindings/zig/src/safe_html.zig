// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe HTML escaping and sanitization operations.
//!
//! Provides functions for escaping user input before insertion into HTML contexts,
//! and for sanitizing HTML by removing or neutralizing potentially dangerous elements.
//! All operations are designed to fail safely without panics or undefined behavior.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// HTML-related error types
pub const HtmlError = error{
    /// Buffer is too small for the operation
    BufferTooSmall,
    /// Memory allocation failed
    OutOfMemory,
    /// Invalid UTF-8 sequence
    InvalidUtf8,
    /// Malformed HTML structure
    MalformedHtml,
};

/// HTML escaping context determines which characters to escape
pub const EscapeContext = enum {
    /// Content between tags (default, escapes < > & " ')
    text_content,
    /// Inside a single-quoted attribute (escapes ' & < >)
    single_quoted_attr,
    /// Inside a double-quoted attribute (escapes " & < >)
    double_quoted_attr,
    /// Inside unquoted attribute (escapes space, tab, & < > " ' ` = )
    unquoted_attr,
};

/// Escape a string for safe insertion as HTML text content.
/// Escapes: & < > " '
pub fn escapeText(input: []const u8, buffer: []u8) HtmlError![]const u8 {
    return escapeForContext(input, buffer, .text_content);
}

/// Escape a string for safe insertion into an HTML attribute.
/// Automatically handles both single and double quoted contexts.
pub fn escapeAttribute(input: []const u8, buffer: []u8) HtmlError![]const u8 {
    return escapeForContext(input, buffer, .double_quoted_attr);
}

/// Escape a string for a specific HTML context.
pub fn escapeForContext(input: []const u8, buffer: []u8, context: EscapeContext) HtmlError![]const u8 {
    var write_index: usize = 0;

    for (input) |char| {
        const replacement: ?[]const u8 = switch (context) {
            .text_content => switch (char) {
                '&' => "&amp;",
                '<' => "&lt;",
                '>' => "&gt;",
                '"' => "&quot;",
                '\'' => "&#x27;",
                else => null,
            },
            .single_quoted_attr => switch (char) {
                '&' => "&amp;",
                '<' => "&lt;",
                '>' => "&gt;",
                '\'' => "&#x27;",
                else => null,
            },
            .double_quoted_attr => switch (char) {
                '&' => "&amp;",
                '<' => "&lt;",
                '>' => "&gt;",
                '"' => "&quot;",
                else => null,
            },
            .unquoted_attr => switch (char) {
                '&' => "&amp;",
                '<' => "&lt;",
                '>' => "&gt;",
                '"' => "&quot;",
                '\'' => "&#x27;",
                '`' => "&#x60;",
                '=' => "&#x3D;",
                ' ', '\t', '\n', '\r' => "&#x20;",
                else => null,
            },
        };

        if (replacement) |repl| {
            if (write_index + repl.len > buffer.len) return error.BufferTooSmall;
            @memcpy(buffer[write_index..][0..repl.len], repl);
            write_index += repl.len;
        } else {
            if (write_index >= buffer.len) return error.BufferTooSmall;
            buffer[write_index] = char;
            write_index += 1;
        }
    }

    return buffer[0..write_index];
}

/// Escape a string with allocation.
pub fn escapeTextAlloc(allocator: Allocator, input: []const u8) HtmlError![]u8 {
    // Worst case: every char becomes entity (max 6 chars for &#x27;)
    const max_size = input.len * 6;
    const buffer = allocator.alloc(u8, max_size) catch return error.OutOfMemory;
    errdefer allocator.free(buffer);

    const result = try escapeText(input, buffer);
    const final = allocator.realloc(buffer, result.len) catch return error.OutOfMemory;
    return final;
}

/// Escape special characters in a URL for use in href/src attributes.
/// This escapes HTML entities but preserves URL structure.
pub fn escapeUrl(input: []const u8, buffer: []u8) HtmlError![]const u8 {
    var write_index: usize = 0;

    for (input) |char| {
        const replacement: ?[]const u8 = switch (char) {
            '&' => "&amp;",
            '"' => "&quot;",
            '\'' => "&#x27;",
            '<' => "&lt;",
            '>' => "&gt;",
            else => null,
        };

        if (replacement) |repl| {
            if (write_index + repl.len > buffer.len) return error.BufferTooSmall;
            @memcpy(buffer[write_index..][0..repl.len], repl);
            write_index += repl.len;
        } else {
            if (write_index >= buffer.len) return error.BufferTooSmall;
            buffer[write_index] = char;
            write_index += 1;
        }
    }

    return buffer[0..write_index];
}

/// Check if a URL scheme is considered safe (no javascript:, data:, vbscript:, etc.)
pub fn isSafeUrlScheme(url: []const u8) bool {
    const trimmed = std.mem.trimLeft(u8, url, " \t\n\r");

    // Check for dangerous schemes (case-insensitive)
    const dangerous_schemes = [_][]const u8{
        "javascript:",
        "vbscript:",
        "data:",
        "blob:",
    };

    for (dangerous_schemes) |scheme| {
        if (trimmed.len >= scheme.len) {
            var matches = true;
            for (trimmed[0..scheme.len], scheme) |a, b| {
                if (std.ascii.toLower(a) != b) {
                    matches = false;
                    break;
                }
            }
            if (matches) return false;
        }
    }

    return true;
}

/// Sanitize a URL for safe use in href/src attributes.
/// Returns null if the URL uses a dangerous scheme.
pub fn sanitizeUrl(input: []const u8, buffer: []u8) ?[]const u8 {
    if (!isSafeUrlScheme(input)) return null;
    return escapeUrl(input, buffer) catch null;
}

/// List of dangerous HTML tag names (lowercase)
const dangerous_tags = [_][]const u8{
    "script",
    "style",
    "iframe",
    "frame",
    "frameset",
    "object",
    "embed",
    "applet",
    "form",
    "input",
    "button",
    "select",
    "textarea",
    "link",
    "meta",
    "base",
    "svg",
    "math",
};

/// Check if an HTML tag name is considered dangerous
pub fn isDangerousTag(tag_name: []const u8) bool {
    for (dangerous_tags) |dangerous| {
        if (eqlIgnoreCase(tag_name, dangerous)) return true;
    }
    return false;
}

/// List of dangerous HTML attribute names (lowercase)
const dangerous_attributes = [_][]const u8{
    "onclick",
    "ondblclick",
    "onmousedown",
    "onmouseup",
    "onmouseover",
    "onmousemove",
    "onmouseout",
    "onmouseenter",
    "onmouseleave",
    "onkeydown",
    "onkeyup",
    "onkeypress",
    "onload",
    "onerror",
    "onabort",
    "onsubmit",
    "onreset",
    "onselect",
    "onblur",
    "onfocus",
    "onchange",
    "oninput",
    "onscroll",
    "onwheel",
    "oncopy",
    "oncut",
    "onpaste",
    "ondrag",
    "ondragstart",
    "ondragend",
    "ondragover",
    "ondragenter",
    "ondragleave",
    "ondrop",
    "onanimationstart",
    "onanimationend",
    "onanimationiteration",
    "ontransitionend",
    "formaction",
    "xlink:href",
};

/// Check if an HTML attribute name is considered dangerous
pub fn isDangerousAttribute(attr_name: []const u8) bool {
    // Check explicit list
    for (dangerous_attributes) |dangerous| {
        if (eqlIgnoreCase(attr_name, dangerous)) return true;
    }

    // Any attribute starting with "on" is potentially an event handler
    if (attr_name.len >= 2) {
        if (std.ascii.toLower(attr_name[0]) == 'o' and
            std.ascii.toLower(attr_name[1]) == 'n')
        {
            return true;
        }
    }

    return false;
}

/// Strip all HTML tags from input, leaving only text content.
pub fn stripTags(input: []const u8, buffer: []u8) HtmlError![]const u8 {
    var write_index: usize = 0;
    var in_tag = false;

    for (input) |char| {
        if (char == '<') {
            in_tag = true;
        } else if (char == '>') {
            in_tag = false;
        } else if (!in_tag) {
            if (write_index >= buffer.len) return error.BufferTooSmall;
            buffer[write_index] = char;
            write_index += 1;
        }
    }

    return buffer[0..write_index];
}

/// Strip HTML tags with allocation.
pub fn stripTagsAlloc(allocator: Allocator, input: []const u8) HtmlError![]u8 {
    const buffer = allocator.alloc(u8, input.len) catch return error.OutOfMemory;
    errdefer allocator.free(buffer);

    const result = try stripTags(input, buffer);
    const final = allocator.realloc(buffer, result.len) catch return error.OutOfMemory;
    return final;
}

/// Encode a string for safe insertion into a JavaScript string literal within HTML.
/// Escapes both HTML entities and JavaScript special characters.
pub fn escapeJsInHtml(input: []const u8, buffer: []u8) HtmlError![]const u8 {
    var write_index: usize = 0;

    for (input) |char| {
        const replacement: ?[]const u8 = switch (char) {
            '\\' => "\\\\",
            '"' => "\\\"",
            '\'' => "\\'",
            '\n' => "\\n",
            '\r' => "\\r",
            '\t' => "\\t",
            '<' => "\\u003C",
            '>' => "\\u003E",
            '&' => "\\u0026",
            '/' => "\\/", // Prevent </script> injection
            else => null,
        };

        if (replacement) |repl| {
            if (write_index + repl.len > buffer.len) return error.BufferTooSmall;
            @memcpy(buffer[write_index..][0..repl.len], repl);
            write_index += repl.len;
        } else {
            if (write_index >= buffer.len) return error.BufferTooSmall;
            buffer[write_index] = char;
            write_index += 1;
        }
    }

    return buffer[0..write_index];
}

/// Encode data for safe insertion into a CSS context within HTML.
pub fn escapeCssInHtml(input: []const u8, buffer: []u8) HtmlError![]const u8 {
    var write_index: usize = 0;

    for (input) |char| {
        const replacement: ?[]const u8 = switch (char) {
            '\\' => "\\\\",
            '"' => "\\\"",
            '\'' => "\\'",
            '<' => "\\3C ",
            '>' => "\\3E ",
            '&' => "\\26 ",
            '/' => "\\2F ",
            '\n' => "\\A ",
            '\r' => "\\D ",
            else => null,
        };

        if (replacement) |repl| {
            if (write_index + repl.len > buffer.len) return error.BufferTooSmall;
            @memcpy(buffer[write_index..][0..repl.len], repl);
            write_index += repl.len;
        } else {
            if (write_index >= buffer.len) return error.BufferTooSmall;
            buffer[write_index] = char;
            write_index += 1;
        }
    }

    return buffer[0..write_index];
}

/// Unescape common HTML entities to their character equivalents.
pub fn unescapeEntities(input: []const u8, buffer: []u8) HtmlError![]const u8 {
    var read_index: usize = 0;
    var write_index: usize = 0;

    while (read_index < input.len) {
        if (input[read_index] == '&') {
            // Look for entity end
            var entity_end: ?usize = null;
            var scan_index = read_index + 1;
            while (scan_index < input.len and scan_index < read_index + 10) {
                if (input[scan_index] == ';') {
                    entity_end = scan_index;
                    break;
                }
                scan_index += 1;
            }

            if (entity_end) |end_pos| {
                const entity = input[read_index .. end_pos + 1];
                const decoded: ?u8 = decodeEntity(entity);

                if (decoded) |char| {
                    if (write_index >= buffer.len) return error.BufferTooSmall;
                    buffer[write_index] = char;
                    write_index += 1;
                    read_index = end_pos + 1;
                    continue;
                }
            }
        }

        if (write_index >= buffer.len) return error.BufferTooSmall;
        buffer[write_index] = input[read_index];
        write_index += 1;
        read_index += 1;
    }

    return buffer[0..write_index];
}

/// Decode a single HTML entity to its character.
fn decodeEntity(entity: []const u8) ?u8 {
    if (eqlIgnoreCase(entity, "&amp;")) return '&';
    if (eqlIgnoreCase(entity, "&lt;")) return '<';
    if (eqlIgnoreCase(entity, "&gt;")) return '>';
    if (eqlIgnoreCase(entity, "&quot;")) return '"';
    if (eqlIgnoreCase(entity, "&#x27;") or eqlIgnoreCase(entity, "&apos;") or eqlIgnoreCase(entity, "&#39;")) return '\'';
    if (eqlIgnoreCase(entity, "&#x20;") or eqlIgnoreCase(entity, "&#32;")) return ' ';
    if (eqlIgnoreCase(entity, "&nbsp;") or eqlIgnoreCase(entity, "&#160;") or eqlIgnoreCase(entity, "&#xa0;")) return ' ';

    // Numeric entities
    if (entity.len >= 4 and entity[0] == '&' and entity[1] == '#') {
        if (entity[2] == 'x' or entity[2] == 'X') {
            // Hex entity &#xNN;
            if (std.fmt.parseInt(u8, entity[3 .. entity.len - 1], 16)) |value| {
                return value;
            } else |_| {}
        } else {
            // Decimal entity &#NN;
            if (std.fmt.parseInt(u8, entity[2 .. entity.len - 1], 10)) |value| {
                return value;
            } else |_| {}
        }
    }

    return null;
}

/// Case-insensitive string comparison
fn eqlIgnoreCase(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |char_a, char_b| {
        if (std.ascii.toLower(char_a) != std.ascii.toLower(char_b)) return false;
    }
    return true;
}

/// Calculate the maximum buffer size needed for escaping.
pub fn maxEscapedLength(input_length: usize) usize {
    return input_length * 6; // Worst case: all chars become &#xNN;
}

// =============================================================================
// Tests
// =============================================================================

test "escapeText basic" {
    var buffer: [100]u8 = undefined;

    const result1 = try escapeText("<script>alert('xss')</script>", &buffer);
    try std.testing.expectEqualStrings("&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;", result1);

    const result2 = try escapeText("Hello & Goodbye", &buffer);
    try std.testing.expectEqualStrings("Hello &amp; Goodbye", result2);

    const result3 = try escapeText("\"quoted\"", &buffer);
    try std.testing.expectEqualStrings("&quot;quoted&quot;", result3);
}

test "escapeText empty" {
    var buffer: [100]u8 = undefined;
    const result = try escapeText("", &buffer);
    try std.testing.expectEqualStrings("", result);
}

test "escapeText no escaping needed" {
    var buffer: [100]u8 = undefined;
    const result = try escapeText("Hello World", &buffer);
    try std.testing.expectEqualStrings("Hello World", result);
}

test "escapeAttribute" {
    var buffer: [100]u8 = undefined;
    const result = try escapeAttribute("value with \"quotes\" and <brackets>", &buffer);
    try std.testing.expectEqualStrings("value with &quot;quotes&quot; and &lt;brackets&gt;", result);
}

test "buffer too small" {
    var buffer: [5]u8 = undefined;
    try std.testing.expectError(error.BufferTooSmall, escapeText("<script>", &buffer));
}

test "isSafeUrlScheme" {
    try std.testing.expect(isSafeUrlScheme("https://example.com"));
    try std.testing.expect(isSafeUrlScheme("http://example.com"));
    try std.testing.expect(isSafeUrlScheme("/relative/path"));
    try std.testing.expect(isSafeUrlScheme("relative/path"));

    try std.testing.expect(!isSafeUrlScheme("javascript:alert(1)"));
    try std.testing.expect(!isSafeUrlScheme("JAVASCRIPT:alert(1)"));
    try std.testing.expect(!isSafeUrlScheme("  javascript:alert(1)"));
    try std.testing.expect(!isSafeUrlScheme("vbscript:msgbox(1)"));
    try std.testing.expect(!isSafeUrlScheme("data:text/html,<script>"));
}

test "sanitizeUrl" {
    var buffer: [100]u8 = undefined;

    const safe = sanitizeUrl("https://example.com?q=test&x=1", &buffer);
    try std.testing.expect(safe != null);
    try std.testing.expectEqualStrings("https://example.com?q=test&amp;x=1", safe.?);

    const dangerous = sanitizeUrl("javascript:alert(1)", &buffer);
    try std.testing.expect(dangerous == null);
}

test "isDangerousTag" {
    try std.testing.expect(isDangerousTag("script"));
    try std.testing.expect(isDangerousTag("SCRIPT"));
    try std.testing.expect(isDangerousTag("iframe"));
    try std.testing.expect(isDangerousTag("svg"));

    try std.testing.expect(!isDangerousTag("div"));
    try std.testing.expect(!isDangerousTag("span"));
    try std.testing.expect(!isDangerousTag("p"));
}

test "isDangerousAttribute" {
    try std.testing.expect(isDangerousAttribute("onclick"));
    try std.testing.expect(isDangerousAttribute("ONCLICK"));
    try std.testing.expect(isDangerousAttribute("onerror"));
    try std.testing.expect(isDangerousAttribute("onmouseover"));
    try std.testing.expect(isDangerousAttribute("onunknown")); // Any on* attribute

    try std.testing.expect(!isDangerousAttribute("class"));
    try std.testing.expect(!isDangerousAttribute("id"));
    try std.testing.expect(!isDangerousAttribute("href"));
}

test "stripTags" {
    var buffer: [100]u8 = undefined;

    const result1 = try stripTags("<p>Hello <b>World</b>!</p>", &buffer);
    try std.testing.expectEqualStrings("Hello World!", result1);

    const result2 = try stripTags("<script>alert('xss')</script>Safe text", &buffer);
    try std.testing.expectEqualStrings("alert('xss')Safe text", result2);

    const result3 = try stripTags("No tags here", &buffer);
    try std.testing.expectEqualStrings("No tags here", result3);
}

test "escapeJsInHtml" {
    var buffer: [100]u8 = undefined;

    const result = try escapeJsInHtml("</script><script>alert(1)", &buffer);
    try std.testing.expectEqualStrings("\\u003C\\/script\\u003E\\u003Cscript\\u003Ealert(1)", result);
}

test "unescapeEntities" {
    var buffer: [100]u8 = undefined;

    const result1 = try unescapeEntities("&lt;script&gt;", &buffer);
    try std.testing.expectEqualStrings("<script>", result1);

    const result2 = try unescapeEntities("Hello &amp; World", &buffer);
    try std.testing.expectEqualStrings("Hello & World", result2);

    const result3 = try unescapeEntities("&#x41;&#66;C", &buffer);
    try std.testing.expectEqualStrings("ABC", result3);
}

test "unescapeEntities preserves unknown" {
    var buffer: [100]u8 = undefined;
    const result = try unescapeEntities("&unknown; text", &buffer);
    try std.testing.expectEqualStrings("&unknown; text", result);
}

test "maxEscapedLength" {
    try std.testing.expectEqual(@as(usize, 30), maxEscapedLength(5));
    try std.testing.expectEqual(@as(usize, 0), maxEscapedLength(0));
}
