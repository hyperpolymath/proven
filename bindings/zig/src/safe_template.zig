// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe template string interpolation with automatic escaping that cannot crash.
//!
//! Provides utilities for rendering templates with variables while automatically
//! escaping output based on context (HTML, URL, JavaScript, etc.). Designed to
//! prevent XSS and injection attacks.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for template operations.
pub const TemplateError = error{
    InvalidTemplate,
    UnknownVariable,
    UnclosedDelimiter,
    NestedDelimiters,
    InvalidEscapeSequence,
    MaxDepthExceeded,
    CircularReference,
    OutOfMemory,
};

/// Escape context for automatic output escaping.
pub const EscapeContext = enum {
    none,
    html,
    html_attribute,
    url,
    javascript,
    css,
    sql,

    /// Get the default context for templates.
    pub fn default() EscapeContext {
        return .html;
    }
};

/// Template variable value.
pub const Value = union(enum) {
    string: []const u8,
    integer: i64,
    float: f64,
    boolean: bool,
    null: void,
    array: []const Value,

    /// Convert value to string representation.
    pub fn toString(self: Value, allocator: Allocator) ![]u8 {
        return switch (self) {
            .string => |s| try allocator.dupe(u8, s),
            .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
            .float => |f| try std.fmt.allocPrint(allocator, "{d}", .{f}),
            .boolean => |b| try allocator.dupe(u8, if (b) "true" else "false"),
            .null => try allocator.dupe(u8, ""),
            .array => try allocator.dupe(u8, "[Array]"),
        };
    }

    /// Check if value is truthy.
    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .string => |s| s.len > 0,
            .integer => |i| i != 0,
            .float => |f| f != 0.0,
            .boolean => |b| b,
            .null => false,
            .array => |a| a.len > 0,
        };
    }
};

/// Template configuration options.
pub const Config = struct {
    /// Opening delimiter for variables (default: "{{").
    open_delim: []const u8 = "{{",
    /// Closing delimiter for variables (default: "}}").
    close_delim: []const u8 = "}}",
    /// Default escape context.
    escape_context: EscapeContext = .html,
    /// Maximum template nesting depth.
    max_depth: usize = 10,
    /// Strip whitespace around delimiters.
    strip_whitespace: bool = false,
};

/// Default configuration.
pub const default_config = Config{};

/// Escape string for HTML context.
pub fn escapeHtml(allocator: Allocator, value: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (value) |c| {
        switch (c) {
            '&' => try result.appendSlice("&amp;"),
            '<' => try result.appendSlice("&lt;"),
            '>' => try result.appendSlice("&gt;"),
            '"' => try result.appendSlice("&quot;"),
            '\'' => try result.appendSlice("&#x27;"),
            '/' => try result.appendSlice("&#x2F;"),
            else => try result.append(c),
        }
    }

    return result.toOwnedSlice();
}

/// Escape string for HTML attribute context.
pub fn escapeHtmlAttribute(allocator: Allocator, value: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (value) |c| {
        switch (c) {
            '&' => try result.appendSlice("&amp;"),
            '<' => try result.appendSlice("&lt;"),
            '>' => try result.appendSlice("&gt;"),
            '"' => try result.appendSlice("&quot;"),
            '\'' => try result.appendSlice("&#x27;"),
            '`' => try result.appendSlice("&#x60;"),
            '=' => try result.appendSlice("&#x3D;"),
            else => try result.append(c),
        }
    }

    return result.toOwnedSlice();
}

/// Escape string for URL context (percent encoding).
pub fn escapeUrl(allocator: Allocator, value: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    const safe_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~";

    for (value) |c| {
        if (std.mem.indexOfScalar(u8, safe_chars, c) != null) {
            try result.append(c);
        } else {
            const hex_chars = "0123456789ABCDEF";
            try result.append('%');
            try result.append(hex_chars[c >> 4]);
            try result.append(hex_chars[c & 0x0F]);
        }
    }

    return result.toOwnedSlice();
}

/// Escape string for JavaScript context.
pub fn escapeJavaScript(allocator: Allocator, value: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (value) |c| {
        switch (c) {
            '\\' => try result.appendSlice("\\\\"),
            '"' => try result.appendSlice("\\\""),
            '\'' => try result.appendSlice("\\'"),
            '\n' => try result.appendSlice("\\n"),
            '\r' => try result.appendSlice("\\r"),
            '\t' => try result.appendSlice("\\t"),
            '<' => try result.appendSlice("\\u003C"),
            '>' => try result.appendSlice("\\u003E"),
            '/' => try result.appendSlice("\\/"),
            else => {
                if (c < 0x20) {
                    var buf: [6]u8 = undefined;
                    const formatted = std.fmt.bufPrint(&buf, "\\u{X:0>4}", .{c}) catch continue;
                    try result.appendSlice(formatted);
                } else {
                    try result.append(c);
                }
            },
        }
    }

    return result.toOwnedSlice();
}

/// Escape string for CSS context.
pub fn escapeCss(allocator: Allocator, value: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (value) |c| {
        if (std.ascii.isAlphanumeric(c)) {
            try result.append(c);
        } else {
            var buf: [8]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "\\{X} ", .{c}) catch continue;
            try result.appendSlice(formatted);
        }
    }

    return result.toOwnedSlice();
}

/// Escape string for SQL context (single quotes).
pub fn escapeSql(allocator: Allocator, value: []const u8) ![]u8 {
    var result = std.array_list.Managed(u8).init(allocator);
    errdefer result.deinit();

    for (value) |c| {
        if (c == '\'') {
            try result.appendSlice("''");
        } else {
            try result.append(c);
        }
    }

    return result.toOwnedSlice();
}

/// Escape a value based on context.
pub fn escapeForContext(allocator: Allocator, value: []const u8, context: EscapeContext) ![]u8 {
    return switch (context) {
        .none => try allocator.dupe(u8, value),
        .html => try escapeHtml(allocator, value),
        .html_attribute => try escapeHtmlAttribute(allocator, value),
        .url => try escapeUrl(allocator, value),
        .javascript => try escapeJavaScript(allocator, value),
        .css => try escapeCss(allocator, value),
        .sql => try escapeSql(allocator, value),
    };
}

/// Variable lookup function type.
pub const VariableLookup = *const fn (name: []const u8) ?Value;

/// Simple template renderer.
pub const Template = struct {
    allocator: Allocator,
    source: []const u8,
    config: Config,

    /// Initialize a template.
    pub fn init(allocator: Allocator, source: []const u8, config: Config) Template {
        return Template{
            .allocator = allocator,
            .source = source,
            .config = config,
        };
    }

    /// Render the template with a hash map of variables.
    pub fn render(self: Template, variables: std.StringHashMap(Value)) ![]u8 {
        return self.renderWithLookup(struct {
            vars: std.StringHashMap(Value),

            pub fn lookup(ctx: @This(), name: []const u8) ?Value {
                return ctx.vars.get(name);
            }
        }{ .vars = variables });
    }

    /// Render the template with a custom lookup function.
    pub fn renderWithLookup(self: Template, lookup_ctx: anytype) ![]u8 {
        var result = std.array_list.Managed(u8).init(self.allocator);
        errdefer result.deinit();

        var pos: usize = 0;
        const open = self.config.open_delim;
        const close = self.config.close_delim;

        while (pos < self.source.len) {
            // Find next opening delimiter
            if (std.mem.indexOfPos(u8, self.source, pos, open)) |start| {
                // Append text before delimiter
                try result.appendSlice(self.source[pos..start]);

                // Find closing delimiter
                const var_start = start + open.len;
                if (std.mem.indexOfPos(u8, self.source, var_start, close)) |end| {
                    // Check for nested delimiters
                    if (std.mem.indexOf(u8, self.source[var_start..end], open) != null) {
                        return error.NestedDelimiters;
                    }

                    // Extract variable name
                    var var_name = self.source[var_start..end];

                    // Strip whitespace if configured
                    if (self.config.strip_whitespace) {
                        var_name = std.mem.trim(u8, var_name, " \t\n\r");
                    }

                    // Parse escape context modifier
                    var escape_ctx = self.config.escape_context;
                    var actual_name = var_name;

                    // Check for raw output (no escaping)
                    if (std.mem.startsWith(u8, var_name, "!")) {
                        escape_ctx = .none;
                        actual_name = var_name[1..];
                    } else if (std.mem.startsWith(u8, var_name, "html:")) {
                        escape_ctx = .html;
                        actual_name = var_name[5..];
                    } else if (std.mem.startsWith(u8, var_name, "url:")) {
                        escape_ctx = .url;
                        actual_name = var_name[4..];
                    } else if (std.mem.startsWith(u8, var_name, "js:")) {
                        escape_ctx = .javascript;
                        actual_name = var_name[3..];
                    } else if (std.mem.startsWith(u8, var_name, "css:")) {
                        escape_ctx = .css;
                        actual_name = var_name[4..];
                    } else if (std.mem.startsWith(u8, var_name, "sql:")) {
                        escape_ctx = .sql;
                        actual_name = var_name[4..];
                    }

                    actual_name = std.mem.trim(u8, actual_name, " \t\n\r");

                    // Look up variable
                    if (lookup_ctx.lookup(actual_name)) |value| {
                        const str_value = try value.toString(self.allocator);
                        defer self.allocator.free(str_value);

                        const escaped = try escapeForContext(self.allocator, str_value, escape_ctx);
                        defer self.allocator.free(escaped);

                        try result.appendSlice(escaped);
                    } else {
                        return error.UnknownVariable;
                    }

                    pos = end + close.len;
                } else {
                    return error.UnclosedDelimiter;
                }
            } else {
                // No more delimiters, append rest of source
                try result.appendSlice(self.source[pos..]);
                break;
            }
        }

        return result.toOwnedSlice();
    }
};

/// Render a template string with variables.
pub fn render(allocator: Allocator, template: []const u8, variables: std.StringHashMap(Value)) ![]u8 {
    const tmpl = Template.init(allocator, template, default_config);
    return tmpl.render(variables);
}

/// Render a template string with custom config.
pub fn renderWithConfig(allocator: Allocator, template: []const u8, variables: std.StringHashMap(Value), config: Config) ![]u8 {
    const tmpl = Template.init(allocator, template, config);
    return tmpl.render(variables);
}

/// Check if a template string is valid (has balanced delimiters).
pub fn isValid(template: []const u8, config: Config) bool {
    var depth: usize = 0;
    var pos: usize = 0;

    while (pos < template.len) {
        if (std.mem.indexOfPos(u8, template, pos, config.open_delim)) |open_pos| {
            depth += 1;
            pos = open_pos + config.open_delim.len;

            if (std.mem.indexOfPos(u8, template, pos, config.close_delim)) |close_pos| {
                depth -= 1;
                pos = close_pos + config.close_delim.len;
            } else {
                return false; // Unclosed delimiter
            }
        } else {
            break;
        }
    }

    return depth == 0;
}

/// Extract variable names from a template.
pub fn extractVariables(allocator: Allocator, template: []const u8, config: Config) ![][]const u8 {
    var variables = std.array_list.Managed([]const u8).init(allocator);
    errdefer variables.deinit();

    var pos: usize = 0;

    while (pos < template.len) {
        if (std.mem.indexOfPos(u8, template, pos, config.open_delim)) |start| {
            const var_start = start + config.open_delim.len;

            if (std.mem.indexOfPos(u8, template, var_start, config.close_delim)) |end| {
                var var_name = template[var_start..end];
                var_name = std.mem.trim(u8, var_name, " \t\n\r");

                // Remove escape context prefix
                if (std.mem.startsWith(u8, var_name, "!")) {
                    var_name = var_name[1..];
                } else if (std.mem.indexOf(u8, var_name, ":")) |colon_pos| {
                    var_name = var_name[colon_pos + 1 ..];
                }

                var_name = std.mem.trim(u8, var_name, " \t\n\r");

                // Check for duplicates
                var found = false;
                for (variables.items) |existing| {
                    if (std.mem.eql(u8, existing, var_name)) {
                        found = true;
                        break;
                    }
                }

                if (!found) {
                    try variables.append(var_name);
                }

                pos = end + config.close_delim.len;
            } else {
                break;
            }
        } else {
            break;
        }
    }

    return variables.toOwnedSlice();
}

test "escapeHtml" {
    const allocator = std.testing.allocator;
    const result = try escapeHtml(allocator, "<script>alert('xss')</script>");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("&lt;script&gt;alert(&#x27;xss&#x27;)&lt;&#x2F;script&gt;", result);
}

test "escapeUrl" {
    const allocator = std.testing.allocator;
    const result = try escapeUrl(allocator, "hello world&foo=bar");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("hello%20world%26foo%3Dbar", result);
}

test "escapeJavaScript" {
    const allocator = std.testing.allocator;
    const result = try escapeJavaScript(allocator, "line\nbreak");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("line\\nbreak", result);
}

test "escapeSql" {
    const allocator = std.testing.allocator;
    const result = try escapeSql(allocator, "it's a test");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("it''s a test", result);
}

test "Template.render basic" {
    const allocator = std.testing.allocator;
    var vars = std.StringHashMap(Value).init(allocator);
    defer vars.deinit();
    try vars.put("name", .{ .string = "World" });

    const tmpl = Template.init(allocator, "Hello, {{name}}!", default_config);
    const result = try tmpl.render(vars);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("Hello, World!", result);
}

test "Template.render with escaping" {
    const allocator = std.testing.allocator;
    var vars = std.StringHashMap(Value).init(allocator);
    defer vars.deinit();
    try vars.put("user", .{ .string = "<script>alert('xss')</script>" });

    const tmpl = Template.init(allocator, "Welcome, {{user}}!", default_config);
    const result = try tmpl.render(vars);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("Welcome, &lt;script&gt;alert(&#x27;xss&#x27;)&lt;&#x2F;script&gt;!", result);
}

test "Template.render raw output" {
    const allocator = std.testing.allocator;
    var vars = std.StringHashMap(Value).init(allocator);
    defer vars.deinit();
    try vars.put("html", .{ .string = "<b>bold</b>" });

    const tmpl = Template.init(allocator, "Content: {{!html}}", default_config);
    const result = try tmpl.render(vars);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("Content: <b>bold</b>", result);
}

test "Template.render URL escaping" {
    const allocator = std.testing.allocator;
    var vars = std.StringHashMap(Value).init(allocator);
    defer vars.deinit();
    try vars.put("query", .{ .string = "hello world" });

    const tmpl = Template.init(allocator, "?q={{url:query}}", default_config);
    const result = try tmpl.render(vars);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("?q=hello%20world", result);
}

test "Template.render unknown variable" {
    const allocator = std.testing.allocator;
    var vars = std.StringHashMap(Value).init(allocator);
    defer vars.deinit();

    const tmpl = Template.init(allocator, "Hello, {{unknown}}!", default_config);
    try std.testing.expectError(error.UnknownVariable, tmpl.render(vars));
}

test "Template.render unclosed delimiter" {
    const allocator = std.testing.allocator;
    var vars = std.StringHashMap(Value).init(allocator);
    defer vars.deinit();
    try vars.put("name", .{ .string = "World" });

    const tmpl = Template.init(allocator, "Hello, {{name", default_config);
    try std.testing.expectError(error.UnclosedDelimiter, tmpl.render(vars));
}

test "isValid" {
    try std.testing.expect(isValid("Hello, {{name}}!", default_config));
    try std.testing.expect(isValid("No variables here", default_config));
    try std.testing.expect(!isValid("Unclosed {{name", default_config));
}

test "extractVariables" {
    const allocator = std.testing.allocator;
    const vars = try extractVariables(allocator, "{{name}} and {{url:link}} and {{name}}", default_config);
    defer allocator.free(vars);

    try std.testing.expectEqual(@as(usize, 2), vars.len);
    try std.testing.expectEqualStrings("name", vars[0]);
    try std.testing.expectEqualStrings("link", vars[1]);
}

test "Value.toString" {
    const allocator = std.testing.allocator;

    const str_val = Value{ .string = "hello" };
    const str_result = try str_val.toString(allocator);
    defer allocator.free(str_result);
    try std.testing.expectEqualStrings("hello", str_result);

    const int_val = Value{ .integer = 42 };
    const int_result = try int_val.toString(allocator);
    defer allocator.free(int_result);
    try std.testing.expectEqualStrings("42", int_result);

    const bool_val = Value{ .boolean = true };
    const bool_result = try bool_val.toString(allocator);
    defer allocator.free(bool_result);
    try std.testing.expectEqualStrings("true", bool_result);
}

test "Value.isTruthy" {
    try std.testing.expect((Value{ .string = "hello" }).isTruthy());
    try std.testing.expect(!(Value{ .string = "" }).isTruthy());
    try std.testing.expect((Value{ .integer = 1 }).isTruthy());
    try std.testing.expect(!(Value{ .integer = 0 }).isTruthy());
    try std.testing.expect((Value{ .boolean = true }).isTruthy());
    try std.testing.expect(!(Value{ .boolean = false }).isTruthy());
    try std.testing.expect(!(Value{ .null = {} }).isTruthy());
}
