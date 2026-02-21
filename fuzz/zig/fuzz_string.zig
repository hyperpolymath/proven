// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Fuzz targets for SafeString operations
//! Tests string handling for buffer overflows and encoding issues

const std = @import("std");

/// Fuzz target: UTF-8 validation
pub export fn fuzz_utf8_validate(data: [*]const u8, len: usize) callconv(.C) c_int {
    if (len == 0) return 0;

    const slice = data[0..len];

    // UTF-8 validation should never crash
    if (std.unicode.utf8ValidateSlice(slice)) {
        // Valid UTF-8
        return 0;
    } else {
        // Invalid UTF-8, but no crash
        return 0;
    }
}

/// Fuzz target: HTML escape
pub export fn fuzz_html_escape(data: [*]const u8, len: usize) callconv(.C) c_int {
    if (len == 0) return 0;

    const slice = data[0..len];
    var allocator = std.heap.page_allocator;

    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    for (slice) |c| {
        switch (c) {
            '<' => result.appendSlice("&lt;") catch return -1,
            '>' => result.appendSlice("&gt;") catch return -1,
            '&' => result.appendSlice("&amp;") catch return -1,
            '"' => result.appendSlice("&quot;") catch return -1,
            '\'' => result.appendSlice("&#x27;") catch return -1,
            else => result.append(c) catch return -1,
        }
    }

    return 0;
}

/// Fuzz target: SQL escape (single quotes)
pub export fn fuzz_sql_escape(data: [*]const u8, len: usize) callconv(.C) c_int {
    if (len == 0) return 0;

    const slice = data[0..len];
    var allocator = std.heap.page_allocator;

    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    for (slice) |c| {
        if (c == '\'') {
            result.appendSlice("''") catch return -1;
        } else {
            result.append(c) catch return -1;
        }
    }

    return 0;
}

/// Fuzz target: URL encoding
pub export fn fuzz_url_encode(data: [*]const u8, len: usize) callconv(.C) c_int {
    if (len == 0) return 0;

    const slice = data[0..len];
    var allocator = std.heap.page_allocator;

    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    const unreserved = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~";

    for (slice) |c| {
        if (std.mem.indexOfScalar(u8, unreserved, c) != null) {
            result.append(c) catch return -1;
        } else {
            result.append('%') catch return -1;
            const hex = "0123456789ABCDEF";
            result.append(hex[c >> 4]) catch return -1;
            result.append(hex[c & 0x0F]) catch return -1;
        }
    }

    return 0;
}

/// Fuzz target: Shell escape
pub export fn fuzz_shell_escape(data: [*]const u8, len: usize) callconv(.C) c_int {
    if (len == 0) return 0;

    const slice = data[0..len];
    var allocator = std.heap.page_allocator;

    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    result.append('\'') catch return -1;
    for (slice) |c| {
        if (c == '\'') {
            result.appendSlice("'\\''") catch return -1;
        } else {
            result.append(c) catch return -1;
        }
    }
    result.append('\'') catch return -1;

    return 0;
}

// Main entry for testing
pub fn main() !void {
    std.debug.print("Fuzz targets for SafeString loaded.\n", .{});
}
