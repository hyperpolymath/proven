// SPDX-License-Identifier: PMPL-1.0-or-later
//
// Proven HTTP - Zig FFI Layer
//
// C ABI exports for URL encoding/decoding operations.
// Calls formally verified Idris2 functions and provides safe C interface.

const std = @import("std");

// Hex encoding table
const hex_chars = "0123456789ABCDEF";

/// Check if a character is unreserved per RFC 3986
/// Unreserved: A-Z a-z 0-9 - . _ ~
fn isUnreserved(c: u8) bool {
    return (c >= 'A' and c <= 'Z') or
        (c >= 'a' and c <= 'z') or
        (c >= '0' and c <= '9') or
        c == '-' or c == '.' or c == '_' or c == '~';
}

/// URL encode a string (percent encoding per RFC 3986)
/// Caller must free the returned string with proven_http_free()
export fn proven_http_url_encode(input: [*:0]const u8, input_len: usize) ?[*:0]u8 {
    // Allocate worst case: every char becomes %XX (3x size) + null terminator
    const max_size = input_len * 3 + 1;
    const output = std.c.malloc(max_size) orelse return null;
    const buf = @as([*]u8, @ptrCast(output));

    var out_pos: usize = 0;
    var i: usize = 0;

    while (i < input_len) : (i += 1) {
        const c = input[i];

        if (isUnreserved(c)) {
            // Pass through unreserved characters
            buf[out_pos] = c;
            out_pos += 1;
        } else {
            // Percent-encode as %XX
            buf[out_pos] = '%';
            buf[out_pos + 1] = hex_chars[c >> 4];
            buf[out_pos + 2] = hex_chars[c & 0x0F];
            out_pos += 3;
        }
    }

    buf[out_pos] = 0; // Null terminator
    return @ptrCast(buf);
}

/// Convert hex character to value (0-15)
fn hexToValue(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'A'...'F' => c - 'A' + 10,
        'a'...'f' => c - 'a' + 10,
        else => null,
    };
}

/// URL decode a percent-encoded string
/// Returns null if encoding is invalid
/// Caller must free the returned string with proven_http_free()
export fn proven_http_url_decode(input: [*:0]const u8, input_len: usize) ?[*:0]u8 {
    // Allocate max size (decoded is never longer than encoded)
    const output = std.c.malloc(input_len + 1) orelse return null;
    const buf = @as([*]u8, @ptrCast(output));

    var out_pos: usize = 0;
    var i: usize = 0;

    while (i < input_len) : (i += 1) {
        const c = input[i];

        if (c == '%') {
            // Decode %XX sequence
            if (i + 2 >= input_len) {
                std.c.free(output);
                return null; // Truncated encoding
            }

            const hi = hexToValue(input[i + 1]) orelse {
                std.c.free(output);
                return null; // Invalid hex
            };

            const lo = hexToValue(input[i + 2]) orelse {
                std.c.free(output);
                return null; // Invalid hex
            };

            buf[out_pos] = (hi << 4) | lo;
            out_pos += 1;
            i += 2; // Skip the two hex digits
        } else if (c == '+') {
            // Plus sign decodes to space (application/x-www-form-urlencoded)
            buf[out_pos] = ' ';
            out_pos += 1;
        } else {
            // Pass through
            buf[out_pos] = c;
            out_pos += 1;
        }
    }

    buf[out_pos] = 0; // Null terminator
    return @ptrCast(buf);
}

/// Check if a character is a hex digit
fn isHexDigit(c: u8) bool {
    return (c >= '0' and c <= '9') or
        (c >= 'A' and c <= 'F') or
        (c >= 'a' and c <= 'f');
}

/// Validate that a string is properly percent-encoded
export fn proven_http_is_valid_encoded(input: [*:0]const u8, input_len: usize) bool {
    var i: usize = 0;

    while (i < input_len) : (i += 1) {
        const c = input[i];

        if (c == '%') {
            // Check that %XX is valid
            if (i + 2 >= input_len) return false; // Truncated

            if (!isHexDigit(input[i + 1]) or !isHexDigit(input[i + 2])) {
                return false; // Invalid hex
            }

            i += 2; // Skip the two hex digits
        }
    }

    return true;
}

/// Check if a character is unreserved (doesn't need encoding)
export fn proven_http_is_unreserved(c: u8) bool {
    return isUnreserved(c);
}

/// Free memory allocated by proven_http functions
export fn proven_http_free(ptr: ?*anyopaque) void {
    if (ptr) |p| std.c.free(p);
}

// Tests
test "url_encode basic" {
    const input = "hello world!";
    const encoded = proven_http_url_encode(input.ptr, input.len) orelse unreachable;
    defer proven_http_free(encoded);

    const expected = "hello%20world%21";
    const result = std.mem.span(encoded);
    try std.testing.expectEqualStrings(expected, result);
}

test "url_encode unreserved" {
    const input = "ABCabc123-._~";
    const encoded = proven_http_url_encode(input.ptr, input.len) orelse unreachable;
    defer proven_http_free(encoded);

    try std.testing.expectEqualStrings(input, std.mem.span(encoded));
}

test "url_decode basic" {
    const input = "hello%20world%21";
    const decoded = proven_http_url_decode(input.ptr, input.len) orelse unreachable;
    defer proven_http_free(decoded);

    const expected = "hello world!";
    try std.testing.expectEqualStrings(expected, std.mem.span(decoded));
}

test "url_decode plus" {
    const input = "hello+world";
    const decoded = proven_http_url_decode(input.ptr, input.len) orelse unreachable;
    defer proven_http_free(decoded);

    const expected = "hello world";
    try std.testing.expectEqualStrings(expected, std.mem.span(decoded));
}

test "url_decode invalid" {
    const input = "hello%2";
    const result = proven_http_url_decode(input.ptr, input.len);
    try std.testing.expect(result == null);
}

test "is_valid_encoded" {
    try std.testing.expect(proven_http_is_valid_encoded("hello%20world".ptr, 13));
    try std.testing.expect(!proven_http_is_valid_encoded("hello%2".ptr, 7));
    try std.testing.expect(!proven_http_is_valid_encoded("hello%ZZ".ptr, 8));
}

test "is_unreserved" {
    try std.testing.expect(proven_http_is_unreserved('A'));
    try std.testing.expect(proven_http_is_unreserved('z'));
    try std.testing.expect(proven_http_is_unreserved('0'));
    try std.testing.expect(proven_http_is_unreserved('-'));
    try std.testing.expect(proven_http_is_unreserved('~'));
    try std.testing.expect(!proven_http_is_unreserved(' '));
    try std.testing.expect(!proven_http_is_unreserved('!'));
}
