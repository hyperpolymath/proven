// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe Base64 encoding and decoding operations with validation.
//!
//! Provides safe functions for Base64 encoding and decoding that cannot crash.
//! Supports standard Base64 (RFC 4648), URL-safe Base64, and Base64 without padding.
//! All operations include proper validation and bounds checking.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for Base64 operations.
pub const Base64Error = error{
    InvalidCharacter,
    InvalidPadding,
    InvalidLength,
    BufferTooSmall,
    OutOfMemory,
};

/// Base64 encoding variants.
pub const Variant = enum {
    standard, // RFC 4648 standard alphabet with padding
    standard_no_pad, // Standard alphabet without padding
    url_safe, // URL-safe alphabet with padding
    url_safe_no_pad, // URL-safe alphabet without padding

    /// Get the alphabet for this variant.
    pub fn alphabet(self: Variant) *const [64]u8 {
        return switch (self) {
            .standard, .standard_no_pad => &standard_alphabet,
            .url_safe, .url_safe_no_pad => &url_safe_alphabet,
        };
    }

    /// Check if this variant uses padding.
    pub fn usesPadding(self: Variant) bool {
        return switch (self) {
            .standard, .url_safe => true,
            .standard_no_pad, .url_safe_no_pad => false,
        };
    }
};

/// Standard Base64 alphabet (RFC 4648).
const standard_alphabet: [64]u8 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".*;

/// URL-safe Base64 alphabet (RFC 4648 Section 5).
const url_safe_alphabet: [64]u8 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".*;

/// Padding character.
const pad_char: u8 = '=';

/// Decode table for standard Base64 (255 = invalid).
const standard_decode_table: [256]u8 = blk: {
    var table: [256]u8 = [_]u8{255} ** 256;
    for (standard_alphabet, 0..) |c, i| {
        table[c] = @intCast(i);
    }
    break :blk table;
};

/// Decode table for URL-safe Base64 (255 = invalid).
const url_safe_decode_table: [256]u8 = blk: {
    var table: [256]u8 = [_]u8{255} ** 256;
    for (url_safe_alphabet, 0..) |c, i| {
        table[c] = @intCast(i);
    }
    break :blk table;
};

/// Calculate encoded length for given input length.
pub fn calcEncodedLen(input_len: usize, with_padding: bool) usize {
    if (input_len == 0) return 0;

    if (with_padding) {
        // Standard: always multiple of 4
        return ((input_len + 2) / 3) * 4;
    } else {
        // No padding: exact characters needed
        const full_groups = input_len / 3;
        const remainder = input_len % 3;
        return full_groups * 4 + switch (remainder) {
            0 => @as(usize, 0),
            1 => @as(usize, 2),
            2 => @as(usize, 3),
            else => unreachable,
        };
    }
}

/// Calculate maximum decoded length for given encoded length.
/// Actual length may be less due to padding.
pub fn calcDecodedLenMax(encoded_len: usize) usize {
    if (encoded_len == 0) return 0;
    return (encoded_len / 4) * 3 + switch (encoded_len % 4) {
        0 => @as(usize, 0),
        1 => @as(usize, 0), // Invalid, but don't crash
        2 => @as(usize, 1),
        3 => @as(usize, 2),
        else => unreachable,
    };
}

/// Encode bytes to Base64.
pub fn encode(input: []const u8, out: []u8, variant: Variant) Base64Error![]const u8 {
    const needed = calcEncodedLen(input.len, variant.usesPadding());
    if (out.len < needed) return error.BufferTooSmall;

    const alpha = variant.alphabet();
    var out_idx: usize = 0;
    var i: usize = 0;

    // Process full 3-byte groups
    while (i + 3 <= input.len) : (i += 3) {
        const b0 = input[i];
        const b1 = input[i + 1];
        const b2 = input[i + 2];

        out[out_idx] = alpha[b0 >> 2];
        out[out_idx + 1] = alpha[((b0 & 0x03) << 4) | (b1 >> 4)];
        out[out_idx + 2] = alpha[((b1 & 0x0F) << 2) | (b2 >> 6)];
        out[out_idx + 3] = alpha[b2 & 0x3F];
        out_idx += 4;
    }

    // Handle remaining bytes
    const remainder = input.len - i;
    if (remainder == 1) {
        const b0 = input[i];
        out[out_idx] = alpha[b0 >> 2];
        out[out_idx + 1] = alpha[(b0 & 0x03) << 4];
        out_idx += 2;

        if (variant.usesPadding()) {
            out[out_idx] = pad_char;
            out[out_idx + 1] = pad_char;
            out_idx += 2;
        }
    } else if (remainder == 2) {
        const b0 = input[i];
        const b1 = input[i + 1];
        out[out_idx] = alpha[b0 >> 2];
        out[out_idx + 1] = alpha[((b0 & 0x03) << 4) | (b1 >> 4)];
        out[out_idx + 2] = alpha[(b1 & 0x0F) << 2];
        out_idx += 3;

        if (variant.usesPadding()) {
            out[out_idx] = pad_char;
            out_idx += 1;
        }
    }

    return out[0..out_idx];
}

/// Encode bytes to Base64 with allocation.
pub fn encodeAlloc(allocator: Allocator, input: []const u8, variant: Variant) Base64Error![]u8 {
    const needed = calcEncodedLen(input.len, variant.usesPadding());
    const out = allocator.alloc(u8, needed) catch return error.OutOfMemory;
    errdefer allocator.free(out);

    _ = try encode(input, out, variant);
    return out;
}

/// Decode Base64 to bytes.
pub fn decode(input: []const u8, out: []u8, variant: Variant) Base64Error![]const u8 {
    if (input.len == 0) return out[0..0];

    // Determine decode table
    const decode_table: *const [256]u8 = switch (variant) {
        .standard, .standard_no_pad => &standard_decode_table,
        .url_safe, .url_safe_no_pad => &url_safe_decode_table,
    };

    // Count padding and validate
    var padding_count: usize = 0;
    var effective_len = input.len;

    if (variant.usesPadding()) {
        // Count trailing padding
        while (effective_len > 0 and input[effective_len - 1] == pad_char) {
            padding_count += 1;
            effective_len -= 1;
            if (padding_count > 2) return error.InvalidPadding;
        }

        // With padding, length should be multiple of 4
        if (input.len % 4 != 0) return error.InvalidLength;
    }

    // Calculate output size
    const out_len = calcDecodedLenWithPadding(effective_len, padding_count);
    if (out.len < out_len) return error.BufferTooSmall;

    var out_idx: usize = 0;
    var i: usize = 0;

    // Process full 4-character groups
    while (i + 4 <= effective_len) {
        const c0 = decode_table[input[i]];
        const c1 = decode_table[input[i + 1]];
        const c2 = decode_table[input[i + 2]];
        const c3 = decode_table[input[i + 3]];

        if (c0 == 255 or c1 == 255 or c2 == 255 or c3 == 255) {
            return error.InvalidCharacter;
        }

        out[out_idx] = (c0 << 2) | (c1 >> 4);
        out[out_idx + 1] = (c1 << 4) | (c2 >> 2);
        out[out_idx + 2] = (c2 << 6) | c3;
        out_idx += 3;
        i += 4;
    }

    // Handle remaining characters
    const remaining = effective_len - i;
    if (remaining == 2) {
        const c0 = decode_table[input[i]];
        const c1 = decode_table[input[i + 1]];
        if (c0 == 255 or c1 == 255) return error.InvalidCharacter;
        out[out_idx] = (c0 << 2) | (c1 >> 4);
        out_idx += 1;
    } else if (remaining == 3) {
        const c0 = decode_table[input[i]];
        const c1 = decode_table[input[i + 1]];
        const c2 = decode_table[input[i + 2]];
        if (c0 == 255 or c1 == 255 or c2 == 255) return error.InvalidCharacter;
        out[out_idx] = (c0 << 2) | (c1 >> 4);
        out[out_idx + 1] = (c1 << 4) | (c2 >> 2);
        out_idx += 2;
    } else if (remaining == 1) {
        // Single trailing character is invalid
        return error.InvalidLength;
    }

    return out[0..out_idx];
}

/// Calculate decoded length accounting for padding.
fn calcDecodedLenWithPadding(effective_len: usize, padding_count: usize) usize {
    if (effective_len == 0) return 0;

    const full_groups = effective_len / 4;
    const remainder = effective_len % 4;

    var len = full_groups * 3;

    if (remainder == 2) {
        len += 1;
    } else if (remainder == 3) {
        len += 2;
    }

    // Adjust for padding
    if (padding_count > 0 and remainder == 0 and full_groups > 0) {
        len -= padding_count;
    }

    return len;
}

/// Decode Base64 with allocation.
pub fn decodeAlloc(allocator: Allocator, input: []const u8, variant: Variant) Base64Error![]u8 {
    const max_len = calcDecodedLenMax(input.len);
    if (max_len == 0) {
        return allocator.alloc(u8, 0) catch return error.OutOfMemory;
    }

    const out = allocator.alloc(u8, max_len) catch return error.OutOfMemory;
    errdefer allocator.free(out);

    const result = try decode(input, out, variant);

    // Shrink allocation if needed
    if (result.len < out.len) {
        if (allocator.resize(out, result.len)) {
            return out[0..result.len];
        }
        // If resize fails, copy to new smaller allocation
        const final = allocator.alloc(u8, result.len) catch return error.OutOfMemory;
        @memcpy(final, result);
        allocator.free(out);
        return final;
    }

    return out;
}

/// Validate a Base64 string.
pub fn isValid(input: []const u8, variant: Variant) bool {
    if (input.len == 0) return true;

    const decode_table: *const [256]u8 = switch (variant) {
        .standard, .standard_no_pad => &standard_decode_table,
        .url_safe, .url_safe_no_pad => &url_safe_decode_table,
    };

    var i: usize = 0;
    var padding_started = false;

    // Check padding requirements
    if (variant.usesPadding()) {
        if (input.len % 4 != 0) return false;
    }

    while (i < input.len) : (i += 1) {
        const c = input[i];

        if (c == pad_char) {
            if (!variant.usesPadding()) return false;
            padding_started = true;
            continue;
        }

        if (padding_started) {
            // Non-padding char after padding
            return false;
        }

        if (decode_table[c] == 255) return false;
    }

    // Validate padding count
    if (variant.usesPadding() and padding_started) {
        var pad_count: usize = 0;
        i = input.len;
        while (i > 0 and input[i - 1] == pad_char) {
            pad_count += 1;
            i -= 1;
        }
        if (pad_count > 2) return false;
    }

    return true;
}

/// Check if a character is valid Base64.
pub fn isValidChar(c: u8, variant: Variant) bool {
    if (c == pad_char) return variant.usesPadding();

    const decode_table: *const [256]u8 = switch (variant) {
        .standard, .standard_no_pad => &standard_decode_table,
        .url_safe, .url_safe_no_pad => &url_safe_decode_table,
    };

    return decode_table[c] != 255;
}

/// Strip whitespace from Base64 string (for lenient parsing).
pub fn stripWhitespace(input: []const u8, out: []u8) Base64Error![]const u8 {
    var out_idx: usize = 0;

    for (input) |c| {
        if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            continue;
        }

        if (out_idx >= out.len) return error.BufferTooSmall;
        out[out_idx] = c;
        out_idx += 1;
    }

    return out[0..out_idx];
}

/// Convert between Base64 variants (e.g., standard to URL-safe).
pub fn convert(input: []const u8, out: []u8, from: Variant, to: Variant) Base64Error![]const u8 {
    if (out.len < input.len) return error.BufferTooSmall;

    const from_alpha = from.alphabet();
    const to_alpha = to.alphabet();

    // Build reverse lookup for source alphabet
    var reverse: [256]u8 = [_]u8{255} ** 256;
    for (from_alpha, 0..) |c, i| {
        reverse[c] = @intCast(i);
    }

    var out_idx: usize = 0;

    for (input) |c| {
        if (c == pad_char) {
            if (to.usesPadding()) {
                out[out_idx] = pad_char;
                out_idx += 1;
            }
            continue;
        }

        const idx = reverse[c];
        if (idx == 255) return error.InvalidCharacter;

        out[out_idx] = to_alpha[idx];
        out_idx += 1;
    }

    // Add padding if needed
    if (to.usesPadding() and !from.usesPadding()) {
        const remainder = out_idx % 4;
        if (remainder > 0) {
            const pad_needed = 4 - remainder;
            if (out_idx + pad_needed > out.len) return error.BufferTooSmall;
            for (0..pad_needed) |_| {
                out[out_idx] = pad_char;
                out_idx += 1;
            }
        }
    }

    return out[0..out_idx];
}

/// Encode a single byte triplet to 4 Base64 characters.
pub fn encodeTriplet(b0: u8, b1: u8, b2: u8, alpha: *const [64]u8) [4]u8 {
    return [4]u8{
        alpha[b0 >> 2],
        alpha[((b0 & 0x03) << 4) | (b1 >> 4)],
        alpha[((b1 & 0x0F) << 2) | (b2 >> 6)],
        alpha[b2 & 0x3F],
    };
}

test "encode standard" {
    var buf: [100]u8 = undefined;

    // Standard test vectors from RFC 4648
    try std.testing.expectEqualStrings("", try encode("", &buf, .standard));
    try std.testing.expectEqualStrings("Zg==", try encode("f", &buf, .standard));
    try std.testing.expectEqualStrings("Zm8=", try encode("fo", &buf, .standard));
    try std.testing.expectEqualStrings("Zm9v", try encode("foo", &buf, .standard));
    try std.testing.expectEqualStrings("Zm9vYg==", try encode("foob", &buf, .standard));
    try std.testing.expectEqualStrings("Zm9vYmE=", try encode("fooba", &buf, .standard));
    try std.testing.expectEqualStrings("Zm9vYmFy", try encode("foobar", &buf, .standard));
}

test "encode standard no padding" {
    var buf: [100]u8 = undefined;

    try std.testing.expectEqualStrings("Zg", try encode("f", &buf, .standard_no_pad));
    try std.testing.expectEqualStrings("Zm8", try encode("fo", &buf, .standard_no_pad));
    try std.testing.expectEqualStrings("Zm9v", try encode("foo", &buf, .standard_no_pad));
}

test "encode url safe" {
    var buf: [100]u8 = undefined;

    // Test with bytes that produce +/ in standard
    const input = [_]u8{ 0xFB, 0xFF };
    const standard_result = try encode(&input, &buf, .standard);
    try std.testing.expect(std.mem.indexOf(u8, standard_result, "+") != null or
        std.mem.indexOf(u8, standard_result, "/") != null);

    const url_result = try encode(&input, &buf, .url_safe);
    try std.testing.expect(std.mem.indexOf(u8, url_result, "+") == null);
    try std.testing.expect(std.mem.indexOf(u8, url_result, "/") == null);
}

test "decode standard" {
    var buf: [100]u8 = undefined;

    try std.testing.expectEqualStrings("", try decode("", &buf, .standard));
    try std.testing.expectEqualStrings("f", try decode("Zg==", &buf, .standard));
    try std.testing.expectEqualStrings("fo", try decode("Zm8=", &buf, .standard));
    try std.testing.expectEqualStrings("foo", try decode("Zm9v", &buf, .standard));
    try std.testing.expectEqualStrings("foobar", try decode("Zm9vYmFy", &buf, .standard));
}

test "decode standard no padding" {
    var buf: [100]u8 = undefined;

    try std.testing.expectEqualStrings("f", try decode("Zg", &buf, .standard_no_pad));
    try std.testing.expectEqualStrings("fo", try decode("Zm8", &buf, .standard_no_pad));
}

test "decode invalid" {
    var buf: [100]u8 = undefined;

    // Invalid character
    try std.testing.expectError(error.InvalidCharacter, decode("!!!!", &buf, .standard));

    // Invalid length (with padding variant)
    try std.testing.expectError(error.InvalidLength, decode("abc", &buf, .standard));
}

test "isValid" {
    try std.testing.expect(isValid("Zm9vYmFy", .standard));
    try std.testing.expect(isValid("Zg==", .standard));
    try std.testing.expect(!isValid("Zg==", .standard_no_pad)); // Padding not allowed
    try std.testing.expect(!isValid("abc", .standard)); // Invalid length
    try std.testing.expect(!isValid("!!!!", .standard)); // Invalid chars
}

test "calcEncodedLen" {
    try std.testing.expectEqual(@as(usize, 0), calcEncodedLen(0, true));
    try std.testing.expectEqual(@as(usize, 4), calcEncodedLen(1, true));
    try std.testing.expectEqual(@as(usize, 4), calcEncodedLen(2, true));
    try std.testing.expectEqual(@as(usize, 4), calcEncodedLen(3, true));
    try std.testing.expectEqual(@as(usize, 8), calcEncodedLen(4, true));

    // Without padding
    try std.testing.expectEqual(@as(usize, 2), calcEncodedLen(1, false));
    try std.testing.expectEqual(@as(usize, 3), calcEncodedLen(2, false));
    try std.testing.expectEqual(@as(usize, 4), calcEncodedLen(3, false));
}

test "roundtrip" {
    const test_cases = [_][]const u8{
        "",
        "a",
        "ab",
        "abc",
        "abcd",
        "Hello, World!",
        &[_]u8{ 0x00, 0xFF, 0x80, 0x7F },
    };

    var encode_buf: [100]u8 = undefined;
    var decode_buf: [100]u8 = undefined;

    for (test_cases) |input| {
        // Standard
        const encoded = try encode(input, &encode_buf, .standard);
        const decoded = try decode(encoded, &decode_buf, .standard);
        try std.testing.expectEqualSlices(u8, input, decoded);

        // URL-safe
        const encoded_url = try encode(input, &encode_buf, .url_safe);
        const decoded_url = try decode(encoded_url, &decode_buf, .url_safe);
        try std.testing.expectEqualSlices(u8, input, decoded_url);
    }
}

test "convert variants" {
    var buf: [100]u8 = undefined;
    var convert_buf: [100]u8 = undefined;

    // Encode with standard, convert to URL-safe
    const standard = try encode(&[_]u8{ 0xFB, 0xFF, 0xBE }, &buf, .standard);
    const url_safe = try convert(standard, &convert_buf, .standard, .url_safe);

    // Verify no + or / in URL-safe version
    for (url_safe) |c| {
        try std.testing.expect(c != '+' and c != '/');
    }
}

test "stripWhitespace" {
    var buf: [100]u8 = undefined;
    const result = try stripWhitespace("Zm9v YmFy\n", &buf);
    try std.testing.expectEqualStrings("Zm9vYmFy", result);
}

test "buffer too small" {
    var small_buf: [2]u8 = undefined;
    try std.testing.expectError(error.BufferTooSmall, encode("hello", &small_buf, .standard));
}
