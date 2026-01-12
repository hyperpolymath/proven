// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe hexadecimal encoding and decoding operations.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Hex error types
pub const HexError = error{
    InvalidCharacter,
    OddLength,
    BufferTooSmall,
    OutOfMemory,
};

/// Check if character is valid hexadecimal
pub fn isHexChar(c: u8) bool {
    return (c >= '0' and c <= '9') or
        (c >= 'a' and c <= 'f') or
        (c >= 'A' and c <= 'F');
}

/// Convert hex character to nibble value (0-15)
pub fn hexCharToNibble(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => null,
    };
}

/// Convert nibble (0-15) to lowercase hex character
pub fn nibbleToHexChar(n: u8) u8 {
    return if (n < 10) '0' + n else 'a' + n - 10;
}

/// Convert nibble (0-15) to uppercase hex character
pub fn nibbleToHexCharUpper(n: u8) u8 {
    return if (n < 10) '0' + n else 'A' + n - 10;
}

/// Encode single byte to two hex characters (lowercase)
pub fn encodeByte(b: u8, out: *[2]u8) void {
    out[0] = nibbleToHexChar(b >> 4);
    out[1] = nibbleToHexChar(b & 0x0F);
}

/// Encode single byte to two hex characters (uppercase)
pub fn encodeByteUpper(b: u8, out: *[2]u8) void {
    out[0] = nibbleToHexCharUpper(b >> 4);
    out[1] = nibbleToHexCharUpper(b & 0x0F);
}

/// Encode bytes to hex string (lowercase)
pub fn encode(bytes: []const u8, out: []u8) HexError![]const u8 {
    if (out.len < bytes.len * 2) return error.BufferTooSmall;

    for (bytes, 0..) |b, i| {
        encodeByte(b, out[i * 2 ..][0..2]);
    }

    return out[0 .. bytes.len * 2];
}

/// Encode bytes to hex string (uppercase)
pub fn encodeUpper(bytes: []const u8, out: []u8) HexError![]const u8 {
    if (out.len < bytes.len * 2) return error.BufferTooSmall;

    for (bytes, 0..) |b, i| {
        encodeByteUpper(b, out[i * 2 ..][0..2]);
    }

    return out[0 .. bytes.len * 2];
}

/// Encode bytes to allocated hex string
pub fn encodeAlloc(allocator: Allocator, bytes: []const u8) HexError![]u8 {
    const result = allocator.alloc(u8, bytes.len * 2) catch return error.OutOfMemory;
    _ = try encode(bytes, result);
    return result;
}

/// Decode two hex characters to a byte
pub fn decodeHexByte(h1: u8, h2: u8) ?u8 {
    const n1 = hexCharToNibble(h1) orelse return null;
    const n2 = hexCharToNibble(h2) orelse return null;
    return (n1 << 4) | n2;
}

/// Decode hex string to bytes
pub fn decode(hex: []const u8, out: []u8) HexError![]const u8 {
    if (hex.len % 2 != 0) return error.OddLength;
    if (out.len < hex.len / 2) return error.BufferTooSmall;

    var i: usize = 0;
    while (i < hex.len) : (i += 2) {
        out[i / 2] = decodeHexByte(hex[i], hex[i + 1]) orelse return error.InvalidCharacter;
    }

    return out[0 .. hex.len / 2];
}

/// Decode hex string to allocated bytes
pub fn decodeAlloc(allocator: Allocator, hex: []const u8) HexError![]u8 {
    if (hex.len % 2 != 0) return error.OddLength;

    const result = allocator.alloc(u8, hex.len / 2) catch return error.OutOfMemory;
    errdefer allocator.free(result);

    _ = try decode(hex, result);
    return result;
}

/// Validate hex string
pub fn isValidHex(str: []const u8) bool {
    for (str) |c| {
        if (!isHexChar(c)) return false;
    }
    return true;
}

/// Validate hex string with even length (for byte decoding)
pub fn isValidHexBytes(str: []const u8) bool {
    return str.len % 2 == 0 and isValidHex(str);
}

/// Format hex with spaces between bytes
pub fn formatSpaced(hex: []const u8, out: []u8) HexError![]const u8 {
    if (hex.len % 2 != 0) return error.OddLength;
    const pairs = hex.len / 2;
    if (pairs == 0) return "";

    const needed = pairs * 2 + (pairs - 1); // pairs of 2 chars + spaces between
    if (out.len < needed) return error.BufferTooSmall;

    var out_idx: usize = 0;
    var i: usize = 0;
    while (i < hex.len) : (i += 2) {
        if (i > 0) {
            out[out_idx] = ' ';
            out_idx += 1;
        }
        out[out_idx] = hex[i];
        out[out_idx + 1] = hex[i + 1];
        out_idx += 2;
    }

    return out[0..out_idx];
}

/// Format hex with colons between bytes
pub fn formatColons(hex: []const u8, out: []u8) HexError![]const u8 {
    if (hex.len % 2 != 0) return error.OddLength;
    const pairs = hex.len / 2;
    if (pairs == 0) return "";

    const needed = pairs * 2 + (pairs - 1);
    if (out.len < needed) return error.BufferTooSmall;

    var out_idx: usize = 0;
    var i: usize = 0;
    while (i < hex.len) : (i += 2) {
        if (i > 0) {
            out[out_idx] = ':';
            out_idx += 1;
        }
        out[out_idx] = hex[i];
        out[out_idx + 1] = hex[i + 1];
        out_idx += 2;
    }

    return out[0..out_idx];
}

/// Constant-time comparison of hex strings
pub fn constantTimeEq(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;

    var diff: u8 = 0;
    for (a, b) |ca, cb| {
        const la = std.ascii.toLower(ca);
        const lb = std.ascii.toLower(cb);
        diff |= la ^ lb;
    }

    return diff == 0;
}

/// Convert integer to hex string with minimum width
pub fn intToHex(value: u64, min_width: usize, out: []u8) HexError![]const u8 {
    var temp: [16]u8 = undefined;
    var len: usize = 0;

    var v = value;
    if (v == 0) {
        temp[0] = '0';
        len = 1;
    } else {
        while (v > 0) : (len += 1) {
            temp[15 - len] = nibbleToHexChar(@intCast(v & 0xF));
            v >>= 4;
        }
    }

    const actual_len = @max(len, min_width);
    if (out.len < actual_len) return error.BufferTooSmall;

    // Pad with zeros
    const padding = actual_len - len;
    for (0..padding) |i| {
        out[i] = '0';
    }

    // Copy digits
    for (0..len) |i| {
        out[padding + i] = temp[16 - len + i];
    }

    return out[0..actual_len];
}

test "encode bytes" {
    var buf: [10]u8 = undefined;
    const result = try encode(&[_]u8{ 0xFF, 0x00, 0xAB }, &buf);
    try std.testing.expectEqualStrings("ff00ab", result);
}

test "decode hex" {
    var buf: [10]u8 = undefined;
    const result = try decode("ff00ab", &buf);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xFF, 0x00, 0xAB }, result);
}

test "isValidHex" {
    try std.testing.expect(isValidHex("abcdef0123456789"));
    try std.testing.expect(!isValidHex("xyz"));
    try std.testing.expect(isValidHexBytes("aabb"));
    try std.testing.expect(!isValidHexBytes("aab")); // odd length
}

test "format spaced" {
    var buf: [20]u8 = undefined;
    const result = try formatSpaced("aabbcc", &buf);
    try std.testing.expectEqualStrings("aa bb cc", result);
}

test "constantTimeEq" {
    try std.testing.expect(constantTimeEq("aabb", "AABB"));
    try std.testing.expect(constantTimeEq("ff00", "FF00"));
    try std.testing.expect(!constantTimeEq("aabb", "aab0"));
}

test "intToHex" {
    var buf: [20]u8 = undefined;
    try std.testing.expectEqualStrings("ff", try intToHex(255, 2, &buf));
    try std.testing.expectEqualStrings("00ff", try intToHex(255, 4, &buf));
    try std.testing.expectEqualStrings("0", try intToHex(0, 1, &buf));
}
