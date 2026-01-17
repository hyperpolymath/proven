// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe checksum and validation algorithms.

const std = @import("std");

/// CRC32 checksum (IEEE polynomial).
pub fn crc32(data: []const u8) u32 {
    return std.hash.Crc32.hash(data);
}

/// Adler-32 checksum.
pub fn adler32(data: []const u8) u32 {
    const MOD_ADLER: u32 = 65521;
    var a: u32 = 1;
    var b: u32 = 0;

    for (data) |byte| {
        a = (a + byte) % MOD_ADLER;
        b = (b + a) % MOD_ADLER;
    }

    return (b << 16) | a;
}

/// Luhn algorithm for credit card validation.
pub fn luhnCheck(digits: []const u8) bool {
    if (digits.len == 0) return false;

    var sum: u32 = 0;
    var alternate = false;

    var i: usize = digits.len;
    while (i > 0) {
        i -= 1;
        const c = digits[i];
        if (c < '0' or c > '9') return false;

        var digit: u32 = c - '0';
        if (alternate) {
            digit *= 2;
            if (digit > 9) digit -= 9;
        }
        sum += digit;
        alternate = !alternate;
    }

    return sum % 10 == 0;
}

/// ISBN-10 validation.
pub fn isValidISBN10(isbn: []const u8) bool {
    var clean: [10]u8 = undefined;
    var idx: usize = 0;

    for (isbn) |c| {
        if (c >= '0' and c <= '9') {
            if (idx >= 10) return false;
            clean[idx] = c;
            idx += 1;
        } else if ((c == 'X' or c == 'x') and idx == 9) {
            clean[idx] = 'X';
            idx += 1;
        }
    }

    if (idx != 10) return false;

    var sum: u32 = 0;
    for (clean, 0..) |c, i| {
        const val: u32 = if (c == 'X') 10 else c - '0';
        sum += val * @as(u32, @intCast(10 - i));
    }

    return sum % 11 == 0;
}

/// ISBN-13 validation.
pub fn isValidISBN13(isbn: []const u8) bool {
    var clean: [13]u8 = undefined;
    var idx: usize = 0;

    for (isbn) |c| {
        if (c >= '0' and c <= '9') {
            if (idx >= 13) return false;
            clean[idx] = c;
            idx += 1;
        }
    }

    if (idx != 13) return false;

    var sum: u32 = 0;
    for (clean, 0..) |c, i| {
        const val: u32 = c - '0';
        const mult: u32 = if (i % 2 == 0) 1 else 3;
        sum += val * mult;
    }

    return sum % 10 == 0;
}

/// XOR checksum.
pub fn xorChecksum(data: []const u8) u8 {
    var result: u8 = 0;
    for (data) |byte| {
        result ^= byte;
    }
    return result;
}

/// Fletcher-16 checksum.
pub fn fletcher16(data: []const u8) u16 {
    var sum1: u16 = 0;
    var sum2: u16 = 0;

    for (data) |byte| {
        sum1 = (sum1 + byte) % 255;
        sum2 = (sum2 + sum1) % 255;
    }

    return (sum2 << 8) | sum1;
}

test "crc32" {
    const result = crc32("hello");
    try std.testing.expect(result != 0);
}

test "luhnCheck" {
    try std.testing.expect(luhnCheck("4532015112830366")); // Valid test number
}

test "isValidISBN10" {
    try std.testing.expect(isValidISBN10("0-306-40615-2"));
}
