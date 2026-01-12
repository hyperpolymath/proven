// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe network operations for IP address validation and classification.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Represents an IPv4 address.
pub const IPv4 = struct {
    a: u8,
    b: u8,
    c: u8,
    d: u8,

    /// Format IPv4 as string.
    pub fn format(self: IPv4, allocator: Allocator) ![]u8 {
        return std.fmt.allocPrint(allocator, "{d}.{d}.{d}.{d}", .{ self.a, self.b, self.c, self.d });
    }
};

/// Error types for network operations.
pub const NetworkError = error{
    InvalidIPv4,
};

/// Parse an IPv4 address string.
pub fn parseIPv4(address: []const u8) NetworkError!IPv4 {
    var parts: [4]u8 = undefined;
    var part_count: usize = 0;
    var current_value: u16 = 0;
    var has_digit = false;

    for (address) |c| {
        if (c == '.') {
            if (!has_digit or part_count >= 3) return error.InvalidIPv4;
            if (current_value > 255) return error.InvalidIPv4;
            parts[part_count] = @intCast(current_value);
            part_count += 1;
            current_value = 0;
            has_digit = false;
        } else if (c >= '0' and c <= '9') {
            current_value = current_value * 10 + (c - '0');
            has_digit = true;
            if (current_value > 255) return error.InvalidIPv4;
        } else {
            return error.InvalidIPv4;
        }
    }

    // Handle last octet
    if (!has_digit or part_count != 3) return error.InvalidIPv4;
    if (current_value > 255) return error.InvalidIPv4;
    parts[part_count] = @intCast(current_value);

    return IPv4{
        .a = parts[0],
        .b = parts[1],
        .c = parts[2],
        .d = parts[3],
    };
}

/// Check if a string is a valid IPv4 address.
pub fn isValidIPv4(address: []const u8) bool {
    return parseIPv4(address) catch null != null;
}

/// Check if an IPv4 address is in a private range.
pub fn isPrivate(address: []const u8) bool {
    const ip = parseIPv4(address) catch return false;

    // 10.0.0.0/8
    if (ip.a == 10) return true;
    // 172.16.0.0/12
    if (ip.a == 172 and ip.b >= 16 and ip.b <= 31) return true;
    // 192.168.0.0/16
    if (ip.a == 192 and ip.b == 168) return true;

    return false;
}

/// Check if an IPv4 address is a loopback address (127.0.0.0/8).
pub fn isLoopback(address: []const u8) bool {
    const ip = parseIPv4(address) catch return false;
    return ip.a == 127;
}

/// Check if an IPv4 address is public (not private or loopback).
pub fn isPublic(address: []const u8) bool {
    return isValidIPv4(address) and !isPrivate(address) and !isLoopback(address);
}

test "isValidIPv4" {
    try std.testing.expect(isValidIPv4("192.168.1.1"));
    try std.testing.expect(!isValidIPv4("invalid"));
    try std.testing.expect(!isValidIPv4("256.1.1.1"));
}

test "isPrivate" {
    try std.testing.expect(isPrivate("192.168.1.1"));
    try std.testing.expect(isPrivate("10.0.0.1"));
    try std.testing.expect(isPrivate("172.16.0.1"));
    try std.testing.expect(!isPrivate("8.8.8.8"));
}

test "isLoopback" {
    try std.testing.expect(isLoopback("127.0.0.1"));
    try std.testing.expect(!isLoopback("192.168.1.1"));
}

test "isPublic" {
    try std.testing.expect(isPublic("8.8.8.8"));
    try std.testing.expect(!isPublic("192.168.1.1"));
}
