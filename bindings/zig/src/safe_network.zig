// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeNetwork - FFI bindings to libproven network operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for network operations.
pub const NetworkError = error{
    InvalidIPv4,
    ProvenError,
};

/// IPv4 address (4 octets).
pub const IPv4 = struct {
    octets: [4]u8,

    /// Format as dotted-decimal string.
    pub fn format(self: IPv4, buf: []u8) ![]const u8 {
        return std.fmt.bufPrint(buf, "{d}.{d}.{d}.{d}", .{
            self.octets[0], self.octets[1], self.octets[2], self.octets[3],
        });
    }
};

/// Parse an IPv4 address string via libproven.
pub fn parseIPv4(address: []const u8) NetworkError!IPv4 {
    const result = c.proven_network_parse_ipv4(address.ptr, address.len);
    if (result.status != c.PROVEN_OK) return error.InvalidIPv4;
    return IPv4{ .octets = result.address.octets };
}

/// Check if an IPv4 address is private (RFC 1918) via libproven.
pub fn isPrivate(addr: IPv4) bool {
    const c_addr = c.ProvenIPv4Address{ .octets = addr.octets };
    return c.proven_network_ipv4_is_private(c_addr);
}

/// Check if an IPv4 address is a loopback address (127.0.0.0/8) via libproven.
pub fn isLoopback(addr: IPv4) bool {
    const c_addr = c.ProvenIPv4Address{ .octets = addr.octets };
    return c.proven_network_ipv4_is_loopback(c_addr);
}

/// Convenience: parse and check if private.
pub fn isPrivateStr(address: []const u8) bool {
    const addr = parseIPv4(address) catch return false;
    return isPrivate(addr);
}

/// Convenience: parse and check if loopback.
pub fn isLoopbackStr(address: []const u8) bool {
    const addr = parseIPv4(address) catch return false;
    return isLoopback(addr);
}

test "parseIPv4" {
    const ip = try parseIPv4("192.168.1.1");
    try std.testing.expectEqual(@as(u8, 192), ip.octets[0]);
    try std.testing.expectEqual(@as(u8, 168), ip.octets[1]);
    try std.testing.expectError(error.InvalidIPv4, parseIPv4("invalid"));
}

test "isPrivate" {
    try std.testing.expect(isPrivateStr("192.168.1.1"));
    try std.testing.expect(isPrivateStr("10.0.0.1"));
    try std.testing.expect(!isPrivateStr("8.8.8.8"));
}

test "isLoopback" {
    try std.testing.expect(isLoopbackStr("127.0.0.1"));
    try std.testing.expect(!isLoopbackStr("192.168.1.1"));
}
