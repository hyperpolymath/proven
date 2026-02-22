// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeHex - FFI bindings to libproven hex operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for hex operations.
pub const HexError = error{
    EncodingError,
    DecodingError,
    ProvenError,
};

/// Managed string from libproven.
pub const ProvenString = struct {
    ptr: [*]u8,
    len: usize,

    pub fn slice(self: ProvenString) []const u8 {
        return self.ptr[0..self.len];
    }

    pub fn deinit(self: ProvenString) void {
        c.proven_free_string(self.ptr);
    }
};

/// Decoded hex bytes from libproven.
pub const HexBytes = struct {
    raw: c.ProvenHexDecodeResult,

    pub fn slice(self: HexBytes) []const u8 {
        return self.raw.data[0..self.raw.length];
    }

    pub fn deinit(self: *HexBytes) void {
        c.proven_hex_free(&self.raw);
    }
};

/// Encode bytes to lowercase hex string via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn encode(bytes: []const u8) HexError!ProvenString {
    const result = c.proven_hex_encode(bytes.ptr, bytes.len, false);
    if (result.status != c.PROVEN_OK) return error.EncodingError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

/// Encode bytes to uppercase hex string via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn encodeUpper(bytes: []const u8) HexError!ProvenString {
    const result = c.proven_hex_encode(bytes.ptr, bytes.len, true);
    if (result.status != c.PROVEN_OK) return error.EncodingError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

/// Decode hex string to bytes via libproven.
/// Caller must call deinit() on the returned HexBytes.
pub fn decode(hex: []const u8) HexError!HexBytes {
    const result = c.proven_hex_decode(hex.ptr, hex.len);
    if (result.status != c.PROVEN_OK) return error.DecodingError;
    return HexBytes{ .raw = result };
}

test "encode" {
    const result = try encode(&[_]u8{ 0xFF, 0x00, 0xAB });
    defer result.deinit();
    try std.testing.expectEqualStrings("ff00ab", result.slice());
}

test "encodeUpper" {
    const result = try encodeUpper(&[_]u8{ 0xFF, 0x00, 0xAB });
    defer result.deinit();
    try std.testing.expectEqualStrings("FF00AB", result.slice());
}

test "decode" {
    var result = try decode("ff00ab");
    defer result.deinit();
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xFF, 0x00, 0xAB }, result.slice());
}
