// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//! Marshaling for the pinned Idris RefC immediate Bits8 representation.
//! Values are tagged words, not pointers to allocated objects.
const std = @import("std");
const word = @import("bits8_word.zig");
const c = @cImport({
    @cInclude("_datatypes.h");
});
const shift = c.idris2_vp_int_shift;
comptime {
    if (@bitSizeOf(usize) != 64 or shift != 32)
        @compileError("RefC Bits8 proof profile requires 64-bit words and SDK shift 32");
}

pub fn encode(value: u8) ?*anyopaque {
    return @ptrFromInt(word.encodeWord(value));
}

pub fn decode(value: ?*anyopaque) error{InvalidRefcResult}!u8 {
    const raw = @intFromPtr(value);
    const payload = raw >> shift;
    if (payload > std.math.maxInt(u8)) return error.InvalidRefcResult;
    const byte: u8 = @intCast(payload);
    if (raw != @intFromPtr(encode(byte))) return error.InvalidRefcResult;
    return byte;
}

test "all canonical Bits8 words round trip through the actual SDK layout" {
    for (0..256) |n| {
        const byte: u8 = @intCast(n);
        try std.testing.expectEqual(byte, try decode(encode(byte)));
    }
}

test "null, noncanonical tags and out-of-range payloads are rejected" {
    try std.testing.expectError(error.InvalidRefcResult, decode(null));
    try std.testing.expectError(error.InvalidRefcResult, decode(@ptrFromInt(2)));
    try std.testing.expectError(error.InvalidRefcResult, decode(@ptrFromInt((@as(usize, 256) << shift) | 1)));
}

test "every unused bit and missing tag is rejected for every byte" {
    for (0..256) |n| {
        const canonical = @intFromPtr(encode(@intCast(n)));
        try std.testing.expectError(error.InvalidRefcResult, decode(@ptrFromInt(canonical ^ 1)));
        for (1..32) |bit| {
            try std.testing.expectError(error.InvalidRefcResult, decode(@ptrFromInt(canonical | (@as(usize, 1) << @intCast(bit)))));
        }
        for (40..64) |bit| {
            try std.testing.expectError(error.InvalidRefcResult, decode(@ptrFromInt(canonical | (@as(usize, 1) << @intCast(bit)))));
        }
    }
}
