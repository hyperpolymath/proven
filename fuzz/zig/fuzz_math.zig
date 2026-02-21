// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//! Fuzz targets for SafeMath operations
//! Tests integer operations for crashes and unexpected behavior

const std = @import("std");

/// Fuzz target: Integer parsing from arbitrary bytes
pub export fn fuzz_parse_integer(data: [*]const u8, len: usize) callconv(.C) c_int {
    if (len < 1) return 0;

    const slice = data[0..len];

    // Try to parse as signed integer
    _ = std.fmt.parseInt(i64, slice, 10) catch {
        // Expected: invalid input should return error, not crash
        return 0;
    };

    return 0;
}

/// Fuzz target: Safe addition overflow detection
pub export fn fuzz_safe_add(data: [*]const u8, len: usize) callconv(.C) c_int {
    if (len < 16) return 0;

    const a = std.mem.readInt(i64, data[0..8], .little);
    const b = std.mem.readInt(i64, data[8..16], .little);

    // Safe addition should never panic
    const result = @addWithOverflow(a, b);
    _ = result;

    return 0;
}

/// Fuzz target: Safe multiplication overflow detection
pub export fn fuzz_safe_mul(data: [*]const u8, len: usize) callconv(.C) c_int {
    if (len < 16) return 0;

    const a = std.mem.readInt(i64, data[0..8], .little);
    const b = std.mem.readInt(i64, data[8..16], .little);

    // Safe multiplication should never panic
    const result = @mulWithOverflow(a, b);
    _ = result;

    return 0;
}

/// Fuzz target: Safe division (no division by zero)
pub export fn fuzz_safe_div(data: [*]const u8, len: usize) callconv(.C) c_int {
    if (len < 16) return 0;

    const a = std.mem.readInt(i64, data[0..8], .little);
    const b = std.mem.readInt(i64, data[8..16], .little);

    // Never divide by zero
    if (b == 0) return 0;
    if (a == std.math.minInt(i64) and b == -1) return 0; // Avoid overflow

    const result = @divTrunc(a, b);
    _ = result;

    return 0;
}

/// Fuzz target: Bounded integer creation
pub export fn fuzz_bounded_int(data: [*]const u8, len: usize) callconv(.C) c_int {
    if (len < 24) return 0;

    const min = std.mem.readInt(i64, data[0..8], .little);
    const max = std.mem.readInt(i64, data[8..16], .little);
    const value = std.mem.readInt(i64, data[16..24], .little);

    // Validate bounds
    if (min > max) return 0;

    // Check if value is in bounds
    if (value >= min and value <= max) {
        // Valid bounded integer created
        return 0;
    }

    return 0;
}

// Main entry for testing
pub fn main() !void {
    std.debug.print("Fuzz targets for SafeMath loaded.\n", .{});
    std.debug.print("Run with: zig build fuzz\n", .{});
}
