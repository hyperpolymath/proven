// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeColor - FFI bindings to libproven color operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for color operations.
pub const ColorError = error{
    ParseFailure,
    FormatError,
    ProvenError,
};

/// RGB color with values 0-255.
pub const RGB = struct {
    r: u8,
    g: u8,
    b: u8,
};

/// HSL color.
pub const HSL = struct {
    h: f64,
    s: f64,
    l: f64,
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

/// Parse hex color string (#RRGGBB or #RGB) via libproven.
pub fn parseHex(hex: []const u8) ColorError!RGB {
    const result = c.proven_color_parse_hex(hex.ptr, hex.len);
    if (result.status != c.PROVEN_OK) return error.ParseFailure;
    return RGB{ .r = result.color.r, .g = result.color.g, .b = result.color.b };
}

/// Convert RGB to HSL via libproven.
pub fn rgbToHsl(rgb: RGB) HSL {
    const c_rgb = c.ProvenRGBColor{ .r = rgb.r, .g = rgb.g, .b = rgb.b };
    const result = c.proven_color_rgb_to_hsl(c_rgb);
    return HSL{ .h = result.h, .s = result.s, .l = result.l };
}

/// Format RGB as hex string ("#rrggbb", lowercase) via libproven.
/// Caller must call deinit() on the returned ProvenString.
pub fn toHex(rgb: RGB) ColorError!ProvenString {
    const c_rgb = c.ProvenRGBColor{ .r = rgb.r, .g = rgb.g, .b = rgb.b };
    const result = c.proven_color_to_hex(c_rgb);
    if (result.status != c.PROVEN_OK) return error.FormatError;
    return ProvenString{ .ptr = @ptrCast(result.value), .len = result.length };
}

test "parseHex" {
    const red = try parseHex("#FF0000");
    try std.testing.expectEqual(@as(u8, 255), red.r);
    try std.testing.expectEqual(@as(u8, 0), red.g);
    try std.testing.expectEqual(@as(u8, 0), red.b);
}

test "toHex" {
    const result = try toHex(RGB{ .r = 255, .g = 0, .b = 0 });
    defer result.deinit();
    _ = result.slice();
}
