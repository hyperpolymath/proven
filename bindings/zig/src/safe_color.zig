// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe color handling with validation and WCAG contrast calculations.

const std = @import("std");

/// Error types for color operations.
pub const ColorError = error{
    InvalidHexColor,
    InvalidRGBValue,
    InvalidHSLValue,
};

/// RGB color with values 0-255.
pub const RGB = struct {
    r: u8,
    g: u8,
    b: u8,

    /// Create RGB from hex string (e.g., "#FF0000" or "FF0000").
    pub fn fromHex(hex: []const u8) ColorError!RGB {
        var start: usize = 0;
        if (hex.len > 0 and hex[0] == '#') start = 1;

        const clean = hex[start..];
        if (clean.len != 6) return error.InvalidHexColor;

        const r = std.fmt.parseInt(u8, clean[0..2], 16) catch return error.InvalidHexColor;
        const g = std.fmt.parseInt(u8, clean[2..4], 16) catch return error.InvalidHexColor;
        const b = std.fmt.parseInt(u8, clean[4..6], 16) catch return error.InvalidHexColor;

        return RGB{ .r = r, .g = g, .b = b };
    }

    /// Convert to hex string (writes to buffer).
    pub fn toHex(self: RGB, buf: []u8) []const u8 {
        if (buf.len < 7) return "";
        _ = std.fmt.bufPrint(buf, "#{X:0>2}{X:0>2}{X:0>2}", .{ self.r, self.g, self.b }) catch return "";
        return buf[0..7];
    }

    /// Calculate relative luminance (WCAG formula).
    pub fn luminance(self: RGB) f64 {
        const r = gammaCorrect(@as(f64, @floatFromInt(self.r)) / 255.0);
        const g = gammaCorrect(@as(f64, @floatFromInt(self.g)) / 255.0);
        const b = gammaCorrect(@as(f64, @floatFromInt(self.b)) / 255.0);
        return 0.2126 * r + 0.7152 * g + 0.0722 * b;
    }

    fn gammaCorrect(value: f64) f64 {
        if (value <= 0.03928) {
            return value / 12.92;
        }
        return std.math.pow(f64, (value + 0.055) / 1.055, 2.4);
    }
};

/// RGBA color with alpha channel.
pub const RGBA = struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8,

    pub fn fromRGB(rgb: RGB, alpha: u8) RGBA {
        return RGBA{ .r = rgb.r, .g = rgb.g, .b = rgb.b, .a = alpha };
    }

    pub fn toRGB(self: RGBA) RGB {
        return RGB{ .r = self.r, .g = self.g, .b = self.b };
    }
};

/// Calculate WCAG contrast ratio between two colors.
pub fn contrastRatio(color1: RGB, color2: RGB) f64 {
    const l1 = color1.luminance();
    const l2 = color2.luminance();
    const lighter = @max(l1, l2);
    const darker = @min(l1, l2);
    return (lighter + 0.05) / (darker + 0.05);
}

/// Check if contrast meets WCAG AA standard (4.5:1 for normal text).
pub fn meetsWCAG_AA(color1: RGB, color2: RGB) bool {
    return contrastRatio(color1, color2) >= 4.5;
}

/// Check if contrast meets WCAG AAA standard (7:1 for normal text).
pub fn meetsWCAG_AAA(color1: RGB, color2: RGB) bool {
    return contrastRatio(color1, color2) >= 7.0;
}

/// Blend two colors with alpha.
pub fn blend(fg: RGBA, bg: RGB) RGB {
    const alpha = @as(f64, @floatFromInt(fg.a)) / 255.0;
    const inv_alpha = 1.0 - alpha;

    return RGB{
        .r = @intFromFloat(@as(f64, @floatFromInt(fg.r)) * alpha + @as(f64, @floatFromInt(bg.r)) * inv_alpha),
        .g = @intFromFloat(@as(f64, @floatFromInt(fg.g)) * alpha + @as(f64, @floatFromInt(bg.g)) * inv_alpha),
        .b = @intFromFloat(@as(f64, @floatFromInt(fg.b)) * alpha + @as(f64, @floatFromInt(bg.b)) * inv_alpha),
    };
}

test "RGB fromHex" {
    const red = try RGB.fromHex("#FF0000");
    try std.testing.expectEqual(@as(u8, 255), red.r);
    try std.testing.expectEqual(@as(u8, 0), red.g);
}

test "contrastRatio" {
    const black = RGB{ .r = 0, .g = 0, .b = 0 };
    const white = RGB{ .r = 255, .g = 255, .b = 255 };
    try std.testing.expect(contrastRatio(black, white) > 20.0);
}
