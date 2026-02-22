// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe color handling with parsing and conversion.
 *
 * Thin FFI wrapper around libproven's SafeColor module. Hex color
 * parsing, RGB-to-HSL conversion, and formatting are performed in
 * formally verified Idris 2 code. This module only marshals data
 * to/from the C ABI.
 */
module proven.safe_color;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// RGB color with values 0-255.
struct RGB
{
    ubyte r;
    ubyte g;
    ubyte b;

    /// Common color constants.
    static immutable RGB BLACK   = RGB(0, 0, 0);
    static immutable RGB WHITE   = RGB(255, 255, 255);
    static immutable RGB RED     = RGB(255, 0, 0);
    static immutable RGB GREEN   = RGB(0, 255, 0);
    static immutable RGB BLUE    = RGB(0, 0, 255);
}

/// HSL color (Hue, Saturation, Lightness).
struct HSL
{
    double h;  /// Hue: 0-360 degrees
    double s;  /// Saturation: 0-1
    double l;  /// Lightness: 0-1
}

/// Color parsing result.
struct ColorResult
{
    RGB color;
    string error;
    bool ok;

    static ColorResult success(RGB color)
    {
        return ColorResult(color, "", true);
    }

    static ColorResult failure(string error)
    {
        return ColorResult(RGB.init, error, false);
    }
}

/// Parse hex color string (#RRGGBB or #RGB). Leading # is optional.
ColorResult parseHexColor(string hex) @trusted nothrow
{
    if (hex.length == 0)
        return ColorResult.failure("Empty color string");

    auto result = proven_color_parse_hex(
        cast(const(ubyte)*) hex.ptr, hex.length
    );

    if (provenFailed(result.status))
        return ColorResult.failure("Invalid hex color");

    return ColorResult.success(RGB(result.color.r, result.color.g, result.color.b));
}

/// Convert RGB to HSL.
HSL rgbToHsl(RGB color) @trusted nothrow @nogc
{
    ProvenRGBColor c;
    c.r = color.r;
    c.g = color.g;
    c.b = color.b;

    auto result = proven_color_rgb_to_hsl(c);
    return HSL(result.h, result.s, result.l);
}

/// Format RGB as hex string ("#rrggbb", lowercase).
string toHex(RGB color) @trusted nothrow
{
    ProvenRGBColor c;
    c.r = color.r;
    c.g = color.g;
    c.b = color.b;

    auto result = proven_color_to_hex(c);
    return provenStringToD(result);
}
