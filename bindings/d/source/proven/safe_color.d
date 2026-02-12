// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe color handling with validation and WCAG contrast calculations.
 * Provides RGB/RGBA types with safe conversions and accessibility-focused
 * contrast ratio calculations.
 */
module proven.safe_color;

import std.conv : to, ConvException;
import std.math : pow;
import std.typecons : Nullable, nullable;

/// RGB color with values 0-255.
struct RGB
{
    ubyte r;
    ubyte g;
    ubyte b;

    /// Common color constants.
    static immutable RGB BLACK = RGB(0, 0, 0);
    static immutable RGB WHITE = RGB(255, 255, 255);
    static immutable RGB RED = RGB(255, 0, 0);
    static immutable RGB GREEN = RGB(0, 255, 0);
    static immutable RGB BLUE = RGB(0, 0, 255);
    static immutable RGB YELLOW = RGB(255, 255, 0);
    static immutable RGB CYAN = RGB(0, 255, 255);
    static immutable RGB MAGENTA = RGB(255, 0, 255);

    /// Format as hex string (e.g., "#FF0000").
    string toHex() const pure @safe
    {
        import std.format : format;
        return format!"#%02X%02X%02X"(r, g, b);
    }

    /// Format as lowercase hex string.
    string toHexLower() const pure @safe
    {
        import std.format : format;
        return format!"#%02x%02x%02x"(r, g, b);
    }

    /// Format as CSS rgb() string.
    string toCss() const pure @safe
    {
        import std.format : format;
        return format!"rgb(%d, %d, %d)"(r, g, b);
    }

    /// Calculate relative luminance (WCAG formula).
    double luminance() const pure nothrow @safe @nogc
    {
        immutable rLin = gammaCorrect(r / 255.0);
        immutable gLin = gammaCorrect(g / 255.0);
        immutable bLin = gammaCorrect(b / 255.0);
        return 0.2126 * rLin + 0.7152 * gLin + 0.0722 * bLin;
    }
}

/// RGBA color with alpha channel.
struct RGBA
{
    ubyte r;
    ubyte g;
    ubyte b;
    ubyte a;

    /// Create from RGB with alpha.
    static RGBA fromRgb(RGB rgb, ubyte alpha) pure nothrow @safe @nogc
    {
        return RGBA(rgb.r, rgb.g, rgb.b, alpha);
    }

    /// Convert to RGB (discarding alpha).
    RGB toRgb() const pure nothrow @safe @nogc
    {
        return RGB(r, g, b);
    }

    /// Format as CSS rgba() string.
    string toCss() const pure @safe
    {
        import std.format : format;
        return format!"rgba(%d, %d, %d, %.3f)"(r, g, b, a / 255.0);
    }

    /// Format as hex string with alpha (e.g., "#FF0000FF").
    string toHex() const pure @safe
    {
        import std.format : format;
        return format!"#%02X%02X%02X%02X"(r, g, b, a);
    }
}

/// HSL color (Hue, Saturation, Lightness).
struct HSL
{
    /// Hue in degrees (0-360).
    float h;
    /// Saturation (0-1).
    float s;
    /// Lightness (0-1).
    float l;

    /// Convert to RGB.
    RGB toRgb() const pure nothrow @safe @nogc
    {
        if (s == 0.0f)
        {
            immutable ubyte gray = cast(ubyte)(l * 255);
            return RGB(gray, gray, gray);
        }

        immutable q = l < 0.5f ? l * (1 + s) : l + s - l * s;
        immutable p = 2 * l - q;
        immutable hNorm = h / 360.0f;

        return RGB(
            cast(ubyte)(hueToRgb(p, q, hNorm + 1.0f / 3.0f) * 255),
            cast(ubyte)(hueToRgb(p, q, hNorm) * 255),
            cast(ubyte)(hueToRgb(p, q, hNorm - 1.0f / 3.0f) * 255)
        );
    }
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

/// Parse hex color string (e.g., "#FF0000" or "FF0000" or "#F00").
ColorResult parseHexColor(string hex) pure @safe
{
    auto clean = hex;
    if (clean.length > 0 && clean[0] == '#')
        clean = clean[1 .. $];

    // Handle shorthand (e.g., "F00" -> "FF0000")
    if (clean.length == 3)
    {
        clean = [clean[0], clean[0], clean[1], clean[1], clean[2], clean[2]].idup;
    }

    if (clean.length != 6)
        return ColorResult.failure("Hex color must be 3 or 6 characters");

    try
    {
        immutable r = clean[0 .. 2].to!ubyte(16);
        immutable g = clean[2 .. 4].to!ubyte(16);
        immutable b = clean[4 .. 6].to!ubyte(16);
        return ColorResult.success(RGB(r, g, b));
    }
    catch (ConvException)
    {
        return ColorResult.failure("Invalid hex color characters");
    }
}

/// Calculate WCAG contrast ratio between two colors.
double contrastRatio(RGB color1, RGB color2) pure nothrow @safe @nogc
{
    immutable l1 = color1.luminance();
    immutable l2 = color2.luminance();
    immutable lighter = l1 > l2 ? l1 : l2;
    immutable darker = l1 > l2 ? l2 : l1;
    return (lighter + 0.05) / (darker + 0.05);
}

/// Check if contrast meets WCAG AA standard (4.5:1 for normal text).
bool meetsWcagAA(RGB color1, RGB color2) pure nothrow @safe @nogc
{
    return contrastRatio(color1, color2) >= 4.5;
}

/// Check if contrast meets WCAG AAA standard (7:1 for normal text).
bool meetsWcagAAA(RGB color1, RGB color2) pure nothrow @safe @nogc
{
    return contrastRatio(color1, color2) >= 7.0;
}

/// Check if contrast meets WCAG AA standard for large text (3:1).
bool meetsWcagAALargeText(RGB color1, RGB color2) pure nothrow @safe @nogc
{
    return contrastRatio(color1, color2) >= 3.0;
}

/// Blend two colors with alpha.
RGB blend(RGBA foreground, RGB background) pure nothrow @safe @nogc
{
    immutable alpha = foreground.a / 255.0;
    immutable invAlpha = 1.0 - alpha;

    return RGB(
        cast(ubyte)(foreground.r * alpha + background.r * invAlpha),
        cast(ubyte)(foreground.g * alpha + background.g * invAlpha),
        cast(ubyte)(foreground.b * alpha + background.b * invAlpha)
    );
}

/// Invert a color.
RGB invert(RGB color) pure nothrow @safe @nogc
{
    return RGB(cast(ubyte)(255 - color.r), cast(ubyte)(255 - color.g), cast(ubyte)(255 - color.b));
}

/// Convert RGB to grayscale using luminance.
RGB toGrayscale(RGB color) pure nothrow @safe @nogc
{
    immutable gray = cast(ubyte)(color.luminance() * 255);
    return RGB(gray, gray, gray);
}

/// Lighten a color by a percentage (0-100).
RGB lighten(RGB color, int percent) pure nothrow @safe @nogc
{
    immutable factor = 1.0 + percent / 100.0;
    return RGB(
        clampToByte(cast(int)(color.r * factor)),
        clampToByte(cast(int)(color.g * factor)),
        clampToByte(cast(int)(color.b * factor))
    );
}

/// Darken a color by a percentage (0-100).
RGB darken(RGB color, int percent) pure nothrow @safe @nogc
{
    immutable factor = 1.0 - percent / 100.0;
    return RGB(
        clampToByte(cast(int)(color.r * factor)),
        clampToByte(cast(int)(color.g * factor)),
        clampToByte(cast(int)(color.b * factor))
    );
}

/// Mix two colors by a ratio (0 = first color, 1 = second color).
RGB mix(RGB c1, RGB c2, double ratio) pure nothrow @safe @nogc
{
    immutable r = ratio < 0.0 ? 0.0 : (ratio > 1.0 ? 1.0 : ratio);
    immutable invR = 1.0 - r;
    return RGB(
        cast(ubyte)(c1.r * invR + c2.r * r),
        cast(ubyte)(c1.g * invR + c2.g * r),
        cast(ubyte)(c1.b * invR + c2.b * r)
    );
}

/// Gamma correction for luminance calculation.
private double gammaCorrect(double value) pure nothrow @safe @nogc
{
    if (value <= 0.03928)
        return value / 12.92;
    return pow((value + 0.055) / 1.055, 2.4);
}

/// Helper for HSL to RGB conversion.
private float hueToRgb(float p, float q, float t) pure nothrow @safe @nogc
{
    float tNorm = t;
    if (tNorm < 0)
        tNorm += 1;
    if (tNorm > 1)
        tNorm -= 1;
    if (tNorm < 1.0f / 6.0f)
        return p + (q - p) * 6 * tNorm;
    if (tNorm < 0.5f)
        return q;
    if (tNorm < 2.0f / 3.0f)
        return p + (q - p) * (2.0f / 3.0f - tNorm) * 6;
    return p;
}

/// Clamp integer to byte range.
private ubyte clampToByte(int value) pure nothrow @safe @nogc
{
    if (value < 0)
        return 0;
    if (value > 255)
        return 255;
    return cast(ubyte) value;
}

// Unit tests
unittest
{
    // Test hex parsing
    auto red = parseHexColor("#FF0000");
    assert(red.ok);
    assert(red.color.r == 255);
    assert(red.color.g == 0);
    assert(red.color.b == 0);

    // Test shorthand
    auto blue = parseHexColor("#00F");
    assert(blue.ok);
    assert(blue.color.b == 255);

    // Test hex output
    assert(RGB.RED.toHex() == "#FF0000");

    // Test contrast ratio
    auto ratio = contrastRatio(RGB.BLACK, RGB.WHITE);
    assert(ratio > 20.0);

    // Test WCAG compliance
    assert(meetsWcagAA(RGB.BLACK, RGB.WHITE));
    assert(meetsWcagAAA(RGB.BLACK, RGB.WHITE));

    // Test color manipulation
    auto inverted = invert(RGB.BLACK);
    assert(inverted.r == 255 && inverted.g == 255 && inverted.b == 255);

    // Test grayscale
    auto gray = toGrayscale(RGB.RED);
    assert(gray.r == gray.g && gray.g == gray.b);
}
