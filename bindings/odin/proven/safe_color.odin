// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"
import "core:strconv"
import "core:fmt"
import "core:math"

// RGB color with values 0-255.
RGB :: struct {
    r: u8,
    g: u8,
    b: u8,
}

// RGBA color with alpha channel.
RGBA :: struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

// HSL color representation.
HSL :: struct {
    h: f64,  // Hue (0-360)
    s: f64,  // Saturation (0-1)
    l: f64,  // Lightness (0-1)
}

// Common color constants.
COLOR_BLACK :: RGB{0, 0, 0}
COLOR_WHITE :: RGB{255, 255, 255}
COLOR_RED :: RGB{255, 0, 0}
COLOR_GREEN :: RGB{0, 255, 0}
COLOR_BLUE :: RGB{0, 0, 255}
COLOR_YELLOW :: RGB{255, 255, 0}
COLOR_CYAN :: RGB{0, 255, 255}
COLOR_MAGENTA :: RGB{255, 0, 255}

// Create RGB from hex string (e.g., "#FF0000" or "FF0000").
rgb_from_hex :: proc(hex: string) -> (color: RGB, ok: bool) {
    s := hex
    if len(s) > 0 && s[0] == '#' {
        s = s[1:]
    }

    if len(s) != 6 {
        return {}, false
    }

    // Parse each component
    r, r_ok := parse_hex_byte(s[0:2])
    if !r_ok { return {}, false }

    g, g_ok := parse_hex_byte(s[2:4])
    if !g_ok { return {}, false }

    b, b_ok := parse_hex_byte(s[4:6])
    if !b_ok { return {}, false }

    return RGB{r, g, b}, true
}

// Parse a two-character hex string to a byte.
@(private)
parse_hex_byte :: proc(s: string) -> (value: u8, ok: bool) {
    if len(s) != 2 { return 0, false }

    high := hex_char_val(s[0])
    low := hex_char_val(s[1])

    if high < 0 || low < 0 { return 0, false }

    return u8(high << 4 | low), true
}

// Get numeric value of hex character.
@(private)
hex_char_val :: proc(c: u8) -> int {
    switch c {
    case '0'..='9': return int(c - '0')
    case 'a'..='f': return int(c - 'a' + 10)
    case 'A'..='F': return int(c - 'A' + 10)
    case: return -1
    }
}

// Convert RGB to hex string.
rgb_to_hex :: proc(color: RGB, allocator := context.allocator) -> string {
    return fmt.aprintf("#%02X%02X%02X", color.r, color.g, color.b)
}

// Convert RGB to lowercase hex string.
rgb_to_hex_lower :: proc(color: RGB, allocator := context.allocator) -> string {
    return fmt.aprintf("#%02x%02x%02x", color.r, color.g, color.b)
}

// Apply gamma correction for luminance calculation.
@(private)
gamma_correct :: proc(value: f64) -> f64 {
    if value <= 0.03928 {
        return value / 12.92
    }
    return math.pow((value + 0.055) / 1.055, 2.4)
}

// Calculate relative luminance (WCAG formula).
luminance :: proc(color: RGB) -> f64 {
    r := gamma_correct(f64(color.r) / 255.0)
    g := gamma_correct(f64(color.g) / 255.0)
    b := gamma_correct(f64(color.b) / 255.0)
    return 0.2126 * r + 0.7152 * g + 0.0722 * b
}

// Calculate WCAG contrast ratio between two colors.
contrast_ratio :: proc(color1, color2: RGB) -> f64 {
    l1 := luminance(color1)
    l2 := luminance(color2)
    lighter := max(l1, l2)
    darker := min(l1, l2)
    return (lighter + 0.05) / (darker + 0.05)
}

// Check if contrast meets WCAG AA standard (4.5:1 for normal text).
meets_wcag_aa :: proc(color1, color2: RGB) -> bool {
    return contrast_ratio(color1, color2) >= 4.5
}

// Check if contrast meets WCAG AA large text standard (3:1).
meets_wcag_aa_large :: proc(color1, color2: RGB) -> bool {
    return contrast_ratio(color1, color2) >= 3.0
}

// Check if contrast meets WCAG AAA standard (7:1 for normal text).
meets_wcag_aaa :: proc(color1, color2: RGB) -> bool {
    return contrast_ratio(color1, color2) >= 7.0
}

// Create RGBA from RGB with alpha.
rgba_from_rgb :: proc(rgb: RGB, alpha: u8 = 255) -> RGBA {
    return RGBA{rgb.r, rgb.g, rgb.b, alpha}
}

// Convert RGBA to RGB (discarding alpha).
rgba_to_rgb :: proc(rgba: RGBA) -> RGB {
    return RGB{rgba.r, rgba.g, rgba.b}
}

// Blend foreground onto background.
blend :: proc(fg: RGBA, bg: RGB) -> RGB {
    alpha := f64(fg.a) / 255.0
    inv_alpha := 1.0 - alpha

    return RGB{
        r = u8(f64(fg.r) * alpha + f64(bg.r) * inv_alpha),
        g = u8(f64(fg.g) * alpha + f64(bg.g) * inv_alpha),
        b = u8(f64(fg.b) * alpha + f64(bg.b) * inv_alpha),
    }
}

// Convert RGB to HSL.
rgb_to_hsl :: proc(color: RGB) -> HSL {
    r := f64(color.r) / 255.0
    g := f64(color.g) / 255.0
    b := f64(color.b) / 255.0

    max_val := max(r, max(g, b))
    min_val := min(r, min(g, b))
    l := (max_val + min_val) / 2.0

    if max_val == min_val {
        return HSL{h = 0, s = 0, l = l}
    }

    d := max_val - min_val
    s := l > 0.5 ? d / (2.0 - max_val - min_val) : d / (max_val + min_val)

    h: f64
    if max_val == r {
        h = (g - b) / d + (g < b ? 6.0 : 0.0)
    } else if max_val == g {
        h = (b - r) / d + 2.0
    } else {
        h = (r - g) / d + 4.0
    }
    h /= 6.0

    return HSL{h = h * 360.0, s = s, l = l}
}

// Helper for HSL to RGB conversion.
@(private)
hue_to_rgb :: proc(p, q, t: f64) -> f64 {
    t_adj := t
    if t_adj < 0 { t_adj += 1 }
    if t_adj > 1 { t_adj -= 1 }

    if t_adj < 1.0/6.0 { return p + (q - p) * 6.0 * t_adj }
    if t_adj < 1.0/2.0 { return q }
    if t_adj < 2.0/3.0 { return p + (q - p) * (2.0/3.0 - t_adj) * 6.0 }
    return p
}

// Convert HSL to RGB.
hsl_to_rgb :: proc(hsl: HSL) -> RGB {
    h := hsl.h / 360.0
    s := hsl.s
    l := hsl.l

    if s == 0 {
        v := u8(l * 255.0)
        return RGB{v, v, v}
    }

    q := l < 0.5 ? l * (1.0 + s) : l + s - l * s
    p := 2.0 * l - q

    r := hue_to_rgb(p, q, h + 1.0/3.0)
    g := hue_to_rgb(p, q, h)
    b := hue_to_rgb(p, q, h - 1.0/3.0)

    return RGB{
        r = u8(r * 255.0),
        g = u8(g * 255.0),
        b = u8(b * 255.0),
    }
}

// Lighten a color by a percentage.
lighten :: proc(color: RGB, amount: f64) -> RGB {
    hsl := rgb_to_hsl(color)
    hsl.l = min(1.0, hsl.l + amount)
    return hsl_to_rgb(hsl)
}

// Darken a color by a percentage.
darken :: proc(color: RGB, amount: f64) -> RGB {
    hsl := rgb_to_hsl(color)
    hsl.l = max(0.0, hsl.l - amount)
    return hsl_to_rgb(hsl)
}

// Saturate a color by a percentage.
saturate :: proc(color: RGB, amount: f64) -> RGB {
    hsl := rgb_to_hsl(color)
    hsl.s = min(1.0, hsl.s + amount)
    return hsl_to_rgb(hsl)
}

// Desaturate a color by a percentage.
desaturate :: proc(color: RGB, amount: f64) -> RGB {
    hsl := rgb_to_hsl(color)
    hsl.s = max(0.0, hsl.s - amount)
    return hsl_to_rgb(hsl)
}

// Get complementary color.
complementary :: proc(color: RGB) -> RGB {
    hsl := rgb_to_hsl(color)
    hsl.h = f64(int(hsl.h + 180.0) % 360)
    return hsl_to_rgb(hsl)
}

// Invert a color.
invert :: proc(color: RGB) -> RGB {
    return RGB{
        r = 255 - color.r,
        g = 255 - color.g,
        b = 255 - color.b,
    }
}

// Convert to grayscale using luminance.
grayscale :: proc(color: RGB) -> RGB {
    lum := u8(luminance(color) * 255.0)
    return RGB{lum, lum, lum}
}

// Mix two colors.
mix_colors :: proc(color1, color2: RGB, weight: f64 = 0.5) -> RGB {
    w := float_clamp(weight, 0.0, 1.0)
    inv_w := 1.0 - w

    return RGB{
        r = u8(f64(color1.r) * w + f64(color2.r) * inv_w),
        g = u8(f64(color1.g) * w + f64(color2.g) * inv_w),
        b = u8(f64(color1.b) * w + f64(color2.b) * inv_w),
    }
}
