// SPDX-License-Identifier: PMPL-1.0
// Safe color handling for V.
//
// Provides color parsing, conversion, and manipulation with
// protection against invalid values and proper clamping.

module proven

import math

// RGB color with values 0-255
pub struct RgbColor {
pub:
	r u8
	g u8
	b u8
}

// RGBA color with alpha 0-255
pub struct RgbaColor {
pub:
	r u8
	g u8
	b u8
	a u8
}

// HSL color with h: 0-360, s: 0-100, l: 0-100
pub struct HslColor {
pub:
	h f64
	s f64
	l f64
}

// HSV color with h: 0-360, s: 0-100, v: 0-100
pub struct HsvColor {
pub:
	h f64
	s f64
	v f64
}

// Create RGB color with validation
pub fn rgb(r int, g int, b int) RgbColor {
	return RgbColor{
		r: clamp_color(r)
		g: clamp_color(g)
		b: clamp_color(b)
	}
}

// Create RGBA color with validation
pub fn rgba(r int, g int, b int, a int) RgbaColor {
	return RgbaColor{
		r: clamp_color(r)
		g: clamp_color(g)
		b: clamp_color(b)
		a: clamp_color(a)
	}
}

// Clamp color component to 0-255
fn clamp_color(v int) u8 {
	if v < 0 {
		return 0
	}
	if v > 255 {
		return 255
	}
	return u8(v)
}

// Parse hex color string
pub fn parse_hex_color(s string) ?RgbColor {
	mut hex := s.trim_space()

	if hex.starts_with('#') {
		hex = hex[1..]
	}

	if hex.len == 3 {
		// Short form: #RGB -> #RRGGBB
		r := parse_hex_char(hex[0])? * 17
		g := parse_hex_char(hex[1])? * 17
		b := parse_hex_char(hex[2])? * 17
		return RgbColor{
			r: u8(r)
			g: u8(g)
			b: u8(b)
		}
	}

	if hex.len == 6 {
		r := parse_hex_pair(hex[0..2])?
		g := parse_hex_pair(hex[2..4])?
		b := parse_hex_pair(hex[4..6])?
		return RgbColor{
			r: r
			g: g
			b: b
		}
	}

	return none
}

// Parse hex character to value
fn parse_hex_char(c u8) ?int {
	if c >= `0` && c <= `9` {
		return int(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return int(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return int(c - `A` + 10)
	}
	return none
}

// Parse hex pair to value
fn parse_hex_pair(s string) ?u8 {
	if s.len != 2 {
		return none
	}
	high := parse_hex_char(s[0])?
	low := parse_hex_char(s[1])?
	return u8(high * 16 + low)
}

// Format color as hex string
pub fn format_hex(c RgbColor) string {
	return '#${c.r:02x}${c.g:02x}${c.b:02x}'
}

// Format color as hex string with alpha
pub fn format_hex_alpha(c RgbaColor) string {
	return '#${c.r:02x}${c.g:02x}${c.b:02x}${c.a:02x}'
}

// Convert RGB to HSL
pub fn rgb_to_hsl(c RgbColor) HslColor {
	r := f64(c.r) / 255.0
	g := f64(c.g) / 255.0
	b := f64(c.b) / 255.0

	max := math.max(math.max(r, g), b)
	min := math.min(math.min(r, g), b)
	delta := max - min

	l := (max + min) / 2.0

	if delta == 0.0 {
		return HslColor{
			h: 0.0
			s: 0.0
			l: l * 100.0
		}
	}

	s := if l > 0.5 { delta / (2.0 - max - min) } else { delta / (max + min) }

	mut h := 0.0
	if max == r {
		h = ((g - b) / delta) + (if g < b { 6.0 } else { 0.0 })
	} else if max == g {
		h = ((b - r) / delta) + 2.0
	} else {
		h = ((r - g) / delta) + 4.0
	}

	h = h * 60.0
	if h < 0.0 {
		h += 360.0
	}

	return HslColor{
		h: h
		s: s * 100.0
		l: l * 100.0
	}
}

// Convert HSL to RGB
pub fn hsl_to_rgb(c HslColor) RgbColor {
	h := c.h / 360.0
	s := c.s / 100.0
	l := c.l / 100.0

	if s == 0.0 {
		v := u8(l * 255.0)
		return RgbColor{
			r: v
			g: v
			b: v
		}
	}

	q := if l < 0.5 { l * (1.0 + s) } else { l + s - l * s }
	p := 2.0 * l - q

	r := hue_to_rgb(p, q, h + 1.0 / 3.0)
	g := hue_to_rgb(p, q, h)
	b := hue_to_rgb(p, q, h - 1.0 / 3.0)

	return RgbColor{
		r: u8(r * 255.0)
		g: u8(g * 255.0)
		b: u8(b * 255.0)
	}
}

// Helper for HSL to RGB conversion
fn hue_to_rgb(p f64, q f64, mut t f64) f64 {
	if t < 0.0 {
		t += 1.0
	}
	if t > 1.0 {
		t -= 1.0
	}

	if t < 1.0 / 6.0 {
		return p + (q - p) * 6.0 * t
	}
	if t < 1.0 / 2.0 {
		return q
	}
	if t < 2.0 / 3.0 {
		return p + (q - p) * (2.0 / 3.0 - t) * 6.0
	}
	return p
}

// Convert RGB to HSV
pub fn rgb_to_hsv(c RgbColor) HsvColor {
	r := f64(c.r) / 255.0
	g := f64(c.g) / 255.0
	b := f64(c.b) / 255.0

	max := math.max(math.max(r, g), b)
	min := math.min(math.min(r, g), b)
	delta := max - min

	v := max

	if max == 0.0 {
		return HsvColor{
			h: 0.0
			s: 0.0
			v: v * 100.0
		}
	}

	s := delta / max

	if delta == 0.0 {
		return HsvColor{
			h: 0.0
			s: s * 100.0
			v: v * 100.0
		}
	}

	mut h := 0.0
	if max == r {
		h = ((g - b) / delta) + (if g < b { 6.0 } else { 0.0 })
	} else if max == g {
		h = ((b - r) / delta) + 2.0
	} else {
		h = ((r - g) / delta) + 4.0
	}

	h = h * 60.0

	return HsvColor{
		h: h
		s: s * 100.0
		v: v * 100.0
	}
}

// Convert HSV to RGB
pub fn hsv_to_rgb(c HsvColor) RgbColor {
	h := c.h / 60.0
	s := c.s / 100.0
	v := c.v / 100.0

	if s == 0.0 {
		val := u8(v * 255.0)
		return RgbColor{
			r: val
			g: val
			b: val
		}
	}

	i := int(h)
	f := h - f64(i)
	p := v * (1.0 - s)
	q := v * (1.0 - s * f)
	t := v * (1.0 - s * (1.0 - f))

	mut r := 0.0
	mut g := 0.0
	mut b := 0.0

	match i % 6 {
		0 {
			r = v
			g = t
			b = p
		}
		1 {
			r = q
			g = v
			b = p
		}
		2 {
			r = p
			g = v
			b = t
		}
		3 {
			r = p
			g = q
			b = v
		}
		4 {
			r = t
			g = p
			b = v
		}
		else {
			r = v
			g = p
			b = q
		}
	}

	return RgbColor{
		r: u8(r * 255.0)
		g: u8(g * 255.0)
		b: u8(b * 255.0)
	}
}

// Lighten a color by percentage (0-100)
pub fn lighten(c RgbColor, percent f64) RgbColor {
	hsl := rgb_to_hsl(c)
	new_l := math.min(100.0, hsl.l + percent)
	return hsl_to_rgb(HslColor{
		h: hsl.h
		s: hsl.s
		l: new_l
	})
}

// Darken a color by percentage (0-100)
pub fn darken(c RgbColor, percent f64) RgbColor {
	hsl := rgb_to_hsl(c)
	new_l := math.max(0.0, hsl.l - percent)
	return hsl_to_rgb(HslColor{
		h: hsl.h
		s: hsl.s
		l: new_l
	})
}

// Saturate a color by percentage (0-100)
pub fn saturate(c RgbColor, percent f64) RgbColor {
	hsl := rgb_to_hsl(c)
	new_s := math.min(100.0, hsl.s + percent)
	return hsl_to_rgb(HslColor{
		h: hsl.h
		s: new_s
		l: hsl.l
	})
}

// Desaturate a color by percentage (0-100)
pub fn desaturate(c RgbColor, percent f64) RgbColor {
	hsl := rgb_to_hsl(c)
	new_s := math.max(0.0, hsl.s - percent)
	return hsl_to_rgb(HslColor{
		h: hsl.h
		s: new_s
		l: hsl.l
	})
}

// Get complementary color (opposite on color wheel)
pub fn complement(c RgbColor) RgbColor {
	hsl := rgb_to_hsl(c)
	new_h := math.fmod(hsl.h + 180.0, 360.0)
	return hsl_to_rgb(HslColor{
		h: new_h
		s: hsl.s
		l: hsl.l
	})
}

// Convert to grayscale
pub fn grayscale(c RgbColor) RgbColor {
	// Weighted average for human perception
	gray := u8(0.299 * f64(c.r) + 0.587 * f64(c.g) + 0.114 * f64(c.b))
	return RgbColor{
		r: gray
		g: gray
		b: gray
	}
}

// Invert a color
pub fn invert(c RgbColor) RgbColor {
	return RgbColor{
		r: 255 - c.r
		g: 255 - c.g
		b: 255 - c.b
	}
}

// Mix two colors
pub fn mix_colors(c1 RgbColor, c2 RgbColor, ratio f64) RgbColor {
	t := math.max(0.0, math.min(1.0, ratio))
	return RgbColor{
		r: u8(f64(c1.r) * (1.0 - t) + f64(c2.r) * t)
		g: u8(f64(c1.g) * (1.0 - t) + f64(c2.g) * t)
		b: u8(f64(c1.b) * (1.0 - t) + f64(c2.b) * t)
	}
}

// Calculate relative luminance (WCAG)
pub fn luminance(c RgbColor) f64 {
	r := linear_component(f64(c.r) / 255.0)
	g := linear_component(f64(c.g) / 255.0)
	b := linear_component(f64(c.b) / 255.0)
	return 0.2126 * r + 0.7152 * g + 0.0722 * b
}

// Convert sRGB component to linear
fn linear_component(c f64) f64 {
	if c <= 0.03928 {
		return c / 12.92
	}
	return math.pow((c + 0.055) / 1.055, 2.4)
}

// Calculate contrast ratio between two colors (WCAG)
pub fn contrast_ratio(c1 RgbColor, c2 RgbColor) f64 {
	l1 := luminance(c1)
	l2 := luminance(c2)

	lighter := math.max(l1, l2)
	darker := math.min(l1, l2)

	return (lighter + 0.05) / (darker + 0.05)
}

// Check if contrast ratio meets WCAG AA for normal text (4.5:1)
pub fn is_wcag_aa(c1 RgbColor, c2 RgbColor) bool {
	return contrast_ratio(c1, c2) >= 4.5
}

// Check if contrast ratio meets WCAG AAA for normal text (7:1)
pub fn is_wcag_aaa(c1 RgbColor, c2 RgbColor) bool {
	return contrast_ratio(c1, c2) >= 7.0
}

// Common colors
pub const color_black = RgbColor{
	r: 0
	g: 0
	b: 0
}
pub const color_white = RgbColor{
	r: 255
	g: 255
	b: 255
}
pub const color_red = RgbColor{
	r: 255
	g: 0
	b: 0
}
pub const color_green = RgbColor{
	r: 0
	g: 255
	b: 0
}
pub const color_blue = RgbColor{
	r: 0
	g: 0
	b: 255
}
