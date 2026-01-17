// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"fmt"
	"math"
	"regexp"
	"strconv"
	"strings"
)

// RGB represents an RGB color.
type RGB struct {
	R, G, B uint8
}

// NewRGB creates an RGB color.
func NewRGB(r, g, b uint8) RGB {
	return RGB{R: r, G: g, B: b}
}

// ParseHex parses a hex color string.
func ParseHex(hex string) (RGB, bool) {
	s := strings.TrimPrefix(strings.TrimSpace(hex), "#")

	if len(s) == 3 {
		s = string(s[0]) + string(s[0]) + string(s[1]) + string(s[1]) + string(s[2]) + string(s[2])
	}

	if len(s) != 6 {
		return RGB{}, false
	}

	matched, _ := regexp.MatchString("^[0-9a-fA-F]+$", s)
	if !matched {
		return RGB{}, false
	}

	r, _ := strconv.ParseUint(s[0:2], 16, 8)
	g, _ := strconv.ParseUint(s[2:4], 16, 8)
	b, _ := strconv.ParseUint(s[4:6], 16, 8)

	return RGB{R: uint8(r), G: uint8(g), B: uint8(b)}, true
}

// ToHex converts to hex string.
func (c RGB) ToHex() string {
	return fmt.Sprintf("#%02x%02x%02x", c.R, c.G, c.B)
}

// ToCss converts to CSS rgb() string.
func (c RGB) ToCss() string {
	return fmt.Sprintf("rgb(%d, %d, %d)", c.R, c.G, c.B)
}

// Luminance calculates relative luminance (WCAG).
func (c RGB) Luminance() float64 {
	r := linearize(float64(c.R) / 255)
	g := linearize(float64(c.G) / 255)
	b := linearize(float64(c.B) / 255)
	return 0.2126*r + 0.7152*g + 0.0722*b
}

func linearize(v float64) float64 {
	if v <= 0.03928 {
		return v / 12.92
	}
	return math.Pow((v+0.055)/1.055, 2.4)
}

// Contrast calculates contrast ratio with another color.
func (c RGB) Contrast(other RGB) float64 {
	l1 := c.Luminance()
	l2 := other.Luminance()
	lighter := math.Max(l1, l2)
	darker := math.Min(l1, l2)
	return (lighter + 0.05) / (darker + 0.05)
}

// MeetsWCAG_AA checks WCAG AA compliance for normal text.
func (c RGB) MeetsWCAG_AA(background RGB) bool {
	return c.Contrast(background) >= 4.5
}

// MeetsWCAG_AAA checks WCAG AAA compliance for normal text.
func (c RGB) MeetsWCAG_AAA(background RGB) bool {
	return c.Contrast(background) >= 7
}

// MeetsWCAG_AALarge checks WCAG AA compliance for large text.
func (c RGB) MeetsWCAG_AALarge(background RGB) bool {
	return c.Contrast(background) >= 3
}

// Mix blends with another color.
func (c RGB) Mix(other RGB, amount float64) RGB {
	t := math.Max(0, math.Min(1, amount))
	return RGB{
		R: uint8(float64(c.R) + (float64(other.R)-float64(c.R))*t),
		G: uint8(float64(c.G) + (float64(other.G)-float64(c.G))*t),
		B: uint8(float64(c.B) + (float64(other.B)-float64(c.B))*t),
	}
}

// Lighten lightens the color.
func (c RGB) Lighten(amount float64) RGB {
	return c.Mix(White, amount)
}

// Darken darkens the color.
func (c RGB) Darken(amount float64) RGB {
	return c.Mix(Black, amount)
}

// Invert inverts the color.
func (c RGB) Invert() RGB {
	return RGB{R: 255 - c.R, G: 255 - c.G, B: 255 - c.B}
}

// Grayscale converts to grayscale.
func (c RGB) Grayscale() RGB {
	gray := uint8(0.299*float64(c.R) + 0.587*float64(c.G) + 0.114*float64(c.B))
	return RGB{R: gray, G: gray, B: gray}
}

// RGBA represents an RGBA color.
type RGBA struct {
	R, G, B uint8
	A       float64
}

// NewRGBA creates an RGBA color.
func NewRGBA(r, g, b uint8, a float64) RGBA {
	if a < 0 {
		a = 0
	} else if a > 1 {
		a = 1
	}
	return RGBA{R: r, G: g, B: b, A: a}
}

// ToRGB converts to RGB (drops alpha).
func (c RGBA) ToRGB() RGB {
	return RGB{R: c.R, G: c.G, B: c.B}
}

// ToCss converts to CSS rgba() string.
func (c RGBA) ToCss() string {
	return fmt.Sprintf("rgba(%d, %d, %d, %.2f)", c.R, c.G, c.B, c.A)
}

// BlendOver blends over a background color.
func (c RGBA) BlendOver(background RGB) RGB {
	return RGB{
		R: uint8(float64(c.R)*c.A + float64(background.R)*(1-c.A)),
		G: uint8(float64(c.G)*c.A + float64(background.G)*(1-c.A)),
		B: uint8(float64(c.B)*c.A + float64(background.B)*(1-c.A)),
	}
}

// Common colors.
var (
	Black   = RGB{0, 0, 0}
	White   = RGB{255, 255, 255}
	Red     = RGB{255, 0, 0}
	Green   = RGB{0, 255, 0}
	Blue    = RGB{0, 0, 255}
	Yellow  = RGB{255, 255, 0}
	Cyan    = RGB{0, 255, 255}
	Magenta = RGB{255, 0, 255}
)
