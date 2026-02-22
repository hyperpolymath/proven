// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeColor provides color space conversions via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// RGB represents an RGB color with 8 bits per channel.
type RGB struct {
	R uint8
	G uint8
	B uint8
}

// HSL represents an HSL color.
type HSL struct {
	H float64 // Hue: 0-360
	S float64 // Saturation: 0-1
	L float64 // Lightness: 0-1
}

// ColorParseHex parses a hex color string (#RRGGBB or #RGB).
func ColorParseHex(hexColor string) (*RGB, error) {
	cs, length := cString(hexColor)
	defer unsafeFree(cs)

	result := C.proven_color_parse_hex(cs, length)
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}
	return &RGB{
		R: uint8(result.color.r),
		G: uint8(result.color.g),
		B: uint8(result.color.b),
	}, nil
}

// ColorRGBToHSL converts an RGB color to HSL.
func ColorRGBToHSL(rgb RGB) HSL {
	cRGB := C.RGBColor{r: C.uint8_t(rgb.R), g: C.uint8_t(rgb.G), b: C.uint8_t(rgb.B)}
	result := C.proven_color_rgb_to_hsl(cRGB)
	return HSL{
		H: float64(result.h),
		S: float64(result.s),
		L: float64(result.l),
	}
}

// ColorToHex formats an RGB color as a hex string.
func ColorToHex(rgb RGB) (string, error) {
	cRGB := C.RGBColor{r: C.uint8_t(rgb.R), g: C.uint8_t(rgb.G), b: C.uint8_t(rgb.B)}
	return goStringResult(C.proven_color_to_hex(cRGB))
}
