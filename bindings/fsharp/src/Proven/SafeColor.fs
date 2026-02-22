// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe color space operations via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeColor =

    /// Parse hex color string (#RRGGBB or #RGB).
    /// Returns the RGBColor struct on success, or None on failure.
    let parseHex (hex: string) : RGBColor option =
        let bytes = toUtf8 hex
        let result = FFI.proven_color_parse_hex(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result.Color
        else None

    /// Convert RGB color to HSL color space.
    let rgbToHsl (rgb: RGBColor) : HSLColor =
        FFI.proven_color_rgb_to_hsl(rgb)

    /// Format RGB color as hex string (e.g., "#ff0000").
    let toHex (rgb: RGBColor) : string option =
        FFI.proven_color_to_hex(rgb) |> stringResultToOption
