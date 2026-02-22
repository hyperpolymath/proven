// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeColor - Color operations that cannot crash.
////
//// Thin FFI wrapper over libproven proven_color_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// RGB color with 8-bit components.
pub type Rgb {
  Rgb(red: Int, green: Int, blue: Int)
}

/// HSL color.
pub type Hsl {
  Hsl(hue: Float, saturation: Float, lightness: Float)
}

/// Parse a hex color string (e.g., "#FF0000" or "#F00").
/// Leading # is optional. Supports 3-digit and 6-digit formats.
@external(erlang, "proven_nif", "color_parse_hex")
pub fn parse_hex(input: String) -> Result(Rgb, String)

/// Convert RGB to HSL.
@external(erlang, "proven_nif", "color_rgb_to_hsl")
pub fn rgb_to_hsl(red: Int, green: Int, blue: Int) -> Result(Hsl, String)

/// Format RGB as hex string (e.g., "#rrggbb").
@external(erlang, "proven_nif", "color_to_hex")
pub fn to_hex(red: Int, green: Int, blue: Int) -> Result(String, String)
