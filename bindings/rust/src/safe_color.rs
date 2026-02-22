// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe color operations via libproven FFI.
//!
//! Parses hex colors, converts between RGB and HSL color spaces.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// RGB color (red, green, blue).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Rgb {
    /// Red component (0-255).
    pub r: u8,
    /// Green component (0-255).
    pub g: u8,
    /// Blue component (0-255).
    pub b: u8,
}

/// RGBA color (red, green, blue, alpha).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Rgba {
    /// Red component (0-255).
    pub r: u8,
    /// Green component (0-255).
    pub g: u8,
    /// Blue component (0-255).
    pub b: u8,
    /// Alpha component (0-255).
    pub a: u8,
}

/// HSL color (hue, saturation, lightness).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Hsl {
    /// Hue in degrees (0-360).
    pub h: f64,
    /// Saturation (0-1).
    pub s: f64,
    /// Lightness (0-1).
    pub l: f64,
}

/// Safe color operations.
pub struct SafeColor;

impl SafeColor {
    /// Parse a hex color string (e.g., "#RRGGBB" or "#RGB").
    pub fn parse_hex(input: &str) -> Result<Rgb> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer and returns a
        // value-type struct.
        let result = unsafe {
            ffi::proven_color_parse_hex(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;
        Ok(Rgb {
            r: result.color.r,
            g: result.color.g,
            b: result.color.b,
        })
    }

    /// Convert RGB to HSL color space.
    pub fn rgb_to_hsl(rgb: &Rgb) -> Hsl {
        let ffi_rgb = ffi::RGBColor {
            r: rgb.r,
            g: rgb.g,
            b: rgb.b,
        };
        // SAFETY: proven_color_rgb_to_hsl takes a value-type RGBColor struct;
        // always safe to call.
        let result = unsafe { ffi::proven_color_rgb_to_hsl(ffi_rgb) };
        Hsl {
            h: result.h,
            s: result.s,
            l: result.l,
        }
    }

    /// Format RGB as hex string (e.g., "#FF8800").
    pub fn to_hex(rgb: &Rgb) -> Result<String> {
        let ffi_rgb = ffi::RGBColor {
            r: rgb.r,
            g: rgb.g,
            b: rgb.b,
        };
        // SAFETY: proven_color_to_hex takes a value-type RGBColor struct;
        // always safe to call. Returns a StringResult that we free.
        let result = unsafe { ffi::proven_color_to_hex(ffi_rgb) };
        core::string_result_to_result(result)
    }
}
