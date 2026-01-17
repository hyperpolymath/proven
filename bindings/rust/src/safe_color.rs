// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe color handling with validation and WCAG contrast calculations.
//!
//! Provides RGB/RGBA color types with safe conversions and
//! accessibility-focused contrast ratio calculations.

use crate::{Error, Result};

/// RGB color with values 0-255.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RGB {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl RGB {
    /// Create a new RGB color.
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }

    /// Create RGB from hex string (e.g., "#FF0000" or "FF0000").
    pub fn from_hex(hex: &str) -> Result<Self> {
        let clean = hex.strip_prefix('#').unwrap_or(hex);
        if clean.len() != 6 {
            return Err(Error::InvalidInput("Hex color must be 6 characters".into()));
        }

        let r = u8::from_str_radix(&clean[0..2], 16)
            .map_err(|_| Error::InvalidInput("Invalid red component".into()))?;
        let g = u8::from_str_radix(&clean[2..4], 16)
            .map_err(|_| Error::InvalidInput("Invalid green component".into()))?;
        let b = u8::from_str_radix(&clean[4..6], 16)
            .map_err(|_| Error::InvalidInput("Invalid blue component".into()))?;

        Ok(Self { r, g, b })
    }

    /// Convert to hex string.
    pub fn to_hex(&self) -> String {
        format!("#{:02X}{:02X}{:02X}", self.r, self.g, self.b)
    }

    /// Calculate relative luminance (WCAG formula).
    pub fn luminance(&self) -> f64 {
        let r = gamma_correct(self.r as f64 / 255.0);
        let g = gamma_correct(self.g as f64 / 255.0);
        let b = gamma_correct(self.b as f64 / 255.0);
        0.2126 * r + 0.7152 * g + 0.0722 * b
    }

    /// Common color constants.
    pub const BLACK: RGB = RGB { r: 0, g: 0, b: 0 };
    pub const WHITE: RGB = RGB { r: 255, g: 255, b: 255 };
    pub const RED: RGB = RGB { r: 255, g: 0, b: 0 };
    pub const GREEN: RGB = RGB { r: 0, g: 255, b: 0 };
    pub const BLUE: RGB = RGB { r: 0, g: 0, b: 255 };
}

/// RGBA color with alpha channel.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RGBA {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl RGBA {
    /// Create a new RGBA color.
    pub fn new(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }

    /// Create from RGB with alpha.
    pub fn from_rgb(rgb: RGB, alpha: u8) -> Self {
        Self {
            r: rgb.r,
            g: rgb.g,
            b: rgb.b,
            a: alpha,
        }
    }

    /// Convert to RGB (discarding alpha).
    pub fn to_rgb(&self) -> RGB {
        RGB {
            r: self.r,
            g: self.g,
            b: self.b,
        }
    }
}

fn gamma_correct(value: f64) -> f64 {
    if value <= 0.03928 {
        value / 12.92
    } else {
        ((value + 0.055) / 1.055).powf(2.4)
    }
}

/// Calculate WCAG contrast ratio between two colors.
pub fn contrast_ratio(color1: RGB, color2: RGB) -> f64 {
    let l1 = color1.luminance();
    let l2 = color2.luminance();
    let lighter = l1.max(l2);
    let darker = l1.min(l2);
    (lighter + 0.05) / (darker + 0.05)
}

/// Check if contrast meets WCAG AA standard (4.5:1 for normal text).
pub fn meets_wcag_aa(color1: RGB, color2: RGB) -> bool {
    contrast_ratio(color1, color2) >= 4.5
}

/// Check if contrast meets WCAG AAA standard (7:1 for normal text).
pub fn meets_wcag_aaa(color1: RGB, color2: RGB) -> bool {
    contrast_ratio(color1, color2) >= 7.0
}

/// Blend two colors with alpha.
pub fn blend(fg: RGBA, bg: RGB) -> RGB {
    let alpha = fg.a as f64 / 255.0;
    let inv_alpha = 1.0 - alpha;

    RGB {
        r: (fg.r as f64 * alpha + bg.r as f64 * inv_alpha) as u8,
        g: (fg.g as f64 * alpha + bg.g as f64 * inv_alpha) as u8,
        b: (fg.b as f64 * alpha + bg.b as f64 * inv_alpha) as u8,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rgb_from_hex() {
        let red = RGB::from_hex("#FF0000").unwrap();
        assert_eq!(red.r, 255);
        assert_eq!(red.g, 0);
        assert_eq!(red.b, 0);
    }

    #[test]
    fn test_contrast_ratio() {
        let ratio = contrast_ratio(RGB::BLACK, RGB::WHITE);
        assert!(ratio > 20.0);
    }

    #[test]
    fn test_wcag_compliance() {
        assert!(meets_wcag_aa(RGB::BLACK, RGB::WHITE));
        assert!(meets_wcag_aaa(RGB::BLACK, RGB::WHITE));
    }
}
