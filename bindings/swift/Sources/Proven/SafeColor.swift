// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Color parsing and conversion delegated to libproven FFI.
///
/// RGB/HSL conversions and hex parsing via the formally verified
/// Idris 2 core.

import CProven

/// RGB color returned by SafeColor operations.
public struct RGBColor: Equatable, Sendable {
    public let r: UInt8
    public let g: UInt8
    public let b: UInt8
}

/// HSL color returned by SafeColor operations.
public struct HSLColor: Equatable, Sendable {
    public let h: Double
    public let s: Double
    public let l: Double
}

public enum SafeColor {
    /// Parse a hex color string (e.g., "#FF0000" or "#F00").
    public static func parseHex(_ hex: String) -> Result<RGBColor, ProvenError> {
        withStringBytes(hex) { ptr, len in
            let result = proven_color_parse_hex(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(RGBColor(r: result.color.r, g: result.color.g, b: result.color.b))
        }
    }

    /// Convert RGB to HSL.
    public static func rgbToHsl(_ rgb: RGBColor) -> HSLColor {
        let cRgb = ProvenRGBColor(r: rgb.r, g: rgb.g, b: rgb.b)
        let hsl = proven_color_rgb_to_hsl(cRgb)
        return HSLColor(h: hsl.h, s: hsl.s, l: hsl.l)
    }

    /// Format RGB as hex string.
    public static func toHex(_ rgb: RGBColor) -> Result<String, ProvenError> {
        let cRgb = ProvenRGBColor(r: rgb.r, g: rgb.g, b: rgb.b)
        return consumeStringResult(proven_color_to_hex(cRgb))
    }
}
