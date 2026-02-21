// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// RGB color representation.
public struct RGB: Equatable, Hashable {
    public let r: UInt8
    public let g: UInt8
    public let b: UInt8

    public init(r: UInt8, g: UInt8, b: UInt8) {
        self.r = r
        self.g = g
        self.b = b
    }

    public init(r: Int, g: Int, b: Int) {
        self.r = UInt8(clamping: r)
        self.g = UInt8(clamping: g)
        self.b = UInt8(clamping: b)
    }

    /// Parse a hex color string.
    public static func fromHex(_ hex: String) -> RGB? {
        var hexString = hex.trimmingCharacters(in: .whitespaces)
        if hexString.hasPrefix("#") {
            hexString = String(hexString.dropFirst())
        }

        // Expand shorthand (e.g., "fff" -> "ffffff")
        if hexString.count == 3 {
            hexString = hexString.map { "\($0)\($0)" }.joined()
        }

        guard hexString.count == 6,
              let value = UInt32(hexString, radix: 16) else {
            return nil
        }

        return RGB(
            r: UInt8((value >> 16) & 0xFF),
            g: UInt8((value >> 8) & 0xFF),
            b: UInt8(value & 0xFF)
        )
    }

    /// Convert to hex string.
    public func toHex() -> String {
        String(format: "#%02x%02x%02x", r, g, b)
    }

    /// Convert to CSS rgb() string.
    public func toCss() -> String {
        "rgb(\(r), \(g), \(b))"
    }

    /// Calculate relative luminance (WCAG).
    public func luminance() -> Double {
        func linearize(_ c: UInt8) -> Double {
            let s = Double(c) / 255.0
            return s <= 0.03928 ? s / 12.92 : pow((s + 0.055) / 1.055, 2.4)
        }

        let r = linearize(self.r)
        let g = linearize(self.g)
        let b = linearize(self.b)

        return 0.2126 * r + 0.7152 * g + 0.0722 * b
    }

    /// Calculate contrast ratio with another color.
    public func contrast(_ other: RGB) -> Double {
        let l1 = self.luminance()
        let l2 = other.luminance()
        let lighter = max(l1, l2)
        let darker = min(l1, l2)
        return (lighter + 0.05) / (darker + 0.05)
    }

    /// Check WCAG AA compliance for normal text (4.5:1).
    public func meetsWCAG_AA(_ background: RGB) -> Bool {
        contrast(background) >= 4.5
    }

    /// Check WCAG AAA compliance for normal text (7:1).
    public func meetsWCAG_AAA(_ background: RGB) -> Bool {
        contrast(background) >= 7.0
    }

    /// Mix with another color.
    public func mix(_ other: RGB, amount: Double = 0.5) -> RGB {
        let t = max(0, min(1, amount))
        return RGB(
            r: Int(Double(r) + (Double(other.r) - Double(r)) * t),
            g: Int(Double(g) + (Double(other.g) - Double(g)) * t),
            b: Int(Double(b) + (Double(other.b) - Double(b)) * t)
        )
    }

    /// Lighten the color.
    public func lighten(_ amount: Double) -> RGB {
        mix(RGB.white, amount: amount)
    }

    /// Darken the color.
    public func darken(_ amount: Double) -> RGB {
        mix(RGB.black, amount: amount)
    }

    /// Invert the color.
    public func invert() -> RGB {
        RGB(r: 255 - Int(r), g: 255 - Int(g), b: 255 - Int(b))
    }

    /// Convert to grayscale.
    public func grayscale() -> RGB {
        let gray = Int(0.299 * Double(r) + 0.587 * Double(g) + 0.114 * Double(b))
        return RGB(r: gray, g: gray, b: gray)
    }

    // Common colors
    public static let black = RGB(r: 0, g: 0, b: 0)
    public static let white = RGB(r: 255, g: 255, b: 255)
    public static let red = RGB(r: 255, g: 0, b: 0)
    public static let green = RGB(r: 0, g: 255, b: 0)
    public static let blue = RGB(r: 0, g: 0, b: 255)
    public static let yellow = RGB(r: 255, g: 255, b: 0)
    public static let cyan = RGB(r: 0, g: 255, b: 255)
    public static let magenta = RGB(r: 255, g: 0, b: 255)
}

/// RGBA color with alpha channel.
public struct RGBA: Equatable, Hashable {
    public let r: UInt8
    public let g: UInt8
    public let b: UInt8
    public let a: Double

    public init(r: UInt8, g: UInt8, b: UInt8, a: Double = 1.0) {
        self.r = r
        self.g = g
        self.b = b
        self.a = max(0, min(1, a))
    }

    public init(rgb: RGB, alpha: Double = 1.0) {
        self.r = rgb.r
        self.g = rgb.g
        self.b = rgb.b
        self.a = max(0, min(1, alpha))
    }

    public func toRGB() -> RGB {
        RGB(r: r, g: g, b: b)
    }

    public func toCss() -> String {
        "rgba(\(r), \(g), \(b), \(String(format: "%.2f", a)))"
    }

    /// Blend over a background color.
    public func blendOver(_ background: RGB) -> RGB {
        RGB(
            r: Int(Double(r) * a + Double(background.r) * (1 - a)),
            g: Int(Double(g) * a + Double(background.g) * (1 - a)),
            b: Int(Double(b) * a + Double(background.b) * (1 - a))
        )
    }
}

/// HSL color representation.
public struct HSL: Equatable, Hashable {
    public let h: Double // 0-360
    public let s: Double // 0-100
    public let l: Double // 0-100

    public init(h: Double, s: Double, l: Double) {
        self.h = h.truncatingRemainder(dividingBy: 360)
        self.s = max(0, min(100, s))
        self.l = max(0, min(100, l))
    }

    /// Create from RGB.
    public static func fromRGB(_ rgb: RGB) -> HSL {
        let r = Double(rgb.r) / 255
        let g = Double(rgb.g) / 255
        let b = Double(rgb.b) / 255

        let maxC = max(r, g, b)
        let minC = min(r, g, b)
        let l = (maxC + minC) / 2

        if maxC == minC {
            return HSL(h: 0, s: 0, l: l * 100)
        }

        let d = maxC - minC
        let s = l > 0.5 ? d / (2 - maxC - minC) : d / (maxC + minC)

        var h: Double
        switch maxC {
        case r:
            h = ((g - b) / d + (g < b ? 6 : 0)) / 6
        case g:
            h = ((b - r) / d + 2) / 6
        default:
            h = ((r - g) / d + 4) / 6
        }

        return HSL(h: h * 360, s: s * 100, l: l * 100)
    }

    /// Convert to RGB.
    public func toRGB() -> RGB {
        let h = self.h / 360
        let s = self.s / 100
        let l = self.l / 100

        if s == 0 {
            let gray = Int(l * 255)
            return RGB(r: gray, g: gray, b: gray)
        }

        func hue2rgb(_ p: Double, _ q: Double, _ t: Double) -> Double {
            var t = t
            if t < 0 { t += 1 }
            if t > 1 { t -= 1 }
            if t < 1/6 { return p + (q - p) * 6 * t }
            if t < 1/2 { return q }
            if t < 2/3 { return p + (q - p) * (2/3 - t) * 6 }
            return p
        }

        let q = l < 0.5 ? l * (1 + s) : l + s - l * s
        let p = 2 * l - q

        return RGB(
            r: Int(hue2rgb(p, q, h + 1/3) * 255),
            g: Int(hue2rgb(p, q, h) * 255),
            b: Int(hue2rgb(p, q, h - 1/3) * 255)
        )
    }

    /// Rotate hue.
    public func rotateHue(_ degrees: Double) -> HSL {
        HSL(h: h + degrees, s: s, l: l)
    }

    /// Get complementary color.
    public func complement() -> HSL {
        rotateHue(180)
    }

    public func toCss() -> String {
        "hsl(\(Int(h)), \(Int(s))%, \(Int(l))%)"
    }
}

/// Color utilities namespace.
public enum SafeColor {
    public static func fromHex(_ hex: String) -> RGB? {
        RGB.fromHex(hex)
    }

    public static func contrast(_ a: RGB, _ b: RGB) -> Double {
        a.contrast(b)
    }
}
