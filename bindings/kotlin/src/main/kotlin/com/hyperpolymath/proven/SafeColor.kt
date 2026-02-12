// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min
import kotlin.math.pow
import kotlin.math.round

/**
 * RGB color with values 0-255.
 */
data class RGB(val r: Int, val g: Int, val b: Int) {
    init {
        require(r in 0..255) { "Red must be 0-255" }
        require(g in 0..255) { "Green must be 0-255" }
        require(b in 0..255) { "Blue must be 0-255" }
    }

    fun toHex(): String = String.format("#%02X%02X%02X", r, g, b)

    fun toRGBA(alpha: Double = 1.0): RGBA = RGBA(r, g, b, alpha.coerceIn(0.0, 1.0))

    fun toHSL(): HSL {
        val rf = r / 255.0
        val gf = g / 255.0
        val bf = b / 255.0

        val max = maxOf(rf, gf, bf)
        val min = minOf(rf, gf, bf)
        val l = (max + min) / 2

        if (max == min) {
            return HSL(0.0, 0.0, l)
        }

        val d = max - min
        val s = if (l > 0.5) d / (2 - max - min) else d / (max + min)

        val h = when (max) {
            rf -> ((gf - bf) / d + (if (gf < bf) 6 else 0)) / 6
            gf -> ((bf - rf) / d + 2) / 6
            else -> ((rf - gf) / d + 4) / 6
        }

        return HSL(h * 360, s, l)
    }

    /**
     * Calculate relative luminance.
     */
    fun luminance(): Double {
        fun channel(c: Int): Double {
            val s = c / 255.0
            return if (s <= 0.03928) s / 12.92 else ((s + 0.055) / 1.055).pow(2.4)
        }
        return 0.2126 * channel(r) + 0.7152 * channel(g) + 0.0722 * channel(b)
    }

    /**
     * Calculate contrast ratio with another color.
     */
    fun contrastRatio(other: RGB): Double {
        val l1 = max(luminance(), other.luminance())
        val l2 = min(luminance(), other.luminance())
        return (l1 + 0.05) / (l2 + 0.05)
    }

    /**
     * Check WCAG AA compliance (4.5:1 for normal text).
     */
    fun meetsWCAG_AA(other: RGB): Boolean = contrastRatio(other) >= 4.5

    /**
     * Check WCAG AAA compliance (7:1 for normal text).
     */
    fun meetsWCAG_AAA(other: RGB): Boolean = contrastRatio(other) >= 7.0

    /**
     * Mix with another color.
     */
    fun mix(other: RGB, ratio: Double = 0.5): RGB {
        val t = ratio.coerceIn(0.0, 1.0)
        return RGB(
            r = ((1 - t) * r + t * other.r).toInt().coerceIn(0, 255),
            g = ((1 - t) * g + t * other.g).toInt().coerceIn(0, 255),
            b = ((1 - t) * b + t * other.b).toInt().coerceIn(0, 255)
        )
    }

    /**
     * Lighten color.
     */
    fun lighten(amount: Double): RGB = toHSL().lighten(amount).toRGB()

    /**
     * Darken color.
     */
    fun darken(amount: Double): RGB = toHSL().darken(amount).toRGB()

    /**
     * Invert color.
     */
    fun invert(): RGB = RGB(255 - r, 255 - g, 255 - b)

    /**
     * Convert to grayscale.
     */
    fun grayscale(): RGB {
        val gray = (0.299 * r + 0.587 * g + 0.114 * b).toInt().coerceIn(0, 255)
        return RGB(gray, gray, gray)
    }

    companion object {
        val BLACK = RGB(0, 0, 0)
        val WHITE = RGB(255, 255, 255)
        val RED = RGB(255, 0, 0)
        val GREEN = RGB(0, 255, 0)
        val BLUE = RGB(0, 0, 255)

        fun fromHex(hex: String): Result<RGB> {
            val clean = hex.removePrefix("#")
            if (clean.length != 6 || !clean.all { it.isDigit() || it.lowercaseChar() in 'a'..'f' }) {
                return Result.failure(IllegalArgumentException("Invalid hex color: $hex"))
            }
            return Result.success(
                RGB(
                    r = clean.substring(0, 2).toInt(16),
                    g = clean.substring(2, 4).toInt(16),
                    b = clean.substring(4, 6).toInt(16)
                )
            )
        }
    }
}

/**
 * RGBA color with alpha 0-1.
 */
data class RGBA(val r: Int, val g: Int, val b: Int, val a: Double) {
    init {
        require(r in 0..255) { "Red must be 0-255" }
        require(g in 0..255) { "Green must be 0-255" }
        require(b in 0..255) { "Blue must be 0-255" }
        require(a in 0.0..1.0) { "Alpha must be 0-1" }
    }

    fun toRGB(): RGB = RGB(r, g, b)

    fun toHex(): String {
        val alpha = (a * 255).toInt().coerceIn(0, 255)
        return String.format("#%02X%02X%02X%02X", r, g, b, alpha)
    }

    fun toCSSRGBA(): String = "rgba($r, $g, $b, ${round(a * 100) / 100})"

    /**
     * Blend over another color (alpha compositing).
     */
    fun blendOver(background: RGB): RGB {
        return RGB(
            r = (a * r + (1 - a) * background.r).toInt().coerceIn(0, 255),
            g = (a * g + (1 - a) * background.g).toInt().coerceIn(0, 255),
            b = (a * b + (1 - a) * background.b).toInt().coerceIn(0, 255)
        )
    }

    companion object {
        val TRANSPARENT = RGBA(0, 0, 0, 0.0)
    }
}

/**
 * HSL color (hue 0-360, saturation 0-1, lightness 0-1).
 */
data class HSL(val h: Double, val s: Double, val l: Double) {
    init {
        require(h in 0.0..360.0) { "Hue must be 0-360" }
        require(s in 0.0..1.0) { "Saturation must be 0-1" }
        require(l in 0.0..1.0) { "Lightness must be 0-1" }
    }

    fun toRGB(): RGB {
        if (s == 0.0) {
            val gray = (l * 255).toInt().coerceIn(0, 255)
            return RGB(gray, gray, gray)
        }

        fun hue2rgb(p: Double, q: Double, t: Double): Double {
            var tt = t
            if (tt < 0) tt += 1
            if (tt > 1) tt -= 1
            return when {
                tt < 1.0 / 6 -> p + (q - p) * 6 * tt
                tt < 1.0 / 2 -> q
                tt < 2.0 / 3 -> p + (q - p) * (2.0 / 3 - tt) * 6
                else -> p
            }
        }

        val q = if (l < 0.5) l * (1 + s) else l + s - l * s
        val p = 2 * l - q
        val hNorm = h / 360

        return RGB(
            r = (hue2rgb(p, q, hNorm + 1.0 / 3) * 255).toInt().coerceIn(0, 255),
            g = (hue2rgb(p, q, hNorm) * 255).toInt().coerceIn(0, 255),
            b = (hue2rgb(p, q, hNorm - 1.0 / 3) * 255).toInt().coerceIn(0, 255)
        )
    }

    fun lighten(amount: Double): HSL {
        return HSL(h, s, (l + amount).coerceIn(0.0, 1.0))
    }

    fun darken(amount: Double): HSL {
        return HSL(h, s, (l - amount).coerceIn(0.0, 1.0))
    }

    fun saturate(amount: Double): HSL {
        return HSL(h, (s + amount).coerceIn(0.0, 1.0), l)
    }

    fun desaturate(amount: Double): HSL {
        return HSL(h, (s - amount).coerceIn(0.0, 1.0), l)
    }

    fun rotate(degrees: Double): HSL {
        var newH = (h + degrees) % 360
        if (newH < 0) newH += 360
        return HSL(newH, s, l)
    }

    fun complement(): HSL = rotate(180.0)
}

/**
 * Color utilities.
 */
object SafeColor {
    /**
     * Parse any color format.
     */
    fun parse(color: String): Result<RGB> {
        val trimmed = color.trim().lowercase()

        // Named colors
        NAMED_COLORS[trimmed]?.let { return Result.success(it) }

        // Hex
        if (trimmed.startsWith("#")) {
            return RGB.fromHex(trimmed)
        }

        // rgb() or rgba()
        val rgbMatch = Regex("""rgba?\((\d+),\s*(\d+),\s*(\d+)(?:,\s*[\d.]+)?\)""").matchEntire(trimmed)
        if (rgbMatch != null) {
            return Result.success(
                RGB(
                    rgbMatch.groupValues[1].toInt(),
                    rgbMatch.groupValues[2].toInt(),
                    rgbMatch.groupValues[3].toInt()
                )
            )
        }

        return Result.failure(IllegalArgumentException("Cannot parse color: $color"))
    }

    private val NAMED_COLORS = mapOf(
        "black" to RGB.BLACK,
        "white" to RGB.WHITE,
        "red" to RGB.RED,
        "green" to RGB(0, 128, 0),
        "blue" to RGB.BLUE,
        "yellow" to RGB(255, 255, 0),
        "cyan" to RGB(0, 255, 255),
        "magenta" to RGB(255, 0, 255),
        "gray" to RGB(128, 128, 128),
        "grey" to RGB(128, 128, 128),
        "orange" to RGB(255, 165, 0),
        "purple" to RGB(128, 0, 128),
        "pink" to RGB(255, 192, 203)
    )
}
