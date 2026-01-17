// SPDX-License-Identifier: PMPL-1.0

package proven

import scala.util.Try

/**
 * RGB color with values 0-255.
 */
case class RGB(r: Int, g: Int, b: Int):
  require(r >= 0 && r <= 255, "Red must be 0-255")
  require(g >= 0 && g <= 255, "Green must be 0-255")
  require(b >= 0 && b <= 255, "Blue must be 0-255")

  /**
   * Convert to hex string (e.g., "#FF0000").
   */
  def toHex: String = f"#$r%02X$g%02X$b%02X"

  /**
   * Convert to lowercase hex string.
   */
  def toHexLower: String = f"#$r%02x$g%02x$b%02x"

  /**
   * Convert to CSS rgb() format.
   */
  def toCss: String = s"rgb($r, $g, $b)"

  /**
   * Calculate relative luminance (WCAG formula).
   */
  def luminance: Double =
    val rLinear = SafeColor.gammaCorrect(r.toDouble / 255.0)
    val gLinear = SafeColor.gammaCorrect(g.toDouble / 255.0)
    val bLinear = SafeColor.gammaCorrect(b.toDouble / 255.0)
    0.2126 * rLinear + 0.7152 * gLinear + 0.0722 * bLinear

  /**
   * Convert to RGBA with specified alpha.
   */
  def withAlpha(a: Int): RGBA = RGBA(r, g, b, a)

  /**
   * Lighten the color by a percentage (0.0 to 1.0).
   */
  def lighten(amount: Double): RGB =
    val factor = 1.0 + amount.min(1.0).max(0.0)
    RGB(
      (r * factor).min(255.0).toInt,
      (g * factor).min(255.0).toInt,
      (b * factor).min(255.0).toInt
    )

  /**
   * Darken the color by a percentage (0.0 to 1.0).
   */
  def darken(amount: Double): RGB =
    val factor = 1.0 - amount.min(1.0).max(0.0)
    RGB(
      (r * factor).max(0.0).toInt,
      (g * factor).max(0.0).toInt,
      (b * factor).max(0.0).toInt
    )

object RGB:
  val Black: RGB = RGB(0, 0, 0)
  val White: RGB = RGB(255, 255, 255)
  val Red: RGB = RGB(255, 0, 0)
  val Green: RGB = RGB(0, 255, 0)
  val Blue: RGB = RGB(0, 0, 255)
  val Yellow: RGB = RGB(255, 255, 0)
  val Cyan: RGB = RGB(0, 255, 255)
  val Magenta: RGB = RGB(255, 0, 255)

  /**
   * Create from hex string (e.g., "#FF0000" or "FF0000").
   */
  def fromHex(hex: String): Option[RGB] =
    val clean = hex.trim.stripPrefix("#")
    if clean.length != 6 then return None

    for
      r <- Try(Integer.parseInt(clean.substring(0, 2), 16)).toOption
      g <- Try(Integer.parseInt(clean.substring(2, 4), 16)).toOption
      b <- Try(Integer.parseInt(clean.substring(4, 6), 16)).toOption
    yield RGB(r, g, b)

/**
 * RGBA color with alpha channel.
 */
case class RGBA(r: Int, g: Int, b: Int, a: Int):
  require(r >= 0 && r <= 255, "Red must be 0-255")
  require(g >= 0 && g <= 255, "Green must be 0-255")
  require(b >= 0 && b <= 255, "Blue must be 0-255")
  require(a >= 0 && a <= 255, "Alpha must be 0-255")

  /**
   * Convert to RGB (discarding alpha).
   */
  def toRgb: RGB = RGB(r, g, b)

  /**
   * Convert to CSS rgba() format.
   */
  def toCss: String = s"rgba($r, $g, $b, ${a.toDouble / 255.0})"

  /**
   * Get alpha as a fraction (0.0 to 1.0).
   */
  def alphaFraction: Double = a.toDouble / 255.0

/**
 * HSL color representation.
 */
case class HSL(h: Double, s: Double, l: Double):
  require(h >= 0 && h < 360, "Hue must be 0-359")
  require(s >= 0 && s <= 1, "Saturation must be 0-1")
  require(l >= 0 && l <= 1, "Lightness must be 0-1")

  /**
   * Convert to RGB.
   */
  def toRgb: RGB =
    if s == 0 then
      val gray = (l * 255).toInt
      RGB(gray, gray, gray)
    else
      val q = if l < 0.5 then l * (1 + s) else l + s - l * s
      val p = 2 * l - q
      val hNorm = h / 360.0

      def hueToRgb(p: Double, q: Double, t: Double): Double =
        val tt = if t < 0 then t + 1 else if t > 1 then t - 1 else t
        if tt < 1.0 / 6 then p + (q - p) * 6 * tt
        else if tt < 1.0 / 2 then q
        else if tt < 2.0 / 3 then p + (q - p) * (2.0 / 3 - tt) * 6
        else p

      RGB(
        (hueToRgb(p, q, hNorm + 1.0 / 3) * 255).toInt,
        (hueToRgb(p, q, hNorm) * 255).toInt,
        (hueToRgb(p, q, hNorm - 1.0 / 3) * 255).toInt
      )

/**
 * Safe color handling with validation and WCAG contrast calculations.
 */
object SafeColor:

  /**
   * Gamma correction for luminance calculation.
   */
  def gammaCorrect(value: Double): Double =
    if value <= 0.03928 then value / 12.92
    else Math.pow((value + 0.055) / 1.055, 2.4)

  /**
   * Calculate WCAG contrast ratio between two colors.
   */
  def contrastRatio(color1: RGB, color2: RGB): Double =
    val l1 = color1.luminance
    val l2 = color2.luminance
    val lighter = l1.max(l2)
    val darker = l1.min(l2)
    (lighter + 0.05) / (darker + 0.05)

  /**
   * Check if contrast meets WCAG AA standard (4.5:1 for normal text).
   */
  def meetsWcagAa(color1: RGB, color2: RGB): Boolean =
    contrastRatio(color1, color2) >= 4.5

  /**
   * Check if contrast meets WCAG AA standard for large text (3:1).
   */
  def meetsWcagAaLarge(color1: RGB, color2: RGB): Boolean =
    contrastRatio(color1, color2) >= 3.0

  /**
   * Check if contrast meets WCAG AAA standard (7:1 for normal text).
   */
  def meetsWcagAaa(color1: RGB, color2: RGB): Boolean =
    contrastRatio(color1, color2) >= 7.0

  /**
   * Blend two colors with alpha.
   */
  def blend(fg: RGBA, bg: RGB): RGB =
    val alpha = fg.a.toDouble / 255.0
    val invAlpha = 1.0 - alpha
    RGB(
      (fg.r * alpha + bg.r * invAlpha).toInt,
      (fg.g * alpha + bg.g * invAlpha).toInt,
      (fg.b * alpha + bg.b * invAlpha).toInt
    )

  /**
   * Parse a color from hex string.
   */
  def parseHex(hex: String): Option[RGB] = RGB.fromHex(hex)

  /**
   * Check if a hex string is valid.
   */
  def isValidHex(hex: String): Boolean = parseHex(hex).isDefined

  /**
   * Convert RGB to HSL.
   */
  def rgbToHsl(rgb: RGB): HSL =
    val r = rgb.r.toDouble / 255
    val g = rgb.g.toDouble / 255
    val b = rgb.b.toDouble / 255

    val max = r.max(g).max(b)
    val min = r.min(g).min(b)
    val l = (max + min) / 2

    if max == min then
      HSL(0, 0, l)
    else
      val d = max - min
      val s = if l > 0.5 then d / (2 - max - min) else d / (max + min)
      val h = (max match
        case `r` => (g - b) / d + (if g < b then 6 else 0)
        case `g` => (b - r) / d + 2
        case `b` => (r - g) / d + 4
      ) * 60

      HSL(h, s, l)

  /**
   * Get a complementary color.
   */
  def complementary(color: RGB): RGB =
    val hsl = rgbToHsl(color)
    HSL((hsl.h + 180) % 360, hsl.s, hsl.l).toRgb

  /**
   * Interpolate between two colors.
   */
  def interpolate(c1: RGB, c2: RGB, t: Double): RGB =
    val factor = t.min(1.0).max(0.0)
    val invFactor = 1.0 - factor
    RGB(
      (c1.r * invFactor + c2.r * factor).toInt,
      (c1.g * invFactor + c2.g * factor).toInt,
      (c1.b * invFactor + c2.b * factor).toInt
    )

  /**
   * Convert grayscale value (0-255) to RGB.
   */
  def grayscale(value: Int): RGB =
    val v = value.min(255).max(0)
    RGB(v, v, v)

  /**
   * Convert RGB to grayscale using luminance weighting.
   */
  def toGrayscale(color: RGB): RGB =
    val gray = (0.299 * color.r + 0.587 * color.g + 0.114 * color.b).toInt
    RGB(gray, gray, gray)
