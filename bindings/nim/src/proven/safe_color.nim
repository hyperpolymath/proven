# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe color operations (RGB, HSL, hex).

import std/[options, strutils, math]

type
  Rgb* = object
    ## RGB color with components in [0, 255].
    r*: uint8
    g*: uint8
    b*: uint8

  Rgba* = object
    ## RGBA color with alpha in [0, 255].
    r*: uint8
    g*: uint8
    b*: uint8
    a*: uint8

  Hsl* = object
    ## HSL color (hue in [0, 360), saturation and lightness in [0, 1]).
    h*: float64
    s*: float64
    l*: float64

proc rgb*(r, g, b: uint8): Rgb =
  ## Create an RGB color.
  Rgb(r: r, g: g, b: b)

proc rgba*(r, g, b, a: uint8): Rgba =
  ## Create an RGBA color.
  Rgba(r: r, g: g, b: b, a: a)

proc hsl*(h, s, l: float64): Option[Hsl] =
  ## Create an HSL color with validation.
  if s < 0.0 or s > 1.0 or l < 0.0 or l > 1.0:
    return none(Hsl)
  var hue = h mod 360.0
  if hue < 0.0:
    hue += 360.0
  result = some(Hsl(h: hue, s: s, l: l))

proc parseHex*(hex: string): Option[Rgb] =
  ## Parse a hex color string (#RGB, #RRGGBB).
  var s = hex
  if s.startsWith("#"):
    s = s[1..^1]

  try:
    case s.len
    of 3:
      let r = parseHexInt($s[0] & $s[0]).uint8
      let g = parseHexInt($s[1] & $s[1]).uint8
      let b = parseHexInt($s[2] & $s[2]).uint8
      result = some(Rgb(r: r, g: g, b: b))
    of 6:
      let r = parseHexInt(s[0..1]).uint8
      let g = parseHexInt(s[2..3]).uint8
      let b = parseHexInt(s[4..5]).uint8
      result = some(Rgb(r: r, g: g, b: b))
    else:
      return none(Rgb)
  except:
    return none(Rgb)

proc toHex*(c: Rgb): string =
  ## Convert RGB to hex string (#RRGGBB).
  result = "#"
  result.add(c.r.toHex(2).toLowerAscii())
  result.add(c.g.toHex(2).toLowerAscii())
  result.add(c.b.toHex(2).toLowerAscii())

proc toRgba*(c: Rgb, alpha: uint8 = 255): Rgba =
  ## Convert RGB to RGBA.
  Rgba(r: c.r, g: c.g, b: c.b, a: alpha)

proc toRgb*(c: Rgba): Rgb =
  ## Convert RGBA to RGB (drops alpha).
  Rgb(r: c.r, g: c.g, b: c.b)

proc rgbToHsl*(c: Rgb): Hsl =
  ## Convert RGB to HSL.
  let r = c.r.float64 / 255.0
  let g = c.g.float64 / 255.0
  let b = c.b.float64 / 255.0

  let maxC = max(max(r, g), b)
  let minC = min(min(r, g), b)
  let l = (maxC + minC) / 2.0

  if maxC == minC:
    return Hsl(h: 0.0, s: 0.0, l: l)

  let d = maxC - minC
  let s = if l > 0.5: d / (2.0 - maxC - minC) else: d / (maxC + minC)

  var h: float64
  if maxC == r:
    h = (g - b) / d + (if g < b: 6.0 else: 0.0)
  elif maxC == g:
    h = (b - r) / d + 2.0
  else:
    h = (r - g) / d + 4.0

  h = h * 60.0
  Hsl(h: h, s: s, l: l)

proc hslToRgb*(c: Hsl): Rgb =
  ## Convert HSL to RGB.
  if c.s == 0.0:
    let v = (c.l * 255.0).uint8
    return Rgb(r: v, g: v, b: v)

  proc hueToRgb(p, q, t: float64): float64 =
    var t = t
    if t < 0.0: t += 1.0
    if t > 1.0: t -= 1.0
    if t < 1.0/6.0: return p + (q - p) * 6.0 * t
    if t < 1.0/2.0: return q
    if t < 2.0/3.0: return p + (q - p) * (2.0/3.0 - t) * 6.0
    return p

  let q = if c.l < 0.5: c.l * (1.0 + c.s) else: c.l + c.s - c.l * c.s
  let p = 2.0 * c.l - q

  let h = c.h / 360.0
  let r = hueToRgb(p, q, h + 1.0/3.0)
  let g = hueToRgb(p, q, h)
  let b = hueToRgb(p, q, h - 1.0/3.0)

  Rgb(
    r: (r * 255.0).uint8,
    g: (g * 255.0).uint8,
    b: (b * 255.0).uint8
  )

proc lighten*(c: Rgb, amount: float64): Rgb =
  ## Lighten a color by a percentage (0.0 to 1.0).
  var hsl = rgbToHsl(c)
  hsl.l = min(1.0, hsl.l + amount)
  hslToRgb(hsl)

proc darken*(c: Rgb, amount: float64): Rgb =
  ## Darken a color by a percentage (0.0 to 1.0).
  var hsl = rgbToHsl(c)
  hsl.l = max(0.0, hsl.l - amount)
  hslToRgb(hsl)

proc saturate*(c: Rgb, amount: float64): Rgb =
  ## Increase saturation by a percentage (0.0 to 1.0).
  var hsl = rgbToHsl(c)
  hsl.s = min(1.0, hsl.s + amount)
  hslToRgb(hsl)

proc desaturate*(c: Rgb, amount: float64): Rgb =
  ## Decrease saturation by a percentage (0.0 to 1.0).
  var hsl = rgbToHsl(c)
  hsl.s = max(0.0, hsl.s - amount)
  hslToRgb(hsl)

proc invert*(c: Rgb): Rgb =
  ## Invert a color.
  Rgb(r: 255 - c.r, g: 255 - c.g, b: 255 - c.b)

proc grayscale*(c: Rgb): Rgb =
  ## Convert to grayscale.
  let gray = ((c.r.int * 299 + c.g.int * 587 + c.b.int * 114) div 1000).uint8
  Rgb(r: gray, g: gray, b: gray)

proc luminance*(c: Rgb): float64 =
  ## Calculate relative luminance (WCAG formula).
  proc adjust(v: float64): float64 =
    if v <= 0.03928: v / 12.92
    else: pow((v + 0.055) / 1.055, 2.4)

  let r = adjust(c.r.float64 / 255.0)
  let g = adjust(c.g.float64 / 255.0)
  let b = adjust(c.b.float64 / 255.0)

  0.2126 * r + 0.7152 * g + 0.0722 * b

proc contrastRatio*(c1, c2: Rgb): float64 =
  ## Calculate contrast ratio between two colors.
  let l1 = luminance(c1)
  let l2 = luminance(c2)
  let lighter = max(l1, l2)
  let darker = min(l1, l2)
  (lighter + 0.05) / (darker + 0.05)
