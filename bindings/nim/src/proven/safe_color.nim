# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe color operations (RGB, HSL, hex).
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

type
  Rgb* = object
    ## RGB color with components in [0, 255].
    r*: uint8
    g*: uint8
    b*: uint8

  Hsl* = object
    ## HSL color (hue in [0, 360), saturation and lightness in [0, 1]).
    h*: float64
    s*: float64
    l*: float64

proc parseHex*(hex: string): Option[Rgb] =
  ## Parse a hex color string (#RRGGBB, #RGB).
  if hex.len == 0:
    return none(Rgb)
  let res = provenColorParseHex(unsafeAddr hex[0], csize_t(hex.len))
  if res.status == PROVEN_OK:
    return some(Rgb(r: res.color.r, g: res.color.g, b: res.color.b))
  none(Rgb)

proc rgbToHsl*(c: Rgb): Hsl =
  ## Convert RGB to HSL.
  let ffiRgb = RGBColor(r: c.r, g: c.g, b: c.b)
  let res = provenColorRgbToHsl(ffiRgb)
  Hsl(h: res.h, s: res.s, l: res.l)

proc toHex*(c: Rgb): Option[string] =
  ## Convert RGB to hex string (#rrggbb).
  let ffiRgb = RGBColor(r: c.r, g: c.g, b: c.b)
  let res = provenColorToHex(ffiRgb)
  if res.status == PROVEN_OK and res.value != nil:
    let hex = $res.value
    provenFreeString(res.value)
    return some(hex)
  none(string)
