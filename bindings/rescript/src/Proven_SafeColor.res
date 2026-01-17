// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeColor - Safe color operations that cannot crash.
 *
 * Provides color parsing, conversion, and WCAG accessibility checking.
 */

/** RGB color with values clamped to 0-255 */
type rgb = {
  r: int,
  g: int,
  b: int,
}

/** RGBA color with alpha clamped to 0-1 */
type rgba = {
  r: int,
  g: int,
  b: int,
  a: float,
}

/** HSL color */
type hsl = {
  h: float, // 0-360
  s: float, // 0-1
  l: float, // 0-1
}

/** HSLA color */
type hsla = {
  h: float,
  s: float,
  l: float,
  a: float,
}

/** Clamp an int to 0-255 */
let clampByte = (n: int): int => {
  if n < 0 {
    0
  } else if n > 255 {
    255
  } else {
    n
  }
}

/** Clamp a float to 0-1 */
let clampUnit = (n: float): float => {
  if n < 0.0 {
    0.0
  } else if n > 1.0 {
    1.0
  } else {
    n
  }
}

/** Create an RGB color with clamping */
let makeRgb = (~r: int, ~g: int, ~b: int): rgb => {
  {r: clampByte(r), g: clampByte(g), b: clampByte(b)}
}

/** Create an RGBA color with clamping */
let makeRgba = (~r: int, ~g: int, ~b: int, ~a: float): rgba => {
  {r: clampByte(r), g: clampByte(g), b: clampByte(b), a: clampUnit(a)}
}

/** Parse hex color (3, 4, 6, or 8 characters, with or without #) */
let fromHex = (hex: string): option<rgba> => {
  let hex = if Js.String2.startsWith(hex, "#") {
    Js.String2.sliceToEnd(hex, ~from=1)
  } else {
    hex
  }

  let parseHexByte = (s: string): option<int> => {
    let parsed = Js.parseInt(s, ~radix=16)
    if Js.Float.isNaN(parsed) {
      None
    } else {
      Some(Belt.Float.toInt(parsed))
    }
  }

  let len = Js.String2.length(hex)
  switch len {
  | 3 =>
    // #RGB
    let r = Js.String2.slice(hex, ~from=0, ~to_=1)
    let g = Js.String2.slice(hex, ~from=1, ~to_=2)
    let b = Js.String2.slice(hex, ~from=2, ~to_=3)
    switch (parseHexByte(r ++ r), parseHexByte(g ++ g), parseHexByte(b ++ b)) {
    | (Some(r), Some(g), Some(b)) => Some({r, g, b, a: 1.0})
    | _ => None
    }
  | 4 =>
    // #RGBA
    let r = Js.String2.slice(hex, ~from=0, ~to_=1)
    let g = Js.String2.slice(hex, ~from=1, ~to_=2)
    let b = Js.String2.slice(hex, ~from=2, ~to_=3)
    let a = Js.String2.slice(hex, ~from=3, ~to_=4)
    switch (parseHexByte(r ++ r), parseHexByte(g ++ g), parseHexByte(b ++ b), parseHexByte(a ++ a)) {
    | (Some(r), Some(g), Some(b), Some(a)) => Some({r, g, b, a: Belt.Int.toFloat(a) /. 255.0})
    | _ => None
    }
  | 6 =>
    // #RRGGBB
    let r = Js.String2.slice(hex, ~from=0, ~to_=2)
    let g = Js.String2.slice(hex, ~from=2, ~to_=4)
    let b = Js.String2.slice(hex, ~from=4, ~to_=6)
    switch (parseHexByte(r), parseHexByte(g), parseHexByte(b)) {
    | (Some(r), Some(g), Some(b)) => Some({r, g, b, a: 1.0})
    | _ => None
    }
  | 8 =>
    // #RRGGBBAA
    let r = Js.String2.slice(hex, ~from=0, ~to_=2)
    let g = Js.String2.slice(hex, ~from=2, ~to_=4)
    let b = Js.String2.slice(hex, ~from=4, ~to_=6)
    let a = Js.String2.slice(hex, ~from=6, ~to_=8)
    switch (parseHexByte(r), parseHexByte(g), parseHexByte(b), parseHexByte(a)) {
    | (Some(r), Some(g), Some(b), Some(a)) => Some({r, g, b, a: Belt.Int.toFloat(a) /. 255.0})
    | _ => None
    }
  | _ => None
  }
}

/** Convert RGBA to hex string */
let toHex = (color: rgba): string => {
  let toHexByte = n => {
    let hex = Js.Int.toStringWithRadix(n, ~radix=16)
    if Js.String2.length(hex) == 1 {
      "0" ++ hex
    } else {
      hex
    }
  }
  if color.a >= 1.0 {
    "#" ++ toHexByte(color.r) ++ toHexByte(color.g) ++ toHexByte(color.b)
  } else {
    let alphaByte = Belt.Float.toInt(color.a *. 255.0)
    "#" ++ toHexByte(color.r) ++ toHexByte(color.g) ++ toHexByte(color.b) ++ toHexByte(alphaByte)
  }
}

/** Convert RGB to HSL */
let rgbToHsl = (color: rgb): hsl => {
  let r = Belt.Int.toFloat(color.r) /. 255.0
  let g = Belt.Int.toFloat(color.g) /. 255.0
  let b = Belt.Int.toFloat(color.b) /. 255.0

  let max = Js.Math.max_float(Js.Math.max_float(r, g), b)
  let min = Js.Math.min_float(Js.Math.min_float(r, g), b)
  let l = (max +. min) /. 2.0

  if max == min {
    {h: 0.0, s: 0.0, l}
  } else {
    let d = max -. min
    let s = if l > 0.5 {
      d /. (2.0 -. max -. min)
    } else {
      d /. (max +. min)
    }

    let h = if max == r {
      (g -. b) /. d +. (if g < b {
        6.0
      } else {
        0.0
      })
    } else if max == g {
      (b -. r) /. d +. 2.0
    } else {
      (r -. g) /. d +. 4.0
    }

    {h: h *. 60.0, s, l}
  }
}

/** Convert HSL to RGB */
let hslToRgb = (color: hsl): rgb => {
  let hueToRgb = (p: float, q: float, t: float): float => {
    let t = if t < 0.0 {
      t +. 1.0
    } else if t > 1.0 {
      t -. 1.0
    } else {
      t
    }

    if t < 1.0 /. 6.0 {
      p +. (q -. p) *. 6.0 *. t
    } else if t < 1.0 /. 2.0 {
      q
    } else if t < 2.0 /. 3.0 {
      p +. (q -. p) *. (2.0 /. 3.0 -. t) *. 6.0
    } else {
      p
    }
  }

  if color.s == 0.0 {
    let v = Belt.Float.toInt(color.l *. 255.0)
    {r: v, g: v, b: v}
  } else {
    let q = if color.l < 0.5 {
      color.l *. (1.0 +. color.s)
    } else {
      color.l +. color.s -. color.l *. color.s
    }
    let p = 2.0 *. color.l -. q
    let h = color.h /. 360.0

    {
      r: Belt.Float.toInt(hueToRgb(p, q, h +. 1.0 /. 3.0) *. 255.0),
      g: Belt.Float.toInt(hueToRgb(p, q, h) *. 255.0),
      b: Belt.Float.toInt(hueToRgb(p, q, h -. 1.0 /. 3.0) *. 255.0),
    }
  }
}

/** Calculate relative luminance (WCAG 2.1) */
let luminance = (color: rgb): float => {
  let adjust = c => {
    let c = Belt.Int.toFloat(c) /. 255.0
    if c <= 0.03928 {
      c /. 12.92
    } else {
      Js.Math.pow_float(~base=(c +. 0.055) /. 1.055, ~exp=2.4)
    }
  }

  let r = adjust(color.r)
  let g = adjust(color.g)
  let b = adjust(color.b)

  0.2126 *. r +. 0.7152 *. g +. 0.0722 *. b
}

/** Calculate contrast ratio between two colors (WCAG 2.1) */
let contrastRatio = (a: rgb, b: rgb): float => {
  let lumA = luminance(a)
  let lumB = luminance(b)
  let lighter = Js.Math.max_float(lumA, lumB)
  let darker = Js.Math.min_float(lumA, lumB)
  (lighter +. 0.05) /. (darker +. 0.05)
}

/** Check if contrast meets WCAG AA for normal text (4.5:1) */
let meetsWcagAA = (foreground: rgb, background: rgb): bool => {
  contrastRatio(foreground, background) >= 4.5
}

/** Check if contrast meets WCAG AA for large text (3:1) */
let meetsWcagAALarge = (foreground: rgb, background: rgb): bool => {
  contrastRatio(foreground, background) >= 3.0
}

/** Check if contrast meets WCAG AAA for normal text (7:1) */
let meetsWcagAAA = (foreground: rgb, background: rgb): bool => {
  contrastRatio(foreground, background) >= 7.0
}

/** Check if contrast meets WCAG AAA for large text (4.5:1) */
let meetsWcagAAALarge = (foreground: rgb, background: rgb): bool => {
  contrastRatio(foreground, background) >= 4.5
}

/** Lighten a color by a percentage (0-1) */
let lighten = (color: rgb, amount: float): rgb => {
  let hsl = rgbToHsl(color)
  let newL = clampUnit(hsl.l +. amount)
  hslToRgb({...hsl, l: newL})
}

/** Darken a color by a percentage (0-1) */
let darken = (color: rgb, amount: float): rgb => {
  let hsl = rgbToHsl(color)
  let newL = clampUnit(hsl.l -. amount)
  hslToRgb({...hsl, l: newL})
}

/** Saturate a color by a percentage (0-1) */
let saturate = (color: rgb, amount: float): rgb => {
  let hsl = rgbToHsl(color)
  let newS = clampUnit(hsl.s +. amount)
  hslToRgb({...hsl, s: newS})
}

/** Desaturate a color by a percentage (0-1) */
let desaturate = (color: rgb, amount: float): rgb => {
  let hsl = rgbToHsl(color)
  let newS = clampUnit(hsl.s -. amount)
  hslToRgb({...hsl, s: newS})
}

/** Invert a color */
let invert = (color: rgb): rgb => {
  {r: 255 - color.r, g: 255 - color.g, b: 255 - color.b}
}

/** Convert to grayscale */
let grayscale = (color: rgb): rgb => {
  let gray = Belt.Float.toInt(luminance(color) *. 255.0)
  {r: gray, g: gray, b: gray}
}

/** Mix two colors */
let mix = (a: rgb, b: rgb, ratio: float): rgb => {
  let ratio = clampUnit(ratio)
  let r = Belt.Float.toInt(
    Belt.Int.toFloat(a.r) *. (1.0 -. ratio) +. Belt.Int.toFloat(b.r) *. ratio,
  )
  let g = Belt.Float.toInt(
    Belt.Int.toFloat(a.g) *. (1.0 -. ratio) +. Belt.Int.toFloat(b.g) *. ratio,
  )
  let bl = Belt.Float.toInt(
    Belt.Int.toFloat(a.b) *. (1.0 -. ratio) +. Belt.Int.toFloat(b.b) *. ratio,
  )
  {r: clampByte(r), g: clampByte(g), b: clampByte(bl)}
}

/** Common colors */
let black: rgb = {r: 0, g: 0, b: 0}
let white: rgb = {r: 255, g: 255, b: 255}
let red: rgb = {r: 255, g: 0, b: 0}
let green: rgb = {r: 0, g: 128, b: 0}
let blue: rgb = {r: 0, g: 0, b: 255}
let yellow: rgb = {r: 255, g: 255, b: 0}
let cyan: rgb = {r: 0, g: 255, b: 255}
let magenta: rgb = {r: 255, g: 0, b: 255}
