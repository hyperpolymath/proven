// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeFloat - Safe floating-point operations that cannot crash.
 *
 * Handles NaN, infinity, and precision issues gracefully.
 */

/** Check if a float is a valid number (not NaN, not infinite) */
let isFinite = (n: float): bool => {
  Js.Float.isFinite(n)
}

/** Check if a float is NaN */
let isNaN = (n: float): bool => {
  Js.Float.isNaN(n)
}

/** Safe division that returns None on division by zero or invalid result */
let div = (numerator: float, denominator: float): option<float> => {
  if denominator == 0.0 {
    None
  } else {
    let result = numerator /. denominator
    if isFinite(result) {
      Some(result)
    } else {
      None
    }
  }
}

/** Safe division with default value */
let divOr = (default: float, numerator: float, denominator: float): float => {
  switch div(numerator, denominator) {
  | Some(v) => v
  | None => default
  }
}

/** Safe square root that returns None for negative numbers */
let sqrt = (n: float): option<float> => {
  if n < 0.0 {
    None
  } else {
    Some(Js.Math.sqrt(n))
  }
}

/** Safe logarithm that returns None for non-positive numbers */
let log = (n: float): option<float> => {
  if n <= 0.0 {
    None
  } else {
    Some(Js.Math.log(n))
  }
}

/** Safe log10 that returns None for non-positive numbers */
let log10 = (n: float): option<float> => {
  if n <= 0.0 {
    None
  } else {
    Some(Js.Math.log10(n))
  }
}

/** Safe log2 that returns None for non-positive numbers */
let log2 = (n: float): option<float> => {
  if n <= 0.0 {
    None
  } else {
    Some(Js.Math.log2(n))
  }
}

/** Safe power that handles edge cases */
let pow = (base: float, exp: float): option<float> => {
  let result = Js.Math.pow_float(~base, ~exp)
  if isFinite(result) {
    Some(result)
  } else {
    None
  }
}

/** Clamp a float to a range */
let clamp = (lo: float, hi: float, value: float): float => {
  if value < lo {
    lo
  } else if value > hi {
    hi
  } else {
    value
  }
}

/** Round to a specific number of decimal places */
let roundTo = (decimals: int, value: float): float => {
  let factor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(decimals))
  Js.Math.round(value *. factor) /. factor
}

/** Floor to a specific number of decimal places */
let floorTo = (decimals: int, value: float): float => {
  let factor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(decimals))
  Js.Math.floor_float(value *. factor) /. factor
}

/** Ceiling to a specific number of decimal places */
let ceilTo = (decimals: int, value: float): float => {
  let factor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(decimals))
  Js.Math.ceil_float(value *. factor) /. factor
}

/** Parse a float from string safely */
let fromString = (str: string): option<float> => {
  let trimmed = Js.String2.trim(str)
  if Js.String2.length(trimmed) == 0 {
    None
  } else {
    let parsed = Js.Float.fromString(trimmed)
    if isNaN(parsed) {
      None
    } else {
      Some(parsed)
    }
  }
}

/** Convert float to string with fixed decimal places */
let toFixed = (decimals: int, value: float): string => {
  Js.Float.toFixedWithPrecision(value, ~digits=decimals)
}

/** Convert float to string with significant digits */
let toPrecision = (digits: int, value: float): string => {
  Js.Float.toPrecisionWithPrecision(value, ~digits)
}

/** Check if two floats are approximately equal within epsilon */
let approxEqual = (epsilon: float, a: float, b: float): bool => {
  Js.Math.abs_float(a -. b) <= epsilon
}

/** Linear interpolation between two values */
let lerp = (a: float, b: float, t: float): float => {
  a +. (b -. a) *. t
}

/** Inverse linear interpolation: find t such that lerp(a, b, t) = value */
let inverseLerp = (a: float, b: float, value: float): option<float> => {
  if a == b {
    None
  } else {
    Some((value -. a) /. (b -. a))
  }
}

/** Remap a value from one range to another */
let remap = (
  inMin: float,
  inMax: float,
  outMin: float,
  outMax: float,
  value: float,
): option<float> => {
  switch inverseLerp(inMin, inMax, value) {
  | None => None
  | Some(t) => Some(lerp(outMin, outMax, t))
  }
}

/** Check if a value is within a range */
let inRange = (value: float, lo: float, hi: float): bool => {
  value >= lo && value <= hi
}

/** Calculate the sign of a number: -1, 0, or 1 */
let sign = (n: float): int => {
  if n < 0.0 {
    -1
  } else if n > 0.0 {
    1
  } else {
    0
  }
}

/** Safe modulo that handles negative numbers correctly */
let safeMod = (a: float, b: float): option<float> => {
  if b == 0.0 {
    None
  } else {
    let result = mod_float(a, b)
    // Make result have same sign as divisor
    if result < 0.0 && b > 0.0 {
      Some(result +. b)
    } else if result > 0.0 && b < 0.0 {
      Some(result +. b)
    } else {
      Some(result)
    }
  }
}
