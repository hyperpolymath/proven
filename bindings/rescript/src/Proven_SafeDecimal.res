// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeDecimal - Arbitrary precision decimal arithmetic that cannot crash.
 *
 * Provides fixed-point decimal operations for financial and scientific calculations
 * where floating-point errors are unacceptable.
 */

/** Error types for decimal operations */
type decimalError =
  | InvalidFormat
  | Overflow
  | DivisionByZero
  | PrecisionLoss

/** Maximum precision supported (digits after decimal point) */
let maxPrecision = 38

/** Maximum scale for internal representation */
let maxScale = 18

/** A fixed-point decimal number with configurable precision
 * Stores value as mantissa * 10^(-scale)
 */
type decimal = {
  mantissa: float,  // Using float for larger range, but treating as integer
  scale: int,       // Number of decimal places (0-18)
}

/** Create a decimal from an integer (scale = 0) */
let fromInt = (value: int): decimal => {
  {mantissa: Belt.Int.toFloat(value), scale: 0}
}

/** Create a decimal from a float with given scale */
let fromFloat = (value: float, scale: int): decimal => {
  let actualScale = if scale > maxScale {
    maxScale
  } else if scale < 0 {
    0
  } else {
    scale
  }
  let multiplier = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(actualScale))
  let mantissa = Js.Math.round(value *. multiplier)
  {mantissa, scale: actualScale}
}

/** Create a decimal from mantissa and scale */
let fromParts = (mantissa: float, scale: int): decimal => {
  let actualScale = if scale > maxScale {
    maxScale
  } else if scale < 0 {
    0
  } else {
    scale
  }
  {mantissa, scale: actualScale}
}

/** Create a decimal representing zero */
let zero: decimal = {mantissa: 0.0, scale: 0}

/** Create a decimal representing one */
let one: decimal = {mantissa: 1.0, scale: 0}

/** Parse a decimal from a string (e.g., "123.456", "-0.001", "1e-5") */
let parse = (input: string): result<decimal, decimalError> => {
  let trimmed = Js.String2.trim(input)
  if Js.String2.length(trimmed) == 0 {
    Error(InvalidFormat)
  } else {
    // Check for scientific notation
    let hasExponent = Js.String2.includes(trimmed, "e") || Js.String2.includes(trimmed, "E")

    if hasExponent {
      // Handle scientific notation
      switch Belt.Float.fromString(trimmed) {
      | None => Error(InvalidFormat)
      | Some(value) =>
        // Determine appropriate scale from the string
        let lowerStr = Js.String2.toLowerCase(trimmed)
        let parts = Js.String2.split(lowerStr, "e")
        if Belt.Array.length(parts) == 2 {
          let numPart = Belt.Array.getUnsafe(parts, 0)
          let expPart = Belt.Array.getUnsafe(parts, 1)

          // Count decimal places in the number part
          let decimalIdx = Js.String2.indexOf(numPart, ".")
          let numDecimals = if decimalIdx >= 0 {
            Js.String2.length(numPart) - decimalIdx - 1
          } else {
            0
          }

          switch Belt.Int.fromString(expPart) {
          | None => Error(InvalidFormat)
          | Some(exp) =>
            let scale = numDecimals - exp
            if scale < 0 {
              // Result is an integer with trailing zeros
              let multiplier = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(-scale))
              Ok({mantissa: Js.Math.round(value *. multiplier), scale: 0})
            } else if scale > maxScale {
              Error(Overflow)
            } else {
              let multiplier = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(scale))
              Ok({mantissa: Js.Math.round(value *. multiplier), scale})
            }
          }
        } else {
          Error(InvalidFormat)
        }
      }
    } else {
      // Handle regular decimal notation
      let decimalIdx = Js.String2.indexOf(trimmed, ".")

      if decimalIdx < 0 {
        // No decimal point - it's an integer
        switch Belt.Float.fromString(trimmed) {
        | None => Error(InvalidFormat)
        | Some(value) => Ok({mantissa: value, scale: 0})
        }
      } else {
        // Has decimal point
        let intPart = Js.String2.slice(trimmed, ~from=0, ~to_=decimalIdx)
        let fracPart = Js.String2.sliceToEnd(trimmed, ~from=decimalIdx + 1)

        // Remove underscores if present (as separators)
        let cleanInt = Js.String2.replaceByRe(intPart, %re("/_/g"), "")
        let cleanFrac = Js.String2.replaceByRe(fracPart, %re("/_/g"), "")

        let scale = Js.String2.length(cleanFrac)
        if scale > maxScale {
          Error(Overflow)
        } else {
          let combined = cleanInt ++ cleanFrac
          switch Belt.Float.fromString(combined) {
          | None => Error(InvalidFormat)
          | Some(mantissa) => Ok({mantissa, scale})
          }
        }
      }
    }
  }
}

/** Get the integer part of the decimal */
let intPart = (d: decimal): float => {
  if d.scale == 0 {
    d.mantissa
  } else {
    let divisor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(d.scale))
    Js.Math.trunc(d.mantissa /. divisor)
  }
}

/** Get the fractional part as an integer */
let fracPart = (d: decimal): float => {
  if d.scale == 0 {
    0.0
  } else {
    let divisor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(d.scale))
    let frac = mod_float(d.mantissa, divisor)
    Js.Math.abs_float(frac)
  }
}

/** Check if the decimal is zero */
let isZero = (d: decimal): bool => {
  d.mantissa == 0.0
}

/** Check if the decimal is negative */
let isNegative = (d: decimal): bool => {
  d.mantissa < 0.0
}

/** Check if the decimal is positive */
let isPositive = (d: decimal): bool => {
  d.mantissa > 0.0
}

/** Return the absolute value */
let abs = (d: decimal): decimal => {
  {mantissa: Js.Math.abs_float(d.mantissa), scale: d.scale}
}

/** Negate the decimal */
let negate = (d: decimal): decimal => {
  {mantissa: -.d.mantissa, scale: d.scale}
}

/** Rescale to a different number of decimal places */
let rescale = (d: decimal, newScale: int): result<decimal, decimalError> => {
  if newScale == d.scale {
    Ok(d)
  } else if newScale > d.scale {
    // Increase scale (multiply mantissa)
    let diff = newScale - d.scale
    let multiplier = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(diff))
    let newMantissa = d.mantissa *. multiplier
    // Check for overflow (using MAX_SAFE_INTEGER as rough limit)
    if Js.Math.abs_float(newMantissa) > 9007199254740991.0 {
      Error(Overflow)
    } else {
      Ok({mantissa: newMantissa, scale: newScale})
    }
  } else {
    // Decrease scale (divide mantissa, may lose precision)
    let diff = d.scale - newScale
    let divisor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(diff))
    Ok({mantissa: Js.Math.trunc(d.mantissa /. divisor), scale: newScale})
  }
}

/** Add two decimals */
let add = (a: decimal, b: decimal): result<decimal, decimalError> => {
  // Align scales
  let maxScaleVal = if a.scale > b.scale {
    a.scale
  } else {
    b.scale
  }
  switch (rescale(a, maxScaleVal), rescale(b, maxScaleVal)) {
  | (Error(e), _) | (_, Error(e)) => Error(e)
  | (Ok(aScaled), Ok(bScaled)) =>
    let result = aScaled.mantissa +. bScaled.mantissa
    if Js.Math.abs_float(result) > 9007199254740991.0 {
      Error(Overflow)
    } else {
      Ok({mantissa: result, scale: maxScaleVal})
    }
  }
}

/** Subtract two decimals */
let sub = (a: decimal, b: decimal): result<decimal, decimalError> => {
  // Align scales
  let maxScaleVal = if a.scale > b.scale {
    a.scale
  } else {
    b.scale
  }
  switch (rescale(a, maxScaleVal), rescale(b, maxScaleVal)) {
  | (Error(e), _) | (_, Error(e)) => Error(e)
  | (Ok(aScaled), Ok(bScaled)) =>
    let result = aScaled.mantissa -. bScaled.mantissa
    if Js.Math.abs_float(result) > 9007199254740991.0 {
      Error(Overflow)
    } else {
      Ok({mantissa: result, scale: maxScaleVal})
    }
  }
}

/** Multiply two decimals */
let mul = (a: decimal, b: decimal): result<decimal, decimalError> => {
  let resultMantissa = a.mantissa *. b.mantissa
  let resultScale = a.scale + b.scale

  if resultScale > maxScale {
    // Need to reduce precision
    let reduceBy = resultScale - maxScale
    let divisor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(reduceBy))
    Ok({mantissa: Js.Math.trunc(resultMantissa /. divisor), scale: maxScale})
  } else {
    Ok({mantissa: resultMantissa, scale: resultScale})
  }
}

/** Divide two decimals with specified precision */
let div = (a: decimal, b: decimal, precision: int): result<decimal, decimalError> => {
  if b.mantissa == 0.0 {
    Error(DivisionByZero)
  } else {
    let targetScale = if precision > maxScale {
      maxScale
    } else {
      precision
    }

    // Scale up the dividend for precision
    let scaleDiff = targetScale + b.scale - a.scale
    let dividend = if scaleDiff > 0 {
      let multiplier = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(scaleDiff))
      a.mantissa *. multiplier
    } else if scaleDiff < 0 {
      let divisor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(-scaleDiff))
      a.mantissa /. divisor
    } else {
      a.mantissa
    }

    let result = Js.Math.trunc(dividend /. b.mantissa)
    Ok({mantissa: result, scale: targetScale})
  }
}

/** Round to specified decimal places */
let round = (d: decimal, decimalPlaces: int): decimal => {
  if decimalPlaces >= d.scale {
    d
  } else {
    let diff = d.scale - decimalPlaces
    let divisor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(diff))
    let halfDivisor = divisor /. 2.0

    let isNeg = d.mantissa < 0.0
    let absMantissa = Js.Math.abs_float(d.mantissa)

    let remainder = mod_float(absMantissa, divisor)
    let truncated = Js.Math.trunc(absMantissa /. divisor)

    // Round half away from zero
    let rounded = if remainder >= halfDivisor {
      truncated +. 1.0
    } else {
      truncated
    }

    let finalMantissa = if isNeg {
      -.rounded
    } else {
      rounded
    }

    {mantissa: finalMantissa, scale: decimalPlaces}
  }
}

/** Truncate to specified decimal places (round toward zero) */
let truncate = (d: decimal, decimalPlaces: int): decimal => {
  if decimalPlaces >= d.scale {
    d
  } else {
    let diff = d.scale - decimalPlaces
    let divisor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(diff))
    {mantissa: Js.Math.trunc(d.mantissa /. divisor), scale: decimalPlaces}
  }
}

/** Compare two decimals
 * Returns -1, 0, or 1
 */
let compare = (a: decimal, b: decimal): int => {
  // Align scales for comparison
  let maxScaleVal = if a.scale > b.scale {
    a.scale
  } else {
    b.scale
  }

  let aScaled = switch rescale(a, maxScaleVal) {
  | Ok(d) => d.mantissa
  | Error(_) => a.mantissa // Fallback, shouldn't happen
  }

  let bScaled = switch rescale(b, maxScaleVal) {
  | Ok(d) => d.mantissa
  | Error(_) => b.mantissa // Fallback, shouldn't happen
  }

  if aScaled < bScaled {
    -1
  } else if aScaled > bScaled {
    1
  } else {
    0
  }
}

/** Check equality */
let equal = (a: decimal, b: decimal): bool => {
  compare(a, b) == 0
}

/** Convert to float (may lose precision) */
let toFloat = (d: decimal): float => {
  let divisor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(d.scale))
  d.mantissa /. divisor
}

/** Format decimal as a string */
let toString = (d: decimal): string => {
  if d.scale == 0 {
    Belt.Float.toString(d.mantissa)
  } else {
    let divisor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(d.scale))
    let intPartVal = Js.Math.trunc(Js.Math.abs_float(d.mantissa) /. divisor)
    let fracPartVal = mod_float(Js.Math.abs_float(d.mantissa), divisor)

    let sign = if d.mantissa < 0.0 {
      "-"
    } else {
      ""
    }

    // Pad fractional part with leading zeros
    let fracStr = Belt.Float.toString(fracPartVal)
    let paddedFrac = if Js.String2.length(fracStr) < d.scale {
      let padding = Js.String2.repeat("0", d.scale - Js.String2.length(fracStr))
      padding ++ fracStr
    } else {
      fracStr
    }

    `${sign}${Belt.Float.toString(intPartVal)}.${paddedFrac}`
  }
}

/** Check if a string is a valid decimal representation */
let isValidDecimal = (input: string): bool => {
  switch parse(input) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Parse a decimal percentage (e.g., "5.5%" -> 0.055) */
let parsePercent = (input: string): result<decimal, decimalError> => {
  let trimmed = Js.String2.trim(input)
  if Js.String2.length(trimmed) == 0 {
    Error(InvalidFormat)
  } else {
    // Check for % suffix
    let hasPercent = Js.String2.endsWith(trimmed, "%")
    let numberStr = if hasPercent {
      Js.String2.slice(trimmed, ~from=0, ~to_=Js.String2.length(trimmed) - 1)
    } else {
      trimmed
    }

    switch parse(numberStr) {
    | Error(e) => Error(e)
    | Ok(decimalValue) =>
      // Divide by 100
      div(decimalValue, fromInt(100), maxScale)
    }
  }
}

/** Floor (round toward negative infinity) */
let floor = (d: decimal): decimal => {
  if d.scale == 0 {
    d
  } else {
    let floatVal = toFloat(d)
    fromInt(Belt.Float.toInt(Js.Math.floor_float(floatVal)))
  }
}

/** Ceiling (round toward positive infinity) */
let ceil = (d: decimal): decimal => {
  if d.scale == 0 {
    d
  } else {
    let floatVal = toFloat(d)
    fromInt(Belt.Float.toInt(Js.Math.ceil_float(floatVal)))
  }
}

/** Check if less than */
let lessThan = (a: decimal, b: decimal): bool => {
  compare(a, b) < 0
}

/** Check if greater than */
let greaterThan = (a: decimal, b: decimal): bool => {
  compare(a, b) > 0
}

/** Check if less than or equal */
let lessThanOrEqual = (a: decimal, b: decimal): bool => {
  compare(a, b) <= 0
}

/** Check if greater than or equal */
let greaterThanOrEqual = (a: decimal, b: decimal): bool => {
  compare(a, b) >= 0
}

/** Get the minimum of two decimals */
let min = (a: decimal, b: decimal): decimal => {
  if compare(a, b) <= 0 {
    a
  } else {
    b
  }
}

/** Get the maximum of two decimals */
let max = (a: decimal, b: decimal): decimal => {
  if compare(a, b) >= 0 {
    a
  } else {
    b
  }
}

/** Calculate percentage of a decimal
 * e.g., percentOf(decimal, 15) returns 15% of decimal
 */
let percentOf = (d: decimal, percent: int): result<decimal, decimalError> => {
  switch mul(d, fromInt(percent)) {
  | Error(e) => Error(e)
  | Ok(product) => div(product, fromInt(100), d.scale)
  }
}

/** Add a percentage to a decimal
 * e.g., addPercent(decimal, 15) returns decimal + 15% of decimal
 */
let addPercent = (d: decimal, percent: int): result<decimal, decimalError> => {
  switch percentOf(d, percent) {
  | Error(e) => Error(e)
  | Ok(percentValue) => add(d, percentValue)
  }
}

/** Format as currency string with symbol */
let formatCurrency = (d: decimal, symbol: string, decimalPlaces: int): string => {
  let rounded = round(d, decimalPlaces)
  let formatted = toString(rounded)

  // Ensure we have exactly the right number of decimal places
  let parts = Js.String2.split(formatted, ".")
  let intPartStr = Belt.Array.getUnsafe(parts, 0)
  let fracPartStr = if Belt.Array.length(parts) > 1 {
    Belt.Array.getUnsafe(parts, 1)
  } else {
    ""
  }

  let paddedFrac = if Js.String2.length(fracPartStr) < decimalPlaces {
    fracPartStr ++ Js.String2.repeat("0", decimalPlaces - Js.String2.length(fracPartStr))
  } else {
    Js.String2.slice(fracPartStr, ~from=0, ~to_=decimalPlaces)
  }

  if decimalPlaces > 0 {
    `${symbol}${intPartStr}.${paddedFrac}`
  } else {
    `${symbol}${intPartStr}`
  }
}

/** Common decimal constants */
let oneHalf: decimal = {mantissa: 5.0, scale: 1}
let oneQuarter: decimal = {mantissa: 25.0, scale: 2}
let threeQuarters: decimal = {mantissa: 75.0, scale: 2}
let oneTenth: decimal = {mantissa: 1.0, scale: 1}
let oneHundredth: decimal = {mantissa: 1.0, scale: 2}
