// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeRational - Exact rational number arithmetic that cannot crash.
 *
 * Provides rational numbers (fractions) with exact precision that cannot
 * overflow unexpectedly. All operations maintain normalized form with
 * the denominator always positive and GCD(numerator, denominator) = 1.
 */

/** Error types for rational operations */
type rationalError =
  | DivisionByZero
  | Overflow
  | InvalidRepresentation

/** A rational number represented as numerator/denominator
 * Always maintained in normalized form: denominator > 0, gcd(num, denom) = 1
 */
type rational = {
  numerator: int,
  denominator: int,
}

/** Compute GCD using Euclidean algorithm */
let rec gcd = (a: int, b: int): int => {
  let absA = if a < 0 {
    -a
  } else {
    a
  }
  let absB = if b < 0 {
    -b
  } else {
    b
  }
  if absB == 0 {
    absA
  } else {
    gcd(absB, mod(absA, absB))
  }
}

/** Absolute value helper */
let absInt = (value: int): int => {
  if value < 0 {
    -value
  } else {
    value
  }
}

/** Create a rational number with validation and normalization */
let make = (numerator: int, denominator: int): result<rational, rationalError> => {
  if denominator == 0 {
    Error(DivisionByZero)
  } else {
    let num = ref(numerator)
    let denom = ref(denominator)

    // Ensure denominator is positive
    if denom.contents < 0 {
      // Check for overflow when negating min_int
      if num.contents == min_int || denom.contents == min_int {
        Error(Overflow)
      } else {
        num := -num.contents
        denom := -denom.contents

        // Normalize by GCD
        let divisor = gcd(absInt(num.contents), absInt(denom.contents))
        if divisor > 1 {
          num := num.contents / divisor
          denom := denom.contents / divisor
        }

        Ok({numerator: num.contents, denominator: denom.contents})
      }
    } else {
      // Normalize by GCD
      let divisor = gcd(absInt(num.contents), absInt(denom.contents))
      if divisor > 1 {
        num := num.contents / divisor
        denom := denom.contents / divisor
      }

      Ok({numerator: num.contents, denominator: denom.contents})
    }
  }
}

/** Create a rational from an integer */
let fromInt = (value: int): rational => {
  {numerator: value, denominator: 1}
}

/** Create zero */
let zero: rational = {numerator: 0, denominator: 1}

/** Create one */
let one: rational = {numerator: 1, denominator: 1}

/** Create negative one */
let negOne: rational = {numerator: -1, denominator: 1}

/** Check if zero */
let isZero = (r: rational): bool => {
  r.numerator == 0
}

/** Check if positive */
let isPositive = (r: rational): bool => {
  r.numerator > 0
}

/** Check if negative */
let isNegative = (r: rational): bool => {
  r.numerator < 0
}

/** Check if this is an integer (denominator == 1) */
let isInteger = (r: rational): bool => {
  r.denominator == 1
}

/** Negate the rational */
let negate = (r: rational): result<rational, rationalError> => {
  if r.numerator == min_int {
    Error(Overflow)
  } else {
    Ok({numerator: -r.numerator, denominator: r.denominator})
  }
}

/** Absolute value */
let abs = (r: rational): rational => {
  {numerator: absInt(r.numerator), denominator: r.denominator}
}

/** Reciprocal (1/x) */
let reciprocal = (r: rational): result<rational, rationalError> => {
  if r.numerator == 0 {
    Error(DivisionByZero)
  } else {
    make(r.denominator, r.numerator)
  }
}

/** Safe multiplication checking for overflow */
let mulChecked = (a: int, b: int): result<int, rationalError> => {
  if a == 0 || b == 0 {
    Ok(0)
  } else {
    let result = a * b
    if result / a != b {
      Error(Overflow)
    } else {
      Ok(result)
    }
  }
}

/** Safe addition checking for overflow */
let addChecked = (a: int, b: int): result<int, rationalError> => {
  let result = a + b
  if (a > 0 && b > 0 && result < 0) || (a < 0 && b < 0 && result > 0) {
    Error(Overflow)
  } else {
    Ok(result)
  }
}

/** Add two rationals
 * a/b + c/d = (a*d + c*b) / (b*d)
 */
let add = (r1: rational, r2: rational): result<rational, rationalError> => {
  switch mulChecked(r1.numerator, r2.denominator) {
  | Error(e) => Error(e)
  | Ok(ad) =>
    switch mulChecked(r2.numerator, r1.denominator) {
    | Error(e) => Error(e)
    | Ok(cb) =>
      switch addChecked(ad, cb) {
      | Error(e) => Error(e)
      | Ok(num) =>
        switch mulChecked(r1.denominator, r2.denominator) {
        | Error(e) => Error(e)
        | Ok(denom) => make(num, denom)
        }
      }
    }
  }
}

/** Subtract two rationals */
let sub = (r1: rational, r2: rational): result<rational, rationalError> => {
  switch negate(r2) {
  | Error(e) => Error(e)
  | Ok(negR2) => add(r1, negR2)
  }
}

/** Multiply two rationals */
let mul = (r1: rational, r2: rational): result<rational, rationalError> => {
  switch mulChecked(r1.numerator, r2.numerator) {
  | Error(e) => Error(e)
  | Ok(num) =>
    switch mulChecked(r1.denominator, r2.denominator) {
    | Error(e) => Error(e)
    | Ok(denom) => make(num, denom)
    }
  }
}

/** Divide two rationals */
let div = (r1: rational, r2: rational): result<rational, rationalError> => {
  switch reciprocal(r2) {
  | Error(e) => Error(e)
  | Ok(r2Recip) => mul(r1, r2Recip)
  }
}

/** Compare two rationals
 * Returns -1 if r1 < r2, 0 if equal, 1 if r1 > r2
 */
let compare = (r1: rational, r2: rational): result<int, rationalError> => {
  // a/b vs c/d => compare a*d vs c*b
  switch mulChecked(r1.numerator, r2.denominator) {
  | Error(e) => Error(e)
  | Ok(ad) =>
    switch mulChecked(r2.numerator, r1.denominator) {
    | Error(e) => Error(e)
    | Ok(cb) =>
      if ad < cb {
        Ok(-1)
      } else if ad > cb {
        Ok(1)
      } else {
        Ok(0)
      }
    }
  }
}

/** Check equality */
let equal = (r1: rational, r2: rational): bool => {
  r1.numerator == r2.numerator && r1.denominator == r2.denominator
}

/** Convert to floating point (may lose precision) */
let toFloat = (r: rational): float => {
  Belt.Int.toFloat(r.numerator) /. Belt.Int.toFloat(r.denominator)
}

/** Floor division (largest integer <= rational) */
let floor = (r: rational): int => {
  if r.numerator >= 0 {
    r.numerator / r.denominator
  } else {
    // For negative numbers, we need to round toward negative infinity
    let q = r.numerator / r.denominator
    let rem = mod(r.numerator, r.denominator)
    if rem != 0 {
      q - 1
    } else {
      q
    }
  }
}

/** Ceiling (smallest integer >= rational) */
let ceil = (r: rational): int => {
  if r.numerator >= 0 {
    (r.numerator + r.denominator - 1) / r.denominator
  } else {
    r.numerator / r.denominator
  }
}

/** Round to nearest integer */
let round = (r: rational): int => {
  let doubled = r.numerator * 2
  if doubled >= 0 {
    (doubled + r.denominator) / (r.denominator * 2)
  } else {
    (doubled - r.denominator) / (r.denominator * 2)
  }
}

/** Raise to an integer power */
let rec pow = (r: rational, exponent: int): result<rational, rationalError> => {
  if exponent == 0 {
    Ok(one)
  } else if exponent < 0 {
    switch reciprocal(r) {
    | Error(e) => Error(e)
    | Ok(rRecip) => pow(rRecip, -exponent)
    }
  } else if exponent == 1 {
    Ok(r)
  } else {
    // Use binary exponentiation for efficiency
    switch pow(r, exponent / 2) {
    | Error(e) => Error(e)
    | Ok(half) =>
      switch mul(half, half) {
      | Error(e) => Error(e)
      | Ok(squared) =>
        if mod(exponent, 2) == 0 {
          Ok(squared)
        } else {
          mul(squared, r)
        }
      }
    }
  }
}

/** Get the mediant of two rationals: (a+c)/(b+d)
 * Useful for Stern-Brocot tree and Farey sequences
 */
let mediant = (r1: rational, r2: rational): result<rational, rationalError> => {
  switch addChecked(r1.numerator, r2.numerator) {
  | Error(e) => Error(e)
  | Ok(num) =>
    switch addChecked(r1.denominator, r2.denominator) {
    | Error(e) => Error(e)
    | Ok(denom) => make(num, denom)
    }
  }
}

/** Format as a string "numerator/denominator" */
let toString = (r: rational): string => {
  if r.denominator == 1 {
    Belt.Int.toString(r.numerator)
  } else {
    `${Belt.Int.toString(r.numerator)}/${Belt.Int.toString(r.denominator)}`
  }
}

/** Format as a mixed number "whole remainder/denominator" */
let toMixedString = (r: rational): string => {
  if r.denominator == 1 {
    Belt.Int.toString(r.numerator)
  } else {
    let wholePart = floor(r)
    let remainder = absInt(r.numerator - wholePart * r.denominator)
    if wholePart == 0 {
      `${Belt.Int.toString(r.numerator)}/${Belt.Int.toString(r.denominator)}`
    } else if remainder == 0 {
      Belt.Int.toString(wholePart)
    } else {
      `${Belt.Int.toString(wholePart)} ${Belt.Int.toString(remainder)}/${Belt.Int.toString(r.denominator)}`
    }
  }
}

/** Parse a rational from a string "a/b" or "a" */
let parse = (input: string): result<rational, rationalError> => {
  let trimmed = Js.String2.trim(input)
  let parts = Js.String2.split(trimmed, "/")

  if Belt.Array.length(parts) == 1 {
    switch Belt.Int.fromString(trimmed) {
    | Some(n) => Ok(fromInt(n))
    | None => Error(InvalidRepresentation)
    }
  } else if Belt.Array.length(parts) == 2 {
    let numStr = Belt.Array.getUnsafe(parts, 0)
    let denomStr = Belt.Array.getUnsafe(parts, 1)
    switch (Belt.Int.fromString(Js.String2.trim(numStr)), Belt.Int.fromString(Js.String2.trim(denomStr))) {
    | (Some(num), Some(denom)) => make(num, denom)
    | _ => Error(InvalidRepresentation)
    }
  } else {
    Error(InvalidRepresentation)
  }
}

/** Get the sign: -1, 0, or 1 */
let sign = (r: rational): int => {
  if r.numerator < 0 {
    -1
  } else if r.numerator > 0 {
    1
  } else {
    0
  }
}

/** Check if less than */
let lessThan = (r1: rational, r2: rational): result<bool, rationalError> => {
  switch compare(r1, r2) {
  | Error(e) => Error(e)
  | Ok(cmp) => Ok(cmp < 0)
  }
}

/** Check if greater than */
let greaterThan = (r1: rational, r2: rational): result<bool, rationalError> => {
  switch compare(r1, r2) {
  | Error(e) => Error(e)
  | Ok(cmp) => Ok(cmp > 0)
  }
}

/** Check if less than or equal */
let lessThanOrEqual = (r1: rational, r2: rational): result<bool, rationalError> => {
  switch compare(r1, r2) {
  | Error(e) => Error(e)
  | Ok(cmp) => Ok(cmp <= 0)
  }
}

/** Check if greater than or equal */
let greaterThanOrEqual = (r1: rational, r2: rational): result<bool, rationalError> => {
  switch compare(r1, r2) {
  | Error(e) => Error(e)
  | Ok(cmp) => Ok(cmp >= 0)
  }
}

/** Get the minimum of two rationals */
let min = (r1: rational, r2: rational): result<rational, rationalError> => {
  switch compare(r1, r2) {
  | Error(e) => Error(e)
  | Ok(cmp) =>
    if cmp <= 0 {
      Ok(r1)
    } else {
      Ok(r2)
    }
  }
}

/** Get the maximum of two rationals */
let max = (r1: rational, r2: rational): result<rational, rationalError> => {
  switch compare(r1, r2) {
  | Error(e) => Error(e)
  | Ok(cmp) =>
    if cmp >= 0 {
      Ok(r1)
    } else {
      Ok(r2)
    }
  }
}

/** Check if the rational is a proper fraction (|numerator| < denominator) */
let isProperFraction = (r: rational): bool => {
  absInt(r.numerator) < r.denominator
}

/** Common fraction constants */
let oneHalf: rational = {numerator: 1, denominator: 2}
let oneThird: rational = {numerator: 1, denominator: 3}
let twoThirds: rational = {numerator: 2, denominator: 3}
let oneQuarter: rational = {numerator: 1, denominator: 4}
let threeQuarters: rational = {numerator: 3, denominator: 4}
