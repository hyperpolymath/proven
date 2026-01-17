// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeComplex - Complex number arithmetic that cannot crash.
 *
 * Provides complex number operations with overflow protection and NaN handling.
 * All operations validate inputs and outputs to ensure numerical stability.
 */

/** Error types for complex number operations */
type complexError =
  | Overflow
  | DivisionByZero
  | NaN
  | Infinity
  | InvalidInput

/** Complex number with 64-bit floating point components */
type complex = {
  real: float,
  imag: float,
}

/** Mathematical constant pi */
let pi = 3.14159265358979323846

/** Check if a float is NaN */
let isNaN = (value: float): bool => {
  value != value
}

/** Check if a float is infinite */
let isInf = (value: float): bool => {
  value == infinity || value == neg_infinity
}

/** Check if a float is finite (not NaN and not infinite) */
let isFinite = (value: float): bool => {
  !isNaN(value) && !isInf(value)
}

/** Validate that a complex number is finite */
let validateFinite = (z: complex): result<unit, complexError> => {
  if isNaN(z.real) || isNaN(z.imag) {
    Error(NaN)
  } else if isInf(z.real) || isInf(z.imag) {
    Error(Infinity)
  } else {
    Ok()
  }
}

/** Create a complex number from real and imaginary parts with validation */
let make = (real: float, imag: float): result<complex, complexError> => {
  if isNaN(real) || isNaN(imag) {
    Error(NaN)
  } else if isInf(real) || isInf(imag) {
    Error(Infinity)
  } else {
    Ok({real, imag})
  }
}

/** Create a purely real complex number */
let fromReal = (real: float): result<complex, complexError> => {
  make(real, 0.0)
}

/** Create a purely imaginary complex number */
let fromImag = (imag: float): result<complex, complexError> => {
  make(0.0, imag)
}

/** Create from polar coordinates (r, theta) */
let fromPolar = (r: float, theta: float): result<complex, complexError> => {
  if isNaN(r) || isNaN(theta) {
    Error(NaN)
  } else if r < 0.0 {
    Error(InvalidInput)
  } else {
    make(r *. Js.Math.cos(theta), r *. Js.Math.sin(theta))
  }
}

/** The zero complex number */
let zero: complex = {real: 0.0, imag: 0.0}

/** The unit imaginary number i */
let i: complex = {real: 0.0, imag: 1.0}

/** The real number one */
let one: complex = {real: 1.0, imag: 0.0}

/** Check if this complex number is valid (not NaN or infinite) */
let isValid = (z: complex): bool => {
  isFinite(z.real) && isFinite(z.imag)
}

/** Check if this complex number is zero */
let isZero = (z: complex): bool => {
  z.real == 0.0 && z.imag == 0.0
}

/** Check if this complex number is purely real */
let isPurelyReal = (z: complex): bool => {
  z.imag == 0.0
}

/** Check if this complex number is purely imaginary */
let isPurelyImaginary = (z: complex): bool => {
  z.real == 0.0 && z.imag != 0.0
}

/** Compute the complex conjugate */
let conjugate = (z: complex): complex => {
  {real: z.real, imag: -.z.imag}
}

/** Compute the negation */
let negate = (z: complex): complex => {
  {real: -.z.real, imag: -.z.imag}
}

/** Safely add two complex numbers */
let add = (a: complex, b: complex): result<complex, complexError> => {
  let real = a.real +. b.real
  let imag = a.imag +. b.imag
  let result = {real, imag}
  switch validateFinite(result) {
  | Error(e) => Error(e)
  | Ok() => Ok(result)
  }
}

/** Safely subtract two complex numbers */
let sub = (a: complex, b: complex): result<complex, complexError> => {
  let real = a.real -. b.real
  let imag = a.imag -. b.imag
  let result = {real, imag}
  switch validateFinite(result) {
  | Error(e) => Error(e)
  | Ok() => Ok(result)
  }
}

/** Safely multiply two complex numbers
 * (a + bi)(c + di) = (ac - bd) + (ad + bc)i
 */
let mul = (a: complex, b: complex): result<complex, complexError> => {
  let real = a.real *. b.real -. a.imag *. b.imag
  let imag = a.real *. b.imag +. a.imag *. b.real
  let result = {real, imag}
  switch validateFinite(result) {
  | Error(e) => Error(e)
  | Ok() => Ok(result)
  }
}

/** Safely divide two complex numbers
 * (a + bi) / (c + di) = ((ac + bd) + (bc - ad)i) / (c^2 + d^2)
 */
let div = (a: complex, b: complex): result<complex, complexError> => {
  let denom = b.real *. b.real +. b.imag *. b.imag
  if denom == 0.0 {
    Error(DivisionByZero)
  } else {
    let real = (a.real *. b.real +. a.imag *. b.imag) /. denom
    let imag = (a.imag *. b.real -. a.real *. b.imag) /. denom
    let result = {real, imag}
    switch validateFinite(result) {
    | Error(e) => Error(e)
    | Ok() => Ok(result)
    }
  }
}

/** Compute the magnitude (absolute value) of a complex number */
let magnitude = (z: complex): result<float, complexError> => {
  let result = Js.Math.sqrt(z.real *. z.real +. z.imag *. z.imag)
  if isNaN(result) {
    Error(NaN)
  } else if isInf(result) {
    Error(Overflow)
  } else {
    Ok(result)
  }
}

/** Compute the squared magnitude (avoids sqrt for comparisons) */
let magnitudeSquared = (z: complex): result<float, complexError> => {
  let result = z.real *. z.real +. z.imag *. z.imag
  if isNaN(result) {
    Error(NaN)
  } else if isInf(result) {
    Error(Overflow)
  } else {
    Ok(result)
  }
}

/** Compute the phase angle (argument) in radians */
let phase = (z: complex): result<float, complexError> => {
  let result = Js.Math.atan2(~y=z.imag, ~x=z.real)
  if isNaN(result) {
    Error(NaN)
  } else {
    Ok(result)
  }
}

/** Safely scale a complex number by a real scalar */
let scale = (z: complex, scalar: float): result<complex, complexError> => {
  if isNaN(scalar) {
    Error(NaN)
  } else if isInf(scalar) {
    Error(Infinity)
  } else {
    let result = {real: z.real *. scalar, imag: z.imag *. scalar}
    switch validateFinite(result) {
    | Error(e) => Error(e)
    | Ok() => Ok(result)
    }
  }
}

/** Compute the reciprocal (1/z) */
let reciprocal = (z: complex): result<complex, complexError> => {
  div(one, z)
}

/** Compute the square of a complex number */
let square = (z: complex): result<complex, complexError> => {
  mul(z, z)
}

/** Safely compute the square root of a complex number */
let sqrt = (z: complex): result<complex, complexError> => {
  switch magnitude(z) {
  | Error(e) => Error(e)
  | Ok(r) =>
    switch phase(z) {
    | Error(e) => Error(e)
    | Ok(theta) => fromPolar(Js.Math.sqrt(r), theta /. 2.0)
    }
  }
}

/** Check approximate equality within epsilon */
let approxEqual = (a: complex, b: complex, epsilon: float): bool => {
  Js.Math.abs_float(a.real -. b.real) <= epsilon &&
    Js.Math.abs_float(a.imag -. b.imag) <= epsilon
}

/** Compute the distance between two complex numbers */
let distance = (a: complex, b: complex): result<float, complexError> => {
  switch sub(a, b) {
  | Error(e) => Error(e)
  | Ok(diff) => magnitude(diff)
  }
}

/** Safe exponentiation by an integer power */
let rec powInt = (z: complex, n: int): result<complex, complexError> => {
  if n == 0 {
    Ok(one)
  } else if n < 0 {
    switch reciprocal(z) {
    | Error(e) => Error(e)
    | Ok(rec_z) => powInt(rec_z, -n)
    }
  } else if n == 1 {
    Ok(z)
  } else {
    // Binary exponentiation
    switch powInt(z, n / 2) {
    | Error(e) => Error(e)
    | Ok(half) =>
      switch mul(half, half) {
      | Error(e) => Error(e)
      | Ok(squared) =>
        if mod(n, 2) == 0 {
          Ok(squared)
        } else {
          mul(squared, z)
        }
      }
    }
  }
}

/** Format a complex number as a string */
let toString = (z: complex): string => {
  if z.imag >= 0.0 {
    `${Belt.Float.toString(z.real)} + ${Belt.Float.toString(z.imag)}i`
  } else {
    `${Belt.Float.toString(z.real)} - ${Belt.Float.toString(Js.Math.abs_float(z.imag))}i`
  }
}

/** Format a complex number in polar form */
let toPolarString = (z: complex): result<string, complexError> => {
  switch (magnitude(z), phase(z)) {
  | (Ok(r), Ok(theta)) => Ok(`${Belt.Float.toString(r)} * e^(${Belt.Float.toString(theta)}i)`)
  | (Error(e), _) | (_, Error(e)) => Error(e)
  }
}

/** Compare two complex numbers by their magnitudes */
let compareMagnitude = (a: complex, b: complex): result<int, complexError> => {
  switch (magnitudeSquared(a), magnitudeSquared(b)) {
  | (Ok(magA), Ok(magB)) =>
    if magA < magB {
      Ok(-1)
    } else if magA > magB {
      Ok(1)
    } else {
      Ok(0)
    }
  | (Error(e), _) | (_, Error(e)) => Error(e)
  }
}

/** Check exact equality of complex numbers */
let equal = (a: complex, b: complex): bool => {
  a.real == b.real && a.imag == b.imag
}

/** Compute the exponential e^z */
let exp = (z: complex): result<complex, complexError> => {
  let expReal = Js.Math.exp(z.real)
  if isInf(expReal) {
    Error(Overflow)
  } else {
    make(expReal *. Js.Math.cos(z.imag), expReal *. Js.Math.sin(z.imag))
  }
}

/** Compute the natural logarithm ln(z) */
let log = (z: complex): result<complex, complexError> => {
  if isZero(z) {
    Error(DivisionByZero)
  } else {
    switch (magnitude(z), phase(z)) {
    | (Ok(r), Ok(theta)) => make(Js.Math.log(r), theta)
    | (Error(e), _) | (_, Error(e)) => Error(e)
    }
  }
}
