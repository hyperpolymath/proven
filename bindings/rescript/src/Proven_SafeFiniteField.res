// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeFiniteField - Modular arithmetic operations that cannot crash.
 *
 * Provides safe operations over finite fields (Z/pZ) with:
 * - Overflow-safe modular arithmetic
 * - Modular exponentiation
 * - Modular multiplicative inverse (extended Euclidean algorithm)
 * - Greatest common divisor and least common multiple
 * - Prime field operations for cryptographic applications
 *
 * All operations return option or result types to handle edge cases safely.
 */

// ============================================================================
// Error types
// ============================================================================

/** Error types for finite field operations */
type finiteFieldError =
  | InvalidModulus
  | DivisionByZero
  | NoInverse
  | Overflow
  | NotPrime

/** Result type for finite field operations */
type result<'a> = Ok('a) | Error(finiteFieldError)

// ============================================================================
// Field element type
// ============================================================================

/** A finite field element with its value and modulus */
type fieldElement = {
  value: int,
  modulus: int,
}

// ============================================================================
// Extended GCD result
// ============================================================================

/** Result of extended GCD computation */
type extendedGcdResult = {
  gcd: int,
  x: int,
  y: int,
}

// ============================================================================
// Basic modular operations
// ============================================================================

/** Compute positive modulo (always returns non-negative result) */
let positiveMod = (a: int, m: int): int => {
  let result = mod(a, m)
  if result < 0 {
    result + m
  } else {
    result
  }
}

/** Create a field element, reducing the value modulo the modulus */
let makeFieldElement = (value: int, modulus: int): option<fieldElement> => {
  if modulus <= 1 {
    None
  } else {
    Some({value: positiveMod(value, modulus), modulus})
  }
}

/** Get the raw value from a field element */
let getValue = (element: fieldElement): int => element.value

/** Get the modulus from a field element */
let getModulus = (element: fieldElement): int => element.modulus

/** Check if two field elements are equal */
let equal = (a: fieldElement, b: fieldElement): bool => {
  a.modulus == b.modulus && a.value == b.value
}

/** Check if field element is zero */
let isZero = (element: fieldElement): bool => element.value == 0

/** Check if field element is one */
let isOne = (element: fieldElement): bool => element.value == 1

/** Get the additive identity (zero) for a given modulus */
let zero = (modulus: int): option<fieldElement> => {
  if modulus <= 1 {
    None
  } else {
    Some({value: 0, modulus})
  }
}

/** Get the multiplicative identity (one) for a given modulus */
let one = (modulus: int): option<fieldElement> => {
  if modulus <= 1 {
    None
  } else {
    Some({value: 1, modulus})
  }
}

// ============================================================================
// Field element arithmetic
// ============================================================================

/** Add two field elements */
let add = (a: fieldElement, b: fieldElement): option<fieldElement> => {
  if a.modulus != b.modulus {
    None
  } else {
    let sum = a.value + b.value
    Some({value: positiveMod(sum, a.modulus), modulus: a.modulus})
  }
}

/** Subtract two field elements */
let sub = (a: fieldElement, b: fieldElement): option<fieldElement> => {
  if a.modulus != b.modulus {
    None
  } else {
    let diff = a.value - b.value
    Some({value: positiveMod(diff, a.modulus), modulus: a.modulus})
  }
}

/** Multiply two field elements */
let mul = (a: fieldElement, b: fieldElement): option<fieldElement> => {
  if a.modulus != b.modulus {
    None
  } else {
    let product = a.value * b.value
    Some({value: positiveMod(product, a.modulus), modulus: a.modulus})
  }
}

/** Negate a field element */
let negate = (element: fieldElement): fieldElement => {
  if element.value == 0 {
    element
  } else {
    {value: element.modulus - element.value, modulus: element.modulus}
  }
}

// ============================================================================
// Extended Euclidean algorithm
// ============================================================================

/** Extended Euclidean algorithm to compute GCD and Bezout coefficients.
    Returns gcd(a, b) and coefficients x, y such that ax + by = gcd(a, b) */
let extendedGcd = (a: int, b: int): extendedGcdResult => {
  let rec loop = (oldR, r, oldS, s, oldT, t) => {
    if r == 0 {
      {gcd: abs(oldR), x: oldS, y: oldT}
    } else {
      let quotient = oldR / r
      loop(r, oldR - quotient * r, s, oldS - quotient * s, t, oldT - quotient * t)
    }
  }
  loop(a, b, 1, 0, 0, 1)
}

/** Compute GCD of two numbers */
let gcd = (a: int, b: int): int => {
  let rec loop = (x, y) => {
    if y == 0 {
      abs(x)
    } else {
      loop(y, mod(x, y))
    }
  }
  loop(a, b)
}

/** Compute LCM of two numbers, returning None on overflow */
let lcm = (a: int, b: int): option<int> => {
  if a == 0 || b == 0 {
    Some(0)
  } else {
    let g = gcd(a, b)
    let divResult = a / g
    // Check for overflow
    let result = divResult * b
    if divResult != 0 && result / divResult != b {
      None
    } else {
      Some(abs(result))
    }
  }
}

/** Check if two numbers are coprime (GCD is 1) */
let coprime = (a: int, b: int): bool => gcd(a, b) == 1

// ============================================================================
// Modular operations (standalone functions)
// ============================================================================

/** Modular addition without overflow */
let modAdd = (a: int, b: int, modulus: int): result<int> => {
  if modulus <= 0 {
    Error(InvalidModulus)
  } else {
    let aMod = positiveMod(a, modulus)
    let bMod = positiveMod(b, modulus)
    Ok(positiveMod(aMod + bMod, modulus))
  }
}

/** Modular subtraction without underflow */
let modSub = (a: int, b: int, modulus: int): result<int> => {
  if modulus <= 0 {
    Error(InvalidModulus)
  } else {
    let aMod = positiveMod(a, modulus)
    let bMod = positiveMod(b, modulus)
    Ok(positiveMod(aMod - bMod, modulus))
  }
}

/** Modular multiplication without overflow */
let modMul = (a: int, b: int, modulus: int): result<int> => {
  if modulus <= 0 {
    Error(InvalidModulus)
  } else {
    let aMod = positiveMod(a, modulus)
    let bMod = positiveMod(b, modulus)
    Ok(positiveMod(aMod * bMod, modulus))
  }
}

/** Modular exponentiation using square-and-multiply */
let modPow = (base: int, exponent: int, modulus: int): result<int> => {
  if modulus <= 0 {
    Error(InvalidModulus)
  } else if modulus == 1 {
    Ok(0)
  } else if exponent < 0 {
    Error(InvalidModulus)
  } else if exponent == 0 {
    Ok(1)
  } else {
    let rec loop = (result, b, exp) => {
      if exp <= 0 {
        result
      } else {
        let newResult = if land(exp, 1) == 1 {
          positiveMod(result * b, modulus)
        } else {
          result
        }
        loop(newResult, positiveMod(b * b, modulus), lsr(exp, 1))
      }
    }
    Ok(loop(1, positiveMod(base, modulus), exponent))
  }
}

/** Compute modular multiplicative inverse using extended Euclidean algorithm */
let modInverse = (a: int, modulus: int): result<int> => {
  if modulus <= 0 {
    Error(InvalidModulus)
  } else if a == 0 {
    Error(DivisionByZero)
  } else {
    let result = extendedGcd(positiveMod(a, modulus), modulus)
    if result.gcd != 1 {
      Error(NoInverse)
    } else {
      Ok(positiveMod(result.x, modulus))
    }
  }
}

/** Modular division (multiply by inverse) */
let modDiv = (a: int, b: int, modulus: int): result<int> => {
  switch modInverse(b, modulus) {
  | Error(e) => Error(e)
  | Ok(bInv) => modMul(a, bInv, modulus)
  }
}

// ============================================================================
// Field element advanced operations
// ============================================================================

/** Compute modular exponentiation for a field element */
let pow = (element: fieldElement, exponent: int): option<fieldElement> => {
  if exponent < 0 {
    None
  } else {
    switch modPow(element.value, exponent, element.modulus) {
    | Error(_) => None
    | Ok(result) => Some({value: result, modulus: element.modulus})
    }
  }
}

/** Compute multiplicative inverse of a field element */
let inverse = (element: fieldElement): result<fieldElement> => {
  switch modInverse(element.value, element.modulus) {
  | Error(e) => Error(e)
  | Ok(inv) => Ok({value: inv, modulus: element.modulus})
  }
}

/** Divide two field elements (multiply by inverse) */
let div = (a: fieldElement, b: fieldElement): result<fieldElement> => {
  if a.modulus != b.modulus {
    Error(InvalidModulus)
  } else {
    switch inverse(b) {
    | Error(e) => Error(e)
    | Ok(bInv) =>
      switch mul(a, bInv) {
      | None => Error(InvalidModulus)
      | Some(result) => Ok(result)
      }
    }
  }
}

// ============================================================================
// Common prime field definitions
// ============================================================================

/** Field with modulus 17 */
let f17 = (value: int): option<fieldElement> => makeFieldElement(value, 17)

/** Field with modulus 31 */
let f31 = (value: int): option<fieldElement> => makeFieldElement(value, 31)

/** Field with modulus 101 */
let f101 = (value: int): option<fieldElement> => makeFieldElement(value, 101)

/** Field with modulus 257 */
let f257 = (value: int): option<fieldElement> => makeFieldElement(value, 257)

/** Field with modulus 65537 (Fermat prime F4) */
let f65537 = (value: int): option<fieldElement> => makeFieldElement(value, 65537)

// ============================================================================
// Utility functions
// ============================================================================

/** Check if a number is prime using trial division */
let isPrime = (n: int): bool => {
  if n <= 1 {
    false
  } else if n <= 3 {
    true
  } else if mod(n, 2) == 0 || mod(n, 3) == 0 {
    false
  } else {
    let rec loop = (i) => {
      if i * i > n {
        true
      } else if mod(n, i) == 0 || mod(n, i + 2) == 0 {
        false
      } else {
        loop(i + 6)
      }
    }
    loop(5)
  }
}

/** Euler's totient function (phi) */
let eulerTotient = (n: int): option<int> => {
  if n <= 0 {
    None
  } else if n == 1 {
    Some(1)
  } else {
    let rec loop = (result, n, p) => {
      if p * p > n {
        if n > 1 {
          result * (n - 1)
        } else {
          result
        }
      } else if mod(n, p) == 0 {
        let rec divideOut = (n, count) => {
          if mod(n, p) == 0 {
            divideOut(n / p, count + 1)
          } else {
            (n, count)
          }
        }
        let (newN, _count) = divideOut(n, 0)
        // phi(p^k) = p^k - p^(k-1) = p^(k-1) * (p - 1)
        let contribution = result * (p - 1)
        let rec mulByP = (acc, remaining) => {
          if remaining <= 1 {
            acc
          } else {
            mulByP(acc * p, remaining - 1)
          }
        }
        loop(mulByP(contribution, 1), newN, p + (if p == 2 { 1 } else { 2 }))
      } else {
        loop(result, n, p + (if p == 2 { 1 } else { 2 }))
      }
    }
    Some(loop(1, n, 2))
  }
}

/** Convert field element to string representation */
let toString = (element: fieldElement): string => {
  Belt.Int.toString(element.value) ++ " (mod " ++ Belt.Int.toString(element.modulus) ++ ")"
}
