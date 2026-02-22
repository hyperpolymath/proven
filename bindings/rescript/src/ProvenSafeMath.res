// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeMath - Typed wrapper for arithmetic operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafeMath FFI wrapper. */
module SafeMathJs = {
  @module("../../javascript/src/safe_math.js") @scope("SafeMath")
  external add: (int, int) => jsResult<int> = "add"

  @module("../../javascript/src/safe_math.js") @scope("SafeMath")
  external sub: (int, int) => jsResult<int> = "sub"

  @module("../../javascript/src/safe_math.js") @scope("SafeMath")
  external mul: (int, int) => jsResult<int> = "mul"

  @module("../../javascript/src/safe_math.js") @scope("SafeMath")
  external div: (int, int) => jsResult<int> = "div"

  @module("../../javascript/src/safe_math.js") @scope("SafeMath")
  external mod: (int, int) => jsResult<int> = "mod"

  @module("../../javascript/src/safe_math.js") @scope("SafeMath")
  external abs: int => jsResult<int> = "abs"

  @module("../../javascript/src/safe_math.js") @scope("SafeMath")
  external clamp: (int, int, int) => int = "clamp"

  @module("../../javascript/src/safe_math.js") @scope("SafeMath")
  external pow: (int, int) => jsResult<int> = "pow"
}

/**
 * Safe addition with overflow check.
 * Delegates to proven_math_add_checked via FFI.
 *
 * @param a First operand.
 * @param b Second operand.
 * @returns Ok(sum) or Error on overflow.
 */
let add = (a: int, b: int): result<int, string> => {
  SafeMathJs.add(a, b)->fromJs
}

/**
 * Safe subtraction with underflow check.
 * Delegates to proven_math_sub_checked via FFI.
 *
 * @param a First operand.
 * @param b Second operand.
 * @returns Ok(difference) or Error on underflow.
 */
let sub = (a: int, b: int): result<int, string> => {
  SafeMathJs.sub(a, b)->fromJs
}

/**
 * Safe multiplication with overflow check.
 * Delegates to proven_math_mul_checked via FFI.
 *
 * @param a First operand.
 * @param b Second operand.
 * @returns Ok(product) or Error on overflow.
 */
let mul = (a: int, b: int): result<int, string> => {
  SafeMathJs.mul(a, b)->fromJs
}

/**
 * Safe integer division with division-by-zero check.
 * Delegates to proven_math_div via FFI.
 *
 * @param a Dividend.
 * @param b Divisor.
 * @returns Ok(quotient) or Error on division by zero.
 */
let div = (a: int, b: int): result<int, string> => {
  SafeMathJs.div(a, b)->fromJs
}

/**
 * Safe modulo operation.
 * Delegates to proven_math_mod via FFI.
 *
 * @param a Dividend.
 * @param b Divisor.
 * @returns Ok(remainder) or Error on division by zero.
 */
let mod = (a: int, b: int): result<int, string> => {
  SafeMathJs.mod(a, b)->fromJs
}

/**
 * Safe absolute value (handles MIN_INT correctly).
 * Delegates to proven_math_abs_safe via FFI.
 *
 * @param n Input value.
 * @returns Ok(absolute_value) or Error if n is MIN_INT.
 */
let abs = (n: int): result<int, string> => {
  SafeMathJs.abs(n)->fromJs
}

/**
 * Clamp value to range [lo, hi].
 * Delegates to proven_math_clamp via FFI.
 *
 * @param value Value to clamp.
 * @param lo Minimum value.
 * @param hi Maximum value.
 * @returns The clamped value.
 */
let clamp = SafeMathJs.clamp

/**
 * Safe integer power with overflow checking.
 * Delegates to proven_math_pow_checked via FFI.
 *
 * @param base Base value.
 * @param exponent Exponent (non-negative integer).
 * @returns Ok(result) or Error on overflow.
 */
let pow = (base: int, exponent: int): result<int, string> => {
  SafeMathJs.pow(base, exponent)->fromJs
}

/**
 * Calculate percentage safely using FFI-backed multiply and divide.
 *
 * @param percent The percentage (e.g. 50 for 50%).
 * @param total The total value.
 * @returns Some(result) or None on overflow/division-by-zero.
 */
let percentOf = (percent: int, total: int): option<int> => {
  switch mul(percent, total) {
  | Ok(product) =>
    switch div(product, 100) {
    | Ok(result) => Some(result)
    | Error(_) => None
    }
  | Error(_) => None
  }
}

/**
 * Calculate what percentage part is of whole using FFI-backed operations.
 *
 * @param part The part value.
 * @param whole The whole value.
 * @returns Some(percentage) or None on division by zero / overflow.
 */
let asPercent = (part: int, whole: int): option<int> => {
  switch mul(part, 100) {
  | Ok(scaled) =>
    switch div(scaled, whole) {
    | Ok(result) => Some(result)
    | Error(_) => None
    }
  | Error(_) => None
  }
}
