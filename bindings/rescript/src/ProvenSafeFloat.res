// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeFloat - Typed wrapper for floating-point operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafeFloat FFI wrapper. */
module SafeFloatJs = {
  @module("../../javascript/src/safe_float.js") @scope("SafeFloat")
  external div: (float, float) => jsResult<float> = "div"

  @module("../../javascript/src/safe_float.js") @scope("SafeFloat")
  external isFinite: float => bool = "isFinite"

  @module("../../javascript/src/safe_float.js") @scope("SafeFloat")
  external isNaN: float => bool = "isNaN"

  @module("../../javascript/src/safe_float.js") @scope("SafeFloat")
  external sqrt: float => jsResult<float> = "sqrt"

  @module("../../javascript/src/safe_float.js") @scope("SafeFloat")
  external ln: float => jsResult<float> = "ln"
}

/**
 * Safe floating-point division.
 * Delegates to proven_float_div via FFI.
 *
 * @param a Dividend.
 * @param b Divisor.
 * @returns Ok(quotient) or Error on division by zero / NaN.
 */
let div = (a: float, b: float): result<float, string> => {
  SafeFloatJs.div(a, b)->fromJs
}

/**
 * Check if a float is finite (not NaN or Infinity).
 * Delegates to proven_float_is_finite via FFI.
 *
 * @param x The value to check.
 * @returns true if the value is finite.
 */
let isFinite = SafeFloatJs.isFinite

/**
 * Check if a float is NaN.
 * Delegates to proven_float_is_nan via FFI.
 *
 * @param x The value to check.
 * @returns true if the value is NaN.
 */
let isNaN = SafeFloatJs.isNaN

/**
 * Safe square root (returns error for negative input).
 * Delegates to proven_float_sqrt via FFI.
 *
 * @param x The value.
 * @returns Ok(sqrt) or Error.
 */
let sqrt = (x: float): result<float, string> => {
  SafeFloatJs.sqrt(x)->fromJs
}

/**
 * Safe natural logarithm (returns error for non-positive input).
 * Delegates to proven_float_ln via FFI.
 *
 * @param x The value.
 * @returns Ok(ln) or Error.
 */
let ln = (x: float): result<float, string> => {
  SafeFloatJs.ln(x)->fromJs
}
