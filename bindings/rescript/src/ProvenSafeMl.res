// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeML - Typed wrapper for machine learning primitives with numerical stability.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

/** JavaScript bindings to the SafeML FFI wrapper. */
module SafeMlJs = {
  @module("../../javascript/src/safe_ml.js") @scope("SafeML")
  external sigmoid: float => float = "sigmoid"

  @module("../../javascript/src/safe_ml.js") @scope("SafeML")
  external relu: float => float = "relu"

  @module("../../javascript/src/safe_ml.js") @scope("SafeML")
  external leakyRelu: (float, float) => float = "leakyRelu"

  @module("../../javascript/src/safe_ml.js") @scope("SafeML")
  external clamp: (float, float, float) => float = "clamp"
}

/**
 * Sigmoid activation function.
 * Delegates to proven_ml_sigmoid via FFI.
 *
 * @param x Input value.
 * @returns Sigmoid(x) in range (0, 1).
 */
let sigmoid = SafeMlJs.sigmoid

/**
 * ReLU activation function.
 * Delegates to proven_ml_relu via FFI.
 *
 * @param x Input value.
 * @returns max(0, x).
 */
let relu = SafeMlJs.relu

/**
 * Leaky ReLU activation function.
 * Delegates to proven_ml_leaky_relu via FFI.
 *
 * @param x Input value.
 * @param alpha Slope for negative values.
 * @returns x if x > 0, alpha * x otherwise.
 */
let leakyRelu = SafeMlJs.leakyRelu

/**
 * Clamp a value to a range.
 * Delegates to proven_ml_clamp via FFI.
 *
 * @param x Input value.
 * @param lo Minimum value.
 * @param hi Maximum value.
 * @returns Clamped value.
 */
let clamp = SafeMlJs.clamp
