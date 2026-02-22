// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeProbability - Typed wrapper for probability values clamped to [0,1].
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

/** JavaScript bindings to the SafeProbability FFI wrapper. */
module SafeProbabilityJs = {
  @module("../../javascript/src/safe_probability.js") @scope("SafeProbability")
  external create: float => float = "create"

  @module("../../javascript/src/safe_probability.js") @scope("SafeProbability")
  external and_: (float, float) => float = "and"

  @module("../../javascript/src/safe_probability.js") @scope("SafeProbability")
  external orExclusive: (float, float) => float = "orExclusive"

  @module("../../javascript/src/safe_probability.js") @scope("SafeProbability")
  external not: float => float = "not"
}

/**
 * Create a probability value clamped to [0, 1].
 * Delegates to proven_probability_create via FFI.
 *
 * @param value The raw probability value.
 * @returns The clamped probability.
 */
let create = SafeProbabilityJs.create

/**
 * Compute the joint probability of two independent events (P(A and B) = P(A) * P(B)).
 * Delegates to proven_probability_and via FFI.
 *
 * @param a First probability.
 * @param b Second probability.
 * @returns The joint probability.
 */
let and_ = SafeProbabilityJs.and_

/**
 * Compute the exclusive-or probability of two events.
 * Delegates to proven_probability_or_exclusive via FFI.
 *
 * @param a First probability.
 * @param b Second probability.
 * @returns P(A xor B).
 */
let orExclusive = SafeProbabilityJs.orExclusive

/**
 * Compute the complement probability (1 - P).
 * Delegates to proven_probability_not via FFI.
 *
 * @param p The probability.
 * @returns 1 - p.
 */
let not = SafeProbabilityJs.not
