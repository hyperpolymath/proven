// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeMonotonic - Typed wrapper for monotonic counters and timestamps.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides MonotonicCounter, MonotonicTimestamp,
 * MonotonicID, MonotonicSequence, and MonotonicVersion classes
 * backed by native monotonic implementations.
 */

/** Opaque handle to a JavaScript MonotonicCounter instance. */
type monotonicCounter

/** Opaque handle to a JavaScript MonotonicTimestamp instance. */
type monotonicTimestamp

/** Opaque handle to a JavaScript MonotonicID instance. */
type monotonicId

/** Opaque handle to a JavaScript MonotonicSequence instance. */
type monotonicSequence

/** Opaque handle to a JavaScript MonotonicVersion instance. */
type monotonicVersion

/** JavaScript bindings to the SafeMonotonic FFI wrapper. */
module SafeMonotonicJs = {
  @module("../../javascript/src/safe_monotonic.js")
  @new
  external createCounter: (int, int) => monotonicCounter = "MonotonicCounter"

  @module("../../javascript/src/safe_monotonic.js")
  @new
  external createTimestamp: unit => monotonicTimestamp = "MonotonicTimestamp"

  @module("../../javascript/src/safe_monotonic.js")
  @new
  external createId: int => monotonicId = "MonotonicID"

  @module("../../javascript/src/safe_monotonic.js")
  @new
  external createSequence: (int, int, int) => monotonicSequence = "MonotonicSequence"

  @module("../../javascript/src/safe_monotonic.js")
  @new
  external createVersion: (int, int, int) => monotonicVersion = "MonotonicVersion"
}

/**
 * Create a new monotonic counter.
 *
 * @param initial Initial value (default: 0).
 * @param max Maximum value.
 * @returns A new MonotonicCounter handle.
 */
let createCounter = SafeMonotonicJs.createCounter

/**
 * Create a new monotonic timestamp.
 *
 * @returns A new MonotonicTimestamp handle.
 */
let createTimestamp = SafeMonotonicJs.createTimestamp

/**
 * Create a new monotonic ID generator.
 *
 * @param machineId Machine ID for distributed ID generation.
 * @returns A new MonotonicID handle.
 */
let createId = SafeMonotonicJs.createId

/**
 * Create a new monotonic sequence.
 *
 * @param start Starting value.
 * @param step Step size.
 * @param max Maximum value.
 * @returns A new MonotonicSequence handle.
 */
let createSequence = SafeMonotonicJs.createSequence

/**
 * Create a new monotonic version.
 *
 * @param major Major version.
 * @param minor Minor version.
 * @param patch Patch version.
 * @returns A new MonotonicVersion handle.
 */
let createVersion = SafeMonotonicJs.createVersion
