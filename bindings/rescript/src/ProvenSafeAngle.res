// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeAngle - Typed wrapper for angle conversions with proper wrapping.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

/** JavaScript bindings to the SafeAngle FFI wrapper. */
module SafeAngleJs = {
  @module("../../javascript/src/safe_angle.js") @scope("SafeAngle")
  external degToRad: float => float = "degToRad"

  @module("../../javascript/src/safe_angle.js") @scope("SafeAngle")
  external radToDeg: float => float = "radToDeg"

  @module("../../javascript/src/safe_angle.js") @scope("SafeAngle")
  external normalizeDegrees: float => float = "normalizeDegrees"

  @module("../../javascript/src/safe_angle.js") @scope("SafeAngle")
  external normalizeRadians: float => float = "normalizeRadians"
}

/**
 * Convert degrees to radians.
 * Delegates to proven_angle_deg_to_rad via FFI.
 */
let degToRad = SafeAngleJs.degToRad

/**
 * Convert radians to degrees.
 * Delegates to proven_angle_rad_to_deg via FFI.
 */
let radToDeg = SafeAngleJs.radToDeg

/**
 * Normalize degrees to [0, 360).
 * Delegates to proven_angle_normalize_degrees via FFI.
 */
let normalizeDegrees = SafeAngleJs.normalizeDegrees

/**
 * Normalize radians to [0, 2*pi).
 * Delegates to proven_angle_normalize_radians via FFI.
 */
let normalizeRadians = SafeAngleJs.normalizeRadians
