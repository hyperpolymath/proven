// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeColor - Typed wrapper for color manipulation with WCAG compliance.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides RGB, RGBA, HSL value types and
 * color parsing/conversion functions backed by native implementations.
 */

/** Opaque handle to a JavaScript RGB instance. */
type rgb

/** Opaque handle to a JavaScript RGBA instance. */
type rgba

/** Opaque handle to a JavaScript HSL instance. */
type hsl

/** JavaScript bindings to the SafeColor FFI wrapper. */
module SafeColorJs = {
  @module("../../javascript/src/safe_color.js") @scope("SafeColor")
  external parseHex: string => Nullable.t<rgb> = "parseHex"

  @module("../../javascript/src/safe_color.js") @scope("SafeColor")
  external toHex: rgb => string = "toHex"

  @module("../../javascript/src/safe_color.js") @scope("SafeColor")
  external rgbToHsl: rgb => hsl = "rgbToHsl"
}

/**
 * Parse a hex color string (e.g. "#ff0000").
 *
 * @param hex The hex color string.
 * @returns Some(rgb) or None on parse failure.
 */
let parseHex = (hex: string): option<rgb> => {
  SafeColorJs.parseHex(hex)->Nullable.toOption
}

/**
 * Convert an RGB color to a hex string.
 *
 * @param color The RGB color.
 * @returns The hex string representation.
 */
let toHex = SafeColorJs.toHex

/**
 * Convert an RGB color to HSL.
 *
 * @param color The RGB color.
 * @returns The HSL representation.
 */
let rgbToHsl = SafeColorJs.rgbToHsl
