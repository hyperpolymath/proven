// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeUnit - Typed wrapper for unit conversions without precision loss.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** Length unit enumeration (maps to FFI integer codes). */
type lengthUnit =
  | Meter
  | Kilometer
  | Centimeter
  | Millimeter
  | Mile
  | Yard
  | Foot
  | Inch

/** Temperature unit enumeration. */
type temperatureUnit =
  | Celsius
  | Fahrenheit
  | Kelvin

/** JavaScript bindings to the SafeUnit FFI wrapper. */
module SafeUnitJs = {
  @module("../../javascript/src/safe_unit.js") @scope("SafeUnit")
  external convertLength: (float, int, int) => jsResult<float> = "convertLength"

  @module("../../javascript/src/safe_unit.js") @scope("SafeUnit")
  external convertTemp: (float, int, int) => jsResult<float> = "convertTemp"
}

/** Convert a length unit to its FFI integer code. */
let lengthUnitToInt = (unit: lengthUnit): int => {
  switch unit {
  | Meter => 0
  | Kilometer => 1
  | Centimeter => 2
  | Millimeter => 3
  | Mile => 4
  | Yard => 5
  | Foot => 6
  | Inch => 7
  }
}

/** Convert a temperature unit to its FFI integer code. */
let tempUnitToInt = (unit: temperatureUnit): int => {
  switch unit {
  | Celsius => 0
  | Fahrenheit => 1
  | Kelvin => 2
  }
}

/**
 * Convert a length value between units.
 * Delegates to proven_unit_convert_length via FFI.
 *
 * @param value The length value to convert.
 * @param from Source unit.
 * @param to_ Destination unit.
 * @returns Ok(converted) or Error.
 */
let convertLength = (value: float, ~from: lengthUnit, ~to_: lengthUnit): result<float, string> => {
  SafeUnitJs.convertLength(value, lengthUnitToInt(from), lengthUnitToInt(to_))->fromJs
}

/**
 * Convert a temperature value between units.
 * Delegates to proven_unit_convert_temp via FFI.
 *
 * @param value The temperature value to convert.
 * @param from Source unit.
 * @param to_ Destination unit.
 * @returns Ok(converted) or Error.
 */
let convertTemp = (value: float, ~from: temperatureUnit, ~to_: temperatureUnit): result<float, string> => {
  SafeUnitJs.convertTemp(value, tempUnitToInt(from), tempUnitToInt(to_))->fromJs
}
