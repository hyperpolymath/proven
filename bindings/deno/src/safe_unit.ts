// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeUnit - Physical unit conversions.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Length unit enumeration (matches Zig LengthUnit enum).
 */
export const LengthUnit = {
  METERS: 0,
  KILOMETERS: 1,
  CENTIMETERS: 2,
  MILLIMETERS: 3,
  FEET: 4,
  INCHES: 5,
  MILES: 6,
  YARDS: 7,
} as const;

/**
 * Temperature unit enumeration (matches Zig TempUnit enum).
 */
export const TemperatureUnit = {
  CELSIUS: 0,
  FAHRENHEIT: 1,
  KELVIN: 2,
} as const;

/**
 * Safe unit conversion operations backed by formally verified Idris 2 code.
 */
export class SafeUnit {
  /**
   * Convert a length value between units.
   *
   * @param value - The value to convert.
   * @param from - Source LengthUnit.
   * @param to - Target LengthUnit.
   * @returns Result containing the converted value or an error string.
   */
  static convertLength(value: number, from: number, to: number): Result<number> {
    const symbols = getLib();
    const result = symbols.proven_unit_convert_length(value, from, to);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Convert a temperature value between units.
   *
   * @param value - The value to convert.
   * @param from - Source TemperatureUnit.
   * @param to - Target TemperatureUnit.
   * @returns Result containing the converted value or an error string.
   */
  static convertTemp(value: number, from: number, to: number): Result<number> {
    const symbols = getLib();
    const result = symbols.proven_unit_convert_temp(value, from, to);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }
}
