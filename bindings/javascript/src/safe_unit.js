// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeUnit - Physical unit conversions.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Length unit enumeration (matches Zig LengthUnit enum).
 * @readonly
 * @enum {number}
 */
export const LengthUnit = Object.freeze({
  METERS: 0,
  KILOMETERS: 1,
  CENTIMETERS: 2,
  MILLIMETERS: 3,
  FEET: 4,
  INCHES: 5,
  MILES: 6,
  YARDS: 7,
});

/**
 * Mass unit enumeration (placeholder for FFI extension).
 * @readonly
 * @enum {string}
 */
export const MassUnit = Object.freeze({
  KILOGRAMS: 'kg',
  GRAMS: 'g',
  POUNDS: 'lb',
  OUNCES: 'oz',
});

/**
 * Temperature unit enumeration (matches Zig TempUnit enum).
 * @readonly
 * @enum {number}
 */
export const TemperatureUnit = Object.freeze({
  CELSIUS: 0,
  FAHRENHEIT: 1,
  KELVIN: 2,
});

/**
 * Time unit enumeration (placeholder for FFI extension).
 * @readonly
 * @enum {string}
 */
export const TimeUnit = Object.freeze({
  SECONDS: 's',
  MINUTES: 'min',
  HOURS: 'h',
  DAYS: 'd',
});

/**
 * Data unit enumeration (placeholder for FFI extension).
 * @readonly
 * @enum {string}
 */
export const DataUnit = Object.freeze({
  BYTES: 'B',
  KILOBYTES: 'KB',
  MEGABYTES: 'MB',
  GIGABYTES: 'GB',
  TERABYTES: 'TB',
});

/**
 * Safe unit conversion operations backed by formally verified Idris 2 code.
 */
export class SafeUnit {
  /**
   * Convert a length value between units.
   *
   * @param {number} value - The value to convert.
   * @param {number} from - Source LengthUnit.
   * @param {number} to - Target LengthUnit.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static convertLength(value, from, to) {
    const symbols = getLib();
    const result = symbols.proven_unit_convert_length(value, from, to);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Convert a temperature value between units.
   *
   * @param {number} value - The value to convert.
   * @param {number} from - Source TemperatureUnit.
   * @param {number} to - Target TemperatureUnit.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static convertTemp(value, from, to) {
    const symbols = getLib();
    const result = symbols.proven_unit_convert_temp(value, from, to);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }
}
