// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeUnit - Physical unit conversions that cannot crash.
 *
 * Provides safe unit conversions for length, mass, temperature, time, and data.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Length units.
 * @readonly
 * @enum {string}
 */
export const LengthUnit = Object.freeze({
  METER: 'meter',
  KILOMETER: 'kilometer',
  CENTIMETER: 'centimeter',
  MILLIMETER: 'millimeter',
  MILE: 'mile',
  YARD: 'yard',
  FOOT: 'foot',
  INCH: 'inch',
  NAUTICAL_MILE: 'nautical_mile',
});

/**
 * Mass units.
 * @readonly
 * @enum {string}
 */
export const MassUnit = Object.freeze({
  KILOGRAM: 'kilogram',
  GRAM: 'gram',
  MILLIGRAM: 'milligram',
  POUND: 'pound',
  OUNCE: 'ounce',
  TON: 'ton',
  TONNE: 'tonne',
});

/**
 * Temperature units.
 * @readonly
 * @enum {string}
 */
export const TemperatureUnit = Object.freeze({
  CELSIUS: 'celsius',
  FAHRENHEIT: 'fahrenheit',
  KELVIN: 'kelvin',
});

/**
 * Time units.
 * @readonly
 * @enum {string}
 */
export const TimeUnit = Object.freeze({
  SECOND: 'second',
  MILLISECOND: 'millisecond',
  MICROSECOND: 'microsecond',
  NANOSECOND: 'nanosecond',
  MINUTE: 'minute',
  HOUR: 'hour',
  DAY: 'day',
  WEEK: 'week',
});

/**
 * Data units.
 * @readonly
 * @enum {string}
 */
export const DataUnit = Object.freeze({
  BIT: 'bit',
  BYTE: 'byte',
  KILOBYTE: 'kilobyte',
  MEGABYTE: 'megabyte',
  GIGABYTE: 'gigabyte',
  TERABYTE: 'terabyte',
  KIBIBYTE: 'kibibyte',
  MEBIBYTE: 'mebibyte',
  GIBIBYTE: 'gibibyte',
  TEBIBYTE: 'tebibyte',
});

/** Length conversion factors to meters */
const LENGTH_TO_METERS = {
  [LengthUnit.METER]: 1,
  [LengthUnit.KILOMETER]: 1000,
  [LengthUnit.CENTIMETER]: 0.01,
  [LengthUnit.MILLIMETER]: 0.001,
  [LengthUnit.MILE]: 1609.344,
  [LengthUnit.YARD]: 0.9144,
  [LengthUnit.FOOT]: 0.3048,
  [LengthUnit.INCH]: 0.0254,
  [LengthUnit.NAUTICAL_MILE]: 1852,
};

/** Mass conversion factors to kilograms */
const MASS_TO_KILOGRAMS = {
  [MassUnit.KILOGRAM]: 1,
  [MassUnit.GRAM]: 0.001,
  [MassUnit.MILLIGRAM]: 0.000001,
  [MassUnit.POUND]: 0.45359237,
  [MassUnit.OUNCE]: 0.028349523125,
  [MassUnit.TON]: 907.18474,
  [MassUnit.TONNE]: 1000,
};

/** Time conversion factors to seconds */
const TIME_TO_SECONDS = {
  [TimeUnit.SECOND]: 1,
  [TimeUnit.MILLISECOND]: 0.001,
  [TimeUnit.MICROSECOND]: 0.000001,
  [TimeUnit.NANOSECOND]: 0.000000001,
  [TimeUnit.MINUTE]: 60,
  [TimeUnit.HOUR]: 3600,
  [TimeUnit.DAY]: 86400,
  [TimeUnit.WEEK]: 604800,
};

/** Data conversion factors to bytes */
const DATA_TO_BYTES = {
  [DataUnit.BIT]: 0.125,
  [DataUnit.BYTE]: 1,
  [DataUnit.KILOBYTE]: 1000,
  [DataUnit.MEGABYTE]: 1000000,
  [DataUnit.GIGABYTE]: 1000000000,
  [DataUnit.TERABYTE]: 1000000000000,
  [DataUnit.KIBIBYTE]: 1024,
  [DataUnit.MEBIBYTE]: 1048576,
  [DataUnit.GIBIBYTE]: 1073741824,
  [DataUnit.TEBIBYTE]: 1099511627776,
};

/**
 * Safe unit conversions.
 */
export class SafeUnit {
  /**
   * Convert length.
   *
   * @param {number} value - Value to convert
   * @param {string} from - Source unit
   * @param {string} to - Target unit
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static convertLength(value, from, to) {
    if (!Number.isFinite(value)) {
      return err('Value must be a finite number');
    }
    if (!(from in LENGTH_TO_METERS)) {
      return err(`Unknown source unit: ${from}`);
    }
    if (!(to in LENGTH_TO_METERS)) {
      return err(`Unknown target unit: ${to}`);
    }

    const meters = value * LENGTH_TO_METERS[from];
    return ok(meters / LENGTH_TO_METERS[to]);
  }

  /**
   * Convert mass.
   *
   * @param {number} value - Value to convert
   * @param {string} from - Source unit
   * @param {string} to - Target unit
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static convertMass(value, from, to) {
    if (!Number.isFinite(value)) {
      return err('Value must be a finite number');
    }
    if (!(from in MASS_TO_KILOGRAMS)) {
      return err(`Unknown source unit: ${from}`);
    }
    if (!(to in MASS_TO_KILOGRAMS)) {
      return err(`Unknown target unit: ${to}`);
    }

    const kg = value * MASS_TO_KILOGRAMS[from];
    return ok(kg / MASS_TO_KILOGRAMS[to]);
  }

  /**
   * Convert temperature.
   *
   * @param {number} value - Value to convert
   * @param {string} from - Source unit
   * @param {string} to - Target unit
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static convertTemperature(value, from, to) {
    if (!Number.isFinite(value)) {
      return err('Value must be a finite number');
    }

    // Convert to Celsius first
    let celsius;
    switch (from) {
      case TemperatureUnit.CELSIUS:
        celsius = value;
        break;
      case TemperatureUnit.FAHRENHEIT:
        celsius = (value - 32) * (5 / 9);
        break;
      case TemperatureUnit.KELVIN:
        celsius = value - 273.15;
        break;
      default:
        return err(`Unknown source unit: ${from}`);
    }

    // Convert from Celsius to target
    switch (to) {
      case TemperatureUnit.CELSIUS:
        return ok(celsius);
      case TemperatureUnit.FAHRENHEIT:
        return ok(celsius * (9 / 5) + 32);
      case TemperatureUnit.KELVIN:
        return ok(celsius + 273.15);
      default:
        return err(`Unknown target unit: ${to}`);
    }
  }

  /**
   * Convert time.
   *
   * @param {number} value - Value to convert
   * @param {string} from - Source unit
   * @param {string} to - Target unit
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static convertTime(value, from, to) {
    if (!Number.isFinite(value)) {
      return err('Value must be a finite number');
    }
    if (!(from in TIME_TO_SECONDS)) {
      return err(`Unknown source unit: ${from}`);
    }
    if (!(to in TIME_TO_SECONDS)) {
      return err(`Unknown target unit: ${to}`);
    }

    const seconds = value * TIME_TO_SECONDS[from];
    return ok(seconds / TIME_TO_SECONDS[to]);
  }

  /**
   * Convert data size.
   *
   * @param {number} value - Value to convert
   * @param {string} from - Source unit
   * @param {string} to - Target unit
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static convertData(value, from, to) {
    if (!Number.isFinite(value)) {
      return err('Value must be a finite number');
    }
    if (!(from in DATA_TO_BYTES)) {
      return err(`Unknown source unit: ${from}`);
    }
    if (!(to in DATA_TO_BYTES)) {
      return err(`Unknown target unit: ${to}`);
    }

    const bytes = value * DATA_TO_BYTES[from];
    return ok(bytes / DATA_TO_BYTES[to]);
  }

  /**
   * Format bytes as human-readable string.
   *
   * @param {number} bytes - Bytes value
   * @param {boolean} [binary=false] - Use binary units (KiB, MiB) vs decimal (KB, MB)
   * @param {number} [decimals=2] - Decimal places
   * @returns {string}
   */
  static formatBytes(bytes, binary = false, decimals = 2) {
    if (!Number.isFinite(bytes) || bytes === 0) {
      return '0 Bytes';
    }

    const factor = binary ? 1024 : 1000;
    const units = binary
      ? ['Bytes', 'KiB', 'MiB', 'GiB', 'TiB', 'PiB']
      : ['Bytes', 'KB', 'MB', 'GB', 'TB', 'PB'];

    const unitIndex = Math.floor(Math.log(Math.abs(bytes)) / Math.log(factor));
    const clampedIndex = Math.min(unitIndex, units.length - 1);

    return `${(bytes / Math.pow(factor, clampedIndex)).toFixed(decimals)} ${units[clampedIndex]}`;
  }

  /**
   * Format duration as human-readable string.
   *
   * @param {number} seconds - Duration in seconds
   * @returns {string}
   */
  static formatDuration(seconds) {
    if (!Number.isFinite(seconds)) {
      return 'Invalid';
    }

    const absSeconds = Math.abs(seconds);
    const sign = seconds < 0 ? '-' : '';

    if (absSeconds < 1) {
      return `${sign}${(absSeconds * 1000).toFixed(0)}ms`;
    }
    if (absSeconds < 60) {
      return `${sign}${absSeconds.toFixed(1)}s`;
    }
    if (absSeconds < 3600) {
      const mins = Math.floor(absSeconds / 60);
      const secs = Math.floor(absSeconds % 60);
      return `${sign}${mins}m ${secs}s`;
    }
    if (absSeconds < 86400) {
      const hours = Math.floor(absSeconds / 3600);
      const mins = Math.floor((absSeconds % 3600) / 60);
      return `${sign}${hours}h ${mins}m`;
    }

    const days = Math.floor(absSeconds / 86400);
    const hours = Math.floor((absSeconds % 86400) / 3600);
    return `${sign}${days}d ${hours}h`;
  }
}

// Export convenience functions
export const convertLength = SafeUnit.convertLength;
export const convertMass = SafeUnit.convertMass;
export const convertTemperature = SafeUnit.convertTemperature;
export const convertTime = SafeUnit.convertTime;
export const convertData = SafeUnit.convertData;
