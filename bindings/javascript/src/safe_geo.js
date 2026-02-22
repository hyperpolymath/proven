// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeGeo - Geographic coordinate operations.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Geographic coordinate.
 */
export class Coordinate {
  /** @type {number} Latitude (-90 to 90). */
  latitude;
  /** @type {number} Longitude (-180 to 180). */
  longitude;

  /**
   * @param {number} latitude
   * @param {number} longitude
   */
  constructor(latitude, longitude) {
    this.latitude = latitude;
    this.longitude = longitude;
  }

  /** @returns {string} */
  toString() { return `(${this.latitude}, ${this.longitude})`; }
}

/**
 * Distance representation.
 */
export class Distance {
  /** @type {number} Distance in meters. */
  meters;

  /**
   * @param {number} meters
   */
  constructor(meters) {
    this.meters = meters;
  }

  /** @returns {number} Distance in kilometers. */
  toKilometers() { return this.meters / 1000; }

  /** @returns {number} Distance in miles. */
  toMiles() { return this.meters / 1609.344; }
}

/**
 * Safe geographic operations backed by formally verified Idris 2 code.
 */
export class SafeGeo {
  /**
   * Validate and normalize geographic coordinates.
   *
   * @param {number} lat - Latitude.
   * @param {number} lon - Longitude.
   * @returns {{ ok: true, value: Coordinate } | { ok: false, error: string }}
   */
  static validate(lat, lon) {
    const symbols = getLib();
    const result = symbols.proven_geo_validate(lat, lon);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(new Coordinate(result[1], result[2]));
  }
}
