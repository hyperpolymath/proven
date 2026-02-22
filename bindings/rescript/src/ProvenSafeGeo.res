// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeGeo - Typed wrapper for geographic coordinates and calculations.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** Geographic coordinate with latitude and longitude. */
type coordinate = {
  lat: float,
  lon: float,
}

/** JavaScript bindings to the SafeGeo FFI wrapper. */
module SafeGeoJs = {
  @module("../../javascript/src/safe_geo.js") @scope("SafeGeo")
  external validate: (float, float) => jsResult<coordinate> = "validate"
}

/**
 * Validate a geographic coordinate pair.
 * Delegates to proven_geo_validate via FFI.
 *
 * @param lat Latitude (-90 to 90).
 * @param lon Longitude (-180 to 180).
 * @returns Ok(coordinate) or Error.
 */
let validate = (lat: float, lon: float): result<coordinate, string> => {
  SafeGeoJs.validate(lat, lon)->fromJs
}
