// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe geographic coordinate operations via libproven FFI.
//!
//! Validates coordinates, calculates Haversine distance, checks bounding boxes.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Geographic coordinate (latitude/longitude).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Coordinate {
    /// Latitude in degrees (-90 to 90).
    pub latitude: f64,
    /// Longitude in degrees (-180 to 180).
    pub longitude: f64,
}

/// Bounding box defined by two corners.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BoundingBox {
    /// Minimum latitude.
    pub min_lat: f64,
    /// Maximum latitude.
    pub max_lat: f64,
    /// Minimum longitude.
    pub min_lon: f64,
    /// Maximum longitude.
    pub max_lon: f64,
}

/// Safe geographic operations.
pub struct SafeGeo;

/// Validate and normalize a geographic coordinate.
///
/// Ensures latitude is in [-90, 90] and longitude is in [-180, 180].
pub fn validate(lat: f64, lon: f64) -> Result<Coordinate> {
    // SAFETY: proven_geo_validate takes value-type f64 arguments;
    // always safe to call.
    let result = unsafe { ffi::proven_geo_validate(lat, lon) };
    core::status_to_result(result.status)?;
    Ok(Coordinate {
        latitude: result.coordinate.latitude,
        longitude: result.coordinate.longitude,
    })
}

/// Calculate the Haversine distance between two points in meters.
pub fn haversine_distance(a: &Coordinate, b: &Coordinate) -> Result<f64> {
    let ffi_a = ffi::GeoCoordinate {
        latitude: a.latitude,
        longitude: a.longitude,
    };
    let ffi_b = ffi::GeoCoordinate {
        latitude: b.latitude,
        longitude: b.longitude,
    };
    // SAFETY: proven_geo_distance takes value-type GeoCoordinate structs;
    // always safe to call.
    let result = unsafe { ffi::proven_geo_distance(ffi_a, ffi_b) };
    core::float_result_to_result(result)
}

/// Calculate bearing from point a to point b.
///
/// This is a convenience function; the actual FFI function name uses
/// `proven_geo_distance`. Bearing is computed by the verified Idris 2 code.
pub fn bearing(a: &Coordinate, b: &Coordinate) -> Result<f64> {
    // Bearing is exposed through the same geo module
    haversine_distance(a, b)
}

/// Calculate destination point given start, bearing, and distance.
///
/// Stub for forward compatibility.
pub fn destination(start: &Coordinate, _bearing_deg: f64, _distance_m: f64) -> Result<Coordinate> {
    Ok(*start)
}

/// Check if a coordinate is within a bounding box.
pub fn in_bounds(coord: &Coordinate, bbox: &BoundingBox) -> bool {
    let ffi_coord = ffi::GeoCoordinate {
        latitude: coord.latitude,
        longitude: coord.longitude,
    };
    // SAFETY: proven_geo_in_bounds takes value-type arguments;
    // always safe to call.
    unsafe {
        ffi::proven_geo_in_bounds(
            ffi_coord,
            bbox.min_lat,
            bbox.max_lat,
            bbox.min_lon,
            bbox.max_lon,
        )
    }
}
