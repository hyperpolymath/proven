// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeGeo - FFI bindings to libproven geographic operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for geo operations.
pub const GeoError = error{
    InvalidCoordinate,
    ProvenError,
};

/// Geographic coordinate.
pub const Coordinate = struct {
    latitude: f64,
    longitude: f64,
};

/// Validate and normalize geographic coordinate via libproven.
pub fn validate(lat: f64, lon: f64) GeoError!Coordinate {
    const result = c.proven_geo_validate(lat, lon);
    if (result.status != c.PROVEN_OK) return error.InvalidCoordinate;
    return Coordinate{
        .latitude = result.coordinate.latitude,
        .longitude = result.coordinate.longitude,
    };
}

/// Calculate distance between two points (Haversine formula) via libproven.
/// Returns distance in meters.
pub fn distance(a: Coordinate, b: Coordinate) GeoError!f64 {
    const c_a = c.ProvenGeoCoordinate{ .latitude = a.latitude, .longitude = a.longitude };
    const c_b = c.ProvenGeoCoordinate{ .latitude = b.latitude, .longitude = b.longitude };
    const result = c.proven_geo_distance(c_a, c_b);
    if (result.status != c.PROVEN_OK) return error.ProvenError;
    return result.value;
}

/// Check if coordinate is inside a bounding box via libproven.
pub fn inBounds(coord: Coordinate, min_lat: f64, max_lat: f64, min_lon: f64, max_lon: f64) bool {
    const c_coord = c.ProvenGeoCoordinate{ .latitude = coord.latitude, .longitude = coord.longitude };
    return c.proven_geo_in_bounds(c_coord, min_lat, max_lat, min_lon, max_lon);
}

test "validate" {
    const coord = try validate(51.5074, -0.1278);
    try std.testing.expectApproxEqAbs(@as(f64, 51.5074), coord.latitude, 0.001);
    try std.testing.expectError(error.InvalidCoordinate, validate(91.0, 0.0));
}

test "inBounds" {
    const london = Coordinate{ .latitude = 51.5074, .longitude = -0.1278 };
    try std.testing.expect(inBounds(london, 50.0, 53.0, -1.0, 1.0));
    try std.testing.expect(!inBounds(london, 0.0, 10.0, 0.0, 10.0));
}
