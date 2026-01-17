// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe geographic coordinate operations.

const std = @import("std");

pub const GeoError = error{
    InvalidLatitude,
    InvalidLongitude,
    InvalidCoordinate,
};

/// Geographic coordinate (latitude, longitude in degrees).
pub const Coordinate = struct {
    lat: f64, // -90 to 90
    lon: f64, // -180 to 180

    /// Create a coordinate with validation.
    pub fn init(lat: f64, lon: f64) GeoError!Coordinate {
        if (lat < -90.0 or lat > 90.0) return error.InvalidLatitude;
        if (lon < -180.0 or lon > 180.0) return error.InvalidLongitude;
        return Coordinate{ .lat = lat, .lon = lon };
    }

    /// Convert degrees to radians.
    fn toRadians(degrees: f64) f64 {
        return degrees * std.math.pi / 180.0;
    }

    /// Haversine distance between two coordinates (in meters).
    pub fn distanceTo(self: Coordinate, other: Coordinate) f64 {
        const R: f64 = 6371000.0; // Earth radius in meters

        const lat1 = toRadians(self.lat);
        const lat2 = toRadians(other.lat);
        const dlat = toRadians(other.lat - self.lat);
        const dlon = toRadians(other.lon - self.lon);

        const a = std.math.sin(dlat / 2.0) * std.math.sin(dlat / 2.0) +
            std.math.cos(lat1) * std.math.cos(lat2) *
            std.math.sin(dlon / 2.0) * std.math.sin(dlon / 2.0);

        const c = 2.0 * std.math.atan2(@sqrt(a), @sqrt(1.0 - a));

        return R * c;
    }

    /// Initial bearing to another coordinate (in degrees).
    pub fn bearingTo(self: Coordinate, other: Coordinate) f64 {
        const lat1 = toRadians(self.lat);
        const lat2 = toRadians(other.lat);
        const dlon = toRadians(other.lon - self.lon);

        const y = std.math.sin(dlon) * std.math.cos(lat2);
        const x = std.math.cos(lat1) * std.math.sin(lat2) -
            std.math.sin(lat1) * std.math.cos(lat2) * std.math.cos(dlon);

        var bearing = std.math.atan2(y, x) * 180.0 / std.math.pi;
        bearing = @mod(bearing + 360.0, 360.0);
        return bearing;
    }

    /// Midpoint between two coordinates.
    pub fn midpointTo(self: Coordinate, other: Coordinate) Coordinate {
        return Coordinate{
            .lat = (self.lat + other.lat) / 2.0,
            .lon = (self.lon + other.lon) / 2.0,
        };
    }
};

/// Bounding box defined by two corners.
pub const BoundingBox = struct {
    min: Coordinate, // Southwest corner
    max: Coordinate, // Northeast corner

    /// Create a bounding box.
    pub fn init(min_lat: f64, min_lon: f64, max_lat: f64, max_lon: f64) GeoError!BoundingBox {
        return BoundingBox{
            .min = try Coordinate.init(min_lat, min_lon),
            .max = try Coordinate.init(max_lat, max_lon),
        };
    }

    /// Check if a coordinate is within the bounding box.
    pub fn contains(self: BoundingBox, coord: Coordinate) bool {
        return coord.lat >= self.min.lat and coord.lat <= self.max.lat and
            coord.lon >= self.min.lon and coord.lon <= self.max.lon;
    }

    /// Get center of bounding box.
    pub fn center(self: BoundingBox) Coordinate {
        return self.min.midpointTo(self.max);
    }

    /// Check if two bounding boxes overlap.
    pub fn overlaps(self: BoundingBox, other: BoundingBox) bool {
        return !(self.max.lat < other.min.lat or
            self.min.lat > other.max.lat or
            self.max.lon < other.min.lon or
            self.min.lon > other.max.lon);
    }
};

test "Coordinate distance" {
    const london = try Coordinate.init(51.5074, -0.1278);
    const paris = try Coordinate.init(48.8566, 2.3522);
    const distance = london.distanceTo(paris);
    // Should be approximately 344 km
    try std.testing.expect(distance > 340000 and distance < 350000);
}

test "BoundingBox contains" {
    const bbox = try BoundingBox.init(40.0, -75.0, 42.0, -73.0);
    const nyc = try Coordinate.init(40.7128, -74.0060);
    try std.testing.expect(bbox.contains(nyc));
}
