// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe geographic coordinate operations.
//!
//! Provides validated latitude/longitude handling and
//! distance calculations with proper bounds checking.

use crate::{Error, Result};
use std::f64::consts::PI;

/// Earth radius in kilometers.
pub const EARTH_RADIUS_KM: f64 = 6371.0;

/// Earth radius in miles.
pub const EARTH_RADIUS_MI: f64 = 3958.8;

/// Geographic coordinate (latitude, longitude).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Coordinate {
    pub lat: f64,
    pub lon: f64,
}

impl Coordinate {
    /// Create a new coordinate with validation.
    pub fn new(lat: f64, lon: f64) -> Result<Self> {
        if !(-90.0..=90.0).contains(&lat) {
            return Err(Error::OutOfRange("Latitude must be between -90 and 90".into()));
        }
        if !(-180.0..=180.0).contains(&lon) {
            return Err(Error::OutOfRange(
                "Longitude must be between -180 and 180".into(),
            ));
        }
        if lat.is_nan() || lon.is_nan() {
            return Err(Error::InvalidInput("Coordinates cannot be NaN".into()));
        }
        Ok(Self { lat, lon })
    }

    /// Create without validation (for internal use).
    pub fn unchecked(lat: f64, lon: f64) -> Self {
        Self { lat, lon }
    }

    /// Convert latitude to radians.
    pub fn lat_rad(&self) -> f64 {
        self.lat * PI / 180.0
    }

    /// Convert longitude to radians.
    pub fn lon_rad(&self) -> f64 {
        self.lon * PI / 180.0
    }

    /// Check if coordinate is in the Northern Hemisphere.
    pub fn is_northern(&self) -> bool {
        self.lat >= 0.0
    }

    /// Check if coordinate is in the Eastern Hemisphere.
    pub fn is_eastern(&self) -> bool {
        self.lon >= 0.0
    }
}

/// Calculate distance between two coordinates using Haversine formula.
pub fn haversine_distance(c1: &Coordinate, c2: &Coordinate, radius: f64) -> f64 {
    let lat1 = c1.lat_rad();
    let lat2 = c2.lat_rad();
    let dlat = (c2.lat - c1.lat) * PI / 180.0;
    let dlon = (c2.lon - c1.lon) * PI / 180.0;

    let a = (dlat / 2.0).sin().powi(2) + lat1.cos() * lat2.cos() * (dlon / 2.0).sin().powi(2);
    let c = 2.0 * a.sqrt().asin();

    radius * c
}

/// Calculate distance in kilometers.
pub fn distance_km(c1: &Coordinate, c2: &Coordinate) -> f64 {
    haversine_distance(c1, c2, EARTH_RADIUS_KM)
}

/// Calculate distance in miles.
pub fn distance_mi(c1: &Coordinate, c2: &Coordinate) -> f64 {
    haversine_distance(c1, c2, EARTH_RADIUS_MI)
}

/// Calculate initial bearing from c1 to c2 in degrees.
pub fn bearing(c1: &Coordinate, c2: &Coordinate) -> f64 {
    let lat1 = c1.lat_rad();
    let lat2 = c2.lat_rad();
    let dlon = (c2.lon - c1.lon) * PI / 180.0;

    let y = dlon.sin() * lat2.cos();
    let x = lat1.cos() * lat2.sin() - lat1.sin() * lat2.cos() * dlon.cos();

    let bearing = y.atan2(x) * 180.0 / PI;
    (bearing + 360.0) % 360.0
}

/// Calculate destination point given start, bearing, and distance.
pub fn destination(start: &Coordinate, bearing_deg: f64, distance_km: f64) -> Result<Coordinate> {
    let lat1 = start.lat_rad();
    let lon1 = start.lon_rad();
    let bearing = bearing_deg * PI / 180.0;
    let angular_dist = distance_km / EARTH_RADIUS_KM;

    let lat2 = (lat1.sin() * angular_dist.cos()
        + lat1.cos() * angular_dist.sin() * bearing.cos())
    .asin();

    let lon2 = lon1
        + (bearing.sin() * angular_dist.sin() * lat1.cos())
            .atan2(angular_dist.cos() - lat1.sin() * lat2.sin());

    let lat_deg = lat2 * 180.0 / PI;
    let lon_deg = ((lon2 * 180.0 / PI) + 540.0) % 360.0 - 180.0; // Normalize

    Coordinate::new(lat_deg, lon_deg)
}

/// Bounding box for geographic area.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BoundingBox {
    pub min_lat: f64,
    pub min_lon: f64,
    pub max_lat: f64,
    pub max_lon: f64,
}

impl BoundingBox {
    /// Create a bounding box.
    pub fn new(min_lat: f64, min_lon: f64, max_lat: f64, max_lon: f64) -> Result<Self> {
        if min_lat > max_lat {
            return Err(Error::InvalidInput("min_lat must be <= max_lat".into()));
        }
        Coordinate::new(min_lat, min_lon)?;
        Coordinate::new(max_lat, max_lon)?;
        Ok(Self {
            min_lat,
            min_lon,
            max_lat,
            max_lon,
        })
    }

    /// Check if coordinate is within the bounding box.
    pub fn contains(&self, coord: &Coordinate) -> bool {
        coord.lat >= self.min_lat
            && coord.lat <= self.max_lat
            && coord.lon >= self.min_lon
            && coord.lon <= self.max_lon
    }

    /// Get center of bounding box.
    pub fn center(&self) -> Coordinate {
        Coordinate::unchecked(
            (self.min_lat + self.max_lat) / 2.0,
            (self.min_lon + self.max_lon) / 2.0,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_coordinate_validation() {
        assert!(Coordinate::new(45.0, 90.0).is_ok());
        assert!(Coordinate::new(91.0, 0.0).is_err());
        assert!(Coordinate::new(0.0, 181.0).is_err());
    }

    #[test]
    fn test_haversine() {
        let paris = Coordinate::new(48.8566, 2.3522).unwrap();
        let london = Coordinate::new(51.5074, -0.1278).unwrap();
        let dist = distance_km(&paris, &london);
        // Should be approximately 343 km
        assert!(dist > 340.0 && dist < 350.0);
    }

    #[test]
    fn test_bounding_box() {
        let bbox = BoundingBox::new(40.0, -75.0, 42.0, -73.0).unwrap();
        let nyc = Coordinate::new(40.7128, -74.0060).unwrap();
        assert!(bbox.contains(&nyc));
    }
}
