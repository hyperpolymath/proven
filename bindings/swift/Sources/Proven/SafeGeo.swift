// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Geographic calculations delegated to libproven FFI.
///
/// Haversine distance and coordinate validation via the formally
/// verified Idris 2 core.

import CProven

/// Geographic coordinate validated by libproven.
public struct GeoCoordinate: Equatable, Sendable {
    public let lat: Double
    public let lon: Double
}

public enum SafeGeo {
    /// Validate and create a geographic coordinate.
    public static func validate(lat: Double, lon: Double) -> Result<GeoCoordinate, ProvenError> {
        let result = proven_geo_validate(lat, lon)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(GeoCoordinate(lat: result.coord.lat, lon: result.coord.lon))
    }

    /// Calculate distance between two coordinates (haversine, in meters).
    public static func distance(_ a: GeoCoordinate, _ b: GeoCoordinate) -> Result<Double, ProvenError> {
        let cA = ProvenGeoCoordinate(lat: a.lat, lon: a.lon)
        let cB = ProvenGeoCoordinate(lat: b.lat, lon: b.lon)
        let result = proven_geo_distance(cA, cB)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Check if coordinate is within a bounding box.
    public static func inBounds(_ coord: GeoCoordinate, minLat: Double, maxLat: Double, minLon: Double, maxLon: Double) -> Bool {
        let cCoord = ProvenGeoCoordinate(lat: coord.lat, lon: coord.lon)
        return proven_geo_in_bounds(cCoord, minLat, maxLat, minLon, maxLon)
    }
}
