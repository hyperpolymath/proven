// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Geographic coordinate.
public struct Coordinate: Equatable, Hashable {
    public let latitude: Double
    public let longitude: Double

    public init?(latitude: Double, longitude: Double) {
        guard latitude >= -90 && latitude <= 90 else { return nil }
        guard longitude >= -180 && longitude <= 180 else { return nil }
        self.latitude = latitude
        self.longitude = longitude
    }

    public static func create(latitude: Double, longitude: Double) -> Coordinate? {
        Coordinate(latitude: latitude, longitude: longitude)
    }

    /// Get latitude in radians.
    public var latitudeRadians: Double { latitude * .pi / 180 }

    /// Get longitude in radians.
    public var longitudeRadians: Double { longitude * .pi / 180 }
}

/// Geographic utilities.
public enum SafeGeo {
    /// Earth's radius in kilometers.
    public static let earthRadiusKm: Double = 6371.0

    /// Earth's radius in miles.
    public static let earthRadiusMiles: Double = 3958.8

    /// Calculate distance using the Haversine formula.
    public static func haversine(from: Coordinate, to: Coordinate, radiusKm: Double = earthRadiusKm) -> Double {
        let lat1 = from.latitudeRadians
        let lat2 = to.latitudeRadians
        let dLat = lat2 - lat1
        let dLon = to.longitudeRadians - from.longitudeRadians

        let a = sin(dLat / 2) * sin(dLat / 2) +
                cos(lat1) * cos(lat2) * sin(dLon / 2) * sin(dLon / 2)
        let c = 2 * atan2(sqrt(a), sqrt(1 - a))

        return radiusKm * c
    }

    /// Calculate distance in miles.
    public static func haversineMiles(from: Coordinate, to: Coordinate) -> Double {
        haversine(from: from, to: to, radiusKm: earthRadiusMiles)
    }

    /// Calculate initial bearing (forward azimuth).
    public static func bearing(from: Coordinate, to: Coordinate) -> Double {
        let lat1 = from.latitudeRadians
        let lat2 = to.latitudeRadians
        let dLon = to.longitudeRadians - from.longitudeRadians

        let y = sin(dLon) * cos(lat2)
        let x = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dLon)

        var bearing = atan2(y, x) * 180 / .pi
        bearing = (bearing + 360).truncatingRemainder(dividingBy: 360)
        return bearing
    }

    /// Calculate destination point given start, bearing, and distance.
    public static func destination(from: Coordinate, bearingDegrees: Double, distanceKm: Double, radiusKm: Double = earthRadiusKm) -> Coordinate? {
        let lat1 = from.latitudeRadians
        let lon1 = from.longitudeRadians
        let bearing = bearingDegrees * .pi / 180
        let angularDistance = distanceKm / radiusKm

        let lat2 = asin(sin(lat1) * cos(angularDistance) +
                        cos(lat1) * sin(angularDistance) * cos(bearing))
        let lon2 = lon1 + atan2(sin(bearing) * sin(angularDistance) * cos(lat1),
                                cos(angularDistance) - sin(lat1) * sin(lat2))

        let latDeg = lat2 * 180 / .pi
        var lonDeg = lon2 * 180 / .pi

        // Normalize longitude to -180 to 180
        lonDeg = ((lonDeg + 540).truncatingRemainder(dividingBy: 360)) - 180

        return Coordinate(latitude: latDeg, longitude: lonDeg)
    }

    /// Calculate midpoint between two coordinates.
    public static func midpoint(from: Coordinate, to: Coordinate) -> Coordinate? {
        let lat1 = from.latitudeRadians
        let lon1 = from.longitudeRadians
        let lat2 = to.latitudeRadians
        let dLon = to.longitudeRadians - lon1

        let bx = cos(lat2) * cos(dLon)
        let by = cos(lat2) * sin(dLon)

        let lat3 = atan2(sin(lat1) + sin(lat2),
                         sqrt((cos(lat1) + bx) * (cos(lat1) + bx) + by * by))
        let lon3 = lon1 + atan2(by, cos(lat1) + bx)

        let latDeg = lat3 * 180 / .pi
        var lonDeg = lon3 * 180 / .pi
        lonDeg = ((lonDeg + 540).truncatingRemainder(dividingBy: 360)) - 180

        return Coordinate(latitude: latDeg, longitude: lonDeg)
    }

    /// Calculate bounding box around a point.
    public static func boundingBox(center: Coordinate, radiusKm: Double) -> (sw: Coordinate, ne: Coordinate)? {
        let latDelta = (radiusKm / earthRadiusKm) * 180 / .pi
        let lonDelta = latDelta / cos(center.latitudeRadians)

        guard let sw = Coordinate(latitude: center.latitude - latDelta, longitude: center.longitude - lonDelta),
              let ne = Coordinate(latitude: center.latitude + latDelta, longitude: center.longitude + lonDelta) else {
            return nil
        }

        return (sw, ne)
    }

    /// Check if a point is within a bounding box.
    public static func isInBounds(_ point: Coordinate, sw: Coordinate, ne: Coordinate) -> Bool {
        point.latitude >= sw.latitude && point.latitude <= ne.latitude &&
        point.longitude >= sw.longitude && point.longitude <= ne.longitude
    }

    /// Convert degrees to cardinal direction.
    public static func toCardinal(_ degrees: Double) -> String {
        let directions = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                          "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"]
        let index = Int((degrees + 11.25).truncatingRemainder(dividingBy: 360) / 22.5)
        return directions[index]
    }
}
