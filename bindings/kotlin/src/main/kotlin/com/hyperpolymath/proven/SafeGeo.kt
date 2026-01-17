// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlin.math.*

/**
 * Geographic coordinate.
 */
data class Coordinate(
    val latitude: Double,
    val longitude: Double
) {
    init {
        require(latitude in -90.0..90.0) { "Latitude must be between -90 and 90" }
        require(longitude in -180.0..180.0) { "Longitude must be between -180 and 180" }
    }

    val latitudeRadians: Double get() = latitude * PI / 180
    val longitudeRadians: Double get() = longitude * PI / 180

    companion object {
        fun create(latitude: Double, longitude: Double): Coordinate? {
            return if (latitude in -90.0..90.0 && longitude in -180.0..180.0) {
                Coordinate(latitude, longitude)
            } else null
        }
    }
}

/**
 * Geographic utilities.
 */
object SafeGeo {
    const val EARTH_RADIUS_KM = 6371.0
    const val EARTH_RADIUS_MILES = 3958.8

    /**
     * Calculate distance using the Haversine formula.
     */
    fun haversine(from: Coordinate, to: Coordinate, radiusKm: Double = EARTH_RADIUS_KM): Double {
        val lat1 = from.latitudeRadians
        val lat2 = to.latitudeRadians
        val dLat = lat2 - lat1
        val dLon = to.longitudeRadians - from.longitudeRadians

        val a = sin(dLat / 2).pow(2) + cos(lat1) * cos(lat2) * sin(dLon / 2).pow(2)
        val c = 2 * atan2(sqrt(a), sqrt(1 - a))

        return radiusKm * c
    }

    /**
     * Calculate distance in miles.
     */
    fun haversineMiles(from: Coordinate, to: Coordinate): Double {
        return haversine(from, to, EARTH_RADIUS_MILES)
    }

    /**
     * Calculate initial bearing (forward azimuth).
     */
    fun bearing(from: Coordinate, to: Coordinate): Double {
        val lat1 = from.latitudeRadians
        val lat2 = to.latitudeRadians
        val dLon = to.longitudeRadians - from.longitudeRadians

        val y = sin(dLon) * cos(lat2)
        val x = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dLon)

        var bearing = atan2(y, x) * 180 / PI
        bearing = (bearing + 360) % 360
        return bearing
    }

    /**
     * Calculate destination point given start, bearing, and distance.
     */
    fun destination(
        from: Coordinate,
        bearingDegrees: Double,
        distanceKm: Double,
        radiusKm: Double = EARTH_RADIUS_KM
    ): Coordinate? {
        val lat1 = from.latitudeRadians
        val lon1 = from.longitudeRadians
        val bearing = bearingDegrees * PI / 180
        val angularDistance = distanceKm / radiusKm

        val lat2 = asin(sin(lat1) * cos(angularDistance) +
                cos(lat1) * sin(angularDistance) * cos(bearing))
        val lon2 = lon1 + atan2(sin(bearing) * sin(angularDistance) * cos(lat1),
            cos(angularDistance) - sin(lat1) * sin(lat2))

        val latDeg = lat2 * 180 / PI
        var lonDeg = lon2 * 180 / PI
        lonDeg = ((lonDeg + 540) % 360) - 180

        return Coordinate.create(latDeg, lonDeg)
    }

    /**
     * Calculate midpoint between two coordinates.
     */
    fun midpoint(from: Coordinate, to: Coordinate): Coordinate? {
        val lat1 = from.latitudeRadians
        val lon1 = from.longitudeRadians
        val lat2 = to.latitudeRadians
        val dLon = to.longitudeRadians - lon1

        val bx = cos(lat2) * cos(dLon)
        val by = cos(lat2) * sin(dLon)

        val lat3 = atan2(sin(lat1) + sin(lat2), sqrt((cos(lat1) + bx).pow(2) + by.pow(2)))
        val lon3 = lon1 + atan2(by, cos(lat1) + bx)

        val latDeg = lat3 * 180 / PI
        var lonDeg = lon3 * 180 / PI
        lonDeg = ((lonDeg + 540) % 360) - 180

        return Coordinate.create(latDeg, lonDeg)
    }

    /**
     * Calculate bounding box around a point.
     */
    fun boundingBox(center: Coordinate, radiusKm: Double): Pair<Coordinate, Coordinate>? {
        val latDelta = (radiusKm / EARTH_RADIUS_KM) * 180 / PI
        val lonDelta = latDelta / cos(center.latitudeRadians)

        val sw = Coordinate.create(center.latitude - latDelta, center.longitude - lonDelta)
        val ne = Coordinate.create(center.latitude + latDelta, center.longitude + lonDelta)

        return if (sw != null && ne != null) sw to ne else null
    }

    /**
     * Check if a point is within a bounding box.
     */
    fun isInBounds(point: Coordinate, sw: Coordinate, ne: Coordinate): Boolean {
        return point.latitude >= sw.latitude && point.latitude <= ne.latitude &&
                point.longitude >= sw.longitude && point.longitude <= ne.longitude
    }

    /**
     * Convert degrees to cardinal direction.
     */
    fun toCardinal(degrees: Double): String {
        val directions = listOf("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
            "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
        val index = ((degrees + 11.25) % 360 / 22.5).toInt()
        return directions[index]
    }
}
