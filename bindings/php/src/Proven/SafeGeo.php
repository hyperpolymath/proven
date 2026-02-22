<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeGeo - FFI wrapper for proven_geo_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe geographic coordinate operations via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeGeo
{
    /**
     * Validate geographic coordinates.
     *
     * Returns an array with latitude and longitude on success, or null on error.
     *
     * @return array{latitude: float, longitude: float}|null
     */
    public static function validate(float $lat, float $lon): ?array
    {
        $result = FFI::getLib()->proven_geo_validate($lat, $lon);
        if ($result->status !== 0) {
            return null;
        }
        return [
            'latitude' => (float) $result->coordinate->latitude,
            'longitude' => (float) $result->coordinate->longitude,
        ];
    }

    /**
     * Calculate the great-circle distance between two coordinates (in meters).
     *
     * @return float|null The distance in meters, or null on error.
     */
    public static function distance(
        float $lat1, float $lon1,
        float $lat2, float $lon2
    ): ?float {
        $a = FFI::getLib()->new('GeoCoordinate');
        $a->latitude = $lat1;
        $a->longitude = $lon1;
        $b = FFI::getLib()->new('GeoCoordinate');
        $b->latitude = $lat2;
        $b->longitude = $lon2;

        $result = FFI::getLib()->proven_geo_distance($a, $b);
        if ($result->status !== 0) {
            return null;
        }
        return (float) $result->value;
    }

    /**
     * Check if a coordinate is within a bounding box.
     */
    public static function inBounds(
        float $lat, float $lon,
        float $minLat, float $maxLat,
        float $minLon, float $maxLon
    ): bool {
        $coord = FFI::getLib()->new('GeoCoordinate');
        $coord->latitude = $lat;
        $coord->longitude = $lon;
        return FFI::getLib()->proven_geo_in_bounds(
            $coord, $minLat, $maxLat, $minLon, $maxLon
        );
    }
}
