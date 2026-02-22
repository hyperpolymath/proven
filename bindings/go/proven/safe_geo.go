// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeGeo provides geographic coordinate operations via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// GeoCoord represents a geographic coordinate (latitude/longitude).
type GeoCoord struct {
	Latitude  float64
	Longitude float64
}

// GeoValidate validates and normalizes a geographic coordinate.
func GeoValidate(lat, lon float64) (*GeoCoord, error) {
	result := C.proven_geo_validate(C.double(lat), C.double(lon))
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}
	return &GeoCoord{
		Latitude:  float64(result.coordinate.latitude),
		Longitude: float64(result.coordinate.longitude),
	}, nil
}

// GeoDistance calculates the distance in meters between two geographic
// coordinates using the Haversine formula.
func GeoDistance(a, b GeoCoord) (float64, error) {
	cA := C.GeoCoordinate{latitude: C.double(a.Latitude), longitude: C.double(a.Longitude)}
	cB := C.GeoCoordinate{latitude: C.double(b.Latitude), longitude: C.double(b.Longitude)}
	result := C.proven_geo_distance(cA, cB)
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return float64(result.value), nil
}

// GeoInBounds checks whether a coordinate is within a bounding box.
func GeoInBounds(coord GeoCoord, minLat, maxLat, minLon, maxLon float64) bool {
	cCoord := C.GeoCoordinate{latitude: C.double(coord.Latitude), longitude: C.double(coord.Longitude)}
	return bool(C.proven_geo_in_bounds(cCoord, C.double(minLat), C.double(maxLat), C.double(minLon), C.double(maxLon)))
}
