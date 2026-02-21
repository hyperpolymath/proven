// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"fmt"
	"math"
)

// Coordinate represents a geographic coordinate.
type Coordinate struct {
	Latitude  float64
	Longitude float64
}

// NewCoordinate creates a validated coordinate.
func NewCoordinate(lat, lon float64) (Coordinate, bool) {
	if lat < -90 || lat > 90 {
		return Coordinate{}, false
	}
	if lon < -180 || lon > 180 {
		return Coordinate{}, false
	}
	return Coordinate{Latitude: lat, Longitude: lon}, true
}

// String returns string representation.
func (c Coordinate) String() string {
	return fmt.Sprintf("%.6f, %.6f", c.Latitude, c.Longitude)
}

// ToDMS returns degrees/minutes/seconds format.
func (c Coordinate) ToDMS() string {
	latDir := "N"
	if c.Latitude < 0 {
		latDir = "S"
	}
	lonDir := "E"
	if c.Longitude < 0 {
		lonDir = "W"
	}

	latDMS := toDMS(math.Abs(c.Latitude))
	lonDMS := toDMS(math.Abs(c.Longitude))

	return fmt.Sprintf("%s %s, %s %s", latDMS, latDir, lonDMS, lonDir)
}

func toDMS(decimal float64) string {
	degrees := int(decimal)
	minutesDecimal := (decimal - float64(degrees)) * 60
	minutes := int(minutesDecimal)
	seconds := (minutesDecimal - float64(minutes)) * 60
	return fmt.Sprintf("%d°%d'%.2f\"", degrees, minutes, seconds)
}

// Distance represents a distance in meters.
type Distance struct {
	Meters float64
}

// NewDistance creates a distance from meters.
func NewDistance(meters float64) Distance {
	return Distance{Meters: meters}
}

// Kilometers returns distance in kilometers.
func (d Distance) Kilometers() float64 {
	return d.Meters / 1000
}

// Miles returns distance in miles.
func (d Distance) Miles() float64 {
	return d.Meters / 1609.344
}

// NauticalMiles returns distance in nautical miles.
func (d Distance) NauticalMiles() float64 {
	return d.Meters / 1852
}

// Feet returns distance in feet.
func (d Distance) Feet() float64 {
	return d.Meters * 3.28084
}

// String returns string representation.
func (d Distance) String() string {
	if d.Meters >= 1000 {
		return fmt.Sprintf("%.2f km", d.Kilometers())
	}
	return fmt.Sprintf("%.2f m", d.Meters)
}

// EarthRadiusMeters is Earth's mean radius in meters.
const EarthRadiusMeters = 6371000.0

// Haversine calculates distance between two coordinates.
func Haversine(from, to Coordinate) Distance {
	lat1Rad := from.Latitude * math.Pi / 180
	lat2Rad := to.Latitude * math.Pi / 180
	deltaLat := (to.Latitude - from.Latitude) * math.Pi / 180
	deltaLon := (to.Longitude - from.Longitude) * math.Pi / 180

	a := math.Sin(deltaLat/2)*math.Sin(deltaLat/2) +
		math.Cos(lat1Rad)*math.Cos(lat2Rad)*
			math.Sin(deltaLon/2)*math.Sin(deltaLon/2)

	c := 2 * math.Atan2(math.Sqrt(a), math.Sqrt(1-a))

	return Distance{Meters: EarthRadiusMeters * c}
}

// Bearing calculates initial bearing from one coordinate to another.
func Bearing(from, to Coordinate) float64 {
	lat1Rad := from.Latitude * math.Pi / 180
	lat2Rad := to.Latitude * math.Pi / 180
	deltaLon := (to.Longitude - from.Longitude) * math.Pi / 180

	y := math.Sin(deltaLon) * math.Cos(lat2Rad)
	x := math.Cos(lat1Rad)*math.Sin(lat2Rad) -
		math.Sin(lat1Rad)*math.Cos(lat2Rad)*math.Cos(deltaLon)

	bearing := math.Atan2(y, x) * 180 / math.Pi
	return math.Mod(bearing+360, 360)
}

// Midpoint calculates the midpoint between two coordinates.
func Midpoint(from, to Coordinate) Coordinate {
	lat1Rad := from.Latitude * math.Pi / 180
	lat2Rad := to.Latitude * math.Pi / 180
	lon1Rad := from.Longitude * math.Pi / 180
	deltaLon := (to.Longitude - from.Longitude) * math.Pi / 180

	bx := math.Cos(lat2Rad) * math.Cos(deltaLon)
	by := math.Cos(lat2Rad) * math.Sin(deltaLon)

	lat3 := math.Atan2(
		math.Sin(lat1Rad)+math.Sin(lat2Rad),
		math.Sqrt((math.Cos(lat1Rad)+bx)*(math.Cos(lat1Rad)+bx)+by*by),
	)
	lon3 := lon1Rad + math.Atan2(by, math.Cos(lat1Rad)+bx)

	return Coordinate{
		Latitude:  lat3 * 180 / math.Pi,
		Longitude: lon3 * 180 / math.Pi,
	}
}

// DestinationPoint calculates destination from start, bearing, and distance.
func DestinationPoint(start Coordinate, bearing float64, distance Distance) Coordinate {
	bearingRad := bearing * math.Pi / 180
	lat1Rad := start.Latitude * math.Pi / 180
	lon1Rad := start.Longitude * math.Pi / 180
	angularDist := distance.Meters / EarthRadiusMeters

	lat2 := math.Asin(
		math.Sin(lat1Rad)*math.Cos(angularDist) +
			math.Cos(lat1Rad)*math.Sin(angularDist)*math.Cos(bearingRad),
	)
	lon2 := lon1Rad + math.Atan2(
		math.Sin(bearingRad)*math.Sin(angularDist)*math.Cos(lat1Rad),
		math.Cos(angularDist)-math.Sin(lat1Rad)*math.Sin(lat2),
	)

	return Coordinate{
		Latitude:  lat2 * 180 / math.Pi,
		Longitude: lon2 * 180 / math.Pi,
	}
}

// BoundingBox represents a geographic bounding box.
type BoundingBox struct {
	MinLat float64
	MaxLat float64
	MinLon float64
	MaxLon float64
}

// NewBoundingBox creates a bounding box from coordinates.
func NewBoundingBox(coords []Coordinate) (BoundingBox, bool) {
	if len(coords) == 0 {
		return BoundingBox{}, false
	}

	bb := BoundingBox{
		MinLat: coords[0].Latitude,
		MaxLat: coords[0].Latitude,
		MinLon: coords[0].Longitude,
		MaxLon: coords[0].Longitude,
	}

	for _, c := range coords[1:] {
		if c.Latitude < bb.MinLat {
			bb.MinLat = c.Latitude
		}
		if c.Latitude > bb.MaxLat {
			bb.MaxLat = c.Latitude
		}
		if c.Longitude < bb.MinLon {
			bb.MinLon = c.Longitude
		}
		if c.Longitude > bb.MaxLon {
			bb.MaxLon = c.Longitude
		}
	}

	return bb, true
}

// Contains checks if a coordinate is within the bounding box.
func (bb BoundingBox) Contains(c Coordinate) bool {
	return c.Latitude >= bb.MinLat && c.Latitude <= bb.MaxLat &&
		c.Longitude >= bb.MinLon && c.Longitude <= bb.MaxLon
}

// Center returns the center of the bounding box.
func (bb BoundingBox) Center() Coordinate {
	return Coordinate{
		Latitude:  (bb.MinLat + bb.MaxLat) / 2,
		Longitude: (bb.MinLon + bb.MaxLon) / 2,
	}
}
