// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"math"
)

// Degrees represents an angle in degrees.
type Degrees struct {
	value float64
}

// NewDegrees creates an angle in degrees.
func NewDegrees(value float64) Degrees {
	return Degrees{value: value}
}

// Value returns the degrees value.
func (d Degrees) Value() float64 {
	return d.value
}

// ToRadians converts to radians.
func (d Degrees) ToRadians() Radians {
	return Radians{value: d.value * math.Pi / 180}
}

// Normalize normalizes to [0, 360).
func (d Degrees) Normalize() Degrees {
	v := math.Mod(d.value, 360)
	if v < 0 {
		v += 360
	}
	return Degrees{value: v}
}

// NormalizeSigned normalizes to [-180, 180).
func (d Degrees) NormalizeSigned() Degrees {
	v := math.Mod(d.value+180, 360) - 180
	if v < -180 {
		v += 360
	}
	return Degrees{value: v}
}

// Add adds two angles.
func (d Degrees) Add(other Degrees) Degrees {
	return Degrees{value: d.value + other.value}
}

// Sub subtracts two angles.
func (d Degrees) Sub(other Degrees) Degrees {
	return Degrees{value: d.value - other.value}
}

// Mul multiplies by scalar.
func (d Degrees) Mul(scalar float64) Degrees {
	return Degrees{value: d.value * scalar}
}

// Sin computes sine.
func (d Degrees) Sin() float64 {
	return math.Sin(d.value * math.Pi / 180)
}

// Cos computes cosine.
func (d Degrees) Cos() float64 {
	return math.Cos(d.value * math.Pi / 180)
}

// Tan computes tangent.
func (d Degrees) Tan() float64 {
	return math.Tan(d.value * math.Pi / 180)
}

// Radians represents an angle in radians.
type Radians struct {
	value float64
}

// NewRadians creates an angle in radians.
func NewRadians(value float64) Radians {
	return Radians{value: value}
}

// Value returns the radians value.
func (r Radians) Value() float64 {
	return r.value
}

// ToDegrees converts to degrees.
func (r Radians) ToDegrees() Degrees {
	return Degrees{value: r.value * 180 / math.Pi}
}

// Normalize normalizes to [0, 2π).
func (r Radians) Normalize() Radians {
	v := math.Mod(r.value, 2*math.Pi)
	if v < 0 {
		v += 2 * math.Pi
	}
	return Radians{value: v}
}

// NormalizeSigned normalizes to [-π, π).
func (r Radians) NormalizeSigned() Radians {
	v := math.Mod(r.value+math.Pi, 2*math.Pi) - math.Pi
	if v < -math.Pi {
		v += 2 * math.Pi
	}
	return Radians{value: v}
}

// Add adds two angles.
func (r Radians) Add(other Radians) Radians {
	return Radians{value: r.value + other.value}
}

// Sub subtracts two angles.
func (r Radians) Sub(other Radians) Radians {
	return Radians{value: r.value - other.value}
}

// Mul multiplies by scalar.
func (r Radians) Mul(scalar float64) Radians {
	return Radians{value: r.value * scalar}
}

// Sin computes sine.
func (r Radians) Sin() float64 {
	return math.Sin(r.value)
}

// Cos computes cosine.
func (r Radians) Cos() float64 {
	return math.Cos(r.value)
}

// Tan computes tangent.
func (r Radians) Tan() float64 {
	return math.Tan(r.value)
}

// Common angle constants.
var (
	Deg0   = NewDegrees(0)
	Deg30  = NewDegrees(30)
	Deg45  = NewDegrees(45)
	Deg60  = NewDegrees(60)
	Deg90  = NewDegrees(90)
	Deg180 = NewDegrees(180)
	Deg270 = NewDegrees(270)
	Deg360 = NewDegrees(360)

	RadPi     = NewRadians(math.Pi)
	RadPiOver2 = NewRadians(math.Pi / 2)
	RadPiOver4 = NewRadians(math.Pi / 4)
	Rad2Pi    = NewRadians(2 * math.Pi)
)

// Asin computes arcsin, returning degrees.
func Asin(x float64) (Degrees, bool) {
	if x < -1 || x > 1 {
		return Degrees{}, false
	}
	return Degrees{value: math.Asin(x) * 180 / math.Pi}, true
}

// Acos computes arccos, returning degrees.
func Acos(x float64) (Degrees, bool) {
	if x < -1 || x > 1 {
		return Degrees{}, false
	}
	return Degrees{value: math.Acos(x) * 180 / math.Pi}, true
}

// Atan computes arctan, returning degrees.
func Atan(x float64) Degrees {
	return Degrees{value: math.Atan(x) * 180 / math.Pi}
}

// Atan2 computes arctan2, returning degrees.
func Atan2(y, x float64) Degrees {
	return Degrees{value: math.Atan2(y, x) * 180 / math.Pi}
}

// AsinRad computes arcsin, returning radians.
func AsinRad(x float64) (Radians, bool) {
	if x < -1 || x > 1 {
		return Radians{}, false
	}
	return Radians{value: math.Asin(x)}, true
}

// AcosRad computes arccos, returning radians.
func AcosRad(x float64) (Radians, bool) {
	if x < -1 || x > 1 {
		return Radians{}, false
	}
	return Radians{value: math.Acos(x)}, true
}

// AtanRad computes arctan, returning radians.
func AtanRad(x float64) Radians {
	return Radians{value: math.Atan(x)}
}

// Atan2Rad computes arctan2, returning radians.
func Atan2Rad(y, x float64) Radians {
	return Radians{value: math.Atan2(y, x)}
}
