// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"math"
)

// SafeFloat provides safe float64 operations preventing NaN/Inf.
type SafeFloat struct {
	value float64
}

// NewSafeFloat creates a safe float, returning false for NaN/Inf.
func NewSafeFloat(value float64) (SafeFloat, bool) {
	if math.IsNaN(value) || math.IsInf(value, 0) {
		return SafeFloat{}, false
	}
	return SafeFloat{value: value}, true
}

// MustSafeFloat creates a safe float, panicking for NaN/Inf.
func MustSafeFloat(value float64) SafeFloat {
	sf, ok := NewSafeFloat(value)
	if !ok {
		panic("invalid float value (NaN or Inf)")
	}
	return sf
}

// Value returns the underlying float64.
func (sf SafeFloat) Value() float64 {
	return sf.value
}

// Add adds two safe floats.
func (sf SafeFloat) Add(other SafeFloat) (SafeFloat, bool) {
	return NewSafeFloat(sf.value + other.value)
}

// Sub subtracts two safe floats.
func (sf SafeFloat) Sub(other SafeFloat) (SafeFloat, bool) {
	return NewSafeFloat(sf.value - other.value)
}

// Mul multiplies two safe floats.
func (sf SafeFloat) Mul(other SafeFloat) (SafeFloat, bool) {
	return NewSafeFloat(sf.value * other.value)
}

// Div divides two safe floats.
func (sf SafeFloat) Div(other SafeFloat) (SafeFloat, bool) {
	if other.value == 0 {
		return SafeFloat{}, false
	}
	return NewSafeFloat(sf.value / other.value)
}

// Sqrt computes square root.
func (sf SafeFloat) Sqrt() (SafeFloat, bool) {
	if sf.value < 0 {
		return SafeFloat{}, false
	}
	return NewSafeFloat(math.Sqrt(sf.value))
}

// Pow computes power.
func (sf SafeFloat) Pow(exp SafeFloat) (SafeFloat, bool) {
	return NewSafeFloat(math.Pow(sf.value, exp.value))
}

// Abs returns absolute value.
func (sf SafeFloat) Abs() SafeFloat {
	return SafeFloat{value: math.Abs(sf.value)}
}

// Neg returns negation.
func (sf SafeFloat) Neg() SafeFloat {
	return SafeFloat{value: -sf.value}
}

// Floor returns floor value.
func (sf SafeFloat) Floor() SafeFloat {
	return SafeFloat{value: math.Floor(sf.value)}
}

// Ceil returns ceiling value.
func (sf SafeFloat) Ceil() SafeFloat {
	return SafeFloat{value: math.Ceil(sf.value)}
}

// Round returns rounded value.
func (sf SafeFloat) Round() SafeFloat {
	return SafeFloat{value: math.Round(sf.value)}
}

// Trunc returns truncated value.
func (sf SafeFloat) Trunc() SafeFloat {
	return SafeFloat{value: math.Trunc(sf.value)}
}

// IsZero checks if value is zero.
func (sf SafeFloat) IsZero() bool {
	return sf.value == 0
}

// IsPositive checks if value is positive.
func (sf SafeFloat) IsPositive() bool {
	return sf.value > 0
}

// IsNegative checks if value is negative.
func (sf SafeFloat) IsNegative() bool {
	return sf.value < 0
}

// AlmostEqual compares with epsilon tolerance.
func (sf SafeFloat) AlmostEqual(other SafeFloat, epsilon float64) bool {
	return math.Abs(sf.value-other.value) < epsilon
}

// Clamp constrains value to range.
func (sf SafeFloat) Clamp(min, max SafeFloat) SafeFloat {
	if sf.value < min.value {
		return min
	}
	if sf.value > max.value {
		return max
	}
	return sf
}

// Lerp performs linear interpolation.
func (sf SafeFloat) Lerp(other SafeFloat, t float64) (SafeFloat, bool) {
	return NewSafeFloat(sf.value + (other.value-sf.value)*t)
}

// IsFinite checks if value is finite.
func IsFinite(value float64) bool {
	return !math.IsNaN(value) && !math.IsInf(value, 0)
}

// IsValidFloat checks if value is valid (not NaN or Inf).
func IsValidFloat(value float64) bool {
	return IsFinite(value)
}

// SafeLog computes natural log safely.
func SafeLog(value float64) (float64, bool) {
	if value <= 0 {
		return 0, false
	}
	result := math.Log(value)
	return result, IsFinite(result)
}

// SafeExp computes e^x safely.
func SafeExp(value float64) (float64, bool) {
	result := math.Exp(value)
	return result, IsFinite(result)
}

// SafeSqrt computes square root safely.
func SafeSqrt(value float64) (float64, bool) {
	if value < 0 {
		return 0, false
	}
	return math.Sqrt(value), true
}

// SafeAsin computes arcsin safely.
func SafeAsin(value float64) (float64, bool) {
	if value < -1 || value > 1 {
		return 0, false
	}
	return math.Asin(value), true
}

// SafeAcos computes arccos safely.
func SafeAcos(value float64) (float64, bool) {
	if value < -1 || value > 1 {
		return 0, false
	}
	return math.Acos(value), true
}

// Epsilon is the machine epsilon for float64.
const Epsilon = 2.220446049250313e-16
