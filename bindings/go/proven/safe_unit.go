// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeUnit provides physical unit conversions via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// Length unit constants.
const (
	LengthMeters      = 0
	LengthKilometers  = 1
	LengthCentimeters = 2
	LengthMillimeters = 3
	LengthFeet        = 4
	LengthInches      = 5
	LengthMiles       = 6
	LengthYards       = 7
)

// Temperature unit constants.
const (
	TempCelsius    = 0
	TempFahrenheit = 1
	TempKelvin     = 2
)

// UnitConvertLength converts a length value between units.
func UnitConvertLength(value float64, from, to int32) (float64, error) {
	result := C.proven_unit_convert_length(C.double(value), C.int32_t(from), C.int32_t(to))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return float64(result.value), nil
}

// UnitConvertTemp converts a temperature value between units.
func UnitConvertTemp(value float64, from, to int32) (float64, error) {
	result := C.proven_unit_convert_temp(C.double(value), C.int32_t(from), C.int32_t(to))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return float64(result.value), nil
}
