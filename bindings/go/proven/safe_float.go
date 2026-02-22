// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeFloat provides safe floating-point operations via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// FloatDiv performs safe floating-point division.
// Returns an error if the divisor is zero.
func FloatDiv(a, b float64) (float64, error) {
	result := C.proven_float_div(C.double(a), C.double(b))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return float64(result.value), nil
}

// FloatIsFinite checks whether a float is finite (not NaN or Inf).
func FloatIsFinite(x float64) bool {
	return bool(C.proven_float_is_finite(C.double(x)))
}

// FloatIsNaN checks whether a float is NaN.
func FloatIsNaN(x float64) bool {
	return bool(C.proven_float_is_nan(C.double(x)))
}

// FloatSqrt computes the safe square root. Returns an error for negative input.
func FloatSqrt(x float64) (float64, error) {
	result := C.proven_float_sqrt(C.double(x))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return float64(result.value), nil
}

// FloatLn computes the safe natural logarithm. Returns an error for non-positive input.
func FloatLn(x float64) (float64, error) {
	result := C.proven_float_ln(C.double(x))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return float64(result.value), nil
}
