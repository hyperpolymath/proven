// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeMath provides checked arithmetic operations that cannot crash.
// All computation is performed in Idris 2 via the Proven FFI.

package proven

// #include <stdint.h>
import "C"

// SafeDiv safely divides two integers. Returns an error on division by zero.
func SafeDiv(numerator, denominator int64) (int64, error) {
	result := C.proven_math_div(C.int64_t(numerator), C.int64_t(denominator))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}

// SafeMod safely computes the modulo. Returns an error on division by zero.
func SafeMod(numerator, denominator int64) (int64, error) {
	result := C.proven_math_mod(C.int64_t(numerator), C.int64_t(denominator))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}

// SafeAdd performs checked addition. Returns an error on overflow.
func SafeAdd(a, b int64) (int64, error) {
	result := C.proven_math_add_checked(C.int64_t(a), C.int64_t(b))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}

// SafeSub performs checked subtraction. Returns an error on underflow.
func SafeSub(a, b int64) (int64, error) {
	result := C.proven_math_sub_checked(C.int64_t(a), C.int64_t(b))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}

// SafeMul performs checked multiplication. Returns an error on overflow.
func SafeMul(a, b int64) (int64, error) {
	result := C.proven_math_mul_checked(C.int64_t(a), C.int64_t(b))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}

// AbsSafe computes the absolute value safely. Returns an error if the value
// is the minimum int64 (which cannot be negated without overflow).
func AbsSafe(n int64) (int64, error) {
	result := C.proven_math_abs_safe(C.int64_t(n))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}

// Clamp restricts a value to the range [lo, hi].
func Clamp(lo, hi, value int64) int64 {
	return int64(C.proven_math_clamp(C.int64_t(lo), C.int64_t(hi), C.int64_t(value)))
}

// PowChecked computes base^exp with overflow checking.
func PowChecked(base int64, exp uint32) (int64, error) {
	result := C.proven_math_pow_checked(C.int64_t(base), C.uint32_t(exp))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}
