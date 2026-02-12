// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

// Package proven provides safety-first utility functions with formal verification guarantees.
package proven

import (
	"math"
)

// SafeDiv safely divides two integers, returning ok=false on division by zero.
func SafeDiv(numerator, denominator int64) (result int64, ok bool) {
	if denominator == 0 {
		return 0, false
	}
	return numerator / denominator, true
}

// SafeMod safely computes modulo, returning ok=false on division by zero.
func SafeMod(numerator, denominator int64) (result int64, ok bool) {
	if denominator == 0 {
		return 0, false
	}
	return numerator % denominator, true
}

// SafeAdd safely adds two integers, returning ok=false on overflow.
func SafeAdd(a, b int64) (result int64, ok bool) {
	if b > 0 && a > math.MaxInt64-b {
		return 0, false
	}
	if b < 0 && a < math.MinInt64-b {
		return 0, false
	}
	return a + b, true
}

// SafeSub safely subtracts two integers, returning ok=false on overflow.
func SafeSub(a, b int64) (result int64, ok bool) {
	if b < 0 && a > math.MaxInt64+b {
		return 0, false
	}
	if b > 0 && a < math.MinInt64+b {
		return 0, false
	}
	return a - b, true
}

// SafeMul safely multiplies two integers, returning ok=false on overflow.
func SafeMul(a, b int64) (result int64, ok bool) {
	if a == 0 || b == 0 {
		return 0, true
	}

	result = a * b
	if result/a != b {
		return 0, false
	}
	return result, true
}

// MathError represents arithmetic errors.
type MathError int

const (
	// MathOK indicates no error.
	MathOK MathError = iota
	// MathOverflow indicates arithmetic overflow.
	MathOverflow
	// MathDivisionByZero indicates division by zero.
	MathDivisionByZero
)

// CheckedAdd adds two integers with error reporting.
func CheckedAdd(a, b int64) (int64, MathError) {
	result, ok := SafeAdd(a, b)
	if !ok {
		return 0, MathOverflow
	}
	return result, MathOK
}

// CheckedSub subtracts two integers with error reporting.
func CheckedSub(a, b int64) (int64, MathError) {
	result, ok := SafeSub(a, b)
	if !ok {
		return 0, MathOverflow
	}
	return result, MathOK
}

// CheckedMul multiplies two integers with error reporting.
func CheckedMul(a, b int64) (int64, MathError) {
	result, ok := SafeMul(a, b)
	if !ok {
		return 0, MathOverflow
	}
	return result, MathOK
}
