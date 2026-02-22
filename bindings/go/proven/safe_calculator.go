// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeCalculator provides expression evaluation via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// CalculatorEval evaluates a mathematical expression string.
// Supports basic arithmetic operators (+, -, *, /) and parentheses.
func CalculatorEval(expression string) (float64, error) {
	cs, length := cString(expression)
	defer unsafeFree(cs)
	result := C.proven_calculator_eval(cs, length)
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return float64(result.value), nil
}
