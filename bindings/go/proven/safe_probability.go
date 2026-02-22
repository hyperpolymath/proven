// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeProbability provides probability values clamped to [0, 1] via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
import "C"

// ProbabilityCreate creates a probability value clamped to the range [0, 1].
func ProbabilityCreate(value float64) float64 {
	return float64(C.proven_probability_create(C.double(value)))
}

// ProbabilityAnd multiplies two probabilities (independent events: P(A and B) = P(A) * P(B)).
func ProbabilityAnd(a, b float64) float64 {
	return float64(C.proven_probability_and(C.double(a), C.double(b)))
}

// ProbabilityOrExclusive adds two probabilities (mutually exclusive events: P(A or B) = P(A) + P(B)).
func ProbabilityOrExclusive(a, b float64) float64 {
	return float64(C.proven_probability_or_exclusive(C.double(a), C.double(b)))
}

// ProbabilityNot computes the complement of a probability (P(not A) = 1 - P(A)).
func ProbabilityNot(p float64) float64 {
	return float64(C.proven_probability_not(C.double(p)))
}
