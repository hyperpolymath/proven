// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeCircuitBreaker provides fault tolerance via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// Circuit state constants.
const (
	CircuitClosed   = 0 // Normal operation
	CircuitOpen     = 1 // Failing, reject requests
	CircuitHalfOpen = 2 // Testing recovery
)

// CircuitBreakerHandle is a handle to a circuit breaker allocated in the FFI layer.
type CircuitBreakerHandle struct {
	ptr *C.CircuitBreaker
}

// CircuitBreakerCreate creates a new circuit breaker.
func CircuitBreakerCreate(failureThreshold, successThreshold uint32, timeoutMs int64) (*CircuitBreakerHandle, error) {
	ptr := C.proven_circuit_breaker_create(
		C.uint32_t(failureThreshold),
		C.uint32_t(successThreshold),
		C.int64_t(timeoutMs),
	)
	if ptr == nil {
		return nil, newError(StatusErrAllocFailed)
	}
	return &CircuitBreakerHandle{ptr: ptr}, nil
}

// Allow checks whether a request should be allowed through.
func (cb *CircuitBreakerHandle) Allow() bool {
	return bool(C.proven_circuit_breaker_allow(cb.ptr))
}

// Success records a successful operation.
func (cb *CircuitBreakerHandle) Success() {
	C.proven_circuit_breaker_success(cb.ptr)
}

// Failure records a failed operation.
func (cb *CircuitBreakerHandle) Failure() {
	C.proven_circuit_breaker_failure(cb.ptr)
}

// State returns the current circuit breaker state.
func (cb *CircuitBreakerHandle) State() int32 {
	return int32(C.proven_circuit_breaker_state(cb.ptr))
}

// Free releases the circuit breaker's memory.
func (cb *CircuitBreakerHandle) Free() {
	if cb.ptr != nil {
		C.proven_circuit_breaker_free(cb.ptr)
		cb.ptr = nil
	}
}
