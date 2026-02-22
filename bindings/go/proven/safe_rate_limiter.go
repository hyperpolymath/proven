// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeRateLimiter provides token bucket rate limiting via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// RateLimiterHandle is a handle to a rate limiter allocated in the Proven FFI layer.
type RateLimiterHandle struct {
	ptr *C.RateLimiter
}

// RateLimiterCreate creates a new token bucket rate limiter.
func RateLimiterCreate(capacity, refillRate float64) (*RateLimiterHandle, error) {
	ptr := C.proven_rate_limiter_create(C.double(capacity), C.double(refillRate))
	if ptr == nil {
		return nil, newError(StatusErrAllocFailed)
	}
	return &RateLimiterHandle{ptr: ptr}, nil
}

// TryAcquire attempts to consume the given number of tokens.
// Returns true if tokens were available, false otherwise.
func (rl *RateLimiterHandle) TryAcquire(tokens float64) bool {
	return bool(C.proven_rate_limiter_try_acquire(rl.ptr, C.double(tokens)))
}

// Free releases the rate limiter's memory.
func (rl *RateLimiterHandle) Free() {
	if rl.ptr != nil {
		C.proven_rate_limiter_free(rl.ptr)
		rl.ptr = nil
	}
}
