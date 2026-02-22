// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeMonotonic provides monotonically increasing sequences via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// MonotonicHandle is a handle to a monotonic counter allocated in the FFI layer.
type MonotonicHandle struct {
	ptr *C.MonotonicCounter
}

// MonotonicCreate creates a new monotonic counter with an initial value
// and a maximum value.
func MonotonicCreate(initial, maxValue uint64) (*MonotonicHandle, error) {
	ptr := C.proven_monotonic_create(C.uint64_t(initial), C.uint64_t(maxValue))
	if ptr == nil {
		return nil, newError(StatusErrAllocFailed)
	}
	return &MonotonicHandle{ptr: ptr}, nil
}

// Next increments the counter and returns the new value.
// Returns an error if the counter has reached its maximum.
func (m *MonotonicHandle) Next() (int64, error) {
	result := C.proven_monotonic_next(m.ptr)
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}

// Free releases the monotonic counter's memory.
func (m *MonotonicHandle) Free() {
	if m.ptr != nil {
		C.proven_monotonic_free(m.ptr)
		m.ptr = nil
	}
}
