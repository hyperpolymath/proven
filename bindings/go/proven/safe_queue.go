// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeQueue provides a bounded FIFO queue via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// QueueHandle is a handle to a bounded queue allocated in the FFI layer.
type QueueHandle struct {
	ptr *C.BoundedQueue
}

// QueueCreate creates a new bounded FIFO queue with the given capacity.
func QueueCreate(capacity int) (*QueueHandle, error) {
	ptr := C.proven_queue_create(C.size_t(capacity))
	if ptr == nil {
		return nil, newError(StatusErrAllocFailed)
	}
	return &QueueHandle{ptr: ptr}, nil
}

// Push adds a value to the back of the queue.
// Returns false if the queue is full.
func (q *QueueHandle) Push(value int64) bool {
	return bool(C.proven_queue_push(q.ptr, C.int64_t(value)))
}

// Pop removes and returns the value at the front of the queue.
// Returns an error if the queue is empty.
func (q *QueueHandle) Pop() (int64, error) {
	result := C.proven_queue_pop(q.ptr)
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}

// Size returns the current number of elements in the queue.
func (q *QueueHandle) Size() int {
	return int(C.proven_queue_size(q.ptr))
}

// Free releases the queue's memory.
func (q *QueueHandle) Free() {
	if q.ptr != nil {
		C.proven_queue_free(q.ptr)
		q.ptr = nil
	}
}
