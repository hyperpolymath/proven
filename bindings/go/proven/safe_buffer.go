// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeBuffer provides bounded buffer operations via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"
import "unsafe"

// Buffer is a handle to a bounded buffer allocated in the Proven FFI layer.
type Buffer struct {
	ptr *C.BoundedBuffer
}

// BufferCreate creates a new bounded buffer with the given capacity.
func BufferCreate(capacity int) (*Buffer, error) {
	result := C.proven_buffer_create(C.size_t(capacity))
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}
	if result.buffer == nil {
		return nil, newError(StatusErrAllocFailed)
	}
	return &Buffer{ptr: result.buffer}, nil
}

// Append appends data to the buffer with bounds checking.
func (b *Buffer) Append(data []byte) error {
	ptr, length := cBytes(data)
	if ptr != nil {
		defer C.free(unsafe.Pointer(ptr))
	}
	status := int(C.proven_buffer_append(b.ptr, ptr, length))
	if status != StatusOK {
		return newError(status)
	}
	return nil
}

// Get returns the current contents of the buffer as a byte slice.
func (b *Buffer) Get() ([]byte, error) {
	var outPtr *C.char
	var outLen C.size_t
	status := int(C.proven_buffer_get(b.ptr, &outPtr, &outLen))
	if status != StatusOK {
		return nil, newError(status)
	}
	if outPtr == nil || outLen == 0 {
		return nil, nil
	}
	return C.GoBytes(unsafe.Pointer(outPtr), C.int(outLen)), nil
}

// Free releases the buffer's memory. Must be called when the buffer is no
// longer needed.
func (b *Buffer) Free() {
	if b.ptr != nil {
		C.proven_buffer_free(b.ptr)
		b.ptr = nil
	}
}
