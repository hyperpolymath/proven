// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeTensor provides basic 2D tensor operations via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// TensorHandle is a handle to a 2D tensor allocated in the FFI layer.
type TensorHandle struct {
	ptr *C.Tensor2D
}

// TensorCreate creates a new 2D tensor (matrix) with the given dimensions.
// All values are initialized to zero.
func TensorCreate(rows, cols int) (*TensorHandle, error) {
	ptr := C.proven_tensor_create(C.size_t(rows), C.size_t(cols))
	if ptr == nil {
		return nil, newError(StatusErrAllocFailed)
	}
	return &TensorHandle{ptr: ptr}, nil
}

// Set sets a value at the specified row and column.
func (t *TensorHandle) Set(row, col int, value float64) error {
	status := int(C.proven_tensor_set(t.ptr, C.size_t(row), C.size_t(col), C.double(value)))
	if status != StatusOK {
		return newError(status)
	}
	return nil
}

// Get retrieves the value at the specified row and column.
func (t *TensorHandle) Get(row, col int) (float64, error) {
	result := C.proven_tensor_get(t.ptr, C.size_t(row), C.size_t(col))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return float64(result.value), nil
}

// Matmul performs matrix multiplication (this tensor * other).
// Returns a new tensor handle. The caller must Free the result.
func (t *TensorHandle) Matmul(other *TensorHandle) (*TensorHandle, error) {
	ptr := C.proven_tensor_matmul(t.ptr, other.ptr)
	if ptr == nil {
		return nil, newError(StatusErrAllocFailed)
	}
	return &TensorHandle{ptr: ptr}, nil
}

// Free releases the tensor's memory.
func (t *TensorHandle) Free() {
	if t.ptr != nil {
		C.proven_tensor_free(t.ptr)
		t.ptr = nil
	}
}
