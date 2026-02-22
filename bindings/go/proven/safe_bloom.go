// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeBloom provides probabilistic set membership (Bloom filter) via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// BloomHandle is a handle to a Bloom filter allocated in the FFI layer.
type BloomHandle struct {
	ptr *C.BloomFilter
}

// BloomCreate creates a new Bloom filter optimized for the expected number
// of elements and desired false positive rate.
func BloomCreate(expectedElements int, falsePositiveRate float64) (*BloomHandle, error) {
	ptr := C.proven_bloom_create(C.size_t(expectedElements), C.double(falsePositiveRate))
	if ptr == nil {
		return nil, newError(StatusErrAllocFailed)
	}
	return &BloomHandle{ptr: ptr}, nil
}

// Add adds an element to the Bloom filter.
func (bf *BloomHandle) Add(data []byte) {
	ptr, length := cBytes(data)
	if ptr != nil {
		defer unsafeFree(ptr)
	}
	C.proven_bloom_add(bf.ptr, ptr, length)
}

// Contains checks whether an element might be in the Bloom filter.
// False positives are possible; false negatives are not.
func (bf *BloomHandle) Contains(data []byte) bool {
	ptr, length := cBytes(data)
	if ptr != nil {
		defer unsafeFree(ptr)
	}
	return bool(C.proven_bloom_contains(bf.ptr, ptr, length))
}

// Free releases the Bloom filter's memory.
func (bf *BloomHandle) Free() {
	if bf.ptr != nil {
		C.proven_bloom_free(bf.ptr)
		bf.ptr = nil
	}
}
