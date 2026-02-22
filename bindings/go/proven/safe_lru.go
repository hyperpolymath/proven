// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeLRU provides a Least Recently Used cache via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// LRUHandle is a handle to an LRU cache allocated in the FFI layer.
type LRUHandle struct {
	ptr *C.LRUCache
}

// LRUCreate creates a new LRU cache with the given capacity.
func LRUCreate(capacity int) (*LRUHandle, error) {
	ptr := C.proven_lru_create(C.size_t(capacity))
	if ptr == nil {
		return nil, newError(StatusErrAllocFailed)
	}
	return &LRUHandle{ptr: ptr}, nil
}

// Get retrieves a value from the LRU cache by key.
// Returns the value and nil error if found, or an error if not found.
func (lru *LRUHandle) Get(key uint64) (int64, error) {
	result := C.proven_lru_get(lru.ptr, C.uint64_t(key))
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}

// Put inserts a key-value pair into the LRU cache.
// If the cache is full, the least recently used entry is evicted.
func (lru *LRUHandle) Put(key uint64, value int64) error {
	status := int(C.proven_lru_put(lru.ptr, C.uint64_t(key), C.int64_t(value)))
	if status != StatusOK {
		return newError(status)
	}
	return nil
}

// Free releases the LRU cache's memory.
func (lru *LRUHandle) Free() {
	if lru.ptr != nil {
		C.proven_lru_free(lru.ptr)
		lru.ptr = nil
	}
}
