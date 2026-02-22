// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeCrypto provides cryptographic primitives via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"
import "unsafe"

// ConstantTimeEq performs a constant-time comparison of two byte slices.
// This prevents timing attacks when comparing secrets.
func ConstantTimeEq(a, b []byte) (bool, error) {
	ptrA, lenA := cBytes(a)
	ptrB, lenB := cBytes(b)
	if ptrA != nil {
		defer C.free(unsafe.Pointer(ptrA))
	}
	if ptrB != nil {
		defer C.free(unsafe.Pointer(ptrB))
	}
	result := C.proven_crypto_constant_time_eq(ptrA, lenA, ptrB, lenB)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// RandomBytes fills the provided byte slice with cryptographically secure
// random bytes.
func RandomBytes(buf []byte) error {
	if len(buf) == 0 {
		return nil
	}
	ptr := (*C.char)(unsafe.Pointer(&buf[0]))
	status := int(C.proven_crypto_random_bytes(ptr, C.size_t(len(buf))))
	if status != StatusOK {
		return newError(status)
	}
	return nil
}
