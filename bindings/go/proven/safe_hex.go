// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeHex provides hexadecimal encoding and decoding via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"
import "unsafe"

// HexEncode encodes a byte slice to a hex string.
// If uppercase is true, uses uppercase hex digits.
func HexEncode(data []byte, uppercase bool) (string, error) {
	ptr, length := cBytes(data)
	if ptr != nil {
		defer C.free(unsafe.Pointer(ptr))
	}
	return goStringResult(C.proven_hex_encode(ptr, length, C._Bool(uppercase)))
}

// HexDecode decodes a hex string to a byte slice.
func HexDecode(hexStr string) ([]byte, error) {
	cs, length := cString(hexStr)
	defer unsafeFree(cs)

	result := C.proven_hex_decode(cs, length)
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}
	if result.data == nil {
		return nil, newError(StatusErrNullPointer)
	}

	// Copy the decoded data into a Go byte slice.
	goBytes := C.GoBytes(unsafe.Pointer(result.data), C.int(result.length))

	// Free the C-allocated decode result.
	C.proven_hex_free(&result)

	return goBytes, nil
}
