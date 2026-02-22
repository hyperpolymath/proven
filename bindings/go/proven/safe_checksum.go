// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeChecksum provides CRC and hash verification via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"
import "unsafe"

// ChecksumCRC32 calculates the CRC32 of a byte slice.
func ChecksumCRC32(data []byte) (int64, error) {
	ptr, length := cBytes(data)
	if ptr != nil {
		defer C.free(unsafe.Pointer(ptr))
	}
	result := C.proven_checksum_crc32(ptr, length)
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}

// ChecksumVerifyCRC32 verifies that the CRC32 of data matches an expected value.
func ChecksumVerifyCRC32(data []byte, expected uint32) (bool, error) {
	ptr, length := cBytes(data)
	if ptr != nil {
		defer C.free(unsafe.Pointer(ptr))
	}
	result := C.proven_checksum_verify_crc32(ptr, length, C.uint32_t(expected))
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}
