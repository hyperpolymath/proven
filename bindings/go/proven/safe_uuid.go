// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeUUID provides UUID generation and validation via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// ProvenUUID represents a 128-bit UUID.
type ProvenUUID struct {
	Bytes [16]byte
}

// UUIDV4 generates a new random UUID v4.
func UUIDV4() (*ProvenUUID, error) {
	result := C.proven_uuid_v4()
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}
	uuid := &ProvenUUID{}
	for i := 0; i < 16; i++ {
		uuid.Bytes[i] = byte(result.uuid.bytes[i])
	}
	return uuid, nil
}

// UUIDToString formats a UUID as a 36-character string with hyphens.
func UUIDToString(uuid *ProvenUUID) (string, error) {
	cUUID := C.UUID{}
	for i := 0; i < 16; i++ {
		cUUID.bytes[i] = C.uint8_t(uuid.Bytes[i])
	}
	return goStringResult(C.proven_uuid_to_string(cUUID))
}

// UUIDParse parses a UUID from a string.
func UUIDParse(s string) (*ProvenUUID, error) {
	cs, length := cString(s)
	defer unsafeFree(cs)
	result := C.proven_uuid_parse(cs, length)
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}
	uuid := &ProvenUUID{}
	for i := 0; i < 16; i++ {
		uuid.Bytes[i] = byte(result.uuid.bytes[i])
	}
	return uuid, nil
}

// UUIDIsNil checks whether a UUID is the nil UUID (all zeros).
func UUIDIsNil(uuid *ProvenUUID) bool {
	cUUID := C.UUID{}
	for i := 0; i < 16; i++ {
		cUUID.bytes[i] = C.uint8_t(uuid.Bytes[i])
	}
	return bool(C.proven_uuid_is_nil(cUUID))
}

// UUIDVersion returns the version number of a UUID.
func UUIDVersion(uuid *ProvenUUID) uint8 {
	cUUID := C.UUID{}
	for i := 0; i < 16; i++ {
		cUUID.bytes[i] = C.uint8_t(uuid.Bytes[i])
	}
	return uint8(C.proven_uuid_version(cUUID))
}
