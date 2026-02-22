// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeJson provides JSON validation and type detection via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// JsonType represents the type of a JSON value.
type JsonType int32

const (
	// JsonNull represents a JSON null value.
	JsonNull JsonType = 0
	// JsonBool represents a JSON boolean value.
	JsonBool JsonType = 1
	// JsonNumber represents a JSON number value.
	JsonNumber JsonType = 2
	// JsonString represents a JSON string value.
	JsonString JsonType = 3
	// JsonArray represents a JSON array value.
	JsonArray JsonType = 4
	// JsonObject represents a JSON object value.
	JsonObject JsonType = 5
	// JsonInvalid represents invalid JSON.
	JsonInvalid JsonType = -1
)

// JSONIsValid checks whether a string is valid JSON.
func JSONIsValid(input string) (bool, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)
	result := C.proven_json_is_valid(cs, length)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// JSONGetType returns the type of the root JSON value.
func JSONGetType(input string) JsonType {
	cs, length := cString(input)
	defer unsafeFree(cs)
	return JsonType(C.proven_json_get_type(cs, length))
}
