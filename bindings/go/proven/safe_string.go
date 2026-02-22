// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeString provides text operations that handle encoding safely.
// All computation is performed in Idris 2 via the Proven FFI.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"
import "unsafe"

// IsValidUTF8 checks whether a byte sequence is valid UTF-8.
func IsValidUTF8(data []byte) (bool, error) {
	ptr, length := cBytes(data)
	if ptr != nil {
		defer C.free(unsafe.Pointer(ptr))
	}
	result := C.proven_string_is_valid_utf8(ptr, length)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// EscapeSQL escapes a string for safe SQL interpolation (single quotes).
// Prefer parameterized queries over string interpolation.
func EscapeSQL(input string) (string, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)
	return goStringResult(C.proven_string_escape_sql(cs, length))
}

// EscapeHTML escapes a string for safe HTML insertion (< > & " ').
func EscapeHTML(input string) (string, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)
	return goStringResult(C.proven_string_escape_html(cs, length))
}

// EscapeJS escapes a string for safe JavaScript string literal insertion.
func EscapeJS(input string) (string, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)
	return goStringResult(C.proven_string_escape_js(cs, length))
}
