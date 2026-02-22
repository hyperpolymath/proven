// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeHeader provides HTTP header operations that prevent CRLF injection.
// All computation is performed in Idris 2 via the Proven FFI.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// HeaderHasCRLF checks for CRLF injection characters in a header value.
func HeaderHasCRLF(value string) (bool, error) {
	cs, length := cString(value)
	defer unsafeFree(cs)
	result := C.proven_header_has_crlf(cs, length)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// HeaderIsValidName checks whether a header name is a valid token per RFC 7230.
func HeaderIsValidName(name string) (bool, error) {
	cs, length := cString(name)
	defer unsafeFree(cs)
	result := C.proven_header_is_valid_name(cs, length)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// HeaderIsDangerous checks whether a header name is in the dangerous headers list.
func HeaderIsDangerous(name string) (bool, error) {
	cs, length := cString(name)
	defer unsafeFree(cs)
	result := C.proven_header_is_dangerous(cs, length)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// HeaderRender creates a validated header string in "Name: Value" format.
func HeaderRender(name, value string) (string, error) {
	nameCS, nameLen := cString(name)
	defer unsafeFree(nameCS)
	valueCS, valueLen := cString(value)
	defer unsafeFree(valueCS)
	return goStringResult(C.proven_header_render(nameCS, nameLen, valueCS, valueLen))
}

// HeaderBuildCSP builds a Content-Security-Policy header value from JSON directives.
func HeaderBuildCSP(directivesJSON string) (string, error) {
	cs, length := cString(directivesJSON)
	defer unsafeFree(cs)
	return goStringResult(C.proven_header_build_csp(cs, length))
}

// HeaderBuildHSTS builds a Strict-Transport-Security header value.
func HeaderBuildHSTS(maxAge int64, includeSubdomains, preload bool) (string, error) {
	return goStringResult(C.proven_header_build_hsts(
		C.int64_t(maxAge),
		C._Bool(includeSubdomains),
		C._Bool(preload),
	))
}
