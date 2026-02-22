// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeHTTP provides HTTP URL encoding and header parsing via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// HTTPURLEncode percent-encodes a string per RFC 3986.
// Unreserved characters (A-Za-z0-9-._~) pass through; all others become %XX.
func HTTPURLEncode(input string) (string, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)
	return goStringResult(C.proven_http_url_encode(cs, length))
}

// HTTPURLDecode decodes a percent-encoded string.
func HTTPURLDecode(input string) (string, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)
	return goStringResult(C.proven_http_url_decode(cs, length))
}

// AuthChallengeResult represents a parsed WWW-Authenticate header.
type AuthChallengeResult struct {
	Scheme  string
	Realm   string
	Service string
	Scope   string
}

// HTTPParseWWWAuthenticate parses a WWW-Authenticate header
// (e.g., Bearer realm="...",service="...",scope="...").
func HTTPParseWWWAuthenticate(input string) (*AuthChallengeResult, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)

	result := C.proven_http_parse_www_authenticate(cs, length)
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}

	challenge := &AuthChallengeResult{}
	if result.challenge.scheme != nil {
		challenge.Scheme = C.GoStringN(result.challenge.scheme, C.int(result.challenge.scheme_len))
	}
	if result.challenge.realm != nil {
		challenge.Realm = C.GoStringN(result.challenge.realm, C.int(result.challenge.realm_len))
	}
	if result.challenge.service != nil {
		challenge.Service = C.GoStringN(result.challenge.service, C.int(result.challenge.service_len))
	}
	if result.challenge.scope != nil {
		challenge.Scope = C.GoStringN(result.challenge.scope, C.int(result.challenge.scope_len))
	}

	return challenge, nil
}
