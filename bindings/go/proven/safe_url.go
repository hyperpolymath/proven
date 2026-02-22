// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeUrl provides URL parsing and validation via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// ParsedURL represents the parsed components of a URL.
type ParsedURL struct {
	Scheme   string
	Host     string
	Port     uint16
	HasPort  bool
	Path     string
	Query    string
	Fragment string
}

// ParseURL parses a URL string into its components via FFI.
// Returns nil and an error if parsing fails.
func ParseURL(urlString string) (*ParsedURL, error) {
	cs, length := cString(urlString)
	defer unsafeFree(cs)

	result := C.proven_url_parse(cs, length)
	status := int(result.status)
	if status != StatusOK {
		return nil, newError(status)
	}

	parsed := &ParsedURL{
		Port:    uint16(result.components.port),
		HasPort: bool(result.components.has_port),
	}

	if result.components.scheme != nil {
		parsed.Scheme = C.GoStringN(result.components.scheme, C.int(result.components.scheme_len))
	}
	if result.components.host != nil {
		parsed.Host = C.GoStringN(result.components.host, C.int(result.components.host_len))
	}
	if result.components.path != nil {
		parsed.Path = C.GoStringN(result.components.path, C.int(result.components.path_len))
	}
	if result.components.query != nil {
		parsed.Query = C.GoStringN(result.components.query, C.int(result.components.query_len))
	}
	if result.components.fragment != nil {
		parsed.Fragment = C.GoStringN(result.components.fragment, C.int(result.components.fragment_len))
	}

	// Free the C-allocated URL components.
	C.proven_url_free(&result.components)

	return parsed, nil
}
