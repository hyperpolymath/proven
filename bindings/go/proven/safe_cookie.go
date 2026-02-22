// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeCookie provides HTTP cookie operations that prevent injection attacks.
// All computation is performed in Idris 2 via the Proven FFI.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// SameSite values for cookie attributes.
const (
	SameSiteStrict = 0
	SameSiteLax    = 1
	SameSiteNone   = 2
)

// CookieAttrs holds the attributes for building a Set-Cookie header.
type CookieAttrs struct {
	Domain      string
	Path        string
	MaxAge      int64 // -1 means not set
	Secure      bool
	HttpOnly    bool
	SameSite    int32
	Partitioned bool
}

// CookieHasInjection checks for cookie injection characters (semicolon, CR, LF).
func CookieHasInjection(value string) (bool, error) {
	cs, length := cString(value)
	defer unsafeFree(cs)
	result := C.proven_cookie_has_injection(cs, length)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// CookieValidateName validates a cookie name per RFC 6265.
func CookieValidateName(name string) (bool, error) {
	cs, length := cString(name)
	defer unsafeFree(cs)
	result := C.proven_cookie_validate_name(cs, length)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// CookieValidateValue validates a cookie value per RFC 6265.
func CookieValidateValue(value string) (bool, error) {
	cs, length := cString(value)
	defer unsafeFree(cs)
	result := C.proven_cookie_validate_value(cs, length)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// CookieGetPrefix returns the cookie prefix type: 0=none, 1=__Secure-, 2=__Host-.
func CookieGetPrefix(name string) (int64, error) {
	cs, length := cString(name)
	defer unsafeFree(cs)
	result := C.proven_cookie_get_prefix(cs, length)
	if int(result.status) != StatusOK {
		return 0, newError(int(result.status))
	}
	return int64(result.value), nil
}

// CookieBuildSetCookie builds a Set-Cookie header value with the given attributes.
func CookieBuildSetCookie(name, value string, attrs CookieAttrs) (string, error) {
	nameCS, nameLen := cString(name)
	defer unsafeFree(nameCS)
	valueCS, valueLen := cString(value)
	defer unsafeFree(valueCS)

	cAttrs := C.CookieAttributes{
		max_age:     C.int64_t(attrs.MaxAge),
		secure:      C._Bool(attrs.Secure),
		http_only:   C._Bool(attrs.HttpOnly),
		same_site:   C.int32_t(attrs.SameSite),
		partitioned: C._Bool(attrs.Partitioned),
	}

	if attrs.Domain != "" {
		domainCS := C.CString(attrs.Domain)
		defer unsafeFree(domainCS)
		cAttrs.domain = domainCS
		cAttrs.domain_len = C.size_t(len(attrs.Domain))
	}
	if attrs.Path != "" {
		pathCS := C.CString(attrs.Path)
		defer unsafeFree(pathCS)
		cAttrs.path = pathCS
		cAttrs.path_len = C.size_t(len(attrs.Path))
	}

	return goStringResult(C.proven_cookie_build_set_cookie(nameCS, nameLen, valueCS, valueLen, cAttrs))
}

// CookieBuildDelete builds a Set-Cookie header value that deletes a cookie.
func CookieBuildDelete(name string) (string, error) {
	cs, length := cString(name)
	defer unsafeFree(cs)
	return goStringResult(C.proven_cookie_build_delete(cs, length))
}
