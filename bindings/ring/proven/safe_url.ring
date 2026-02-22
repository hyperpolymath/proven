# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe URL parsing and validation.
# Thin wrapper over libproven FFI -- all logic lives in Idris 2.
# Returns NULL on error.

load "proven/lib_proven.ring"

# Parse a URL into components. Returns a list with the parsed fields
# or NULL on invalid URL.
# The returned list format is:
#   [scheme, host, port, path, query, fragment]
# where port is a number (0 if not present), and query/fragment may be "".
func parse_url value
    if len(value) = 0 return NULL ok
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_url_parse", "pi", value, len(value))
    if res = NULL return NULL ok
    # URL parsing requires struct field access; return the raw result
    # for manual unpacking by the caller.
    return res

# Validate a URL (returns 1 if parseable, 0 otherwise).
func is_valid_url value
    if len(value) = 0 return 0 ok
    lib = proven_lib()
    if lib = NULL return 0 ok
    res = calldll(lib, "proven_url_parse", "pi", value, len(value))
    if res = NULL return 0 ok
    # If we got a result, the URL is valid; free URL components.
    calldll(lib, "proven_url_free", "p", res)
    return 1

# Validate an IPv4 address string.
# Returns 1 if valid, 0 otherwise.
func is_valid_ipv4 value
    if len(value) = 0 return 0 ok
    lib = proven_lib()
    if lib = NULL return 0 ok
    res = calldll(lib, "proven_network_parse_ipv4", "pi", value, len(value))
    if isNumber(res) and res = PROVEN_OK
        return 1
    ok
    return 0
