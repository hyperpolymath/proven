# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeUrl.io - URL parsing and encoding for the Io language.
#
# All operations delegate to libproven via the native C addon (IoProven.c).
# Returns nil on error.
#
# Usage:
#   Proven init
#   parts := Proven parseUrl("https://example.com:8080/path?q=1#frag")
#   if(parts isNil not,
#       parts host println   # "example.com"
#       parts port println   # 8080
#   )
#   Proven deinit

SafeUrl := Proven clone do(
    //doc SafeUrl category Safety
    //doc SafeUrl description URL parsing, validation, and percent-encoding.

    //doc SafeUrl parseUrl(url) Parse a URL into an object with scheme, host, port, path, query, fragment slots.
    # parseUrl is provided by the native C addon (IoProven.c).

    //doc SafeUrl urlEncode(str) RFC 3986 percent-encode a string.
    # urlEncode is provided by the native C addon (IoProven.c).

    //doc SafeUrl urlDecode(str) Decode a percent-encoded string.
    # urlDecode is provided by the native C addon (IoProven.c).
)
