# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven::SafeUrl - URL parsing, validation, and encoding.
#
# Thin wrapper over libproven's SafeUrl and SafeHttp URL-encoding modules.
# All URL parsing and encoding is performed in formally verified Idris 2 code.
# This module does NOT reimplement any URL logic.

unit module Proven::SafeUrl;

use NativeCall;
use Proven::LibProven;

# ============================================================================
# URL encoding (RFC 3986 percent-encoding)
# ============================================================================

#| URL-encode a string (RFC 3986 percent-encoding).
#| Unreserved characters (A-Za-z0-9-._~) pass through; others become %XX.
#| Returns the encoded string, or Nil on error.
sub url-encode(Str:D $s --> Str) is export {
    my ($buf, $len) = str-to-buf($s);
    my StringResult $r = proven_http_url_encode(nativecast(Pointer, $buf), $len);
    return extract-string($r);
}

#| URL-decode a percent-encoded string.
#| Returns the decoded string, or Nil on error.
sub url-decode(Str:D $s --> Str) is export {
    my ($buf, $len) = str-to-buf($s);
    my StringResult $r = proven_http_url_decode(nativecast(Pointer, $buf), $len);
    return extract-string($r);
}
