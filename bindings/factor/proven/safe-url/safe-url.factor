! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! proven.safe-url - URL encoding/decoding via libproven FFI.
! All computation is performed in verified Idris 2 code via the Zig FFI
! layer. This vocabulary is a thin wrapper; it does NOT reimplement any logic.

USING: kernel sequences proven.ffi accessors ;
IN: proven.safe-url

! URL-encode a string (RFC 3986 percent encoding) via libproven.
! Unreserved chars (A-Za-z0-9-._~) pass through; others become %XX.
! ( string -- encoded t ) or ( string -- f f )
: url-encode ( string -- encoded/f ? )
    dup length proven_http_url_encode
    dup status>> PROVEN-OK = [
        value>> t
    ] [
        drop f f
    ] if ;

! URL-decode a percent-encoded string via libproven.
! ( string -- decoded t ) or ( string -- f f )
: url-decode ( string -- decoded/f ? )
    dup length proven_http_url_decode
    dup status>> PROVEN-OK = [
        value>> t
    ] [
        drop f f
    ] if ;
