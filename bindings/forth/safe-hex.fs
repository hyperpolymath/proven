\ SPDX-License-Identifier: PMPL-1.0-or-later
\ Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
\
\ Proven SafeHex - FFI bindings to libproven hex operations.
\ All hex encoding/decoding is performed in verified Idris 2 code via libproven.

c-library proven_hex
s" proven" add-lib

\ ProvenStringResult is { int32_t status; char* value; size_t length; }
c-function proven-hex-encode  proven_hex_encode  a n n -- a
c-function proven-hex-decode  proven_hex_decode  a n -- a
c-function proven-free-string proven_free_string a -- void

end-c-library

\ Encode bytes to lowercase hex string via libproven
\ ( c-addr len -- c-addr' len' status )
: hex-encode ( c-addr len -- c-addr' len' status )
    0 \ uppercase=false
    proven-hex-encode ;

\ Encode bytes to uppercase hex string via libproven
\ ( c-addr len -- c-addr' len' status )
: hex-encode-upper ( c-addr len -- c-addr' len' status )
    1 \ uppercase=true
    proven-hex-encode ;

\ Decode hex string to bytes via libproven
\ ( c-addr len -- c-addr' len' status )
: hex-decode ( c-addr len -- c-addr' len' status )
    proven-hex-decode ;
