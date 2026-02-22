\ SPDX-License-Identifier: PMPL-1.0-or-later
\ Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
\
\ Proven SafeJSON - FFI bindings to libproven JSON validation.
\ All JSON validation is performed in verified Idris 2 code via libproven.

c-library proven_json
s" proven" add-lib

\ ProvenBoolResult is { int32_t status; bool value; }
c-function proven-json-valid  proven_json_is_valid a n -- a
c-function proven-json-type   proven_json_get_type a n -- n

end-c-library

\ JSON type constants (matching ProvenJsonType enum)
0 constant JSON-NULL
1 constant JSON-BOOL
2 constant JSON-NUMBER
3 constant JSON-STRING
4 constant JSON-ARRAY
5 constant JSON-OBJECT
-1 constant JSON-INVALID

\ Check if string is valid JSON via libproven
\ ( c-addr len -- flag )
: json-valid? ( c-addr len -- flag )
    proven-json-valid ;

\ Get JSON value type at root level via libproven
\ ( c-addr len -- type )
: json-type ( c-addr len -- type )
    proven-json-type ;
