! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! proven.safe-json - JSON validation and type detection via libproven FFI.
! All computation is performed in verified Idris 2 code via the Zig FFI
! layer. This vocabulary is a thin wrapper; it does NOT reimplement any logic.

USING: kernel sequences proven.ffi accessors ;
IN: proven.safe-json

! JSON type constants (matching ProvenJsonType enum)
CONSTANT: JSON-NULL     0
CONSTANT: JSON-BOOL     1
CONSTANT: JSON-NUMBER   2
CONSTANT: JSON-STRING   3
CONSTANT: JSON-ARRAY    4
CONSTANT: JSON-OBJECT   5
CONSTANT: JSON-INVALID -1

! Check if string is valid JSON via libproven.
! ( string -- bool t ) or ( string -- f f )
: json-valid? ( string -- bool/f ? )
    dup length proven_json_is_valid
    dup status>> PROVEN-OK = [
        value>> 0 = not t
    ] [
        drop f f
    ] if ;

! Get JSON root value type via libproven.
! Returns one of the JSON-* constants, or JSON-INVALID.
! ( string -- type )
: json-type ( string -- type )
    dup length proven_json_get_type ;
