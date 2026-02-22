! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! proven.safe-crypto - Cryptographic primitives via libproven FFI.
! All computation is performed in verified Idris 2 code via the Zig FFI
! layer. This vocabulary is a thin wrapper; it does NOT reimplement any logic.

USING: kernel sequences proven.ffi accessors ;
IN: proven.safe-crypto

! Constant-time byte comparison (timing-attack safe) via libproven.
! Returns true if equal, false if different. Returns false if lengths differ.
! ( string1 string2 -- bool t ) or ( string1 string2 -- f f )
: constant-time= ( string1 string2 -- bool/f ? )
    [ dup length ] bi@ proven_crypto_constant_time_eq
    dup status>> PROVEN-OK = [
        value>> 0 = not t
    ] [
        drop f f
    ] if ;

! Hex-encode bytes via libproven.
! ( string uppercase? -- hex t ) or ( string uppercase? -- f f )
: hex-encode ( string uppercase? -- hex/f ? )
    [ dup length ] dip proven_hex_encode
    dup status>> PROVEN-OK = [
        value>> t
    ] [
        drop f f
    ] if ;
