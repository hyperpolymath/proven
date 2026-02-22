! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! proven.safe-math - Overflow-checked arithmetic via libproven FFI.
! All computation is performed in verified Idris 2 code via the Zig FFI
! layer. This vocabulary is a thin wrapper; it does NOT reimplement any logic.

USING: kernel math proven.ffi accessors ;
IN: proven.safe-math

! Helper: extract value from IntResult, push value and t, or f on error.
! ( result -- value t ) or ( result -- f )
: int-result>value ( result -- value/f ? )
    dup status>> PROVEN-OK = [
        value>> t
    ] [
        drop f f
    ] if ;

! Checked addition with overflow detection via libproven.
! ( a b -- value t ) or ( a b -- f f )
: safe+ ( a b -- value/f ? )
    proven_math_add_checked int-result>value ;

! Checked subtraction with underflow detection via libproven.
! ( a b -- value t ) or ( a b -- f f )
: safe- ( a b -- value/f ? )
    proven_math_sub_checked int-result>value ;

! Checked multiplication with overflow detection via libproven.
! ( a b -- value t ) or ( a b -- f f )
: safe* ( a b -- value/f ? )
    proven_math_mul_checked int-result>value ;

! Safe integer division via libproven.
! ( a b -- value t ) or ( a b -- f f )
: safe/ ( a b -- value/f ? )
    proven_math_div int-result>value ;

! Safe modulo via libproven.
! ( a b -- value t ) or ( a b -- f f )
: safe-mod ( a b -- value/f ? )
    proven_math_mod int-result>value ;

! Safe absolute value via libproven.
! ( n -- value t ) or ( n -- f f )
: safe-abs ( n -- value/f ? )
    proven_math_abs_safe int-result>value ;

! Clamp value to [lo, hi] range via libproven.
! ( lo hi value -- clamped )
: safe-clamp ( lo hi value -- clamped )
    proven_math_clamp ;

! Checked exponentiation via libproven.
! ( base exp -- value t ) or ( base exp -- f f )
: safe-pow ( base exp -- value/f ? )
    proven_math_pow_checked int-result>value ;
