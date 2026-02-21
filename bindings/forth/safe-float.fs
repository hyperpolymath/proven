\ SPDX-License-Identifier: PMPL-1.0-or-later
\ SPDX-FileCopyrightText: 2025 Hyperpolymath

\ SAFE-FLOAT - Safe floating-point operations for Forth
\ Stack-based FP operations with NaN/Inf checking

\ Note: This module uses fixed-point arithmetic for portability
\ across Forth implementations. Values are scaled by FLOAT-SCALE.

\ ============================================================
\ Fixed-Point Configuration
\ ============================================================

\ Scale factor: 1000000 = 6 decimal places
1000000 constant FLOAT-SCALE
3 constant FLOAT-DECIMALS-DISPLAY

\ Special values (encoded as large integers)
2147483647 constant FP-INFINITY
-2147483648 constant FP-NEG-INFINITY
-2147483647 constant FP-NAN

\ ============================================================
\ Fixed-Point Conversion
\ ============================================================

\ Convert integer to fixed-point
\ ( n -- fp )
: int>fp ( n -- fp )
    FLOAT-SCALE *
;

\ Convert fixed-point to integer (truncate)
\ ( fp -- n )
: fp>int ( fp -- n )
    FLOAT-SCALE /
;

\ Create fixed-point from integer and fractional parts
\ ( int frac -- fp )
: make-fp ( int frac -- fp )
    swap FLOAT-SCALE * +
;

\ ============================================================
\ Safe Value Checks
\ ============================================================

\ Check if value is NaN
\ ( fp -- flag )
: fp-nan? ( fp -- flag )
    FP-NAN =
;

\ Check if value is positive infinity
\ ( fp -- flag )
: fp-inf? ( fp -- flag )
    FP-INFINITY =
;

\ Check if value is negative infinity
\ ( fp -- flag )
: fp-neg-inf? ( fp -- flag )
    FP-NEG-INFINITY =
;

\ Check if value is any infinity
\ ( fp -- flag )
: fp-infinite? ( fp -- flag )
    dup fp-inf? swap fp-neg-inf? or
;

\ Check if value is finite (not NaN or Inf)
\ ( fp -- flag )
: fp-finite? ( fp -- flag )
    dup fp-nan? swap fp-infinite? or invert
;

\ Check if value is zero
\ ( fp -- flag )
: fp-zero? ( fp -- flag )
    0=
;

\ Check if value is positive
\ ( fp -- flag )
: fp-positive? ( fp -- flag )
    0>
;

\ Check if value is negative
\ ( fp -- flag )
: fp-negative? ( fp -- flag )
    0<
;

\ ============================================================
\ Safe Arithmetic
\ ============================================================

\ Safe fixed-point addition
\ ( fp1 fp2 -- fp3 )
: fp+ ( fp1 fp2 -- fp3 )
    over fp-nan? if drop exit then
    dup fp-nan? if nip exit then
    over fp-infinite? over fp-infinite? and if
        \ Inf + (-Inf) = NaN
        2dup xor 0< if
            2drop FP-NAN exit
        then
    then
    over fp-infinite? if nip exit then
    dup fp-infinite? if nip exit then
    +
;

\ Safe fixed-point subtraction
\ ( fp1 fp2 -- fp3 )
: fp- ( fp1 fp2 -- fp3 )
    over fp-nan? if drop exit then
    dup fp-nan? if nip exit then
    over fp-infinite? over fp-infinite? and if
        \ Inf - Inf = NaN
        2dup = if
            2drop FP-NAN exit
        then
    then
    over fp-infinite? if nip exit then
    dup fp-infinite? if negate nip exit then
    -
;

\ Safe fixed-point multiplication
\ ( fp1 fp2 -- fp3 )
: fp* ( fp1 fp2 -- fp3 )
    over fp-nan? if drop exit then
    dup fp-nan? if nip exit then
    over fp-infinite? if
        over fp-zero? if 2drop FP-NAN exit then
        2dup xor 0< if 2drop FP-NEG-INFINITY else 2drop FP-INFINITY then
        exit
    then
    dup fp-infinite? if
        over fp-zero? if 2drop FP-NAN exit then
        2dup xor 0< if 2drop FP-NEG-INFINITY else 2drop FP-INFINITY then
        exit
    then
    FLOAT-SCALE */
;

\ Safe fixed-point division
\ ( fp1 fp2 -- fp3 )
: fp/ ( fp1 fp2 -- fp3 )
    over fp-nan? if drop exit then
    dup fp-nan? if nip exit then
    dup fp-zero? if
        drop
        dup fp-zero? if drop FP-NAN exit then
        dup fp-positive? if drop FP-INFINITY else drop FP-NEG-INFINITY then
        exit
    then
    over fp-infinite? dup fp-infinite? and if
        2drop FP-NAN exit
    then
    over fp-infinite? if nip exit then
    dup fp-infinite? if 2drop 0 exit then
    swap FLOAT-SCALE swap */
;

\ Safe absolute value
\ ( fp -- fp )
: fp-abs ( fp -- fp )
    dup fp-nan? if exit then
    dup fp-neg-inf? if drop FP-INFINITY exit then
    abs
;

\ Safe negation
\ ( fp -- fp )
: fp-negate ( fp -- fp )
    dup fp-nan? if exit then
    dup fp-inf? if drop FP-NEG-INFINITY exit then
    dup fp-neg-inf? if drop FP-INFINITY exit then
    negate
;

\ ============================================================
\ Comparison
\ ============================================================

\ Safe equality (NaN != NaN)
\ ( fp1 fp2 -- flag )
: fp= ( fp1 fp2 -- flag )
    over fp-nan? if 2drop false exit then
    dup fp-nan? if 2drop false exit then
    =
;

\ Safe less than
\ ( fp1 fp2 -- flag )
: fp< ( fp1 fp2 -- flag )
    over fp-nan? if 2drop false exit then
    dup fp-nan? if 2drop false exit then
    <
;

\ Safe greater than
\ ( fp1 fp2 -- flag )
: fp> ( fp1 fp2 -- flag )
    over fp-nan? if 2drop false exit then
    dup fp-nan? if 2drop false exit then
    >
;

\ Safe less than or equal
\ ( fp1 fp2 -- flag )
: fp<= ( fp1 fp2 -- flag )
    over fp-nan? if 2drop false exit then
    dup fp-nan? if 2drop false exit then
    <=
;

\ Safe greater than or equal
\ ( fp1 fp2 -- flag )
: fp>= ( fp1 fp2 -- flag )
    over fp-nan? if 2drop false exit then
    dup fp-nan? if 2drop false exit then
    >=
;

\ ============================================================
\ Rounding
\ ============================================================

\ Round to nearest integer (in fixed-point)
\ ( fp -- fp )
: fp-round ( fp -- fp )
    dup fp-finite? 0= if exit then
    dup 0< if
        FLOAT-SCALE 2 / - FLOAT-SCALE / FLOAT-SCALE *
    else
        FLOAT-SCALE 2 / + FLOAT-SCALE / FLOAT-SCALE *
    then
;

\ Floor (round toward negative infinity)
\ ( fp -- fp )
: fp-floor ( fp -- fp )
    dup fp-finite? 0= if exit then
    dup 0< if
        dup FLOAT-SCALE mod 0<> if
            FLOAT-SCALE / 1- FLOAT-SCALE *
        else
            FLOAT-SCALE / FLOAT-SCALE *
        then
    else
        FLOAT-SCALE / FLOAT-SCALE *
    then
;

\ Ceiling (round toward positive infinity)
\ ( fp -- fp )
: fp-ceil ( fp -- fp )
    dup fp-finite? 0= if exit then
    dup 0> if
        dup FLOAT-SCALE mod 0<> if
            FLOAT-SCALE / 1+ FLOAT-SCALE *
        else
            FLOAT-SCALE / FLOAT-SCALE *
        then
    else
        FLOAT-SCALE / FLOAT-SCALE *
    then
;

\ ============================================================
\ Min/Max with NaN propagation
\ ============================================================

\ Minimum (NaN propagates)
\ ( fp1 fp2 -- fp )
: fp-min ( fp1 fp2 -- fp )
    over fp-nan? if drop exit then
    dup fp-nan? if nip exit then
    2dup < if drop else nip then
;

\ Maximum (NaN propagates)
\ ( fp1 fp2 -- fp )
: fp-max ( fp1 fp2 -- fp )
    over fp-nan? if drop exit then
    dup fp-nan? if nip exit then
    2dup > if drop else nip then
;

\ ============================================================
\ Display
\ ============================================================

32 constant FP-OUT-MAX
create fp-out-buffer FP-OUT-MAX allot
variable fp-out-pos

: fp-out-reset ( -- ) 0 fp-out-pos ! ;
: fp-out-emit ( c -- ) fp-out-buffer fp-out-pos @ + c! 1 fp-out-pos +! ;
: fp-out-result ( -- addr len ) fp-out-buffer fp-out-pos @ ;

\ Format fixed-point number
\ ( fp -- addr len )
: fp-format ( fp -- addr len )
    fp-out-reset
    dup FP-NAN = if drop s" NaN" exit then
    dup FP-INFINITY = if drop s" Inf" exit then
    dup FP-NEG-INFINITY = if drop s" -Inf" exit then
    dup 0< if [char] - fp-out-emit negate then
    dup FLOAT-SCALE /mod        \ frac int
    \ Emit integer part
    dup 0= if
        drop [char] 0 fp-out-emit
    else
        0 swap                  \ count int
        begin dup 0> while
            10 /mod swap        \ count ... digit int
            [char] 0 + swap
            rot 1+ -rot
        repeat
        drop                    \ drop zero
        0 ?do fp-out-emit loop
    then
    [char] . fp-out-emit
    \ Emit fractional part (6 digits)
    FLOAT-SCALE 10 / 0 ?do
        dup 10 /mod swap
        [char] 0 + fp-out-emit
        swap
    loop
    drop
    fp-out-result
;

\ ============================================================
\ Module Info
\ ============================================================

: safe-float-version ( -- ) ." SAFE-FLOAT for Forth v0.4.0" cr ;

: safe-float-help ( -- )
    cr
    ." SAFE-FLOAT - Safe Floating-Point Operations" cr
    ." ============================================" cr
    cr
    ." Uses fixed-point (scale: " FLOAT-SCALE . ." )" cr
    cr
    ." Conversion:" cr
    ."   int>fp    ( n -- fp )" cr
    ."   fp>int    ( fp -- n )" cr
    ."   make-fp   ( int frac -- fp )" cr
    cr
    ." Checks:" cr
    ."   fp-nan? fp-inf? fp-neg-inf? fp-infinite? fp-finite?" cr
    ."   fp-zero? fp-positive? fp-negative?" cr
    cr
    ." Arithmetic:" cr
    ."   fp+ fp- fp* fp/ fp-abs fp-negate" cr
    cr
    ." Comparison:" cr
    ."   fp= fp< fp> fp<= fp>=" cr
    cr
    ." Rounding:" cr
    ."   fp-round fp-floor fp-ceil" cr
    cr
    ." Min/Max:" cr
    ."   fp-min fp-max" cr
    cr
    ." Display:" cr
    ."   fp-format ( fp -- addr len )" cr
    cr
;
