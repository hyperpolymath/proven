Red [
    Title:       "Proven SafeMath"
    Description: "Safe arithmetic operations via libproven FFI"
    Author:      "Jonathan D.A. Jewell (hyperpolymath)"
    Email:       "jonathan.jewell@open.ac.uk"
    License:     "PMPL-1.0-or-later"
    Version:     0.5.0
    File:        %safe-math.red
    Needs:       'proven
    Notes: {
        SPDX-License-Identifier: PMPL-1.0-or-later
        Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

        Safe arithmetic operations that cannot crash.
        ALL computation is performed in Idris 2 via the Zig FFI bridge.
        This file ONLY wraps FFI calls. It does NOT reimplement any math.
    }
]

#include %proven.red

; ============================================================================
; SafeMath Module
; ============================================================================

safe-math: context [

    ; Safe addition with overflow detection.
    ;
    ; Returns the sum as an integer, or none if overflow would occur.
    add: func [a [integer!] b [integer!]] [
        result: proven_math_add_checked a b
        either succeeded? result/status [result/value][none]
    ]

    ; Safe subtraction with underflow detection.
    ;
    ; Returns the difference as an integer, or none if underflow would occur.
    sub: func [a [integer!] b [integer!]] [
        result: proven_math_sub_checked a b
        either succeeded? result/status [result/value][none]
    ]

    ; Safe multiplication with overflow detection.
    ;
    ; Returns the product as an integer, or none if overflow would occur.
    mul: func [a [integer!] b [integer!]] [
        result: proven_math_mul_checked a b
        either succeeded? result/status [result/value][none]
    ]

    ; Safe division with zero-check.
    ;
    ; Returns the quotient as an integer, or none if denominator is 0
    ; or if INT64_MIN / -1 would overflow.
    div: func [numerator [integer!] denominator [integer!]] [
        result: proven_math_div numerator denominator
        either succeeded? result/status [result/value][none]
    ]

    ; Safe modulo with zero-check.
    ;
    ; Returns the remainder as an integer, or none if denominator is 0.
    mod: func [numerator [integer!] denominator [integer!]] [
        result: proven_math_mod numerator denominator
        either succeeded? result/status [result/value][none]
    ]

    ; Safe absolute value.
    ;
    ; Returns the absolute value, or none for the minimum i64 value
    ; (whose absolute value cannot be represented as a positive i64).
    abs: func [n [integer!]] [
        result: proven_math_abs_safe n
        either succeeded? result/status [result/value][none]
    ]

    ; Clamp value to range [lo, hi].
    ;
    ; Always succeeds. Returns lo if value < lo, hi if value > hi.
    clamp: func [lo [integer!] hi [integer!] value [integer!]] [
        proven_math_clamp lo hi value
    ]

    ; Integer power with overflow checking.
    ;
    ; Returns base^exp as an integer, or none if the result would overflow.
    pow: func [base [integer!] exp [integer!]] [
        result: proven_math_pow_checked base exp
        either succeeded? result/status [result/value][none]
    ]
]
