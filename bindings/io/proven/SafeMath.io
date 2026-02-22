# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeMath.io - Overflow-safe integer arithmetic for the Io language.
#
# All operations delegate to libproven via the native C addon (IoProven.c).
# Returns nil on error (overflow, division by zero, etc.).
#
# Usage:
#   Proven init
#   result := Proven safeAdd(1000000000, 2000000000)
#   if(result isNil, "Overflow!" println, result println)
#   Proven deinit

SafeMath := Proven clone do(
    //doc SafeMath category Safety
    //doc SafeMath description Overflow-checked integer arithmetic.

    //doc SafeMath safeAdd(a, b) Checked addition. Returns nil on overflow.
    # safeAdd is provided by the native C addon (IoProven.c).

    //doc SafeMath safeSub(a, b) Checked subtraction. Returns nil on underflow.
    # safeSub is provided by the native C addon (IoProven.c).

    //doc SafeMath safeMul(a, b) Checked multiplication. Returns nil on overflow.
    # safeMul is provided by the native C addon (IoProven.c).

    //doc SafeMath safeDiv(a, b) Safe division. Returns nil on division by zero or overflow.
    # safeDiv is provided by the native C addon (IoProven.c).

    //doc SafeMath safeMod(a, b) Safe modulo. Returns nil on division by zero.
    # safeMod is provided by the native C addon (IoProven.c).

    //doc SafeMath safeAbs(n) Safe absolute value. Returns nil for INT64_MIN.
    # safeAbs is provided by the native C addon (IoProven.c).

    //doc SafeMath clamp(lo, hi, value) Clamp value to [lo, hi] range.
    # clamp is provided by the native C addon (IoProven.c).

    //doc SafeMath safePow(base, exp) Checked exponentiation. Returns nil on overflow.
    # safePow is provided by the native C addon (IoProven.c).
)
