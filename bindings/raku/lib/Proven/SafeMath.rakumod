# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven::SafeMath - Safe arithmetic that cannot crash.
#
# Thin wrapper over libproven's SafeMath module.  All computation is performed
# in formally verified Idris 2 code via the Zig FFI bridge.  This module does
# NOT reimplement any arithmetic logic.
#
# Every operation returns Nil on error (overflow, underflow, division by zero)
# rather than throwing an exception -- the idiomatic Raku error-signalling
# pattern for optional results.

unit module Proven::SafeMath;

use NativeCall;
use Proven::LibProven;

# ============================================================================
# Checked integer arithmetic
# ============================================================================

#| Safe addition with overflow detection.
#| Returns Nil on overflow.
sub safe-add(Int:D $a, Int:D $b --> Int) is export {
    my IntResult $r = proven_math_add_checked($a, $b);
    return Nil unless $r.status == 0;
    return $r.value;
}

#| Safe subtraction with underflow detection.
#| Returns Nil on underflow.
sub safe-sub(Int:D $a, Int:D $b --> Int) is export {
    my IntResult $r = proven_math_sub_checked($a, $b);
    return Nil unless $r.status == 0;
    return $r.value;
}

#| Safe multiplication with overflow detection.
#| Returns Nil on overflow.
sub safe-mul(Int:D $a, Int:D $b --> Int) is export {
    my IntResult $r = proven_math_mul_checked($a, $b);
    return Nil unless $r.status == 0;
    return $r.value;
}

#| Safe division.
#| Returns Nil on division by zero or INT64_MIN / -1 overflow.
sub safe-div(Int:D $a, Int:D $b --> Int) is export {
    my IntResult $r = proven_math_div($a, $b);
    return Nil unless $r.status == 0;
    return $r.value;
}

#| Safe modulo.
#| Returns Nil on division by zero.
sub safe-mod(Int:D $a, Int:D $b --> Int) is export {
    my IntResult $r = proven_math_mod($a, $b);
    return Nil unless $r.status == 0;
    return $r.value;
}

#| Safe absolute value.
#| Returns Nil for INT64_MIN (cannot be represented as positive int64).
sub safe-abs(Int:D $n --> Int) is export {
    my IntResult $r = proven_math_abs_safe($n);
    return Nil unless $r.status == 0;
    return $r.value;
}

#| Clamp an integer to [lo, hi].
sub safe-clamp(Int:D $lo, Int:D $hi, Int:D $value --> Int) is export {
    return proven_math_clamp($lo, $hi, $value);
}

#| Safe integer exponentiation with overflow checking.
#| Returns Nil on overflow or if exponent is negative.
sub safe-pow(Int:D $base, Int:D $exp --> Int) is export {
    my IntResult $r = proven_math_pow_checked($base, $exp);
    return Nil unless $r.status == 0;
    return $r.value;
}
