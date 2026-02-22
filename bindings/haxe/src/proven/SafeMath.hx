// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeMath - Overflow-checked arithmetic operations via libproven FFI.
// All computation is performed in verified Idris 2 code. This module
// provides an idiomatic Haxe wrapper around the raw C FFI calls.

package proven;

import proven.LibProven;

/**
 * Safe arithmetic operations that detect overflow, underflow, and
 * division by zero instead of producing undefined behavior.
 *
 * All operations delegate to the formally verified Idris 2 core via
 * libproven. Returns `Null<Int64>` where `null` indicates an error
 * (overflow, division by zero, etc.).
 */
class SafeMath {
    /**
     * Checked addition with overflow detection.
     * @param a First operand
     * @param b Second operand
     * @return Sum, or null if overflow would occur
     */
    public static function addChecked(a:haxe.Int64, b:haxe.Int64):Null<haxe.Int64> {
        var r = LibProven.mathAddChecked(a, b);
        if (r.status != ProvenStatus.OK) return null;
        return r.value;
    }

    /**
     * Checked subtraction with underflow detection.
     * @param a First operand
     * @param b Second operand
     * @return Difference, or null if underflow would occur
     */
    public static function subChecked(a:haxe.Int64, b:haxe.Int64):Null<haxe.Int64> {
        var r = LibProven.mathSubChecked(a, b);
        if (r.status != ProvenStatus.OK) return null;
        return r.value;
    }

    /**
     * Checked multiplication with overflow detection.
     * @param a First operand
     * @param b Second operand
     * @return Product, or null if overflow would occur
     */
    public static function mulChecked(a:haxe.Int64, b:haxe.Int64):Null<haxe.Int64> {
        var r = LibProven.mathMulChecked(a, b);
        if (r.status != ProvenStatus.OK) return null;
        return r.value;
    }

    /**
     * Safe integer division.
     * @param a Numerator
     * @param b Denominator
     * @return Quotient, or null on division by zero or INT64_MIN / -1
     */
    public static function div(a:haxe.Int64, b:haxe.Int64):Null<haxe.Int64> {
        var r = LibProven.mathDiv(a, b);
        if (r.status != ProvenStatus.OK) return null;
        return r.value;
    }

    /**
     * Safe modulo operation.
     * @param a Numerator
     * @param b Denominator
     * @return Remainder, or null on division by zero
     */
    public static function mod(a:haxe.Int64, b:haxe.Int64):Null<haxe.Int64> {
        var r = LibProven.mathMod(a, b);
        if (r.status != ProvenStatus.OK) return null;
        return r.value;
    }

    /**
     * Safe absolute value.
     * @param n Input value
     * @return Absolute value, or null for INT64_MIN
     */
    public static function absSafe(n:haxe.Int64):Null<haxe.Int64> {
        var r = LibProven.mathAbsSafe(n);
        if (r.status != ProvenStatus.OK) return null;
        return r.value;
    }

    /**
     * Clamp value to [lo, hi] range.
     * @param lo Lower bound (inclusive)
     * @param hi Upper bound (inclusive)
     * @param value Value to clamp
     * @return Clamped value
     */
    public static function clamp(lo:haxe.Int64, hi:haxe.Int64, value:haxe.Int64):haxe.Int64 {
        return LibProven.mathClamp(lo, hi, value);
    }

    /**
     * Checked integer exponentiation.
     * @param base Base value
     * @param exp Exponent (non-negative)
     * @return Result, or null if overflow would occur
     */
    public static function powChecked(base:haxe.Int64, exp:Int):Null<haxe.Int64> {
        var r = LibProven.mathPowChecked(base, exp);
        if (r.status != ProvenStatus.OK) return null;
        return r.value;
    }
}
