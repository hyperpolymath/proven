// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeMath - Overflow-safe integer arithmetic for Squirrel.
//
// All operations delegate to libproven via the native "proven" table.
// Returns null on error (overflow, division by zero, etc.).

/**
 * SafeMath - Overflow-checked integer arithmetic.
 *
 * Every method returns null when the operation would produce an invalid
 * result (overflow, underflow, division by zero). This prevents silent
 * wraparound bugs common in game scripting.
 *
 * @example
 *   local math = SafeMath();
 *   local sum = math.add(2147483647, 1);  // null (overflow)
 *   local div = math.div(10, 0);          // null (division by zero)
 *   local ok  = math.add(40, 2);          // 42
 */
class SafeMath {
    /**
     * Checked addition.
     * @param {integer} a - First operand.
     * @param {integer} b - Second operand.
     * @return {integer|null} Sum, or null on overflow.
     */
    function add(a, b) {
        return proven.safe_add(a, b);
    }

    /**
     * Checked subtraction.
     * @param {integer} a - First operand.
     * @param {integer} b - Second operand.
     * @return {integer|null} Difference, or null on underflow.
     */
    function sub(a, b) {
        return proven.safe_sub(a, b);
    }

    /**
     * Checked multiplication.
     * @param {integer} a - First operand.
     * @param {integer} b - Second operand.
     * @return {integer|null} Product, or null on overflow.
     */
    function mul(a, b) {
        return proven.safe_mul(a, b);
    }

    /**
     * Safe integer division.
     * @param {integer} a - Dividend.
     * @param {integer} b - Divisor.
     * @return {integer|null} Quotient, or null on division by zero or overflow.
     */
    function div(a, b) {
        return proven.safe_div(a, b);
    }

    /**
     * Safe modulo operation.
     * @param {integer} a - Dividend.
     * @param {integer} b - Divisor.
     * @return {integer|null} Remainder, or null on division by zero.
     */
    function mod(a, b) {
        return proven.safe_mod(a, b);
    }

    /**
     * Safe absolute value.
     * @param {integer} n - Input value.
     * @return {integer|null} Absolute value, or null for INT64_MIN.
     */
    function abs(n) {
        return proven.safe_abs(n);
    }

    /**
     * Clamp value to [lo, hi] range.
     * @param {integer} lo - Lower bound.
     * @param {integer} hi - Upper bound.
     * @param {integer} value - Value to clamp.
     * @return {integer} Clamped value.
     */
    function clamp(lo, hi, value) {
        return proven.safe_clamp(lo, hi, value);
    }

    /**
     * Checked integer exponentiation.
     * @param {integer} base - Base value.
     * @param {integer} exp - Exponent (must be non-negative).
     * @return {integer|null} Result, or null on overflow.
     */
    function pow(base, exp) {
        return proven.safe_pow(base, exp);
    }
}
