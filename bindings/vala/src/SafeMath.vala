// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeMath.vala -- Checked integer and floating-point arithmetic.
 *
 * Every method delegates to libproven via C extern calls declared in
 * lib_proven.vapi.  No arithmetic is reimplemented here.
 */
namespace Proven {

    /**
     * Safe integer arithmetic that cannot crash.
     *
     * All operations detect overflow, underflow, and division by zero at
     * runtime and return null instead of undefined behaviour.
     */
    public class SafeMath : GLib.Object {

        /**
         * Checked addition.  Returns null on overflow.
         *
         * @param a First operand.
         * @param b Second operand.
         * @return Sum, or null if the result would overflow int64.
         */
        public static int64? add (int64 a, int64 b) {
            LibProven.IntResult r = LibProven.math_add_checked (a, b);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Checked subtraction.  Returns null on underflow.
         *
         * @param a First operand.
         * @param b Second operand.
         * @return Difference, or null if the result would underflow int64.
         */
        public static int64? sub (int64 a, int64 b) {
            LibProven.IntResult r = LibProven.math_sub_checked (a, b);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Checked multiplication.  Returns null on overflow.
         *
         * @param a First operand.
         * @param b Second operand.
         * @return Product, or null if the result would overflow int64.
         */
        public static int64? mul (int64 a, int64 b) {
            LibProven.IntResult r = LibProven.math_mul_checked (a, b);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Safe integer division.  Returns null on division by zero or
         * INT64_MIN / -1 overflow.
         *
         * @param numerator   Dividend.
         * @param denominator Divisor.
         * @return Quotient, or null on error.
         */
        public static int64? div (int64 numerator, int64 denominator) {
            LibProven.IntResult r = LibProven.math_div (numerator, denominator);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Safe modulo.  Returns null on division by zero.
         *
         * @param numerator   Dividend.
         * @param denominator Divisor.
         * @return Remainder, or null on error.
         */
        public static int64? mod (int64 numerator, int64 denominator) {
            LibProven.IntResult r = LibProven.math_mod (numerator, denominator);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Safe absolute value.  Returns null for INT64_MIN (which cannot
         * be represented as a positive int64).
         *
         * @param n Value.
         * @return Absolute value, or null on overflow.
         */
        public static int64? abs (int64 n) {
            LibProven.IntResult r = LibProven.math_abs_safe (n);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Clamp an integer to a [lo, hi] range.
         *
         * This operation cannot fail.
         *
         * @param lo    Lower bound (inclusive).
         * @param hi    Upper bound (inclusive).
         * @param value Value to clamp.
         * @return Clamped value.
         */
        public static int64 clamp (int64 lo, int64 hi, int64 value) {
            return LibProven.math_clamp (lo, hi, value);
        }

        /**
         * Checked exponentiation.  Returns null on overflow.
         *
         * @param base_val Base value.
         * @param exp      Exponent (non-negative).
         * @return base_val raised to exp, or null on overflow.
         */
        public static int64? pow (int64 base_val, uint32 exp) {
            LibProven.IntResult r = LibProven.math_pow_checked (base_val, exp);
            if (r.status != 0) {
                return null;
            }
            return r.value;
        }

        /**
         * Get the status code from the last checked-add operation.
         *
         * Useful for distinguishing overflow from underflow.
         *
         * @param a First operand.
         * @param b Second operand.
         * @return Status code from libproven.
         */
        public static LibProven.Status add_status (int64 a, int64 b) {
            LibProven.IntResult r = LibProven.math_add_checked (a, b);
            return (LibProven.Status) r.status;
        }
    }
}
