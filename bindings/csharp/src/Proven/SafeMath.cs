// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeMath.cs - Overflow-checked arithmetic operations.
//
// Thin P/Invoke wrapper over libproven. ALL computation is performed in
// verified Idris 2 code via the Zig FFI bridge. No logic is reimplemented here.

namespace Proven
{
    /// <summary>
    /// Safe arithmetic operations with overflow, underflow, and division-by-zero
    /// detection. Every method calls through to the formally verified Idris 2
    /// implementation via the Zig FFI bridge. Returns null on error.
    /// </summary>
    public static class SafeMath
    {
        /// <summary>
        /// Checked addition with overflow detection.
        /// Delegates to proven_math_add_checked via FFI.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <returns>The sum, or null on overflow.</returns>
        public static long? Add(long a, long b)
        {
            return MarshalHelpers.IntResultToNullable(
                LibProven.proven_math_add_checked(a, b));
        }

        /// <summary>
        /// Checked subtraction with underflow detection.
        /// Delegates to proven_math_sub_checked via FFI.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <returns>The difference, or null on underflow.</returns>
        public static long? Sub(long a, long b)
        {
            return MarshalHelpers.IntResultToNullable(
                LibProven.proven_math_sub_checked(a, b));
        }

        /// <summary>
        /// Checked multiplication with overflow detection.
        /// Delegates to proven_math_mul_checked via FFI.
        /// </summary>
        /// <param name="a">First operand.</param>
        /// <param name="b">Second operand.</param>
        /// <returns>The product, or null on overflow.</returns>
        public static long? Mul(long a, long b)
        {
            return MarshalHelpers.IntResultToNullable(
                LibProven.proven_math_mul_checked(a, b));
        }

        /// <summary>
        /// Safe integer division with division-by-zero and overflow checks.
        /// Delegates to proven_math_div via FFI.
        /// </summary>
        /// <param name="numerator">Dividend.</param>
        /// <param name="denominator">Divisor.</param>
        /// <returns>The quotient, or null on division by zero or INT64_MIN / -1.</returns>
        public static long? Div(long numerator, long denominator)
        {
            return MarshalHelpers.IntResultToNullable(
                LibProven.proven_math_div(numerator, denominator));
        }

        /// <summary>
        /// Safe modulo operation.
        /// Delegates to proven_math_mod via FFI.
        /// </summary>
        /// <param name="numerator">Dividend.</param>
        /// <param name="denominator">Divisor.</param>
        /// <returns>The remainder, or null on division by zero.</returns>
        public static long? Mod(long numerator, long denominator)
        {
            return MarshalHelpers.IntResultToNullable(
                LibProven.proven_math_mod(numerator, denominator));
        }

        /// <summary>
        /// Safe absolute value (handles INT64_MIN correctly).
        /// Delegates to proven_math_abs_safe via FFI.
        /// </summary>
        /// <param name="n">Input value.</param>
        /// <returns>The absolute value, or null if n is INT64_MIN.</returns>
        public static long? Abs(long n)
        {
            return MarshalHelpers.IntResultToNullable(
                LibProven.proven_math_abs_safe(n));
        }

        /// <summary>
        /// Clamp value to [lo, hi] range.
        /// Delegates to proven_math_clamp via FFI.
        /// </summary>
        /// <param name="lo">Lower bound (inclusive).</param>
        /// <param name="hi">Upper bound (inclusive).</param>
        /// <param name="value">Value to clamp.</param>
        /// <returns>The clamped value.</returns>
        public static long Clamp(long lo, long hi, long value)
        {
            return LibProven.proven_math_clamp(lo, hi, value);
        }

        /// <summary>
        /// Integer exponentiation with overflow checking.
        /// Delegates to proven_math_pow_checked via FFI.
        /// </summary>
        /// <param name="baseValue">Base value.</param>
        /// <param name="exponent">Exponent (non-negative).</param>
        /// <returns>The result, or null on overflow.</returns>
        public static long? Pow(long baseValue, uint exponent)
        {
            return MarshalHelpers.IntResultToNullable(
                LibProven.proven_math_pow_checked(baseValue, exponent));
        }

        /// <summary>
        /// Safe floating-point division.
        /// Delegates to proven_float_div via FFI.
        /// </summary>
        /// <param name="a">Dividend.</param>
        /// <param name="b">Divisor.</param>
        /// <returns>The quotient, or null on division by zero or NaN.</returns>
        public static double? FloatDiv(double a, double b)
        {
            return MarshalHelpers.FloatResultToNullable(
                LibProven.proven_float_div(a, b));
        }

        /// <summary>
        /// Check if a floating-point value is finite (not NaN or Infinity).
        /// Delegates to proven_float_is_finite via FFI.
        /// </summary>
        /// <param name="x">Value to check.</param>
        /// <returns>true if finite.</returns>
        public static bool IsFinite(double x)
        {
            return LibProven.proven_float_is_finite(x);
        }

        /// <summary>
        /// Check if a floating-point value is NaN.
        /// Delegates to proven_float_is_nan via FFI.
        /// </summary>
        /// <param name="x">Value to check.</param>
        /// <returns>true if NaN.</returns>
        public static bool IsNaN(double x)
        {
            return LibProven.proven_float_is_nan(x);
        }

        /// <summary>
        /// Safe square root.
        /// Delegates to proven_float_sqrt via FFI.
        /// </summary>
        /// <param name="x">Value to take the square root of.</param>
        /// <returns>The square root, or null for negative or NaN input.</returns>
        public static double? Sqrt(double x)
        {
            return MarshalHelpers.FloatResultToNullable(
                LibProven.proven_float_sqrt(x));
        }

        /// <summary>
        /// Safe natural logarithm.
        /// Delegates to proven_float_ln via FFI.
        /// </summary>
        /// <param name="x">Value to take the natural log of.</param>
        /// <returns>The natural logarithm, or null for non-positive or NaN input.</returns>
        public static double? Ln(double x)
        {
            return MarshalHelpers.FloatResultToNullable(
                LibProven.proven_float_ln(x));
        }

        /// <summary>
        /// Evaluate a mathematical expression safely.
        /// Supports +, -, *, /, parentheses, negative and decimal numbers.
        /// Delegates to proven_calculator_eval via FFI.
        /// </summary>
        /// <param name="expression">The expression string to evaluate.</param>
        /// <returns>The result, or null on parse failure or division by zero.</returns>
        public static double? Evaluate(string expression)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(expression);
            return MarshalHelpers.FloatResultToNullable(
                LibProven.proven_calculator_eval(bytes, (nuint)bytes.Length));
        }
    }
}
