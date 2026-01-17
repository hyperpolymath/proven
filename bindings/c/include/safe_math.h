/* SPDX-License-Identifier: PMPL-1.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_math.h
 * @brief Arithmetic operations that cannot crash
 *
 * Provides safe integer arithmetic with overflow/underflow detection,
 * safe division (no division by zero), and related operations.
 */

#ifndef SAFE_MATH_H
#define SAFE_MATH_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Safe Division and Modulo
 * ============================================================================ */

/**
 * @brief Safe integer division
 * @param numerator Dividend
 * @param denominator Divisor
 * @return Result with status and quotient
 *
 * Returns PROVEN_ERR_DIVISION_BY_ZERO if denominator is 0.
 * Returns PROVEN_ERR_OVERFLOW for MIN_INT / -1.
 */
ProvenIntResult proven_math_div(int64_t numerator, int64_t denominator);

/**
 * @brief Safe modulo operation
 * @param numerator Dividend
 * @param denominator Divisor
 * @return Result with status and remainder
 *
 * Returns PROVEN_ERR_DIVISION_BY_ZERO if denominator is 0.
 */
ProvenIntResult proven_math_mod(int64_t numerator, int64_t denominator);

/* ============================================================================
 * Checked Arithmetic
 * ============================================================================ */

/**
 * @brief Checked addition with overflow detection
 * @param a First operand
 * @param b Second operand
 * @return Result with status and sum
 *
 * Returns PROVEN_ERR_OVERFLOW if result would overflow.
 */
ProvenIntResult proven_math_add_checked(int64_t a, int64_t b);

/**
 * @brief Checked subtraction with underflow detection
 * @param a First operand
 * @param b Second operand
 * @return Result with status and difference
 *
 * Returns PROVEN_ERR_UNDERFLOW if result would underflow.
 */
ProvenIntResult proven_math_sub_checked(int64_t a, int64_t b);

/**
 * @brief Checked multiplication with overflow detection
 * @param a First operand
 * @param b Second operand
 * @return Result with status and product
 *
 * Returns PROVEN_ERR_OVERFLOW if result would overflow.
 */
ProvenIntResult proven_math_mul_checked(int64_t a, int64_t b);

/**
 * @brief Integer exponentiation with overflow checking
 * @param base Base value
 * @param exp Exponent (must be non-negative)
 * @return Result with status and power
 *
 * Returns PROVEN_ERR_OVERFLOW if result would overflow.
 */
ProvenIntResult proven_math_pow_checked(int64_t base, uint32_t exp);

/* ============================================================================
 * Safe Operations
 * ============================================================================ */

/**
 * @brief Safe absolute value
 * @param n Integer value
 * @return Result with status and absolute value
 *
 * Returns PROVEN_ERR_OVERFLOW for MIN_INT (cannot be represented).
 */
ProvenIntResult proven_math_abs_safe(int64_t n);

/**
 * @brief Clamp value to range
 * @param lo Lower bound (inclusive)
 * @param hi Upper bound (inclusive)
 * @param value Value to clamp
 * @return Clamped value
 */
int64_t proven_math_clamp(int64_t lo, int64_t hi, int64_t value);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_MATH_H */
