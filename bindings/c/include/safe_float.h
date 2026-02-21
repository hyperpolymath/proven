/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_float.h
 * @brief Safe floating-point operations
 *
 * Provides safe floating-point arithmetic with proper handling
 * of NaN, infinity, and division by zero.
 */

#ifndef SAFE_FLOAT_H
#define SAFE_FLOAT_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Safe Arithmetic
 * ============================================================================ */

/**
 * @brief Safe floating-point division
 * @param a Numerator
 * @param b Denominator
 * @return Result with status and quotient
 *
 * Returns PROVEN_ERR_DIVISION_BY_ZERO if b is 0.
 * Returns PROVEN_ERR_INVALID_ARGUMENT if either operand is NaN.
 * Returns PROVEN_ERR_OVERFLOW if result would be infinite.
 */
ProvenFloatResult proven_float_div(double a, double b);

/**
 * @brief Safe square root
 * @param x Value to take square root of
 * @return Result with status and square root
 *
 * Returns PROVEN_ERR_INVALID_ARGUMENT if x is negative or NaN.
 */
ProvenFloatResult proven_float_sqrt(double x);

/**
 * @brief Safe natural logarithm
 * @param x Value to take log of
 * @return Result with status and logarithm
 *
 * Returns PROVEN_ERR_INVALID_ARGUMENT if x <= 0 or NaN.
 */
ProvenFloatResult proven_float_ln(double x);

/* ============================================================================
 * Float Classification
 * ============================================================================ */

/**
 * @brief Check if float is finite (not NaN or Inf)
 * @param x Value to check
 * @return true if finite
 */
bool proven_float_is_finite(double x);

/**
 * @brief Check if float is NaN
 * @param x Value to check
 * @return true if NaN
 */
bool proven_float_is_nan(double x);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_FLOAT_H */
