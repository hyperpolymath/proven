/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_calculator.h
 * @brief Safe expression evaluation
 *
 * Provides a safe arithmetic expression evaluator that parses
 * and evaluates mathematical expressions without code injection risks.
 */

#ifndef SAFE_CALCULATOR_H
#define SAFE_CALCULATOR_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Calculator Operations
 * ============================================================================ */

/**
 * @brief Evaluate an arithmetic expression
 * @param ptr Pointer to expression string
 * @param len Length of expression string
 * @return Result with status and computed value
 *
 * Supports:
 * - Basic operators: +, -, *, /
 * - Parentheses for grouping
 * - Negative numbers
 * - Decimal numbers
 *
 * Returns PROVEN_ERR_NULL_POINTER if ptr is NULL.
 * Returns PROVEN_ERR_PARSE_FAILURE if expression is invalid.
 * Returns PROVEN_ERR_DIVISION_BY_ZERO if division by zero occurs.
 *
 * Example expressions:
 * - "2 + 3 * 4" = 14
 * - "(2 + 3) * 4" = 20
 * - "-5 + 10" = 5
 */
ProvenFloatResult proven_calculator_eval(const uint8_t* ptr, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_CALCULATOR_H */
