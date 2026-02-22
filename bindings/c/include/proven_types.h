/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */

/**
 * @file proven_types.h
 * @brief Core type definitions for Proven C bindings
 *
 * This header defines the core result types shared across all Proven modules.
 * Include this header when you need just the base types and status codes
 * without pulling in all 103+ function declarations.
 *
 * For the complete API, include <proven.h> instead.
 *
 * @note This header exists for backward compatibility and lightweight usage.
 *       All types defined here are also available via <proven.h>.
 */

#ifndef PROVEN_TYPES_H
#define PROVEN_TYPES_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Status Codes
 * ============================================================================ */

/**
 * @brief Status codes returned by Proven operations.
 *
 * Zero indicates success; negative values indicate specific error conditions.
 */
typedef enum ProvenStatus {
    PROVEN_OK                      =   0,
    PROVEN_ERR_NULL_POINTER        =  -1,
    PROVEN_ERR_INVALID_ARGUMENT    =  -2,
    PROVEN_ERR_OVERFLOW            =  -3,
    PROVEN_ERR_UNDERFLOW           =  -4,
    PROVEN_ERR_DIVISION_BY_ZERO    =  -5,
    PROVEN_ERR_PARSE_FAILURE       =  -6,
    PROVEN_ERR_VALIDATION_FAILED   =  -7,
    PROVEN_ERR_OUT_OF_BOUNDS       =  -8,
    PROVEN_ERR_ENCODING_ERROR      =  -9,
    PROVEN_ERR_ALLOCATION_FAILED   = -10,
    PROVEN_ERR_NOT_IMPLEMENTED     = -99
} ProvenStatus;

/* ============================================================================
 * Core Result Types
 * ============================================================================ */

/** @brief Result for integer operations */
typedef struct ProvenIntResult {
    ProvenStatus status;
    int64_t value;
} ProvenIntResult;

/** @brief Result for boolean operations */
typedef struct ProvenBoolResult {
    ProvenStatus status;
    bool value;
} ProvenBoolResult;

/**
 * @brief Result for string operations
 * @note Caller must free the value using proven_free_string()
 */
typedef struct ProvenStringResult {
    ProvenStatus status;
    char* value;
    size_t length;
} ProvenStringResult;

/** @brief Result for floating-point operations */
typedef struct ProvenFloatResult {
    ProvenStatus status;
    double value;
} ProvenFloatResult;

/* ============================================================================
 * Convenience Macros
 * ============================================================================ */

/** @brief Check if a result indicates success */
#define PROVEN_SUCCEEDED(result) ((result).status == PROVEN_OK)

/** @brief Check if a result indicates failure */
#define PROVEN_FAILED(result) ((result).status != PROVEN_OK)

/** @brief Library version string */
#define PROVEN_VERSION_STRING "0.9.0"

/** @brief Total module count */
#define PROVEN_MODULE_COUNT 41

#ifdef __cplusplus
}
#endif

#endif /* PROVEN_TYPES_H */
