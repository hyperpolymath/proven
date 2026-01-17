/* SPDX-License-Identifier: PMPL-1.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_json.h
 * @brief JSON validation and type detection
 *
 * Provides safe JSON validation and type detection without full parsing.
 */

#ifndef SAFE_JSON_H
#define SAFE_JSON_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * JSON Types
 * ============================================================================ */

/**
 * @brief JSON value type
 */
typedef enum ProvenJsonType {
    PROVEN_JSON_NULL = 0,
    PROVEN_JSON_BOOL = 1,
    PROVEN_JSON_NUMBER = 2,
    PROVEN_JSON_STRING = 3,
    PROVEN_JSON_ARRAY = 4,
    PROVEN_JSON_OBJECT = 5,
    PROVEN_JSON_INVALID = -1
} ProvenJsonType;

/* ============================================================================
 * JSON Validation
 * ============================================================================ */

/**
 * @brief Check if string is valid JSON
 * @param ptr Pointer to JSON string
 * @param len Length of string
 * @return Result with validation status
 *
 * Validates balanced braces, brackets, and string quoting.
 */
ProvenBoolResult proven_json_is_valid(const uint8_t* ptr, size_t len);

/**
 * @brief Get JSON value type at root level
 * @param ptr Pointer to JSON string
 * @param len Length of string
 * @return JSON type (PROVEN_JSON_INVALID if not valid)
 */
ProvenJsonType proven_json_get_type(const uint8_t* ptr, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_JSON_H */
