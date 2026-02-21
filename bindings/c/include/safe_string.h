/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_string.h
 * @brief Text operations that handle encoding safely
 *
 * Provides string escaping for various contexts (SQL, HTML, JavaScript)
 * and UTF-8 validation.
 */

#ifndef SAFE_STRING_H
#define SAFE_STRING_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * UTF-8 Validation
 * ============================================================================ */

/**
 * @brief Check if bytes are valid UTF-8
 * @param ptr Pointer to byte data
 * @param len Length of data
 * @return Result with validation status
 */
ProvenBoolResult proven_string_is_valid_utf8(const uint8_t* ptr, size_t len);

/* ============================================================================
 * String Escaping
 * ============================================================================ */

/**
 * @brief Escape string for SQL (single quotes)
 * @param ptr Pointer to string data
 * @param len Length of string
 * @return Result with escaped string (caller must free)
 *
 * @note Prefer parameterized queries over string escaping!
 */
ProvenStringResult proven_string_escape_sql(const uint8_t* ptr, size_t len);

/**
 * @brief Escape string for HTML
 * @param ptr Pointer to string data
 * @param len Length of string
 * @return Result with escaped string (caller must free)
 *
 * Escapes < > & " ' characters to prevent XSS.
 */
ProvenStringResult proven_string_escape_html(const uint8_t* ptr, size_t len);

/**
 * @brief Escape string for JavaScript string literals
 * @param ptr Pointer to string data
 * @param len Length of string
 * @return Result with escaped string (caller must free)
 */
ProvenStringResult proven_string_escape_js(const uint8_t* ptr, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_STRING_H */
