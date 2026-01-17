/* SPDX-License-Identifier: PMPL-1.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_path.h
 * @brief Filesystem operations that prevent traversal attacks
 *
 * Provides path validation and sanitization to prevent directory
 * traversal attacks and dangerous filename injection.
 */

#ifndef SAFE_PATH_H
#define SAFE_PATH_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Path Validation
 * ============================================================================ */

/**
 * @brief Check if path contains directory traversal sequences
 * @param ptr Pointer to path string
 * @param len Length of path
 * @return Result with traversal detection status
 *
 * Detects ".." sequences that could escape the intended directory.
 */
ProvenBoolResult proven_path_has_traversal(const uint8_t* ptr, size_t len);

/* ============================================================================
 * Path Sanitization
 * ============================================================================ */

/**
 * @brief Sanitize a filename by removing dangerous characters
 * @param ptr Pointer to filename
 * @param len Length of filename
 * @return Result with sanitized filename (caller must free)
 *
 * Keeps only alphanumeric characters, dots, dashes, and underscores.
 */
ProvenStringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_PATH_H */
