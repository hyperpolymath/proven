/* SPDX-License-Identifier: PMPL-1.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_version.h
 * @brief Semantic versioning support
 *
 * Provides parsing and comparison of semantic version strings
 * following the semver.org specification.
 */

#ifndef SAFE_VERSION_H
#define SAFE_VERSION_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Version Types
 * ============================================================================ */

/**
 * @brief Semantic version structure
 */
typedef struct ProvenSemanticVersion {
    uint32_t major;
    uint32_t minor;
    uint32_t patch;
    size_t prerelease_len;
    char* prerelease;  /**< Pre-release string (may be NULL) */
} ProvenSemanticVersion;

/**
 * @brief Version parsing result
 */
typedef struct ProvenVersionResult {
    ProvenStatus status;
    ProvenSemanticVersion version;
} ProvenVersionResult;

/* ============================================================================
 * Version Parsing
 * ============================================================================ */

/**
 * @brief Parse semantic version string
 * @param ptr Pointer to version string (e.g., "1.2.3-alpha")
 * @param len Length of string
 * @return Result with parsed version
 *
 * Supports optional 'v' or 'V' prefix.
 * Caller must free prerelease string with proven_version_free if present.
 */
ProvenVersionResult proven_version_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Free version result resources
 * @param version Pointer to version to free
 */
void proven_version_free(ProvenSemanticVersion* version);

/* ============================================================================
 * Version Comparison
 * ============================================================================ */

/**
 * @brief Compare two semantic versions
 * @param a First version
 * @param b Second version
 * @return Negative if a < b, 0 if equal, positive if a > b
 *
 * Pre-release versions are considered lower than release versions.
 */
int32_t proven_version_compare(ProvenSemanticVersion a, ProvenSemanticVersion b);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_VERSION_H */
