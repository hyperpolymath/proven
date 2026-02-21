/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_url.h
 * @brief URL parsing and validation
 *
 * Provides safe URL parsing with component extraction.
 */

#ifndef SAFE_URL_H
#define SAFE_URL_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * URL Types
 * ============================================================================ */

/**
 * @brief URL components structure
 */
typedef struct ProvenUrlComponents {
    char* scheme;
    size_t scheme_len;
    char* host;
    size_t host_len;
    uint16_t port;
    bool has_port;
    char* path;
    size_t path_len;
    char* query;
    size_t query_len;
    char* fragment;
    size_t fragment_len;
} ProvenUrlComponents;

/**
 * @brief Result for URL parsing
 */
typedef struct ProvenUrlResult {
    ProvenStatus status;
    ProvenUrlComponents components;
} ProvenUrlResult;

/* ============================================================================
 * URL Parsing
 * ============================================================================ */

/**
 * @brief Parse a URL into components
 * @param ptr Pointer to URL string
 * @param len Length of URL
 * @return Result with URL components (caller must free with proven_url_free)
 */
ProvenUrlResult proven_url_parse(const uint8_t* ptr, size_t len);

/**
 * @brief Free URL components
 * @param components Pointer to URL components to free
 */
void proven_url_free(ProvenUrlComponents* components);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_URL_H */
