/* SPDX-License-Identifier: PMPL-1.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_bloom.h
 * @brief Probabilistic set membership (Bloom filter)
 *
 * Provides a space-efficient probabilistic data structure for
 * approximate set membership testing.
 */

#ifndef SAFE_BLOOM_H
#define SAFE_BLOOM_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Bloom Filter Types
 * ============================================================================ */

/**
 * @brief Bloom filter structure
 * @note Use proven_bloom_create() to allocate, proven_bloom_free() to deallocate
 */
typedef struct ProvenBloomFilter {
    uint8_t* bits;
    size_t bit_count;
    uint32_t hash_count;
} ProvenBloomFilter;

/* ============================================================================
 * Bloom Filter Operations
 * ============================================================================ */

/**
 * @brief Create a Bloom filter
 * @param expected_elements Expected number of elements to store
 * @param false_positive_rate Desired false positive rate (0 < rate < 1)
 * @return Bloom filter pointer, or NULL on failure
 *
 * The filter size and hash count are calculated automatically
 * based on the expected elements and desired false positive rate.
 */
ProvenBloomFilter* proven_bloom_create(size_t expected_elements, double false_positive_rate);

/**
 * @brief Add element to Bloom filter
 * @param filter Bloom filter
 * @param ptr Pointer to element data
 * @param len Length of element data
 *
 * Does nothing if filter or ptr is NULL.
 */
void proven_bloom_add(ProvenBloomFilter* filter, const uint8_t* ptr, size_t len);

/**
 * @brief Check if element might be in filter
 * @param filter Bloom filter
 * @param ptr Pointer to element data
 * @param len Length of element data
 * @return true if element might be present, false if definitely not present
 *
 * Returns false if filter or ptr is NULL.
 * Note: A true result may be a false positive.
 */
bool proven_bloom_contains(ProvenBloomFilter* filter, const uint8_t* ptr, size_t len);

/**
 * @brief Free Bloom filter
 * @param filter Filter to free (may be NULL)
 */
void proven_bloom_free(ProvenBloomFilter* filter);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_BLOOM_H */
