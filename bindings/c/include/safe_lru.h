/* SPDX-License-Identifier: Apache-2.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_lru.h
 * @brief Least Recently Used (LRU) cache
 *
 * Provides a bounded cache with automatic eviction of least
 * recently used entries.
 */

#ifndef SAFE_LRU_H
#define SAFE_LRU_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * LRU Types
 * ============================================================================ */

/**
 * @brief LRU cache entry
 */
typedef struct ProvenLRUEntry {
    uint64_t key;
    int64_t value;
    size_t prev;
    size_t next;
    bool valid;
} ProvenLRUEntry;

/**
 * @brief LRU cache structure
 * @note Use proven_lru_create() to allocate, proven_lru_free() to deallocate
 */
typedef struct ProvenLRUCache {
    ProvenLRUEntry* entries;
    size_t capacity;
    size_t head;
    size_t tail;
    size_t count;
} ProvenLRUCache;

/* ============================================================================
 * LRU Operations
 * ============================================================================ */

/**
 * @brief Create an LRU cache
 * @param capacity Maximum number of entries (max 100,000)
 * @return Cache pointer, or NULL on failure
 */
ProvenLRUCache* proven_lru_create(size_t capacity);

/**
 * @brief Get value from cache
 * @param cache LRU cache
 * @param key Key to look up
 * @return Result with status and value
 *
 * Returns PROVEN_ERR_NULL_POINTER if cache is NULL.
 * Returns PROVEN_ERR_OUT_OF_BOUNDS if key not found.
 */
ProvenIntResult proven_lru_get(ProvenLRUCache* cache, uint64_t key);

/**
 * @brief Put value in cache
 * @param cache LRU cache
 * @param key Key to store
 * @param value Value to store
 * @return Status code
 *
 * If cache is full, the least recently used entry is evicted.
 */
ProvenStatus proven_lru_put(ProvenLRUCache* cache, uint64_t key, int64_t value);

/**
 * @brief Free LRU cache
 * @param cache Cache to free (may be NULL)
 */
void proven_lru_free(ProvenLRUCache* cache);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_LRU_H */
