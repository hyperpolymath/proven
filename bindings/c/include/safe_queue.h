/* SPDX-License-Identifier: PMPL-1.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_queue.h
 * @brief Bounded FIFO queue
 *
 * Provides a bounded first-in-first-out queue with overflow protection.
 */

#ifndef SAFE_QUEUE_H
#define SAFE_QUEUE_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Queue Types
 * ============================================================================ */

/**
 * @brief Bounded queue structure
 * @note Use proven_queue_create() to allocate, proven_queue_free() to deallocate
 */
typedef struct ProvenBoundedQueue {
    int64_t* data;
    size_t capacity;
    size_t head;
    size_t tail;
    size_t count;
} ProvenBoundedQueue;

/* ============================================================================
 * Queue Operations
 * ============================================================================ */

/**
 * @brief Create a bounded queue
 * @param capacity Maximum number of elements (max 1,000,000)
 * @return Queue pointer, or NULL on failure
 */
ProvenBoundedQueue* proven_queue_create(size_t capacity);

/**
 * @brief Push value to queue
 * @param queue Queue to push to
 * @param value Value to push
 * @return true if successful, false if queue is full or NULL
 */
bool proven_queue_push(ProvenBoundedQueue* queue, int64_t value);

/**
 * @brief Pop value from queue
 * @param queue Queue to pop from
 * @return Result with status and value
 *
 * Returns PROVEN_ERR_NULL_POINTER if queue is NULL.
 * Returns PROVEN_ERR_OUT_OF_BOUNDS if queue is empty.
 */
ProvenIntResult proven_queue_pop(ProvenBoundedQueue* queue);

/**
 * @brief Get queue size
 * @param queue Queue to query
 * @return Number of elements in queue (0 if NULL)
 */
size_t proven_queue_size(ProvenBoundedQueue* queue);

/**
 * @brief Free queue
 * @param queue Queue to free (may be NULL)
 */
void proven_queue_free(ProvenBoundedQueue* queue);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_QUEUE_H */
