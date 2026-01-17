/* SPDX-License-Identifier: PMPL-1.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_buffer.h
 * @brief Bounded buffer operations
 *
 * Provides a bounded buffer with automatic bounds checking
 * to prevent buffer overflows.
 */

#ifndef SAFE_BUFFER_H
#define SAFE_BUFFER_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Buffer Types
 * ============================================================================ */

/**
 * @brief Bounded buffer structure
 * @note Use proven_buffer_create() to allocate, proven_buffer_free() to deallocate
 */
typedef struct ProvenBoundedBuffer {
    uint8_t* data;
    size_t capacity;
    size_t length;
} ProvenBoundedBuffer;

/**
 * @brief Buffer creation result
 */
typedef struct ProvenBufferResult {
    ProvenStatus status;
    ProvenBoundedBuffer* buffer;
} ProvenBufferResult;

/* ============================================================================
 * Buffer Operations
 * ============================================================================ */

/**
 * @brief Create a bounded buffer
 * @param capacity Maximum capacity in bytes (max 100MB)
 * @return Result with buffer pointer
 *
 * Returns PROVEN_ERR_INVALID_ARGUMENT if capacity is 0 or exceeds limit.
 * Returns PROVEN_ERR_ALLOCATION_FAILED on memory allocation failure.
 */
ProvenBufferResult proven_buffer_create(size_t capacity);

/**
 * @brief Append data to buffer
 * @param buffer Buffer to append to
 * @param ptr Data to append
 * @param len Length of data
 * @return Status code
 *
 * Returns PROVEN_ERR_NULL_POINTER if buffer or ptr is NULL.
 * Returns PROVEN_ERR_OUT_OF_BOUNDS if data would exceed capacity.
 */
ProvenStatus proven_buffer_append(ProvenBoundedBuffer* buffer, const uint8_t* ptr, size_t len);

/**
 * @brief Get buffer contents
 * @param buffer Buffer to read from
 * @param[out] out_ptr Pointer to receive data pointer
 * @param[out] out_len Pointer to receive data length
 * @return Status code
 */
ProvenStatus proven_buffer_get(ProvenBoundedBuffer* buffer, const uint8_t** out_ptr, size_t* out_len);

/**
 * @brief Free buffer
 * @param buffer Buffer to free (may be NULL)
 */
void proven_buffer_free(ProvenBoundedBuffer* buffer);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_BUFFER_H */
