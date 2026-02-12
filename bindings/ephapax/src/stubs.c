// SPDX-License-Identifier: Apache-2.0
// Temporary C stub implementations for testing FFI
// TODO: Replace with actual Idris2-compiled proven library

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

// Opaque types (will match Idris2 types)
typedef struct LRUCache LRUCache;
typedef struct Buffer Buffer;
typedef struct ResourceHandle ResourceHandle;

// Simple implementation for testing
struct LRUCache {
    uint64_t capacity;
    uint64_t size;
    uint64_t access_counter;
    void* entries; // Simplified for now
};

struct Buffer {
    uint64_t capacity;
    uint64_t size;
    uint8_t* data;
};

struct ResourceHandle {
    uint64_t resource_id;
    uint32_t state; // 0=Unacquired, 1=Acquired, 2=Released
    uint64_t acquired_at;
    uint64_t released_at;
};

// LRU functions (stub implementations)
LRUCache* idris_proven_lru_new(uint64_t capacity) {
    LRUCache* cache = malloc(sizeof(LRUCache));
    cache->capacity = capacity;
    cache->size = 0;
    cache->access_counter = 0;
    cache->entries = NULL;
    return cache;
}

LRUCache* idris_proven_lru_put(LRUCache* cache, const uint8_t* key, uint64_t key_len,
                                 const uint8_t* value, uint64_t value_len) {
    // Stub: just increment counter
    cache->access_counter++;
    if (cache->size < cache->capacity) {
        cache->size++;
    }
    return cache;
}

const uint8_t* idris_proven_lru_get(LRUCache* cache, const uint8_t* key, uint64_t key_len,
                                     uint64_t* out_len) {
    // Stub: return NULL (not found)
    *out_len = 0;
    return NULL;
}

bool idris_proven_lru_contains(LRUCache* cache, const uint8_t* key, uint64_t key_len) {
    // Stub: always return false
    return false;
}

uint64_t idris_proven_lru_size(LRUCache* cache) {
    return cache->size;
}

bool idris_proven_lru_is_full(LRUCache* cache) {
    return cache->size >= cache->capacity;
}

void idris_proven_lru_free(LRUCache* cache) {
    free(cache);
}

// Buffer functions (stub implementations)
Buffer* idris_proven_buffer_new(uint64_t capacity) {
    Buffer* buf = malloc(sizeof(Buffer));
    buf->capacity = capacity;
    buf->size = 0;
    buf->data = malloc(capacity);
    return buf;
}

int32_t idris_proven_buffer_write(Buffer* buf, const uint8_t* data, uint64_t data_len) {
    if (buf->size + data_len > buf->capacity) {
        return 1; // Error: would overflow
    }
    memcpy(buf->data + buf->size, data, data_len);
    buf->size += data_len;
    return 0; // Ok
}

int32_t idris_proven_buffer_read(Buffer* buf, uint64_t offset, uint64_t len, uint8_t* out_data) {
    if (offset + len > buf->size) {
        return 1; // Error: out of bounds
    }
    memcpy(out_data, buf->data + offset, len);
    return 0; // Ok
}

uint64_t idris_proven_buffer_capacity(Buffer* buf) {
    return buf->capacity;
}

uint64_t idris_proven_buffer_size(Buffer* buf) {
    return buf->size;
}

bool idris_proven_buffer_is_full(Buffer* buf) {
    return buf->size >= buf->capacity;
}

uint64_t idris_proven_buffer_remaining(Buffer* buf) {
    return buf->capacity - buf->size;
}

void idris_proven_buffer_free(Buffer* buf) {
    free(buf->data);
    free(buf);
}

// Resource functions (stub implementations)
ResourceHandle* idris_proven_resource_new_handle(uint64_t id) {
    ResourceHandle* handle = malloc(sizeof(ResourceHandle));
    handle->resource_id = id;
    handle->state = 0; // Unacquired
    handle->acquired_at = 0;
    handle->released_at = 0;
    return handle;
}

ResourceHandle* idris_proven_resource_mark_acquired(ResourceHandle* handle, uint64_t timestamp) {
    handle->state = 1; // Acquired
    handle->acquired_at = timestamp;
    return handle;
}

ResourceHandle* idris_proven_resource_mark_released(ResourceHandle* handle, uint64_t timestamp) {
    handle->state = 2; // Released
    handle->released_at = timestamp;
    return handle;
}

bool idris_proven_resource_is_held(ResourceHandle* handle) {
    return handle->state == 1; // Acquired
}

uint32_t idris_proven_resource_get_state(ResourceHandle* handle) {
    return handle->state;
}

void idris_proven_resource_free_handle(ResourceHandle* handle) {
    free(handle);
}
