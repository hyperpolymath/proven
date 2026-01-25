// SPDX-License-Identifier: PMPL-1.0
// Simple C test program for FFI

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

// Forward declarations from stubs.c
typedef struct LRUCache LRUCache;
typedef struct Buffer Buffer;
typedef struct ResourceHandle ResourceHandle;

extern LRUCache* idris_proven_lru_new(uint64_t capacity);
extern LRUCache* idris_proven_lru_put(LRUCache* cache, const uint8_t* key, uint64_t key_len,
                                       const uint8_t* value, uint64_t value_len);
extern uint64_t idris_proven_lru_size(LRUCache* cache);
extern bool idris_proven_lru_is_full(LRUCache* cache);
extern void idris_proven_lru_free(LRUCache* cache);

extern Buffer* idris_proven_buffer_new(uint64_t capacity);
extern int32_t idris_proven_buffer_write(Buffer* buf, const uint8_t* data, uint64_t data_len);
extern uint64_t idris_proven_buffer_size(Buffer* buf);
extern void idris_proven_buffer_free(Buffer* buf);

extern ResourceHandle* idris_proven_resource_new_handle(uint64_t id);
extern ResourceHandle* idris_proven_resource_mark_acquired(ResourceHandle* handle, uint64_t timestamp);
extern bool idris_proven_resource_is_held(ResourceHandle* handle);
extern void idris_proven_resource_free_handle(ResourceHandle* handle);

int main() {
    printf("=== Ephapax ↔ Proven FFI Test ===\n\n");

    // Test 1: LRU Cache
    printf("Test 1: LRU Cache\n");
    LRUCache* cache = idris_proven_lru_new(1024);
    assert(cache != NULL);

    uint64_t size = idris_proven_lru_size(cache);
    printf("  Initial size: %lu\n", size);
    assert(size == 0);

    const char* key = "test_key";
    const char* value = "test_value";
    cache = idris_proven_lru_put(cache, (const uint8_t*)key, strlen(key),
                                  (const uint8_t*)value, strlen(value));

    size = idris_proven_lru_size(cache);
    printf("  Size after put: %lu\n", size);
    assert(size == 1);

    bool is_full = idris_proven_lru_is_full(cache);
    printf("  Is full: %s\n", is_full ? "yes" : "no");
    assert(!is_full);

    idris_proven_lru_free(cache);
    printf("  ✓ LRU Cache test passed\n\n");

    // Test 2: Buffer
    printf("Test 2: Buffer\n");
    Buffer* buf = idris_proven_buffer_new(1024);
    assert(buf != NULL);

    const char* data = "Hello, World!";
    int32_t write_result = idris_proven_buffer_write(buf, (const uint8_t*)data, strlen(data));
    printf("  Write result: %d (0=OK)\n", write_result);
    assert(write_result == 0);

    uint64_t buf_size = idris_proven_buffer_size(buf);
    printf("  Buffer size: %lu\n", buf_size);
    assert(buf_size == strlen(data));

    idris_proven_buffer_free(buf);
    printf("  ✓ Buffer test passed\n\n");

    // Test 3: Resource Handle
    printf("Test 3: Resource Handle\n");
    ResourceHandle* handle = idris_proven_resource_new_handle(42);
    assert(handle != NULL);

    bool held_before = idris_proven_resource_is_held(handle);
    printf("  Held before acquire: %s\n", held_before ? "yes" : "no");
    assert(!held_before);

    handle = idris_proven_resource_mark_acquired(handle, 1000);

    bool held_after = idris_proven_resource_is_held(handle);
    printf("  Held after acquire: %s\n", held_after ? "yes" : "no");
    assert(held_after);

    idris_proven_resource_free_handle(handle);
    printf("  ✓ Resource Handle test passed\n\n");

    printf("=== All FFI tests passed! ===\n");
    return 0;
}
