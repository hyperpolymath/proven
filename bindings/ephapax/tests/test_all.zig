// SPDX-License-Identifier: Apache-2.0
//! Test suite for Ephapax ↔ Proven FFI bindings

const std = @import("std");
const testing = std.testing;

// Import FFI functions
const lru = @import("../src/lru_adapter.zig");
const buffer = @import("../src/buffer_adapter.zig");
const resource = @import("../src/resource_adapter.zig");
const types = @import("../src/types.zig");

test "LRU cache create and free" {
    const cache = lru.ephapax_proven_lru_new(1024);
    defer lru.ephapax_proven_lru_free(cache);

    const size = lru.ephapax_proven_lru_size(cache);
    try testing.expectEqual(@as(u64, 0), size);
}

test "LRU cache put and get" {
    const cache_initial = lru.ephapax_proven_lru_new(1024);
    defer lru.ephapax_proven_lru_free(cache_initial);

    const key = "test_key";
    const value = "test_value";

    // Put value
    const cache_after_put = lru.ephapax_proven_lru_put(
        cache_initial,
        key.ptr,
        key.len,
        value.ptr,
        value.len,
    );

    // Get value
    var out_len: u64 = 0;
    const result = lru.ephapax_proven_lru_get(
        cache_after_put,
        key.ptr,
        key.len,
        &out_len,
    );

    try testing.expect(result != null);
    try testing.expectEqual(@as(u64, value.len), out_len);
}

test "LRU cache contains" {
    const cache_initial = lru.ephapax_proven_lru_new(1024);
    defer lru.ephapax_proven_lru_free(cache_initial);

    const key = "test_key";
    const value = "test_value";

    // Initially should not contain key
    const contains_before = lru.ephapax_proven_lru_contains(cache_initial, key.ptr, key.len);
    try testing.expectEqual(false, contains_before);

    // Put value
    const cache_after_put = lru.ephapax_proven_lru_put(
        cache_initial,
        key.ptr,
        key.len,
        value.ptr,
        value.len,
    );

    // Should now contain key
    const contains_after = lru.ephapax_proven_lru_contains(cache_after_put, key.ptr, key.len);
    try testing.expectEqual(true, contains_after);
}

test "Buffer create and free" {
    const buffer_handle = buffer.ephapax_proven_buffer_new(1024);
    defer buffer.ephapax_proven_buffer_free(buffer_handle);

    const capacity = buffer.ephapax_proven_buffer_capacity(buffer_handle);
    try testing.expectEqual(@as(u64, 1024), capacity);

    const size = buffer.ephapax_proven_buffer_size(buffer_handle);
    try testing.expectEqual(@as(u64, 0), size);
}

test "Buffer write and read" {
    const buffer_handle = buffer.ephapax_proven_buffer_new(1024);
    defer buffer.ephapax_proven_buffer_free(buffer_handle);

    const data = "Hello, World!";

    // Write data
    const write_result = buffer.ephapax_proven_buffer_write(buffer_handle, data.ptr, data.len);
    try testing.expectEqual(types.Result.Ok, write_result);

    // Verify size
    const size_after_write = buffer.ephapax_proven_buffer_size(buffer_handle);
    try testing.expectEqual(@as(u64, data.len), size_after_write);

    // Read data back
    var read_buffer: [1024]u8 = undefined;
    const read_result = buffer.ephapax_proven_buffer_read(buffer_handle, 0, data.len, &read_buffer);
    try testing.expectEqual(types.Result.Ok, read_result);
}

test "Resource handle lifecycle" {
    const handle_initial = resource.ephapax_proven_resource_new_handle(42);
    defer resource.ephapax_proven_resource_free_handle(handle_initial);

    // Initially unacquired
    const state_initial = resource.ephapax_proven_resource_get_state(handle_initial);
    try testing.expectEqual(resource.ResourceState.Unacquired, state_initial);

    // Mark as acquired
    const handle_acquired = resource.ephapax_proven_resource_mark_acquired(handle_initial, 1000);
    const state_acquired = resource.ephapax_proven_resource_get_state(handle_acquired);
    try testing.expectEqual(resource.ResourceState.Acquired, state_acquired);

    const is_held = resource.ephapax_proven_resource_is_held(handle_acquired);
    try testing.expectEqual(true, is_held);

    // Mark as released
    const handle_released = resource.ephapax_proven_resource_mark_released(handle_acquired, 2000);
    const state_released = resource.ephapax_proven_resource_get_state(handle_released);
    try testing.expectEqual(resource.ResourceState.Released, state_released);
}
