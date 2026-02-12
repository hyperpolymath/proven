// SPDX-License-Identifier: Apache-2.0
//! Main FFI module for Ephapax ↔ Proven bindings
//!
//! Pure Zig implementation - no C files, no headers
//! Exports C-compatible ABI for bidirectional FFI

pub const lru = @import("lru_impl.zig");
pub const buffer = @import("buffer_impl.zig");
pub const resource = @import("resource_impl.zig");

// Re-export main types
pub const LRUCache = lru.LRUCache;
pub const Buffer = buffer.Buffer;
pub const ResourceHandle = resource.ResourceHandle;
pub const ResourceState = resource.ResourceState;

// Force export functions to be included in compilation
comptime {
    // LRU exports
    _ = &lru.idris_proven_lru_new;
    _ = &lru.idris_proven_lru_put;
    _ = &lru.idris_proven_lru_get;
    _ = &lru.idris_proven_lru_contains;
    _ = &lru.idris_proven_lru_size;
    _ = &lru.idris_proven_lru_is_full;
    _ = &lru.idris_proven_lru_free;

    // Buffer exports
    _ = &buffer.idris_proven_buffer_new;
    _ = &buffer.idris_proven_buffer_write;
    _ = &buffer.idris_proven_buffer_read;
    _ = &buffer.idris_proven_buffer_capacity;
    _ = &buffer.idris_proven_buffer_size;
    _ = &buffer.idris_proven_buffer_is_full;
    _ = &buffer.idris_proven_buffer_remaining;
    _ = &buffer.idris_proven_buffer_free;

    // Resource exports
    _ = &resource.idris_proven_resource_new_handle;
    _ = &resource.idris_proven_resource_mark_acquired;
    _ = &resource.idris_proven_resource_mark_released;
    _ = &resource.idris_proven_resource_is_held;
    _ = &resource.idris_proven_resource_get_state;
    _ = &resource.idris_proven_resource_free_handle;
}
