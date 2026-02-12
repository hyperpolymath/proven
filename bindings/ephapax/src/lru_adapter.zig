// SPDX-License-Identifier: Apache-2.0
//! SafeLRU adapter for Ephapax
//!
//! This adapter provides FFI bindings to Idris2's proven SafeLRU module.
//! ONE adapter handles BOTH affine and linear modes - the difference is in
//! how Ephapax's type checker enforces usage, not in the FFI layer.

const std = @import("std");
const types = @import("types.zig");

// External C functions from proven library (currently stubs, will be Idris2-compiled)
extern fn idris_proven_lru_new(capacity: u64) ?*types.LRUCacheHandle;
extern fn idris_proven_lru_put(cache: *types.LRUCacheHandle, key: [*]const u8, key_len: u64, value: [*]const u8, value_len: u64) ?*types.LRUCacheHandle;
extern fn idris_proven_lru_get(cache: *types.LRUCacheHandle, key: [*]const u8, key_len: u64, out_len: *u64) ?[*]const u8;
extern fn idris_proven_lru_contains(cache: *types.LRUCacheHandle, key: [*]const u8, key_len: u64) bool;
extern fn idris_proven_lru_size(cache: *types.LRUCacheHandle) u64;
extern fn idris_proven_lru_is_full(cache: *types.LRUCacheHandle) bool;
extern fn idris_proven_lru_free(cache: *types.LRUCacheHandle) void;

//-----------------------------------------------------------------------------
// FFI Exports for Ephapax
//-----------------------------------------------------------------------------

/// Create a new LRU cache with given capacity
///
/// Used by BOTH:
/// - Ephapax affine mode: let cache = ProvenLRUAffine.new(1024);
/// - Ephapax linear mode: let! cache = ProvenLRULinear.new(1024);
export fn ephapax_proven_lru_new(capacity: u64) callconv(.C) ?*types.LRUCacheHandle {
    return idris_proven_lru_new(capacity);
}

/// Insert a key-value pair into the cache
///
/// Affine mode: cache = ProvenLRUAffine.put(cache, key, value);  // May or may not use result
/// Linear mode: let! cache = ProvenLRULinear.put(cache, key, value);  // MUST consume result
export fn ephapax_proven_lru_put(
    cache: *types.LRUCacheHandle,
    key: [*]const u8,
    key_len: u64,
    value: [*]const u8,
    value_len: u64,
) callconv(.C) ?*types.LRUCacheHandle {
    return idris_proven_lru_put(cache, key, key_len, value, value_len);
}

/// Get a value from the cache
/// Returns null if not found, otherwise pointer to value and updates out_len
///
/// Affine mode: if let Some(val) = ProvenLRUAffine.get(cache, key)
/// Linear mode: let! (result, cache) = ProvenLRULinear.get(cache, key);
export fn ephapax_proven_lru_get(
    cache: *types.LRUCacheHandle,
    key: [*]const u8,
    key_len: u64,
    out_len: *u64,
) callconv(.C) ?[*]const u8 {
    return idris_proven_lru_get(cache, key, key_len, out_len);
}

/// Free the cache
///
/// Affine mode: Optional explicit free (automatic at region end if not called)
/// Linear mode: REQUIRED explicit free (compiler error if not called)
export fn ephapax_proven_lru_free(cache: *types.LRUCacheHandle) callconv(.C) void {
    idris_proven_lru_free(cache);
}

/// Check if a key exists in the cache
export fn ephapax_proven_lru_contains(
    cache: *types.LRUCacheHandle,
    key: [*]const u8,
    key_len: u64,
) callconv(.C) bool {
    return idris_proven_lru_contains(cache, key, key_len);
}

/// Get current cache size
export fn ephapax_proven_lru_size(cache: *types.LRUCacheHandle) callconv(.C) u64 {
    return idris_proven_lru_size(cache);
}

/// Check if cache is full
export fn ephapax_proven_lru_is_full(cache: *types.LRUCacheHandle) callconv(.C) bool {
    return idris_proven_lru_is_full(cache);
}
