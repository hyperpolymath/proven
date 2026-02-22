// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeLru - FFI bindings to libproven LRU cache operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for LRU operations.
pub const LruError = error{
    CreationFailed,
    NotFound,
    ProvenError,
};

/// LRU cache backed by libproven.
pub const LruCache = struct {
    ptr: *c.ProvenLRUCache,

    /// Get value from cache (promotes to most recently used) via libproven.
    pub fn get(self: LruCache, key: u64) LruError!i64 {
        const result = c.proven_lru_get(self.ptr, key);
        return switch (result.status) {
            c.PROVEN_OK => result.value,
            c.PROVEN_ERR_OUT_OF_BOUNDS => error.NotFound,
            else => error.ProvenError,
        };
    }

    /// Put value in cache (evicts LRU entry if full) via libproven.
    pub fn put(self: LruCache, key: u64, value: i64) LruError!void {
        const status = c.proven_lru_put(self.ptr, key, value);
        if (status != c.PROVEN_OK) return error.ProvenError;
    }

    /// Free LRU cache via libproven.
    pub fn deinit(self: LruCache) void {
        c.proven_lru_free(self.ptr);
    }
};

/// Create an LRU cache via libproven.
/// capacity: Maximum entries (max 100,000).
pub fn create(capacity: usize) LruError!LruCache {
    const ptr = c.proven_lru_create(capacity);
    if (ptr == null) return error.CreationFailed;
    return LruCache{ .ptr = ptr.? };
}

test "create and put/get" {
    const cache = try create(10);
    defer cache.deinit();
    try cache.put(1, 100);
    try std.testing.expectEqual(@as(i64, 100), try cache.get(1));
    try std.testing.expectError(error.NotFound, cache.get(999));
}
