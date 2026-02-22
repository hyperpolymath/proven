// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// LRU cache delegated to libproven FFI.
///
/// Cache management is performed by the formally verified Idris 2 core
/// via the Zig FFI bridge. This class manages the lifecycle of the
/// C-allocated LRU cache.

import CProven

/// LRU cache backed by libproven. Keys are UInt64, values are Int64.
public final class SafeLRU {
    private let cache: UnsafeMutablePointer<ProvenLRUCache>

    /// Create an LRU cache with the given capacity.
    /// Returns nil if allocation fails.
    public init?(capacity: Int) {
        guard let ptr = proven_lru_create(capacity) else {
            return nil
        }
        self.cache = ptr
    }

    deinit {
        proven_lru_free(cache)
    }

    /// Get a value from the cache by key.
    /// Returns .failure if the key is not present or an error occurs.
    public func get(_ key: UInt64) -> Result<Int64, ProvenError> {
        let result = proven_lru_get(cache, key)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Put a key-value pair into the cache.
    /// Evicts the least recently used entry if at capacity.
    @discardableResult
    public func put(_ key: UInt64, _ value: Int64) -> Result<Void, ProvenError> {
        let status = proven_lru_put(cache, key, value)
        if let error = ProvenError.fromStatus(status) {
            return .failure(error)
        }
        return .success(())
    }
}
