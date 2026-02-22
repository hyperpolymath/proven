// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Bloom filter delegated to libproven FFI.
///
/// Probabilistic set membership testing via the formally verified
/// Idris 2 core.

import CProven

/// Bloom filter backed by libproven.
/// This class manages the lifecycle of a C-allocated bloom filter.
public final class SafeBloom {
    private let filter: UnsafeMutablePointer<ProvenBloomFilter>

    /// Create a bloom filter with expected elements and false positive rate.
    /// Returns nil if allocation fails.
    public init?(expectedElements: Int, falsePositiveRate: Double = 0.01) {
        guard let ptr = proven_bloom_create(expectedElements, falsePositiveRate) else {
            return nil
        }
        self.filter = ptr
    }

    deinit {
        proven_bloom_free(filter)
    }

    /// Add an element to the bloom filter.
    public func add(_ item: String) {
        let utf8 = Array(item.utf8)
        utf8.withUnsafeBufferPointer { buffer in
            proven_bloom_add(filter, buffer.baseAddress, buffer.count)
        }
    }

    /// Add raw bytes to the bloom filter.
    public func add(_ data: [UInt8]) {
        data.withUnsafeBufferPointer { buffer in
            proven_bloom_add(filter, buffer.baseAddress, buffer.count)
        }
    }

    /// Check if an element might be in the set.
    /// True means "possibly present", false means "definitely not present".
    public func contains(_ item: String) -> Bool {
        let utf8 = Array(item.utf8)
        return utf8.withUnsafeBufferPointer { buffer in
            proven_bloom_contains(filter, buffer.baseAddress, buffer.count)
        }
    }

    /// Check if raw bytes might be in the set.
    public func contains(_ data: [UInt8]) -> Bool {
        data.withUnsafeBufferPointer { buffer in
            proven_bloom_contains(filter, buffer.baseAddress, buffer.count)
        }
    }
}
