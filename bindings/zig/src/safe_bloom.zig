// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeBloom - FFI bindings to libproven Bloom filter operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for Bloom filter operations.
pub const BloomError = error{
    CreationFailed,
};

/// Bloom filter backed by libproven.
pub const BloomFilter = struct {
    ptr: *c.ProvenBloomFilter,

    /// Add element to Bloom filter via libproven.
    pub fn add(self: BloomFilter, data: []const u8) void {
        c.proven_bloom_add(self.ptr, data.ptr, data.len);
    }

    /// Check if element might be in filter via libproven.
    /// Returns true if probably present (may be false positive),
    /// false if definitely absent.
    pub fn contains(self: BloomFilter, data: []const u8) bool {
        return c.proven_bloom_contains(self.ptr, data.ptr, data.len);
    }

    /// Free Bloom filter via libproven.
    pub fn deinit(self: BloomFilter) void {
        c.proven_bloom_free(self.ptr);
    }
};

/// Create a Bloom filter via libproven.
/// expected_elements: Expected number of elements.
/// false_positive_rate: Desired false positive rate (0 < rate < 1).
pub fn create(expected_elements: usize, false_positive_rate: f64) BloomError!BloomFilter {
    const ptr = c.proven_bloom_create(expected_elements, false_positive_rate);
    if (ptr == null) return error.CreationFailed;
    return BloomFilter{ .ptr = ptr.? };
}

test "create and add/contains" {
    const bf = try create(1000, 0.01);
    defer bf.deinit();
    bf.add("hello");
    bf.add("world");
    try std.testing.expect(bf.contains("hello"));
    try std.testing.expect(bf.contains("world"));
    try std.testing.expect(!bf.contains("definitely not present xyz123"));
}
