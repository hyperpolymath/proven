// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe Bloom filter implementation.

const std = @import("std");

/// Bloom filter with configurable size and hash count.
pub fn BloomFilter(comptime size: usize, comptime num_hashes: usize) type {
    return struct {
        const Self = @This();
        const BitSet = std.StaticBitSet(size);

        bits: BitSet = BitSet.initEmpty(),

        pub fn init() Self {
            return .{};
        }

        /// Insert an item into the filter.
        pub fn insert(self: *Self, data: []const u8) void {
            var i: usize = 0;
            while (i < num_hashes) : (i += 1) {
                const hash = computeHash(data, i);
                self.bits.set(hash % size);
            }
        }

        /// Check if an item might be in the filter.
        /// Returns false if definitely not present, true if possibly present.
        pub fn contains(self: *const Self, data: []const u8) bool {
            var i: usize = 0;
            while (i < num_hashes) : (i += 1) {
                const hash = computeHash(data, i);
                if (!self.bits.isSet(hash % size)) {
                    return false;
                }
            }
            return true;
        }

        /// Count set bits.
        pub fn countOnes(self: *const Self) usize {
            return self.bits.count();
        }

        /// Get fill ratio.
        pub fn fillRatio(self: *const Self) f64 {
            return @as(f64, @floatFromInt(self.countOnes())) / @as(f64, @floatFromInt(size));
        }

        /// Estimate false positive rate.
        pub fn estimatedFPR(self: *const Self) f64 {
            const ratio = self.fillRatio();
            return std.math.pow(f64, ratio, @as(f64, @floatFromInt(num_hashes)));
        }

        /// Clear the filter.
        pub fn clear(self: *Self) void {
            self.bits = BitSet.initEmpty();
        }

        /// Union of two filters.
        pub fn unionWith(self: *Self, other: *const Self) void {
            self.bits = self.bits.unionWith(other.bits);
        }

        /// Intersection of two filters.
        pub fn intersectWith(self: *Self, other: *const Self) void {
            self.bits = self.bits.intersectWith(other.bits);
        }

        fn computeHash(data: []const u8, seed: usize) usize {
            // Use two hash functions and combine them
            const h1 = std.hash.Fnv1a_64.hash(data);
            var h2_data: [8]u8 = undefined;
            std.mem.writeInt(u64, &h2_data, @as(u64, @intCast(seed)), .little);
            const h2 = std.hash.Fnv1a_64.hash(&h2_data);
            return @intCast((h1 +% seed *% h2) % size);
        }
    };
}

/// Calculate optimal size for given parameters.
pub fn optimalSize(expected_items: usize, false_positive_rate: f64) usize {
    const n = @as(f64, @floatFromInt(expected_items));
    const p = false_positive_rate;
    const ln2_sq = 0.4804530139182014; // ln(2)^2
    const m = -(n * @log(p)) / ln2_sq;
    return @intFromFloat(@ceil(m));
}

/// Calculate optimal hash count.
pub fn optimalHashes(filter_size: usize, expected_items: usize) usize {
    const m = @as(f64, @floatFromInt(filter_size));
    const n = @as(f64, @floatFromInt(expected_items));
    const k = (m / n) * 0.6931471805599453; // ln(2)
    return @max(1, @as(usize, @intFromFloat(@ceil(k))));
}

test "BloomFilter" {
    var bf = BloomFilter(1024, 3).init();
    bf.insert("hello");
    bf.insert("world");

    try std.testing.expect(bf.contains("hello"));
    try std.testing.expect(bf.contains("world"));
    // May have false positives, but definitely not here
    try std.testing.expect(!bf.contains("definitely not present xyz123"));
}

test "optimalSize" {
    const size = optimalSize(1000, 0.01);
    try std.testing.expect(size > 9000 and size < 10000);
}
