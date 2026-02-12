// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Bloom filter for probabilistic set membership testing.
public class BloomFilter {
    private var bits: [UInt8]
    public let size: Int
    public let hashCount: Int
    private var _count: Int = 0

    public init(size: Int, hashCount: Int) {
        self.size = max(1, size)
        self.hashCount = max(1, hashCount)
        self.bits = [UInt8](repeating: 0, count: (self.size + 7) / 8)
    }

    /// Create an optimal bloom filter for expected items and false positive rate.
    public static func optimal(expectedItems: Int, falsePositiveRate: Double = 0.01) -> BloomFilter {
        let n = max(1, expectedItems)
        let p = max(0.001, min(0.999, falsePositiveRate))

        // m = -(n * ln(p)) / (ln(2)^2)
        let m = Int(ceil(-Double(n) * log(p) / (log(2) * log(2))))
        // k = (m/n) * ln(2)
        let k = Int(ceil(Double(m) / Double(n) * log(2)))

        return BloomFilter(size: m, hashCount: k)
    }

    public var itemCount: Int { _count }

    /// Add an item to the filter.
    public func add(_ item: String) {
        for i in 0..<hashCount {
            let index = hash(item, seed: i)
            setBit(index)
        }
        _count += 1
    }

    /// Check if an item might be in the set.
    public func contains(_ item: String) -> Bool {
        for i in 0..<hashCount {
            let index = hash(item, seed: i)
            if !getBit(index) {
                return false
            }
        }
        return true
    }

    /// Estimate current false positive rate.
    public func falsePositiveRate() -> Double {
        // (1 - e^(-k*n/m))^k
        let exponent = -Double(hashCount) * Double(_count) / Double(size)
        return pow(1 - exp(exponent), Double(hashCount))
    }

    /// Clear the filter.
    public func clear() {
        bits = [UInt8](repeating: 0, count: bits.count)
        _count = 0
    }

    /// Union with another filter (same size and hash count).
    public func union(_ other: BloomFilter) -> BloomFilter? {
        guard size == other.size && hashCount == other.hashCount else { return nil }

        let result = BloomFilter(size: size, hashCount: hashCount)
        for i in 0..<bits.count {
            result.bits[i] = bits[i] | other.bits[i]
        }
        result._count = _count + other._count
        return result
    }

    /// Intersection with another filter.
    public func intersection(_ other: BloomFilter) -> BloomFilter? {
        guard size == other.size && hashCount == other.hashCount else { return nil }

        let result = BloomFilter(size: size, hashCount: hashCount)
        for i in 0..<bits.count {
            result.bits[i] = bits[i] & other.bits[i]
        }
        return result
    }

    private func hash(_ item: String, seed: Int) -> Int {
        // FNV-1a hash with seed
        var hash: UInt32 = 2166136261 ^ UInt32(seed)
        for char in item.utf8 {
            hash ^= UInt32(char)
            hash = hash &* 16777619
        }
        return Int(hash % UInt32(size))
    }

    private func setBit(_ index: Int) {
        let byteIndex = index / 8
        let bitIndex = index % 8
        bits[byteIndex] |= UInt8(1 << bitIndex)
    }

    private func getBit(_ index: Int) -> Bool {
        let byteIndex = index / 8
        let bitIndex = index % 8
        return (bits[byteIndex] & UInt8(1 << bitIndex)) != 0
    }
}

/// Counting bloom filter that allows removal.
public class CountingBloomFilter {
    private var counters: [UInt8]
    public let size: Int
    public let hashCount: Int
    private var _count: Int = 0

    public init(size: Int, hashCount: Int) {
        self.size = max(1, size)
        self.hashCount = max(1, hashCount)
        self.counters = [UInt8](repeating: 0, count: self.size)
    }

    public var itemCount: Int { _count }

    /// Add an item to the filter.
    public func add(_ item: String) {
        for i in 0..<hashCount {
            let index = hash(item, seed: i)
            if counters[index] < 255 {
                counters[index] += 1
            }
        }
        _count += 1
    }

    /// Remove an item from the filter.
    @discardableResult
    public func remove(_ item: String) -> Bool {
        guard contains(item) else { return false }

        for i in 0..<hashCount {
            let index = hash(item, seed: i)
            if counters[index] > 0 {
                counters[index] -= 1
            }
        }
        _count -= 1
        return true
    }

    /// Check if an item might be in the set.
    public func contains(_ item: String) -> Bool {
        for i in 0..<hashCount {
            let index = hash(item, seed: i)
            if counters[index] == 0 {
                return false
            }
        }
        return true
    }

    /// Clear the filter.
    public func clear() {
        counters = [UInt8](repeating: 0, count: size)
        _count = 0
    }

    private func hash(_ item: String, seed: Int) -> Int {
        var hash: UInt32 = 2166136261 ^ UInt32(seed)
        for char in item.utf8 {
            hash ^= UInt32(char)
            hash = hash &* 16777619
        }
        return Int(hash % UInt32(size))
    }
}
