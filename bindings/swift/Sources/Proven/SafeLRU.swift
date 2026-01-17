// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// LRU Cache node.
private class LRUNode<K: Hashable, V> {
    let key: K
    var value: V
    var prev: LRUNode<K, V>?
    var next: LRUNode<K, V>?

    init(key: K, value: V) {
        self.key = key
        self.value = value
    }
}

/// Least Recently Used cache.
public class LRUCache<K: Hashable, V> {
    private var cache: [K: LRUNode<K, V>] = [:]
    private var head: LRUNode<K, V>?
    private var tail: LRUNode<K, V>?
    public let capacity: Int
    private var _hits: Int = 0
    private var _misses: Int = 0

    public init(capacity: Int) {
        self.capacity = max(1, capacity)
    }

    public var count: Int { cache.count }
    public var hits: Int { _hits }
    public var misses: Int { _misses }

    public var hitRate: Double {
        let total = _hits + _misses
        return total == 0 ? 0 : Double(_hits) / Double(total)
    }

    /// Get a value from the cache.
    public func get(_ key: K) -> V? {
        guard let node = cache[key] else {
            _misses += 1
            return nil
        }

        _hits += 1
        moveToFront(node)
        return node.value
    }

    /// Put a value in the cache.
    /// Returns the evicted entry if any.
    @discardableResult
    public func put(_ key: K, _ value: V) -> (key: K, value: V)? {
        if let existing = cache[key] {
            existing.value = value
            moveToFront(existing)
            return nil
        }

        var evicted: (key: K, value: V)?

        // Evict if at capacity
        if cache.count >= capacity, let tailNode = tail {
            evicted = (key: tailNode.key, value: tailNode.value)
            cache.removeValue(forKey: tailNode.key)
            removeTail()
        }

        // Add new node
        let node = LRUNode(key: key, value: value)
        cache[key] = node
        addToFront(node)

        return evicted
    }

    /// Remove a key from the cache.
    @discardableResult
    public func remove(_ key: K) -> V? {
        guard let node = cache[key] else { return nil }
        removeNode(node)
        cache.removeValue(forKey: key)
        return node.value
    }

    /// Check if a key exists (without affecting LRU order).
    public func contains(_ key: K) -> Bool {
        cache[key] != nil
    }

    /// Peek at a value without affecting LRU order.
    public func peek(_ key: K) -> V? {
        cache[key]?.value
    }

    /// Get all keys in LRU order (most recent first).
    public func keys() -> [K] {
        var result: [K] = []
        var node = head
        while let current = node {
            result.append(current.key)
            node = current.next
        }
        return result
    }

    /// Clear the cache.
    public func clear() {
        cache.removeAll()
        head = nil
        tail = nil
    }

    /// Reset statistics.
    public func resetStats() {
        _hits = 0
        _misses = 0
    }

    private func moveToFront(_ node: LRUNode<K, V>) {
        guard node !== head else { return }
        removeNode(node)
        addToFront(node)
    }

    private func addToFront(_ node: LRUNode<K, V>) {
        node.prev = nil
        node.next = head

        if let head = head {
            head.prev = node
        }
        head = node

        if tail == nil {
            tail = node
        }
    }

    private func removeNode(_ node: LRUNode<K, V>) {
        if let prev = node.prev {
            prev.next = node.next
        } else {
            head = node.next
        }

        if let next = node.next {
            next.prev = node.prev
        } else {
            tail = node.prev
        }
    }

    private func removeTail() {
        guard let tail = tail else { return }
        removeNode(tail)
    }
}

/// LRU Cache with time-to-live support.
public class TTLLRUCache<K: Hashable, V> {
    private struct TTLEntry {
        let value: V
        let expiresAt: Double
    }

    private let cache: LRUCache<K, TTLEntry>
    public let defaultTTL: Double // milliseconds
    private let getNow: () -> Double

    /// Create a TTL LRU cache.
    /// - Parameters:
    ///   - capacity: Maximum capacity
    ///   - defaultTTLMs: Default TTL in milliseconds
    ///   - getNow: Function to get current time (for testing)
    public init(capacity: Int, defaultTTLMs: Double, getNow: @escaping () -> Double = { Date().timeIntervalSince1970 * 1000 }) {
        self.cache = LRUCache(capacity: capacity)
        self.defaultTTL = defaultTTLMs
        self.getNow = getNow
    }

    public var count: Int { cache.count }

    /// Get a value from the cache.
    public func get(_ key: K) -> V? {
        guard let entry = cache.get(key) else { return nil }

        if getNow() > entry.expiresAt {
            cache.remove(key)
            return nil
        }

        return entry.value
    }

    /// Put a value with default TTL.
    public func put(_ key: K, _ value: V) {
        put(key, value, ttlMs: defaultTTL)
    }

    /// Put a value with specific TTL.
    public func put(_ key: K, _ value: V, ttlMs: Double) {
        let entry = TTLEntry(value: value, expiresAt: getNow() + ttlMs)
        cache.put(key, entry)
    }

    /// Remove a key.
    @discardableResult
    public func remove(_ key: K) -> Bool {
        cache.remove(key) != nil
    }

    /// Clear the cache.
    public func clear() {
        cache.clear()
    }
}
