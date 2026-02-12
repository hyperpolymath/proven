// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Bounded stack-like buffer.
public class BoundedBuffer<T> {
    private var data: [T]
    public let capacity: Int

    public init(capacity: Int) {
        self.capacity = max(1, capacity)
        self.data = []
    }

    public var count: Int { data.count }
    public var isEmpty: Bool { data.isEmpty }
    public var isFull: Bool { data.count >= capacity }
    public var remaining: Int { capacity - data.count }

    /// Push an item (fails if full).
    @discardableResult
    public func push(_ item: T) -> Bool {
        guard !isFull else { return false }
        data.append(item)
        return true
    }

    /// Pop the last item.
    public func pop() -> T? {
        data.popLast()
    }

    /// Peek at the last item without removing.
    public func peek() -> T? {
        data.last
    }

    /// Get item at index.
    public func get(_ index: Int) -> T? {
        guard index >= 0 && index < data.count else { return nil }
        return data[index]
    }

    /// Clear the buffer.
    public func clear() {
        data.removeAll()
    }

    /// Convert to array.
    public func toArray() -> [T] {
        Array(data)
    }
}

/// Circular buffer (ring buffer).
public class RingBuffer<T> {
    private var data: [T?]
    public let capacity: Int
    private var head: Int = 0
    private var tail: Int = 0
    private var _count: Int = 0

    public init(capacity: Int) {
        self.capacity = max(1, capacity)
        self.data = Array(repeating: nil, count: self.capacity)
    }

    public var count: Int { _count }
    public var isEmpty: Bool { _count == 0 }
    public var isFull: Bool { _count >= capacity }

    /// Push an item (overwrites oldest if full).
    /// Returns the overwritten item if any.
    @discardableResult
    public func push(_ item: T) -> T? {
        var overwritten: T? = nil

        if isFull {
            overwritten = data[head]
            head = (head + 1) % capacity
        } else {
            _count += 1
        }

        data[tail] = item
        tail = (tail + 1) % capacity

        return overwritten
    }

    /// Pop the oldest item (FIFO).
    public func pop() -> T? {
        guard !isEmpty else { return nil }

        let item = data[head]
        data[head] = nil
        head = (head + 1) % capacity
        _count -= 1

        return item
    }

    /// Peek at the oldest item without removing.
    public func peek() -> T? {
        guard !isEmpty else { return nil }
        return data[head]
    }

    /// Peek at the newest item without removing.
    public func peekNewest() -> T? {
        guard !isEmpty else { return nil }
        let index = (tail - 1 + capacity) % capacity
        return data[index]
    }

    /// Get item at logical index (0 = oldest).
    public func get(_ index: Int) -> T? {
        guard index >= 0 && index < _count else { return nil }
        let actualIndex = (head + index) % capacity
        return data[actualIndex]
    }

    /// Clear the buffer.
    public func clear() {
        data = Array(repeating: nil, count: capacity)
        head = 0
        tail = 0
        _count = 0
    }

    /// Convert to array (oldest first).
    public func toArray() -> [T] {
        var result: [T] = []
        for i in 0..<_count {
            let actualIndex = (head + i) % capacity
            if let item = data[actualIndex] {
                result.append(item)
            }
        }
        return result
    }
}

/// Growable buffer with optional max capacity.
public class GrowableBuffer<T> {
    private var data: [T]
    public let maxCapacity: Int

    public init(initialCapacity: Int = 16, maxCapacity: Int = Int.max) {
        self.data = []
        self.data.reserveCapacity(max(1, initialCapacity))
        self.maxCapacity = maxCapacity
    }

    public var count: Int { data.count }
    public var isEmpty: Bool { data.isEmpty }
    public var isFull: Bool { data.count >= maxCapacity }

    /// Push an item (fails if at max capacity).
    @discardableResult
    public func push(_ item: T) -> Bool {
        guard !isFull else { return false }
        data.append(item)
        return true
    }

    /// Push multiple items.
    /// Returns the number of items added.
    public func pushMany(_ items: [T]) -> Int {
        var added = 0
        for item in items {
            if !push(item) { break }
            added += 1
        }
        return added
    }

    /// Pop the last item.
    public func pop() -> T? {
        data.popLast()
    }

    /// Get item at index.
    public func get(_ index: Int) -> T? {
        guard index >= 0 && index < data.count else { return nil }
        return data[index]
    }

    /// Clear the buffer.
    public func clear() {
        data.removeAll()
    }

    /// Convert to array.
    public func toArray() -> [T] {
        Array(data)
    }
}
