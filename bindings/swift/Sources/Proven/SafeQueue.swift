// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Bounded FIFO queue.
public class BoundedQueue<T> {
    private var data: [T]
    public let capacity: Int

    public init(capacity: Int) {
        self.capacity = max(1, capacity)
        self.data = []
    }

    public var count: Int { data.count }
    public var isEmpty: Bool { data.isEmpty }
    public var isFull: Bool { data.count >= capacity }

    /// Enqueue an item (fails if full).
    @discardableResult
    public func enqueue(_ item: T) -> Bool {
        guard !isFull else { return false }
        data.append(item)
        return true
    }

    /// Dequeue the first item.
    public func dequeue() -> T? {
        guard !isEmpty else { return nil }
        return data.removeFirst()
    }

    /// Peek at the first item without removing.
    public func peek() -> T? {
        data.first
    }

    /// Clear the queue.
    public func clear() {
        data.removeAll()
    }

    /// Convert to array.
    public func toArray() -> [T] {
        Array(data)
    }
}

/// Priority queue (min-heap).
public class PriorityQueue<T> {
    private var heap: [T]
    public let capacity: Int
    private let comparator: (T, T) -> Bool

    /// Create a priority queue.
    /// - Parameters:
    ///   - capacity: Maximum capacity
    ///   - comparator: Returns true if first argument has higher priority
    public init(capacity: Int, comparator: @escaping (T, T) -> Bool) {
        self.capacity = max(1, capacity)
        self.heap = []
        self.comparator = comparator
    }

    public var count: Int { heap.count }
    public var isEmpty: Bool { heap.isEmpty }
    public var isFull: Bool { heap.count >= capacity }

    /// Push an item (fails if full).
    @discardableResult
    public func push(_ item: T) -> Bool {
        guard !isFull else { return false }
        heap.append(item)
        bubbleUp(heap.count - 1)
        return true
    }

    /// Pop the highest priority item.
    public func pop() -> T? {
        guard !isEmpty else { return nil }

        let top = heap[0]
        let last = heap.removeLast()

        if !heap.isEmpty {
            heap[0] = last
            bubbleDown(0)
        }

        return top
    }

    /// Peek at the highest priority item without removing.
    public func peek() -> T? {
        heap.first
    }

    /// Clear the queue.
    public func clear() {
        heap.removeAll()
    }

    private func bubbleUp(_ index: Int) {
        var index = index
        while index > 0 {
            let parent = (index - 1) / 2
            if comparator(heap[index], heap[parent]) {
                heap.swapAt(index, parent)
                index = parent
            } else {
                break
            }
        }
    }

    private func bubbleDown(_ index: Int) {
        var index = index
        let count = heap.count

        while true {
            let left = 2 * index + 1
            let right = 2 * index + 2
            var highest = index

            if left < count && comparator(heap[left], heap[highest]) {
                highest = left
            }
            if right < count && comparator(heap[right], heap[highest]) {
                highest = right
            }

            if highest == index {
                break
            }

            heap.swapAt(index, highest)
            index = highest
        }
    }
}

extension PriorityQueue where T: Comparable {
    /// Create a min-heap.
    public static func minHeap(capacity: Int) -> PriorityQueue<T> {
        PriorityQueue(capacity: capacity) { $0 < $1 }
    }

    /// Create a max-heap.
    public static func maxHeap(capacity: Int) -> PriorityQueue<T> {
        PriorityQueue(capacity: capacity) { $0 > $1 }
    }
}

/// Double-ended queue with bounded capacity.
public class BoundedDeque<T> {
    private var data: [T]
    public let capacity: Int

    public init(capacity: Int) {
        self.capacity = max(1, capacity)
        self.data = []
    }

    public var count: Int { data.count }
    public var isEmpty: Bool { data.isEmpty }
    public var isFull: Bool { data.count >= capacity }

    /// Push to front.
    @discardableResult
    public func pushFront(_ item: T) -> Bool {
        guard !isFull else { return false }
        data.insert(item, at: 0)
        return true
    }

    /// Push to back.
    @discardableResult
    public func pushBack(_ item: T) -> Bool {
        guard !isFull else { return false }
        data.append(item)
        return true
    }

    /// Pop from front.
    public func popFront() -> T? {
        guard !isEmpty else { return nil }
        return data.removeFirst()
    }

    /// Pop from back.
    public func popBack() -> T? {
        guard !isEmpty else { return nil }
        return data.removeLast()
    }

    /// Peek front.
    public func peekFront() -> T? {
        data.first
    }

    /// Peek back.
    public func peekBack() -> T? {
        data.last
    }

    /// Clear the deque.
    public func clear() {
        data.removeAll()
    }

    /// Convert to array.
    public func toArray() -> [T] {
        Array(data)
    }
}
