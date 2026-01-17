// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

/**
 * Bounded FIFO queue.
 */
class BoundedQueue<T>(val capacity: Int) {
    private val lock = ReentrantLock()
    private val data = ArrayDeque<T>(capacity)

    init {
        require(capacity > 0) { "Capacity must be positive" }
    }

    val size: Int get() = lock.withLock { data.size }
    val isEmpty: Boolean get() = lock.withLock { data.isEmpty() }
    val isFull: Boolean get() = lock.withLock { data.size >= capacity }

    /**
     * Enqueue item.
     */
    fun enqueue(item: T): Boolean = lock.withLock {
        if (data.size >= capacity) false
        else {
            data.addLast(item)
            true
        }
    }

    /**
     * Dequeue item.
     */
    fun dequeue(): T? = lock.withLock {
        if (data.isEmpty()) null else data.removeFirst()
    }

    /**
     * Peek at front item.
     */
    fun peek(): T? = lock.withLock { data.firstOrNull() }

    /**
     * Clear the queue.
     */
    fun clear() = lock.withLock { data.clear() }

    /**
     * Get all items.
     */
    fun toList(): List<T> = lock.withLock { data.toList() }
}

/**
 * Priority queue with min-heap.
 */
class PriorityQueue<T>(
    private val comparator: Comparator<T>,
    val capacity: Int = Int.MAX_VALUE
) {
    private val lock = ReentrantLock()
    private val heap = ArrayList<T>()

    val size: Int get() = lock.withLock { heap.size }
    val isEmpty: Boolean get() = lock.withLock { heap.isEmpty() }
    val isFull: Boolean get() = lock.withLock { heap.size >= capacity }

    /**
     * Insert item.
     */
    fun insert(item: T): Boolean = lock.withLock {
        if (heap.size >= capacity) return false
        heap.add(item)
        siftUp(heap.lastIndex)
        true
    }

    /**
     * Extract minimum item.
     */
    fun extractMin(): T? = lock.withLock {
        if (heap.isEmpty()) return null
        val min = heap[0]
        val last = heap.removeAt(heap.lastIndex)
        if (heap.isNotEmpty()) {
            heap[0] = last
            siftDown(0)
        }
        min
    }

    /**
     * Peek at minimum item.
     */
    fun peekMin(): T? = lock.withLock { heap.firstOrNull() }

    /**
     * Clear the queue.
     */
    fun clear() = lock.withLock { heap.clear() }

    private fun siftUp(index: Int) {
        var i = index
        while (i > 0) {
            val parent = (i - 1) / 2
            if (comparator.compare(heap[i], heap[parent]) >= 0) break
            heap[i] = heap[parent].also { heap[parent] = heap[i] }
            i = parent
        }
    }

    private fun siftDown(index: Int) {
        var i = index
        while (true) {
            val left = 2 * i + 1
            val right = 2 * i + 2
            var smallest = i

            if (left < heap.size && comparator.compare(heap[left], heap[smallest]) < 0) {
                smallest = left
            }
            if (right < heap.size && comparator.compare(heap[right], heap[smallest]) < 0) {
                smallest = right
            }

            if (smallest == i) break
            heap[i] = heap[smallest].also { heap[smallest] = heap[i] }
            i = smallest
        }
    }

    companion object {
        /**
         * Create min priority queue for comparable items.
         */
        fun <T : Comparable<T>> minQueue(capacity: Int = Int.MAX_VALUE): PriorityQueue<T> {
            return PriorityQueue(compareBy { it }, capacity)
        }

        /**
         * Create max priority queue for comparable items.
         */
        fun <T : Comparable<T>> maxQueue(capacity: Int = Int.MAX_VALUE): PriorityQueue<T> {
            return PriorityQueue(compareByDescending { it }, capacity)
        }
    }
}

/**
 * Bounded double-ended queue (deque).
 */
class BoundedDeque<T>(val capacity: Int) {
    private val lock = ReentrantLock()
    private val data = ArrayDeque<T>(capacity)

    init {
        require(capacity > 0) { "Capacity must be positive" }
    }

    val size: Int get() = lock.withLock { data.size }
    val isEmpty: Boolean get() = lock.withLock { data.isEmpty() }
    val isFull: Boolean get() = lock.withLock { data.size >= capacity }

    /**
     * Push to front.
     */
    fun pushFront(item: T): Boolean = lock.withLock {
        if (data.size >= capacity) false
        else {
            data.addFirst(item)
            true
        }
    }

    /**
     * Push to back.
     */
    fun pushBack(item: T): Boolean = lock.withLock {
        if (data.size >= capacity) false
        else {
            data.addLast(item)
            true
        }
    }

    /**
     * Pop from front.
     */
    fun popFront(): T? = lock.withLock {
        if (data.isEmpty()) null else data.removeFirst()
    }

    /**
     * Pop from back.
     */
    fun popBack(): T? = lock.withLock {
        if (data.isEmpty()) null else data.removeLast()
    }

    /**
     * Peek at front.
     */
    fun peekFront(): T? = lock.withLock { data.firstOrNull() }

    /**
     * Peek at back.
     */
    fun peekBack(): T? = lock.withLock { data.lastOrNull() }

    /**
     * Get item at index.
     */
    fun get(index: Int): T? = lock.withLock {
        if (index < 0 || index >= data.size) null else data.elementAt(index)
    }

    /**
     * Clear the deque.
     */
    fun clear() = lock.withLock { data.clear() }

    /**
     * Get all items.
     */
    fun toList(): List<T> = lock.withLock { data.toList() }
}
