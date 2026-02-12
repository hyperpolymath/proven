// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

/**
 * Bounded buffer with fixed capacity.
 */
class BoundedBuffer<T>(val capacity: Int) {
    private val lock = ReentrantLock()
    private val data = ArrayList<T>(capacity)

    init {
        require(capacity > 0) { "Capacity must be positive" }
    }

    val size: Int get() = lock.withLock { data.size }
    val isEmpty: Boolean get() = lock.withLock { data.isEmpty() }
    val isFull: Boolean get() = lock.withLock { data.size >= capacity }
    val remaining: Int get() = lock.withLock { capacity - data.size }

    /**
     * Write item to buffer.
     */
    fun write(item: T): Boolean = lock.withLock {
        if (data.size >= capacity) false
        else {
            data.add(item)
            true
        }
    }

    /**
     * Write multiple items, returning count written.
     */
    fun writeMany(items: Collection<T>): Int = lock.withLock {
        var count = 0
        for (item in items) {
            if (data.size >= capacity) break
            data.add(item)
            count++
        }
        count
    }

    /**
     * Read and remove first item.
     */
    fun read(): T? = lock.withLock {
        if (data.isEmpty()) null else data.removeAt(0)
    }

    /**
     * Read multiple items.
     */
    fun readMany(count: Int): List<T> = lock.withLock {
        val n = minOf(count, data.size)
        val result = data.subList(0, n).toList()
        repeat(n) { data.removeAt(0) }
        result
    }

    /**
     * Peek at first item without removing.
     */
    fun peek(): T? = lock.withLock { data.firstOrNull() }

    /**
     * Get all items without removing.
     */
    fun peekAll(): List<T> = lock.withLock { data.toList() }

    /**
     * Clear the buffer.
     */
    fun clear() = lock.withLock { data.clear() }

    /**
     * Check if buffer contains item.
     */
    fun contains(item: T): Boolean = lock.withLock { data.contains(item) }
}

/**
 * Ring buffer (circular buffer).
 */
class RingBuffer<T : Any>(val capacity: Int) {
    private val lock = ReentrantLock()
    private val data = arrayOfNulls<Any>(capacity)
    private var head = 0
    private var tail = 0
    private var count = 0

    init {
        require(capacity > 0) { "Capacity must be positive" }
    }

    val size: Int get() = lock.withLock { count }
    val isEmpty: Boolean get() = lock.withLock { count == 0 }
    val isFull: Boolean get() = lock.withLock { count == capacity }

    /**
     * Write item, overwriting oldest if full.
     */
    fun write(item: T): Boolean = lock.withLock {
        val wasOverwritten = count == capacity
        data[tail] = item
        tail = (tail + 1) % capacity
        if (wasOverwritten) {
            head = (head + 1) % capacity
        } else {
            count++
        }
        wasOverwritten
    }

    /**
     * Read oldest item.
     */
    @Suppress("UNCHECKED_CAST")
    fun read(): T? = lock.withLock {
        if (count == 0) null
        else {
            val item = data[head] as T
            data[head] = null
            head = (head + 1) % capacity
            count--
            item
        }
    }

    /**
     * Peek at oldest item.
     */
    @Suppress("UNCHECKED_CAST")
    fun peek(): T? = lock.withLock {
        if (count == 0) null else data[head] as T
    }

    /**
     * Get all items in order (oldest first).
     */
    @Suppress("UNCHECKED_CAST")
    fun toList(): List<T> = lock.withLock {
        val result = mutableListOf<T>()
        for (i in 0 until count) {
            result.add(data[(head + i) % capacity] as T)
        }
        result
    }

    /**
     * Clear the buffer.
     */
    fun clear() = lock.withLock {
        for (i in 0 until capacity) data[i] = null
        head = 0
        tail = 0
        count = 0
    }
}

/**
 * Growable buffer with optional maximum capacity.
 */
class GrowableBuffer<T>(
    initialCapacity: Int = 16,
    private val maxCapacity: Int = Int.MAX_VALUE
) {
    private val lock = ReentrantLock()
    private val data = ArrayList<T>(initialCapacity)

    init {
        require(initialCapacity > 0) { "Initial capacity must be positive" }
        require(maxCapacity >= initialCapacity) { "Max capacity must be >= initial capacity" }
    }

    val size: Int get() = lock.withLock { data.size }
    val isEmpty: Boolean get() = lock.withLock { data.isEmpty() }
    val remaining: Int get() = lock.withLock { maxCapacity - data.size }

    /**
     * Write item to buffer.
     */
    fun write(item: T): Boolean = lock.withLock {
        if (data.size >= maxCapacity) false
        else {
            data.add(item)
            true
        }
    }

    /**
     * Read and remove first item.
     */
    fun read(): T? = lock.withLock {
        if (data.isEmpty()) null else data.removeAt(0)
    }

    /**
     * Read and remove last item.
     */
    fun readLast(): T? = lock.withLock {
        if (data.isEmpty()) null else data.removeAt(data.lastIndex)
    }

    /**
     * Get item at index.
     */
    fun get(index: Int): T? = lock.withLock {
        if (index < 0 || index >= data.size) null else data[index]
    }

    /**
     * Get all items.
     */
    fun toList(): List<T> = lock.withLock { data.toList() }

    /**
     * Clear the buffer.
     */
    fun clear() = lock.withLock { data.clear() }

    /**
     * Shrink to fit current size.
     */
    fun shrinkToFit() = lock.withLock { data.trimToSize() }
}
