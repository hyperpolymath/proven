// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

/**
 * Monotonically increasing counter.
 */
class MonotonicCounter(initialValue: Long = 0) {
    private val value = AtomicLong(initialValue)

    /**
     * Get current value.
     */
    fun get(): Long = value.get()

    /**
     * Increment and get new value.
     */
    fun increment(): Long = value.incrementAndGet()

    /**
     * Add amount and get new value.
     */
    fun add(amount: Long): Long {
        require(amount >= 0) { "Cannot add negative value to monotonic counter" }
        return value.addAndGet(amount)
    }

    /**
     * Get and increment.
     */
    fun getAndIncrement(): Long = value.getAndIncrement()

    /**
     * Compare and set if greater.
     */
    fun updateIfGreater(newValue: Long): Boolean {
        while (true) {
            val current = value.get()
            if (newValue <= current) return false
            if (value.compareAndSet(current, newValue)) return true
        }
    }
}

/**
 * Monotonically increasing timestamp.
 */
class MonotonicTimestamp {
    private val lock = ReentrantLock()
    private var lastTimestamp = 0L

    /**
     * Get next timestamp (guaranteed >= previous).
     */
    fun next(): Long = lock.withLock {
        val now = System.currentTimeMillis()
        lastTimestamp = if (now > lastTimestamp) now else lastTimestamp + 1
        lastTimestamp
    }

    /**
     * Get current value without advancing.
     */
    fun current(): Long = lock.withLock { lastTimestamp }
}

/**
 * Snowflake-like monotonic ID generator.
 */
class MonotonicID(
    private val nodeId: Int = 0,
    private val epoch: Long = 1704067200000L // 2024-01-01
) {
    private val lock = ReentrantLock()
    private var lastTimestamp = -1L
    private var sequence = 0L

    init {
        require(nodeId in 0..1023) { "Node ID must be 0-1023" }
    }

    /**
     * Generate next ID.
     */
    fun next(): Long = lock.withLock {
        var timestamp = System.currentTimeMillis() - epoch

        if (timestamp == lastTimestamp) {
            sequence = (sequence + 1) and 0xFFF
            if (sequence == 0L) {
                // Wait for next millisecond
                while (timestamp <= lastTimestamp) {
                    timestamp = System.currentTimeMillis() - epoch
                }
            }
        } else {
            sequence = 0
        }

        lastTimestamp = timestamp

        // Format: timestamp (41 bits) | nodeId (10 bits) | sequence (12 bits)
        (timestamp shl 22) or (nodeId.toLong() shl 12) or sequence
    }

    /**
     * Extract timestamp from ID.
     */
    fun extractTimestamp(id: Long): Long = (id shr 22) + epoch

    /**
     * Extract node ID from ID.
     */
    fun extractNodeId(id: Long): Int = ((id shr 12) and 0x3FF).toInt()

    /**
     * Extract sequence from ID.
     */
    fun extractSequence(id: Long): Int = (id and 0xFFF).toInt()
}

/**
 * Monotonic sequence for ordering.
 */
class MonotonicSequence<T>(
    private val comparator: Comparator<T>? = null
) {
    private val lock = ReentrantLock()
    private val items = mutableListOf<Pair<Long, T>>()
    private var sequenceNum = 0L

    /**
     * Add item to sequence.
     */
    fun add(item: T): Long = lock.withLock {
        val seq = sequenceNum++
        items.add(seq to item)
        seq
    }

    /**
     * Get item by sequence number.
     */
    fun get(seq: Long): T? = lock.withLock {
        items.find { it.first == seq }?.second
    }

    /**
     * Get all items in order.
     */
    fun getAll(): List<T> = lock.withLock {
        if (comparator != null) {
            items.sortedWith { a, b -> comparator.compare(a.second, b.second) }.map { it.second }
        } else {
            items.sortedBy { it.first }.map { it.second }
        }
    }

    /**
     * Get items after sequence number.
     */
    fun getAfter(seq: Long): List<T> = lock.withLock {
        items.filter { it.first > seq }.sortedBy { it.first }.map { it.second }
    }

    /**
     * Get current sequence number.
     */
    fun currentSequence(): Long = lock.withLock { sequenceNum - 1 }

    /**
     * Get count.
     */
    fun size(): Int = lock.withLock { items.size }

    /**
     * Clear sequence.
     */
    fun clear() = lock.withLock {
        items.clear()
        // Don't reset sequence number to maintain monotonicity
    }
}

/**
 * Monotonic version tracker.
 */
class MonotonicVersion {
    private val version = AtomicLong(0)

    /**
     * Get current version.
     */
    fun get(): Long = version.get()

    /**
     * Bump version and get new value.
     */
    fun bump(): Long = version.incrementAndGet()

    /**
     * Check if version matches.
     */
    fun matches(expectedVersion: Long): Boolean = version.get() == expectedVersion

    /**
     * Update only if version matches (optimistic locking).
     */
    fun compareAndBump(expectedVersion: Long): Boolean {
        return version.compareAndSet(expectedVersion, expectedVersion + 1)
    }

    /**
     * Set to specific version if higher.
     */
    fun setIfHigher(newVersion: Long): Boolean {
        while (true) {
            val current = version.get()
            if (newVersion <= current) return false
            if (version.compareAndSet(current, newVersion)) return true
        }
    }
}
