// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlin.math.abs
import kotlin.math.ceil
import kotlin.math.ln
import kotlin.math.pow

/**
 * Bloom filter for probabilistic membership testing.
 */
class BloomFilter(
    val capacity: Int,
    val falsePositiveRate: Double = 0.01
) {
    private val numBits: Int
    private val numHashes: Int
    private val bits: BooleanArray
    private var itemCount = 0

    init {
        require(capacity > 0) { "Capacity must be positive" }
        require(falsePositiveRate > 0 && falsePositiveRate < 1) { "False positive rate must be between 0 and 1" }

        // Calculate optimal bit array size: m = -n*ln(p) / (ln(2)^2)
        numBits = ceil(-capacity * ln(falsePositiveRate) / (ln(2.0).pow(2))).toInt()
        // Calculate optimal number of hash functions: k = (m/n) * ln(2)
        numHashes = ceil((numBits.toDouble() / capacity) * ln(2.0)).toInt()
        bits = BooleanArray(numBits)
    }

    val size: Int get() = itemCount

    /**
     * Add an item to the filter.
     */
    fun add(item: String) {
        for (i in 0 until numHashes) {
            val index = hash(item, i)
            bits[index] = true
        }
        itemCount++
    }

    /**
     * Add multiple items.
     */
    fun addAll(items: Collection<String>) {
        for (item in items) add(item)
    }

    /**
     * Check if an item might be in the set.
     * Returns true if possibly present, false if definitely not present.
     */
    fun mightContain(item: String): Boolean {
        for (i in 0 until numHashes) {
            val index = hash(item, i)
            if (!bits[index]) return false
        }
        return true
    }

    /**
     * Check if an item is definitely not in the set.
     */
    fun definitelyNotContains(item: String): Boolean = !mightContain(item)

    /**
     * Estimate current false positive rate.
     */
    fun estimatedFalsePositiveRate(): Double {
        val setBits = bits.count { it }
        val fillRatio = setBits.toDouble() / numBits
        return fillRatio.pow(numHashes.toDouble())
    }

    /**
     * Clear the filter.
     */
    fun clear() {
        bits.fill(false)
        itemCount = 0
    }

    private fun hash(item: String, seed: Int): Int {
        // Use two base hashes to simulate multiple hash functions
        val h1 = item.hashCode()
        val h2 = murmurHash(item.toByteArray(), seed)
        return abs((h1 + seed * h2) % numBits)
    }

    private fun murmurHash(data: ByteArray, seed: Int): Int {
        var h = seed
        for (b in data) {
            h = h xor b.toInt()
            h = h * 0x5bd1e995
            h = h xor (h ushr 15)
        }
        return h
    }
}

/**
 * Counting Bloom filter supporting deletions.
 */
class CountingBloomFilter(
    val capacity: Int,
    val falsePositiveRate: Double = 0.01
) {
    private val numBits: Int
    private val numHashes: Int
    private val counters: IntArray
    private var itemCount = 0

    init {
        require(capacity > 0) { "Capacity must be positive" }
        require(falsePositiveRate > 0 && falsePositiveRate < 1) { "False positive rate must be between 0 and 1" }

        numBits = ceil(-capacity * ln(falsePositiveRate) / (ln(2.0).pow(2))).toInt()
        numHashes = ceil((numBits.toDouble() / capacity) * ln(2.0)).toInt()
        counters = IntArray(numBits)
    }

    val size: Int get() = itemCount

    /**
     * Add an item.
     */
    fun add(item: String) {
        for (i in 0 until numHashes) {
            val index = hash(item, i)
            if (counters[index] < Int.MAX_VALUE) {
                counters[index]++
            }
        }
        itemCount++
    }

    /**
     * Remove an item.
     */
    fun remove(item: String): Boolean {
        if (!mightContain(item)) return false

        for (i in 0 until numHashes) {
            val index = hash(item, i)
            if (counters[index] > 0) {
                counters[index]--
            }
        }
        itemCount = maxOf(0, itemCount - 1)
        return true
    }

    /**
     * Check if an item might be in the set.
     */
    fun mightContain(item: String): Boolean {
        for (i in 0 until numHashes) {
            val index = hash(item, i)
            if (counters[index] == 0) return false
        }
        return true
    }

    /**
     * Get count estimate for an item.
     */
    fun getCount(item: String): Int {
        var minCount = Int.MAX_VALUE
        for (i in 0 until numHashes) {
            val index = hash(item, i)
            minCount = minOf(minCount, counters[index])
        }
        return minCount
    }

    /**
     * Clear the filter.
     */
    fun clear() {
        counters.fill(0)
        itemCount = 0
    }

    private fun hash(item: String, seed: Int): Int {
        val h1 = item.hashCode()
        val h2 = murmurHash(item.toByteArray(), seed)
        return abs((h1 + seed * h2) % numBits)
    }

    private fun murmurHash(data: ByteArray, seed: Int): Int {
        var h = seed
        for (b in data) {
            h = h xor b.toInt()
            h = h * 0x5bd1e995
            h = h xor (h ushr 15)
        }
        return h
    }
}
