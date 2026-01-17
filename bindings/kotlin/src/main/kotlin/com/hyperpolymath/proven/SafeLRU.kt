// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

/**
 * LRU (Least Recently Used) cache.
 */
class LRUCache<K, V>(val capacity: Int) {
    private val lock = ReentrantLock()
    private val cache = LinkedHashMap<K, V>(capacity, 0.75f, true)

    init {
        require(capacity > 0) { "Capacity must be positive" }
    }

    val size: Int get() = lock.withLock { cache.size }

    /**
     * Get value for key.
     */
    fun get(key: K): V? = lock.withLock { cache[key] }

    /**
     * Set value for key. Returns evicted entry if any.
     */
    fun set(key: K, value: V): Pair<K, V>? = lock.withLock {
        val evicted = if (cache.size >= capacity && !cache.containsKey(key)) {
            val oldestKey = cache.keys.first()
            val oldestValue = cache.remove(oldestKey)!!
            oldestKey to oldestValue
        } else null

        cache[key] = value
        evicted
    }

    /**
     * Check if key exists.
     */
    fun contains(key: K): Boolean = lock.withLock { cache.containsKey(key) }

    /**
     * Remove key.
     */
    fun remove(key: K): V? = lock.withLock { cache.remove(key) }

    /**
     * Clear the cache.
     */
    fun clear() = lock.withLock { cache.clear() }

    /**
     * Get all keys.
     */
    fun keys(): Set<K> = lock.withLock { cache.keys.toSet() }

    /**
     * Get all entries.
     */
    fun entries(): List<Pair<K, V>> = lock.withLock { cache.entries.map { it.key to it.value } }

    /**
     * Get or compute value.
     */
    fun getOrPut(key: K, compute: () -> V): V = lock.withLock {
        cache[key] ?: compute().also { set(key, it) }
    }
}

/**
 * LRU cache with TTL (Time To Live).
 */
class TTLLRUCache<K, V>(
    val capacity: Int,
    val defaultTTLMillis: Long
) {
    private data class Entry<V>(val value: V, val expiresAt: Long)

    private val lock = ReentrantLock()
    private val cache = LinkedHashMap<K, Entry<V>>(capacity, 0.75f, true)

    init {
        require(capacity > 0) { "Capacity must be positive" }
        require(defaultTTLMillis > 0) { "TTL must be positive" }
    }

    val size: Int get() = lock.withLock {
        cleanup()
        cache.size
    }

    /**
     * Get value for key if not expired.
     */
    fun get(key: K): V? = lock.withLock {
        cleanup()
        val entry = cache[key] ?: return null
        if (System.currentTimeMillis() > entry.expiresAt) {
            cache.remove(key)
            null
        } else {
            entry.value
        }
    }

    /**
     * Set value with custom TTL.
     */
    fun set(key: K, value: V, ttlMillis: Long = defaultTTLMillis): Pair<K, V>? = lock.withLock {
        cleanup()

        val evicted = if (cache.size >= capacity && !cache.containsKey(key)) {
            val oldestKey = cache.keys.first()
            val oldestEntry = cache.remove(oldestKey)!!
            oldestKey to oldestEntry.value
        } else null

        val expiresAt = System.currentTimeMillis() + ttlMillis
        cache[key] = Entry(value, expiresAt)
        evicted
    }

    /**
     * Check if key exists and is not expired.
     */
    fun contains(key: K): Boolean = lock.withLock {
        val entry = cache[key] ?: return false
        if (System.currentTimeMillis() > entry.expiresAt) {
            cache.remove(key)
            false
        } else {
            true
        }
    }

    /**
     * Remove key.
     */
    fun remove(key: K): V? = lock.withLock {
        cache.remove(key)?.value
    }

    /**
     * Clear the cache.
     */
    fun clear() = lock.withLock { cache.clear() }

    /**
     * Get TTL remaining for key in milliseconds.
     */
    fun ttl(key: K): Long? = lock.withLock {
        val entry = cache[key] ?: return null
        val remaining = entry.expiresAt - System.currentTimeMillis()
        if (remaining <= 0) {
            cache.remove(key)
            null
        } else {
            remaining
        }
    }

    /**
     * Refresh TTL for key.
     */
    fun touch(key: K, ttlMillis: Long = defaultTTLMillis): Boolean = lock.withLock {
        val entry = cache[key] ?: return false
        val now = System.currentTimeMillis()
        if (now > entry.expiresAt) {
            cache.remove(key)
            return false
        }
        cache[key] = Entry(entry.value, now + ttlMillis)
        true
    }

    /**
     * Get all non-expired entries.
     */
    fun entries(): List<Pair<K, V>> = lock.withLock {
        cleanup()
        cache.entries.map { it.key to it.value.value }
    }

    private fun cleanup() {
        val now = System.currentTimeMillis()
        val iterator = cache.entries.iterator()
        while (iterator.hasNext()) {
            if (now > iterator.next().value.expiresAt) {
                iterator.remove()
            }
        }
    }
}
