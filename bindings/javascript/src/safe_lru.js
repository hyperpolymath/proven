// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeLru - Least-recently-used cache.
 *
 * Provides thread-safe LRU cache with configurable capacity.
 * @module
 */

import { ok, err } from './result.js';

/**
 * LRU cache with fixed capacity.
 *
 * @template K, V
 */
export class LruCache {
  /** @type {Map<K, V>} */
  #cache;
  /** @type {number} */
  #capacity;
  /** @type {number} */
  #hits;
  /** @type {number} */
  #misses;

  /**
   * Create an LRU cache.
   *
   * @param {number} capacity - Maximum capacity
   */
  constructor(capacity) {
    if (capacity < 1) {
      throw new Error('Capacity must be at least 1');
    }
    this.#capacity = capacity;
    this.#cache = new Map();
    this.#hits = 0;
    this.#misses = 0;
  }

  /**
   * Get current size.
   *
   * @returns {number}
   */
  get size() {
    return this.#cache.size;
  }

  /**
   * Get capacity.
   *
   * @returns {number}
   */
  get capacity() {
    return this.#capacity;
  }

  /**
   * Get cache hit count.
   *
   * @returns {number}
   */
  get hits() {
    return this.#hits;
  }

  /**
   * Get cache miss count.
   *
   * @returns {number}
   */
  get misses() {
    return this.#misses;
  }

  /**
   * Get hit ratio.
   *
   * @returns {number}
   */
  get hitRatio() {
    const total = this.#hits + this.#misses;
    return total === 0 ? 0 : this.#hits / total;
  }

  /**
   * Get a value from the cache.
   *
   * @param {K} key - Key to get
   * @returns {V | undefined}
   */
  get(key) {
    if (!this.#cache.has(key)) {
      this.#misses++;
      return undefined;
    }

    // Move to end (most recently used)
    const value = this.#cache.get(key);
    this.#cache.delete(key);
    this.#cache.set(key, value);

    this.#hits++;
    return value;
  }

  /**
   * Check if key exists (without affecting LRU order).
   *
   * @param {K} key - Key to check
   * @returns {boolean}
   */
  has(key) {
    return this.#cache.has(key);
  }

  /**
   * Peek at a value without affecting LRU order.
   *
   * @param {K} key - Key to peek
   * @returns {V | undefined}
   */
  peek(key) {
    return this.#cache.get(key);
  }

  /**
   * Set a value in the cache.
   *
   * @param {K} key - Key to set
   * @param {V} value - Value to set
   * @returns {V | undefined} Evicted value, or undefined
   */
  set(key, value) {
    let evicted;

    // If key exists, delete it first (to update position)
    if (this.#cache.has(key)) {
      this.#cache.delete(key);
    } else if (this.#cache.size >= this.#capacity) {
      // Evict least recently used (first item)
      const firstKey = this.#cache.keys().next().value;
      evicted = this.#cache.get(firstKey);
      this.#cache.delete(firstKey);
    }

    this.#cache.set(key, value);
    return evicted;
  }

  /**
   * Delete a key from the cache.
   *
   * @param {K} key - Key to delete
   * @returns {boolean} True if key was present
   */
  delete(key) {
    return this.#cache.delete(key);
  }

  /**
   * Clear the cache.
   */
  clear() {
    this.#cache.clear();
    this.#hits = 0;
    this.#misses = 0;
  }

  /**
   * Reset statistics.
   */
  resetStats() {
    this.#hits = 0;
    this.#misses = 0;
  }

  /**
   * Get all keys (oldest first).
   *
   * @returns {K[]}
   */
  keys() {
    return Array.from(this.#cache.keys());
  }

  /**
   * Get all values (oldest first).
   *
   * @returns {V[]}
   */
  values() {
    return Array.from(this.#cache.values());
  }

  /**
   * Get all entries (oldest first).
   *
   * @returns {Array<[K, V]>}
   */
  entries() {
    return Array.from(this.#cache.entries());
  }

  /**
   * Iterate over entries (oldest first).
   *
   * @param {(value: V, key: K) => void} callback - Callback function
   */
  forEach(callback) {
    this.#cache.forEach(callback);
  }

  /**
   * Get or compute a value.
   *
   * @param {K} key - Key to get
   * @param {() => V} factory - Factory function if key not found
   * @returns {V}
   */
  getOrCompute(key, factory) {
    const existing = this.get(key);
    if (existing !== undefined) {
      return existing;
    }

    const value = factory();
    this.set(key, value);
    return value;
  }

  /**
   * Get or compute a value (async).
   *
   * @param {K} key - Key to get
   * @param {() => Promise<V>} factory - Async factory function if key not found
   * @returns {Promise<V>}
   */
  async getOrComputeAsync(key, factory) {
    const existing = this.get(key);
    if (existing !== undefined) {
      return existing;
    }

    const value = await factory();
    this.set(key, value);
    return value;
  }

  /**
   * Resize the cache.
   *
   * @param {number} newCapacity - New capacity
   * @returns {V[]} Evicted values
   */
  resize(newCapacity) {
    if (newCapacity < 1) {
      throw new Error('Capacity must be at least 1');
    }

    const evicted = [];
    while (this.#cache.size > newCapacity) {
      const firstKey = this.#cache.keys().next().value;
      evicted.push(this.#cache.get(firstKey));
      this.#cache.delete(firstKey);
    }

    this.#capacity = newCapacity;
    return evicted;
  }

  /**
   * Iterate over entries.
   *
   * @returns {IterableIterator<[K, V]>}
   */
  *[Symbol.iterator]() {
    yield* this.#cache;
  }
}
