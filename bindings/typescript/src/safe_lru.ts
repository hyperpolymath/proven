// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

interface LRUNode<K, V> {
  key: K;
  value: V;
  prev?: LRUNode<K, V>;
  next?: LRUNode<K, V>;
}

/**
 * LRUCache is a bounded least-recently-used cache.
 */
export class LRUCache<K, V> {
  private readonly capacity: number;
  private readonly cache: Map<K, LRUNode<K, V>>;
  private head?: LRUNode<K, V>;
  private tail?: LRUNode<K, V>;
  private hits: number = 0;
  private misses: number = 0;

  constructor(capacity: number) {
    this.capacity = Math.max(1, capacity);
    this.cache = new Map();
  }

  /**
   * Get a value from the cache.
   */
  get(key: K): Result<V> {
    const node = this.cache.get(key);
    if (!node) {
      this.misses++;
      return { ok: false, error: 'Key not found' };
    }

    this.hits++;
    this.moveToFront(node);
    return { ok: true, value: node.value };
  }

  /**
   * Put a value in the cache.
   * Returns the evicted entry if any.
   */
  put(key: K, value: V): { evicted?: { key: K; value: V } } {
    const existing = this.cache.get(key);
    if (existing) {
      existing.value = value;
      this.moveToFront(existing);
      return {};
    }

    let evicted: { key: K; value: V } | undefined;

    // Evict if at capacity
    if (this.cache.size >= this.capacity && this.tail) {
      evicted = { key: this.tail.key, value: this.tail.value };
      this.cache.delete(this.tail.key);
      this.removeTail();
    }

    // Add new node
    const node: LRUNode<K, V> = { key, value };
    this.cache.set(key, node);
    this.addToFront(node);

    return { evicted };
  }

  /**
   * Remove a key from the cache.
   */
  remove(key: K): Result<V> {
    const node = this.cache.get(key);
    if (!node) {
      return { ok: false, error: 'Key not found' };
    }

    this.removeNode(node);
    this.cache.delete(key);
    return { ok: true, value: node.value };
  }

  /**
   * Check if a key exists (without affecting LRU order).
   */
  has(key: K): boolean {
    return this.cache.has(key);
  }

  /**
   * Peek at a value without affecting LRU order.
   */
  peek(key: K): Result<V> {
    const node = this.cache.get(key);
    if (!node) {
      return { ok: false, error: 'Key not found' };
    }
    return { ok: true, value: node.value };
  }

  /**
   * Get current size.
   */
  get size(): number {
    return this.cache.size;
  }

  /**
   * Get capacity.
   */
  get cap(): number {
    return this.capacity;
  }

  /**
   * Clear the cache.
   */
  clear(): void {
    this.cache.clear();
    this.head = undefined;
    this.tail = undefined;
  }

  /**
   * Get all keys in LRU order (most recent first).
   */
  keys(): K[] {
    const result: K[] = [];
    let node = this.head;
    while (node) {
      result.push(node.key);
      node = node.next;
    }
    return result;
  }

  /**
   * Get cache hit rate.
   */
  hitRate(): number {
    const total = this.hits + this.misses;
    return total === 0 ? 0 : this.hits / total;
  }

  /**
   * Get cache statistics.
   */
  stats(): { hits: number; misses: number; hitRate: number } {
    return {
      hits: this.hits,
      misses: this.misses,
      hitRate: this.hitRate(),
    };
  }

  /**
   * Reset hit/miss counters.
   */
  resetStats(): void {
    this.hits = 0;
    this.misses = 0;
  }

  private moveToFront(node: LRUNode<K, V>): void {
    if (node === this.head) return;
    this.removeNode(node);
    this.addToFront(node);
  }

  private addToFront(node: LRUNode<K, V>): void {
    node.prev = undefined;
    node.next = this.head;

    if (this.head) {
      this.head.prev = node;
    }
    this.head = node;

    if (!this.tail) {
      this.tail = node;
    }
  }

  private removeNode(node: LRUNode<K, V>): void {
    if (node.prev) {
      node.prev.next = node.next;
    } else {
      this.head = node.next;
    }

    if (node.next) {
      node.next.prev = node.prev;
    } else {
      this.tail = node.prev;
    }
  }

  private removeTail(): void {
    if (!this.tail) return;
    this.removeNode(this.tail);
  }
}

interface TTLEntry<V> {
  value: V;
  expiresAt: number;
}

/**
 * TTLLRUCache is an LRU cache with time-to-live support.
 */
export class TTLLRUCache<K, V> {
  private readonly cache: LRUCache<K, TTLEntry<V>>;
  private readonly defaultTTL: number;
  private readonly getNow: () => number;

  /**
   * Create a TTL LRU cache.
   * @param capacity Maximum capacity
   * @param defaultTTLMs Default TTL in milliseconds
   * @param getNow Function to get current time (for testing)
   */
  constructor(capacity: number, defaultTTLMs: number, getNow: () => number = Date.now) {
    this.cache = new LRUCache(capacity);
    this.defaultTTL = defaultTTLMs;
    this.getNow = getNow;
  }

  /**
   * Get a value from the cache.
   */
  get(key: K): Result<V> {
    const result = this.cache.get(key);
    if (!result.ok) {
      return { ok: false, error: result.error };
    }

    const entry = result.value!;
    if (this.getNow() > entry.expiresAt) {
      this.cache.remove(key);
      return { ok: false, error: 'Entry expired' };
    }

    return { ok: true, value: entry.value };
  }

  /**
   * Put a value with default TTL.
   */
  put(key: K, value: V): void {
    this.putWithTTL(key, value, this.defaultTTL);
  }

  /**
   * Put a value with specific TTL.
   */
  putWithTTL(key: K, value: V, ttlMs: number): void {
    const entry: TTLEntry<V> = {
      value,
      expiresAt: this.getNow() + ttlMs,
    };
    this.cache.put(key, entry);
  }

  /**
   * Remove a key.
   */
  remove(key: K): boolean {
    return this.cache.remove(key).ok;
  }

  /**
   * Get current size (may include expired entries).
   */
  get size(): number {
    return this.cache.size;
  }

  /**
   * Clear the cache.
   */
  clear(): void {
    this.cache.clear();
  }
}

export const SafeLRU = {
  LRUCache,
  TTLLRUCache,
};
