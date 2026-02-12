// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

// lruNode is a node in the doubly linked list.
type lruNode[K comparable, V any] struct {
	key   K
	value V
	prev  *lruNode[K, V]
	next  *lruNode[K, V]
}

// LRUCache is a bounded least-recently-used cache.
type LRUCache[K comparable, V any] struct {
	capacity int
	cache    map[K]*lruNode[K, V]
	head     *lruNode[K, V] // Most recently used
	tail     *lruNode[K, V] // Least recently used
	hits     int
	misses   int
}

// NewLRUCache creates an LRU cache with the given capacity.
func NewLRUCache[K comparable, V any](capacity int) *LRUCache[K, V] {
	if capacity < 1 {
		capacity = 1
	}
	return &LRUCache[K, V]{
		capacity: capacity,
		cache:    make(map[K]*lruNode[K, V]),
	}
}

// Get retrieves a value from the cache.
func (c *LRUCache[K, V]) Get(key K) (V, bool) {
	var zero V
	node, exists := c.cache[key]
	if !exists {
		c.misses++
		return zero, false
	}

	c.hits++
	c.moveToFront(node)
	return node.value, true
}

// Put adds or updates a value in the cache.
// Returns the evicted key and value if an eviction occurred.
func (c *LRUCache[K, V]) Put(key K, value V) (K, V, bool) {
	var evictedKey K
	var evictedValue V
	evicted := false

	if node, exists := c.cache[key]; exists {
		node.value = value
		c.moveToFront(node)
		return evictedKey, evictedValue, false
	}

	// Evict if at capacity
	if len(c.cache) >= c.capacity {
		evictedKey = c.tail.key
		evictedValue = c.tail.value
		evicted = true
		c.removeTail()
	}

	// Add new node
	node := &lruNode[K, V]{key: key, value: value}
	c.cache[key] = node
	c.addToFront(node)

	return evictedKey, evictedValue, evicted
}

// Remove removes a key from the cache.
func (c *LRUCache[K, V]) Remove(key K) (V, bool) {
	var zero V
	node, exists := c.cache[key]
	if !exists {
		return zero, false
	}

	c.removeNode(node)
	delete(c.cache, key)
	return node.value, true
}

// Contains checks if a key exists (without affecting LRU order).
func (c *LRUCache[K, V]) Contains(key K) bool {
	_, exists := c.cache[key]
	return exists
}

// Peek gets a value without affecting LRU order.
func (c *LRUCache[K, V]) Peek(key K) (V, bool) {
	var zero V
	node, exists := c.cache[key]
	if !exists {
		return zero, false
	}
	return node.value, true
}

// Len returns the current number of items.
func (c *LRUCache[K, V]) Len() int {
	return len(c.cache)
}

// Cap returns the capacity.
func (c *LRUCache[K, V]) Cap() int {
	return c.capacity
}

// Clear removes all items.
func (c *LRUCache[K, V]) Clear() {
	c.cache = make(map[K]*lruNode[K, V])
	c.head = nil
	c.tail = nil
}

// Keys returns all keys in LRU order (most recent first).
func (c *LRUCache[K, V]) Keys() []K {
	keys := make([]K, 0, len(c.cache))
	for node := c.head; node != nil; node = node.next {
		keys = append(keys, node.key)
	}
	return keys
}

// HitRate returns the cache hit rate.
func (c *LRUCache[K, V]) HitRate() float64 {
	total := c.hits + c.misses
	if total == 0 {
		return 0
	}
	return float64(c.hits) / float64(total)
}

// Stats returns cache statistics.
func (c *LRUCache[K, V]) Stats() (hits, misses int) {
	return c.hits, c.misses
}

// ResetStats resets hit/miss counters.
func (c *LRUCache[K, V]) ResetStats() {
	c.hits = 0
	c.misses = 0
}

func (c *LRUCache[K, V]) moveToFront(node *lruNode[K, V]) {
	if node == c.head {
		return
	}
	c.removeNode(node)
	c.addToFront(node)
}

func (c *LRUCache[K, V]) addToFront(node *lruNode[K, V]) {
	node.prev = nil
	node.next = c.head

	if c.head != nil {
		c.head.prev = node
	}
	c.head = node

	if c.tail == nil {
		c.tail = node
	}
}

func (c *LRUCache[K, V]) removeNode(node *lruNode[K, V]) {
	if node.prev != nil {
		node.prev.next = node.next
	} else {
		c.head = node.next
	}

	if node.next != nil {
		node.next.prev = node.prev
	} else {
		c.tail = node.prev
	}
}

func (c *LRUCache[K, V]) removeTail() {
	if c.tail == nil {
		return
	}
	delete(c.cache, c.tail.key)
	c.removeNode(c.tail)
}

// TTLLRUCache is an LRU cache with time-to-live support.
type TTLLRUCache[K comparable, V any] struct {
	cache      *LRUCache[K, ttlEntry[V]]
	defaultTTL int64 // Milliseconds
	getNow     func() int64
}

type ttlEntry[V any] struct {
	value     V
	expiresAt int64
}

// NewTTLLRUCache creates an LRU cache with TTL support.
func NewTTLLRUCache[K comparable, V any](capacity int, defaultTTLMs int64, getNow func() int64) *TTLLRUCache[K, V] {
	return &TTLLRUCache[K, V]{
		cache:      NewLRUCache[K, ttlEntry[V]](capacity),
		defaultTTL: defaultTTLMs,
		getNow:     getNow,
	}
}

// Get retrieves a value, returning false if expired or missing.
func (c *TTLLRUCache[K, V]) Get(key K) (V, bool) {
	var zero V
	entry, ok := c.cache.Get(key)
	if !ok {
		return zero, false
	}

	if c.getNow() > entry.expiresAt {
		c.cache.Remove(key)
		return zero, false
	}

	return entry.value, true
}

// Put adds a value with the default TTL.
func (c *TTLLRUCache[K, V]) Put(key K, value V) {
	c.PutWithTTL(key, value, c.defaultTTL)
}

// PutWithTTL adds a value with a specific TTL in milliseconds.
func (c *TTLLRUCache[K, V]) PutWithTTL(key K, value V, ttlMs int64) {
	entry := ttlEntry[V]{
		value:     value,
		expiresAt: c.getNow() + ttlMs,
	}
	c.cache.Put(key, entry)
}

// Remove removes a key.
func (c *TTLLRUCache[K, V]) Remove(key K) bool {
	_, ok := c.cache.Remove(key)
	return ok
}

// Len returns the current count (may include expired entries).
func (c *TTLLRUCache[K, V]) Len() int {
	return c.cache.Len()
}

// Clear removes all entries.
func (c *TTLLRUCache[K, V]) Clear() {
	c.cache.Clear()
}
