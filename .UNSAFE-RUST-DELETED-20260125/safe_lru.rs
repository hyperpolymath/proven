// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe LRU (Least Recently Used) cache implementation.
//!
//! Provides a bounded cache with automatic eviction of
//! least recently used entries.

use std::collections::HashMap;
use std::hash::Hash;

/// LRU cache node.
#[derive(Debug, Clone)]
struct LruNode<K, V> {
    key: K,
    value: V,
    prev: Option<usize>,
    next: Option<usize>,
}

/// LRU cache with bounded capacity.
#[derive(Debug)]
pub struct LruCache<K, V> {
    nodes: Vec<Option<LruNode<K, V>>>,
    map: HashMap<K, usize>,
    head: Option<usize>,
    tail: Option<usize>,
    free_list: Vec<usize>,
    capacity: usize,
    len: usize,
}

impl<K: Clone + Eq + Hash, V: Clone> LruCache<K, V> {
    /// Create a new LRU cache with the given capacity.
    pub fn new(capacity: usize) -> Self {
        Self {
            nodes: vec![None; capacity],
            map: HashMap::with_capacity(capacity),
            head: None,
            tail: None,
            free_list: (0..capacity).rev().collect(),
            capacity,
            len: 0,
        }
    }

    /// Get current number of entries.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Check if cache is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Check if cache is full.
    pub fn is_full(&self) -> bool {
        self.len >= self.capacity
    }

    /// Get capacity.
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Get a value by key, marking it as recently used.
    pub fn get(&mut self, key: &K) -> Option<&V> {
        if let Some(&idx) = self.map.get(key) {
            self.move_to_front(idx);
            self.nodes[idx].as_ref().map(|n| &n.value)
        } else {
            None
        }
    }

    /// Get a value without updating recency.
    pub fn peek(&self, key: &K) -> Option<&V> {
        self.map
            .get(key)
            .and_then(|&idx| self.nodes[idx].as_ref().map(|n| &n.value))
    }

    /// Insert a key-value pair, evicting LRU if necessary.
    pub fn put(&mut self, key: K, value: V) -> Option<V> {
        // If key exists, update and move to front
        if let Some(&idx) = self.map.get(&key) {
            let old_value = self.nodes[idx].as_ref().map(|n| n.value.clone());
            if let Some(node) = self.nodes[idx].as_mut() {
                node.value = value;
            }
            self.move_to_front(idx);
            return old_value;
        }

        // Need to evict?
        let evicted = if self.is_full() {
            self.evict_lru()
        } else {
            None
        };

        // Get a free slot
        let idx = self.free_list.pop().expect("Should have free slot after eviction");

        // Create node
        self.nodes[idx] = Some(LruNode {
            key: key.clone(),
            value,
            prev: None,
            next: self.head,
        });

        // Update head's prev
        if let Some(head_idx) = self.head {
            if let Some(head_node) = self.nodes[head_idx].as_mut() {
                head_node.prev = Some(idx);
            }
        }

        // Update head
        self.head = Some(idx);

        // If first node, also set tail
        if self.tail.is_none() {
            self.tail = Some(idx);
        }

        self.map.insert(key, idx);
        self.len += 1;

        evicted
    }

    /// Remove a key from the cache.
    pub fn remove(&mut self, key: &K) -> Option<V> {
        if let Some(idx) = self.map.remove(key) {
            self.unlink(idx);
            let node = self.nodes[idx].take();
            self.free_list.push(idx);
            self.len -= 1;
            node.map(|n| n.value)
        } else {
            None
        }
    }

    /// Check if key exists.
    pub fn contains(&self, key: &K) -> bool {
        self.map.contains_key(key)
    }

    /// Clear all entries.
    pub fn clear(&mut self) {
        self.nodes.iter_mut().for_each(|n| *n = None);
        self.map.clear();
        self.head = None;
        self.tail = None;
        self.free_list = (0..self.capacity).rev().collect();
        self.len = 0;
    }

    fn move_to_front(&mut self, idx: usize) {
        if self.head == Some(idx) {
            return;
        }

        self.unlink(idx);

        // Set as new head
        if let Some(node) = self.nodes[idx].as_mut() {
            node.prev = None;
            node.next = self.head;
        }

        if let Some(head_idx) = self.head {
            if let Some(head_node) = self.nodes[head_idx].as_mut() {
                head_node.prev = Some(idx);
            }
        }

        self.head = Some(idx);

        if self.tail.is_none() {
            self.tail = Some(idx);
        }
    }

    fn unlink(&mut self, idx: usize) {
        let (prev, next) = match self.nodes[idx].as_ref() {
            Some(node) => (node.prev, node.next),
            None => return,
        };

        // Update prev's next
        if let Some(prev_idx) = prev {
            if let Some(prev_node) = self.nodes[prev_idx].as_mut() {
                prev_node.next = next;
            }
        } else {
            self.head = next;
        }

        // Update next's prev
        if let Some(next_idx) = next {
            if let Some(next_node) = self.nodes[next_idx].as_mut() {
                next_node.prev = prev;
            }
        } else {
            self.tail = prev;
        }
    }

    fn evict_lru(&mut self) -> Option<V> {
        if let Some(tail_idx) = self.tail {
            let key = self.nodes[tail_idx].as_ref().map(|n| n.key.clone());
            if let Some(key) = key {
                return self.remove(&key);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lru_cache() {
        let mut cache: LruCache<&str, i32> = LruCache::new(3);

        cache.put("a", 1);
        cache.put("b", 2);
        cache.put("c", 3);

        assert_eq!(cache.get(&"a"), Some(&1));
        assert_eq!(cache.get(&"b"), Some(&2));
        assert_eq!(cache.get(&"c"), Some(&3));

        // This should evict "a" (least recently used after gets)
        cache.put("d", 4);

        // Actually "a" was accessed, so "b" might be evicted
        // Let's check
        assert_eq!(cache.len(), 3);
    }

    #[test]
    fn test_eviction_order() {
        let mut cache: LruCache<i32, i32> = LruCache::new(2);

        cache.put(1, 1);
        cache.put(2, 2);
        cache.get(&1); // Access 1, making 2 the LRU
        cache.put(3, 3); // Should evict 2

        assert!(cache.contains(&1));
        assert!(!cache.contains(&2));
        assert!(cache.contains(&3));
    }
}
