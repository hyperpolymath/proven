# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeLru - Least-recently-used cache with bounded size.

Provides a thread-safe LRU cache implementation.
"""

from typing import TypeVar, Generic, Optional, Dict, List, Tuple
from collections import OrderedDict
import threading

K = TypeVar("K")
V = TypeVar("V")


class LruCache(Generic[K, V]):
    """
    Least-recently-used cache with automatic eviction.

    Example:
        >>> cache = LruCache(3)
        >>> cache.put("a", 1)
        >>> cache.put("b", 2)
        >>> cache.put("c", 3)
        >>> cache.get("a")  # Moves "a" to most recent
        1
        >>> cache.put("d", 4)  # Evicts "b" (least recently used)
        >>> cache.get("b")
        None
    """

    def __init__(self, capacity: int):
        """
        Create an LRU cache.

        Args:
            capacity: Maximum number of items

        Raises:
            ValueError: If capacity <= 0
        """
        if capacity <= 0:
            raise ValueError("Capacity must be positive")
        self._capacity = capacity
        self._cache: OrderedDict[K, V] = OrderedDict()
        self._lock = threading.Lock()
        self._hits = 0
        self._misses = 0

    @property
    def capacity(self) -> int:
        """Get maximum capacity."""
        return self._capacity

    def __len__(self) -> int:
        """Get current number of items."""
        with self._lock:
            return len(self._cache)

    def get(self, key: K) -> Optional[V]:
        """
        Get value by key, marking it as recently used.

        Args:
            key: Key to look up

        Returns:
            Value, or None if not found
        """
        with self._lock:
            if key not in self._cache:
                self._misses += 1
                return None
            # Move to end (most recently used)
            self._cache.move_to_end(key)
            self._hits += 1
            return self._cache[key]

    def put(self, key: K, value: V) -> Optional[Tuple[K, V]]:
        """
        Insert or update a value.

        Args:
            key: Key
            value: Value

        Returns:
            Evicted (key, value) tuple if eviction occurred, None otherwise
        """
        with self._lock:
            evicted = None

            if key in self._cache:
                # Update existing
                self._cache.move_to_end(key)
                self._cache[key] = value
            else:
                # Insert new
                if len(self._cache) >= self._capacity:
                    # Evict oldest
                    oldest_key, oldest_value = self._cache.popitem(last=False)
                    evicted = (oldest_key, oldest_value)
                self._cache[key] = value

            return evicted

    def remove(self, key: K) -> Optional[V]:
        """
        Remove and return value by key.

        Args:
            key: Key to remove

        Returns:
            Value, or None if not found
        """
        with self._lock:
            if key not in self._cache:
                return None
            return self._cache.pop(key)

    def contains(self, key: K) -> bool:
        """
        Check if key exists (does not update LRU order).

        Args:
            key: Key to check

        Returns:
            True if exists
        """
        with self._lock:
            return key in self._cache

    def __contains__(self, key: K) -> bool:
        """Support 'in' operator."""
        return self.contains(key)

    def peek(self, key: K) -> Optional[V]:
        """
        Get value without updating LRU order.

        Args:
            key: Key to look up

        Returns:
            Value, or None if not found
        """
        with self._lock:
            return self._cache.get(key)

    def clear(self) -> None:
        """Remove all items."""
        with self._lock:
            self._cache.clear()

    def keys(self) -> List[K]:
        """Get all keys (oldest to newest)."""
        with self._lock:
            return list(self._cache.keys())

    def values(self) -> List[V]:
        """Get all values (oldest to newest)."""
        with self._lock:
            return list(self._cache.values())

    def items(self) -> List[Tuple[K, V]]:
        """Get all items (oldest to newest)."""
        with self._lock:
            return list(self._cache.items())

    def stats(self) -> Dict[str, int]:
        """
        Get cache statistics.

        Returns:
            Dict with hits, misses, size, capacity
        """
        with self._lock:
            return {
                "hits": self._hits,
                "misses": self._misses,
                "size": len(self._cache),
                "capacity": self._capacity,
            }

    def hit_rate(self) -> float:
        """
        Get cache hit rate.

        Returns:
            Hit rate (0.0-1.0), or 0.0 if no accesses
        """
        with self._lock:
            total = self._hits + self._misses
            return self._hits / total if total > 0 else 0.0
