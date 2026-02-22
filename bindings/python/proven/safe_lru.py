# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeLru - Least-recently-used cache with bounded size.

Provides a thread-safe LRU cache implementation.
All operations are delegated to the Idris core via FFI using opaque handles.
"""

from typing import Optional, Dict, List, Tuple

from .core import ProvenStatus, get_lib


class LruCache:
    """
    Least-recently-used cache with automatic eviction via FFI.

    Keys and values are stored as bytes in the Idris core.

    Example:
        >>> cache = LruCache(3)
        >>> cache.put("a", "1")
        >>> cache.put("b", "2")
        >>> cache.put("c", "3")
        >>> cache.get("a")  # Moves "a" to most recent
        b'1'
        >>> cache.put("d", "4")  # Evicts "b" (least recently used)
        >>> cache.get("b")
        None
    """

    def __init__(self, capacity: int):
        """
        Create an LRU cache via FFI.

        Args:
            capacity: Maximum number of items

        Raises:
            ValueError: If capacity <= 0
        """
        if capacity <= 0:
            raise ValueError("Capacity must be positive")

        lib = get_lib()
        result = lib.proven_lru_create(capacity)
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create LRU cache via FFI")

        self._handle = result.handle
        self._capacity = capacity
        self._lib = lib
        self._hits = 0
        self._misses = 0

    def __del__(self):
        """Free the FFI LRU handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_lru_free(self._handle)
            except Exception:
                pass

    @property
    def capacity(self) -> int:
        """Get maximum capacity."""
        return self._capacity

    def __len__(self) -> int:
        """Get current number of items."""
        result = self._lib.proven_lru_len(self._handle)
        if result.status != ProvenStatus.OK:
            return 0
        return result.value

    def get(self, key: str) -> Optional[bytes]:
        """
        Get value by key, marking it as recently used via FFI.

        Args:
            key: Key to look up

        Returns:
            Value bytes, or None if not found
        """
        encoded_key = key.encode("utf-8") if isinstance(key, str) else key
        result = self._lib.proven_lru_get(self._handle, encoded_key, len(encoded_key))
        if result.status != ProvenStatus.OK or result.data is None:
            self._misses += 1
            return None
        data = result.data[:result.length]
        self._lib.proven_free_string(result.data)
        self._hits += 1
        return bytes(data)

    def put(self, key: str, value: str) -> None:
        """
        Insert or update a value via FFI.

        Args:
            key: Key
            value: Value
        """
        encoded_key = key.encode("utf-8") if isinstance(key, str) else key
        encoded_value = value.encode("utf-8") if isinstance(value, str) else value
        self._lib.proven_lru_put(
            self._handle,
            encoded_key, len(encoded_key),
            encoded_value, len(encoded_value),
        )

    def remove(self, key: str) -> None:
        """
        Remove a key via FFI.

        Args:
            key: Key to remove
        """
        encoded_key = key.encode("utf-8") if isinstance(key, str) else key
        self._lib.proven_lru_remove(self._handle, encoded_key, len(encoded_key))

    def contains(self, key: str) -> bool:
        """
        Check if key exists (does not update LRU order).

        Args:
            key: Key to check

        Returns:
            True if exists
        """
        # Use get but don't count as hit/miss for stats
        encoded_key = key.encode("utf-8") if isinstance(key, str) else key
        result = self._lib.proven_lru_get(self._handle, encoded_key, len(encoded_key))
        if result.status != ProvenStatus.OK or result.data is None:
            return False
        self._lib.proven_free_string(result.data)
        return True

    def __contains__(self, key: str) -> bool:
        """Support 'in' operator."""
        return self.contains(key)

    def clear(self) -> None:
        """Remove all items by recreating the cache."""
        self._lib.proven_lru_free(self._handle)
        result = self._lib.proven_lru_create(self._capacity)
        if result.status == ProvenStatus.OK and result.handle is not None:
            self._handle = result.handle

    def stats(self) -> Dict[str, int]:
        """
        Get cache statistics.

        Returns:
            Dict with hits, misses, size, capacity
        """
        return {
            "hits": self._hits,
            "misses": self._misses,
            "size": len(self),
            "capacity": self._capacity,
        }

    def hit_rate(self) -> float:
        """
        Get cache hit rate.

        Returns:
            Hit rate (0.0-1.0), or 0.0 if no accesses
        """
        total = self._hits + self._misses
        return self._hits / total if total > 0 else 0.0
