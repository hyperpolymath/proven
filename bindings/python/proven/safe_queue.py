# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeQueue - Bounded FIFO and priority queues without overflow.

Provides safe queue operations with capacity limits.
All operations are delegated to the Idris core via FFI using opaque handles.
"""

from typing import TypeVar, Optional, List

from .core import ProvenStatus, get_lib

T = TypeVar("T")


class BoundedQueue:
    """
    Bounded FIFO queue via FFI.

    Example:
        >>> q = BoundedQueue(3)
        >>> q.push(b"first")
        True
        >>> q.push(b"second")
        True
        >>> q.pop()
        b'first'
    """

    def __init__(self, capacity: int):
        """
        Create a bounded queue via FFI.

        Args:
            capacity: Maximum number of items

        Raises:
            ValueError: If capacity <= 0
        """
        if capacity <= 0:
            raise ValueError("Capacity must be positive")

        lib = get_lib()
        result = lib.proven_queue_create(capacity)
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create queue via FFI")

        self._handle = result.handle
        self._capacity = capacity
        self._lib = lib

    def __del__(self):
        """Free the FFI queue handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_queue_free(self._handle)
            except Exception:
                pass

    @property
    def capacity(self) -> int:
        """Get maximum capacity."""
        return self._capacity

    def __len__(self) -> int:
        """Get current number of items."""
        result = self._lib.proven_queue_len(self._handle)
        if result.status != ProvenStatus.OK:
            return 0
        return result.value

    def is_empty(self) -> bool:
        """Check if queue is empty."""
        return len(self) == 0

    def is_full(self) -> bool:
        """Check if queue is at capacity."""
        return len(self) >= self._capacity

    def push(self, item: bytes) -> bool:
        """
        Add item to back of queue via FFI.

        Args:
            item: Bytes item to add

        Returns:
            True if added, False if full
        """
        if not isinstance(item, bytes):
            item = str(item).encode("utf-8")
        result = self._lib.proven_queue_push(self._handle, item, len(item))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    def pop(self) -> Optional[bytes]:
        """
        Remove and return front item via FFI.

        Returns:
            Item bytes, or None if empty
        """
        result = self._lib.proven_queue_pop(self._handle)
        if result.status != ProvenStatus.OK or result.data is None:
            return None
        data = result.data[:result.length]
        self._lib.proven_free_string(result.data)
        return bytes(data)

    def clear(self) -> None:
        """Remove all items."""
        self._lib.proven_queue_free(self._handle)
        result = self._lib.proven_queue_create(self._capacity)
        if result.status == ProvenStatus.OK and result.handle is not None:
            self._handle = result.handle


class PriorityQueue:
    """
    Bounded priority queue via FFI.

    Uses the queue FFI handle with priority semantics
    managed by the Idris core.

    Example:
        >>> pq = PriorityQueue(5)
        >>> pq.push(3, b"medium")
        True
        >>> pq.push(1, b"high")
        True
        >>> pq.pop()
        b'high'
    """

    def __init__(self, capacity: int, max_heap: bool = False):
        """
        Create a bounded priority queue via FFI.

        Args:
            capacity: Maximum number of items
            max_heap: If True, highest priority first

        Raises:
            ValueError: If capacity <= 0
        """
        if capacity <= 0:
            raise ValueError("Capacity must be positive")

        lib = get_lib()
        result = lib.proven_queue_create(capacity)
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create priority queue via FFI")

        self._handle = result.handle
        self._capacity = capacity
        self._max_heap = max_heap
        self._lib = lib

    def __del__(self):
        """Free the FFI queue handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_queue_free(self._handle)
            except Exception:
                pass

    @property
    def capacity(self) -> int:
        """Get maximum capacity."""
        return self._capacity

    def __len__(self) -> int:
        """Get current number of items."""
        result = self._lib.proven_queue_len(self._handle)
        if result.status != ProvenStatus.OK:
            return 0
        return result.value

    def is_empty(self) -> bool:
        """Check if queue is empty."""
        return len(self) == 0

    def is_full(self) -> bool:
        """Check if queue is at capacity."""
        return len(self) >= self._capacity

    def push(self, priority: float, item: bytes) -> bool:
        """
        Add item with priority via FFI.

        Args:
            priority: Priority value
            item: Bytes item to add

        Returns:
            True if added, False if full
        """
        if not isinstance(item, bytes):
            item = str(item).encode("utf-8")
        # Encode priority into the item payload for the FFI layer
        heap_priority = -priority if self._max_heap else priority
        import struct
        payload = struct.pack("!d", heap_priority) + item
        result = self._lib.proven_queue_push(self._handle, payload, len(payload))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    def pop(self) -> Optional[bytes]:
        """
        Remove and return highest priority item via FFI.

        Returns:
            Item bytes, or None if empty
        """
        result = self._lib.proven_queue_pop(self._handle)
        if result.status != ProvenStatus.OK or result.data is None:
            return None
        data = result.data[:result.length]
        self._lib.proven_free_string(result.data)
        raw = bytes(data)
        # Strip priority prefix (8 bytes for double)
        import struct
        if len(raw) > 8:
            return raw[8:]
        return raw

    def clear(self) -> None:
        """Remove all items."""
        self._lib.proven_queue_free(self._handle)
        result = self._lib.proven_queue_create(self._capacity)
        if result.status == ProvenStatus.OK and result.handle is not None:
            self._handle = result.handle
