# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeBuffer - Bounded buffers and ring buffers without overflow.

Provides safe buffer operations with bounds checking.
"""

from typing import TypeVar, Generic, Optional, List, Iterator

T = TypeVar("T")


class BoundedBuffer(Generic[T]):
    """
    Fixed-capacity buffer with bounds checking.

    Example:
        >>> buf = BoundedBuffer(3)
        >>> buf.push(1)
        True
        >>> buf.push(2)
        True
        >>> buf.push(3)
        True
        >>> buf.push(4)  # Full
        False
        >>> buf.pop()
        3
    """

    def __init__(self, capacity: int):
        """
        Create a bounded buffer.

        Args:
            capacity: Maximum number of items

        Raises:
            ValueError: If capacity <= 0
        """
        if capacity <= 0:
            raise ValueError("Capacity must be positive")
        self._capacity = capacity
        self._items: List[T] = []

    @property
    def capacity(self) -> int:
        """Get maximum capacity."""
        return self._capacity

    def __len__(self) -> int:
        """Get current number of items."""
        return len(self._items)

    def is_empty(self) -> bool:
        """Check if buffer is empty."""
        return len(self._items) == 0

    def is_full(self) -> bool:
        """Check if buffer is at capacity."""
        return len(self._items) >= self._capacity

    def push(self, item: T) -> bool:
        """
        Add item to buffer.

        Args:
            item: Item to add

        Returns:
            True if added, False if full
        """
        if self.is_full():
            return False
        self._items.append(item)
        return True

    def pop(self) -> Optional[T]:
        """
        Remove and return last item.

        Returns:
            Item, or None if empty
        """
        if self.is_empty():
            return None
        return self._items.pop()

    def peek(self) -> Optional[T]:
        """
        Get last item without removing.

        Returns:
            Item, or None if empty
        """
        if self.is_empty():
            return None
        return self._items[-1]

    def get(self, index: int) -> Optional[T]:
        """
        Get item at index.

        Args:
            index: Index (0-based)

        Returns:
            Item, or None if out of bounds
        """
        if 0 <= index < len(self._items):
            return self._items[index]
        return None

    def clear(self) -> None:
        """Remove all items."""
        self._items.clear()

    def __iter__(self) -> Iterator[T]:
        """Iterate over items."""
        return iter(self._items)

    def to_list(self) -> List[T]:
        """Get copy of items as list."""
        return list(self._items)


class RingBuffer(Generic[T]):
    """
    Fixed-capacity ring buffer (circular buffer).

    Overwrites oldest items when full.

    Example:
        >>> ring = RingBuffer(3)
        >>> ring.push(1)
        >>> ring.push(2)
        >>> ring.push(3)
        >>> ring.push(4)  # Overwrites 1
        >>> list(ring)
        [2, 3, 4]
    """

    def __init__(self, capacity: int):
        """
        Create a ring buffer.

        Args:
            capacity: Maximum number of items

        Raises:
            ValueError: If capacity <= 0
        """
        if capacity <= 0:
            raise ValueError("Capacity must be positive")
        self._capacity = capacity
        self._buffer: List[Optional[T]] = [None] * capacity
        self._head = 0  # Next write position
        self._count = 0

    @property
    def capacity(self) -> int:
        """Get maximum capacity."""
        return self._capacity

    def __len__(self) -> int:
        """Get current number of items."""
        return self._count

    def is_empty(self) -> bool:
        """Check if buffer is empty."""
        return self._count == 0

    def is_full(self) -> bool:
        """Check if buffer is at capacity."""
        return self._count >= self._capacity

    def push(self, item: T) -> Optional[T]:
        """
        Add item, overwriting oldest if full.

        Args:
            item: Item to add

        Returns:
            Overwritten item if full, None otherwise
        """
        overwritten = None
        if self.is_full():
            # Calculate oldest position
            oldest = (self._head - self._count) % self._capacity
            overwritten = self._buffer[oldest]

        self._buffer[self._head] = item
        self._head = (self._head + 1) % self._capacity

        if not self.is_full():
            self._count += 1

        return overwritten

    def pop(self) -> Optional[T]:
        """
        Remove and return newest item.

        Returns:
            Item, or None if empty
        """
        if self.is_empty():
            return None

        self._head = (self._head - 1) % self._capacity
        item = self._buffer[self._head]
        self._buffer[self._head] = None
        self._count -= 1
        return item

    def peek(self) -> Optional[T]:
        """
        Get newest item without removing.

        Returns:
            Item, or None if empty
        """
        if self.is_empty():
            return None
        newest = (self._head - 1) % self._capacity
        return self._buffer[newest]

    def peek_oldest(self) -> Optional[T]:
        """
        Get oldest item without removing.

        Returns:
            Item, or None if empty
        """
        if self.is_empty():
            return None
        oldest = (self._head - self._count) % self._capacity
        return self._buffer[oldest]

    def get(self, index: int) -> Optional[T]:
        """
        Get item at index (0 = oldest).

        Args:
            index: Index from oldest

        Returns:
            Item, or None if out of bounds
        """
        if not 0 <= index < self._count:
            return None
        pos = (self._head - self._count + index) % self._capacity
        return self._buffer[pos]

    def clear(self) -> None:
        """Remove all items."""
        self._buffer = [None] * self._capacity
        self._head = 0
        self._count = 0

    def __iter__(self) -> Iterator[T]:
        """Iterate from oldest to newest."""
        for i in range(self._count):
            pos = (self._head - self._count + i) % self._capacity
            yield self._buffer[pos]

    def to_list(self) -> List[T]:
        """Get items as list (oldest first)."""
        return list(self)
