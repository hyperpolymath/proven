# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeQueue - Bounded FIFO and priority queues without overflow.

Provides safe queue operations with capacity limits.
"""

from typing import TypeVar, Generic, Optional, List, Iterator, Callable
import heapq

T = TypeVar("T")


class BoundedQueue(Generic[T]):
    """
    Bounded FIFO queue.

    Example:
        >>> q = BoundedQueue(3)
        >>> q.push(1)
        True
        >>> q.push(2)
        True
        >>> q.pop()
        1
    """

    def __init__(self, capacity: int):
        """
        Create a bounded queue.

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
        """Check if queue is empty."""
        return len(self._items) == 0

    def is_full(self) -> bool:
        """Check if queue is at capacity."""
        return len(self._items) >= self._capacity

    def push(self, item: T) -> bool:
        """
        Add item to back of queue.

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
        Remove and return front item.

        Returns:
            Item, or None if empty
        """
        if self.is_empty():
            return None
        return self._items.pop(0)

    def peek(self) -> Optional[T]:
        """
        Get front item without removing.

        Returns:
            Item, or None if empty
        """
        if self.is_empty():
            return None
        return self._items[0]

    def clear(self) -> None:
        """Remove all items."""
        self._items.clear()

    def __iter__(self) -> Iterator[T]:
        """Iterate over items front to back."""
        return iter(self._items)

    def to_list(self) -> List[T]:
        """Get copy of items as list."""
        return list(self._items)


class PriorityQueue(Generic[T]):
    """
    Bounded priority queue (min-heap by default).

    Example:
        >>> pq = PriorityQueue(5)
        >>> pq.push(3, "medium")
        True
        >>> pq.push(1, "high")
        True
        >>> pq.push(5, "low")
        True
        >>> pq.pop()
        'high'
    """

    def __init__(self, capacity: int, max_heap: bool = False):
        """
        Create a bounded priority queue.

        Args:
            capacity: Maximum number of items
            max_heap: If True, highest priority first (default: lowest first)

        Raises:
            ValueError: If capacity <= 0
        """
        if capacity <= 0:
            raise ValueError("Capacity must be positive")
        self._capacity = capacity
        self._max_heap = max_heap
        self._heap: List[tuple] = []
        self._counter = 0  # For stable ordering

    @property
    def capacity(self) -> int:
        """Get maximum capacity."""
        return self._capacity

    def __len__(self) -> int:
        """Get current number of items."""
        return len(self._heap)

    def is_empty(self) -> bool:
        """Check if queue is empty."""
        return len(self._heap) == 0

    def is_full(self) -> bool:
        """Check if queue is at capacity."""
        return len(self._heap) >= self._capacity

    def push(self, priority: float, item: T) -> bool:
        """
        Add item with priority.

        Args:
            priority: Priority value (lower = higher priority by default)
            item: Item to add

        Returns:
            True if added, False if full
        """
        if self.is_full():
            return False

        # Use negative priority for max heap
        heap_priority = -priority if self._max_heap else priority
        heapq.heappush(self._heap, (heap_priority, self._counter, item))
        self._counter += 1
        return True

    def pop(self) -> Optional[T]:
        """
        Remove and return highest priority item.

        Returns:
            Item, or None if empty
        """
        if self.is_empty():
            return None
        _, _, item = heapq.heappop(self._heap)
        return item

    def peek(self) -> Optional[T]:
        """
        Get highest priority item without removing.

        Returns:
            Item, or None if empty
        """
        if self.is_empty():
            return None
        return self._heap[0][2]

    def peek_priority(self) -> Optional[float]:
        """
        Get priority of highest priority item.

        Returns:
            Priority, or None if empty
        """
        if self.is_empty():
            return None
        priority = self._heap[0][0]
        return -priority if self._max_heap else priority

    def clear(self) -> None:
        """Remove all items."""
        self._heap.clear()
        self._counter = 0

    def __iter__(self) -> Iterator[T]:
        """Iterate over items (not in priority order)."""
        return (item for _, _, item in self._heap)
