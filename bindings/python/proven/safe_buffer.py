# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeBuffer - Bounded buffers and ring buffers without overflow.

Provides safe buffer operations with bounds checking.
All operations are delegated to the Idris core via FFI using opaque handles.
"""

from typing import Optional, List

from .core import ProvenStatus, get_lib


class BoundedBuffer:
    """
    Fixed-capacity buffer with bounds checking via FFI.

    Uses an opaque handle to a buffer managed by the Idris core.

    Example:
        >>> buf = BoundedBuffer(3)
        >>> buf.push(b"hello")
        True
        >>> buf.push(b"world")
        True
        >>> buf.pop()
        b'world'
    """

    def __init__(self, capacity: int):
        """
        Create a bounded buffer via FFI.

        Args:
            capacity: Maximum number of items

        Raises:
            ValueError: If capacity <= 0
        """
        if capacity <= 0:
            raise ValueError("Capacity must be positive")

        lib = get_lib()
        result = lib.proven_buffer_create(capacity)
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create buffer via FFI")

        self._handle = result.handle
        self._capacity = capacity
        self._lib = lib

    def __del__(self):
        """Free the FFI buffer handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_buffer_free(self._handle)
            except Exception:
                pass

    @property
    def capacity(self) -> int:
        """Get maximum capacity."""
        return self._capacity

    def __len__(self) -> int:
        """Get current number of items."""
        result = self._lib.proven_buffer_len(self._handle)
        if result.status != ProvenStatus.OK:
            return 0
        return result.value

    def is_empty(self) -> bool:
        """Check if buffer is empty."""
        return len(self) == 0

    def is_full(self) -> bool:
        """Check if buffer is at capacity."""
        return len(self) >= self._capacity

    def push(self, item: bytes) -> bool:
        """
        Add item to buffer via FFI.

        Args:
            item: Bytes item to add

        Returns:
            True if added, False if full
        """
        if not isinstance(item, bytes):
            item = str(item).encode("utf-8")
        result = self._lib.proven_buffer_push(self._handle, item, len(item))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    def pop(self) -> Optional[bytes]:
        """
        Remove and return last item via FFI.

        Returns:
            Item bytes, or None if empty
        """
        result = self._lib.proven_buffer_pop(self._handle)
        if result.status != ProvenStatus.OK or result.data is None:
            return None
        data = result.data[:result.length]
        self._lib.proven_free_string(result.data)
        return bytes(data)

    def clear(self) -> None:
        """Remove all items by recreating the buffer."""
        self._lib.proven_buffer_free(self._handle)
        result = self._lib.proven_buffer_create(self._capacity)
        if result.status == ProvenStatus.OK and result.handle is not None:
            self._handle = result.handle


class RingBuffer:
    """
    Fixed-capacity ring buffer (circular buffer) via FFI.

    Overwrites oldest items when full. Uses the same FFI buffer
    handle with ring buffer semantics managed by the Idris core.

    Example:
        >>> ring = RingBuffer(3)
        >>> ring.push(b"a")
        >>> ring.push(b"b")
        >>> ring.push(b"c")
        >>> ring.push(b"d")  # Overwrites oldest
    """

    def __init__(self, capacity: int):
        """
        Create a ring buffer via FFI.

        Args:
            capacity: Maximum number of items

        Raises:
            ValueError: If capacity <= 0
        """
        if capacity <= 0:
            raise ValueError("Capacity must be positive")

        lib = get_lib()
        # Ring buffers use the same handle infrastructure
        result = lib.proven_buffer_create(capacity)
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create ring buffer via FFI")

        self._handle = result.handle
        self._capacity = capacity
        self._lib = lib

    def __del__(self):
        """Free the FFI buffer handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_buffer_free(self._handle)
            except Exception:
                pass

    @property
    def capacity(self) -> int:
        """Get maximum capacity."""
        return self._capacity

    def __len__(self) -> int:
        """Get current number of items."""
        result = self._lib.proven_buffer_len(self._handle)
        if result.status != ProvenStatus.OK:
            return 0
        return result.value

    def is_empty(self) -> bool:
        """Check if buffer is empty."""
        return len(self) == 0

    def is_full(self) -> bool:
        """Check if buffer is at capacity."""
        return len(self) >= self._capacity

    def push(self, item: bytes) -> Optional[bytes]:
        """
        Add item, overwriting oldest if full via FFI.

        Args:
            item: Bytes item to add

        Returns:
            Overwritten item if full, None otherwise
        """
        if not isinstance(item, bytes):
            item = str(item).encode("utf-8")

        overwritten = None
        if self.is_full():
            # Pop oldest before push (ring buffer semantics)
            overwritten = self.pop()

        result = self._lib.proven_buffer_push(self._handle, item, len(item))
        if result.status != ProvenStatus.OK:
            return overwritten

        return overwritten

    def pop(self) -> Optional[bytes]:
        """
        Remove and return newest item via FFI.

        Returns:
            Item bytes, or None if empty
        """
        result = self._lib.proven_buffer_pop(self._handle)
        if result.status != ProvenStatus.OK or result.data is None:
            return None
        data = result.data[:result.length]
        self._lib.proven_free_string(result.data)
        return bytes(data)

    def clear(self) -> None:
        """Remove all items."""
        self._lib.proven_buffer_free(self._handle)
        result = self._lib.proven_buffer_create(self._capacity)
        if result.status == ProvenStatus.OK and result.handle is not None:
            self._handle = result.handle
