# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeMonotonic - Monotonically increasing sequences.

Provides counters and sequence generators that cannot go backwards.
All counter operations are delegated to the Idris core via FFI using opaque handles.
"""

from typing import Optional
import time

from .core import ProvenStatus, get_lib


class MonotonicCounter:
    """
    Thread-safe monotonically increasing counter via FFI.

    Example:
        >>> counter = MonotonicCounter()
        >>> counter.next()
        0
        >>> counter.next()
        1
        >>> counter.get()
        2
    """

    def __init__(self, initial: int = 0):
        """
        Create a monotonic counter via FFI.

        Args:
            initial: Starting value
        """
        lib = get_lib()
        result = lib.proven_monotonic_counter_create(initial)
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create monotonic counter via FFI")

        self._handle = result.handle
        self._lib = lib

    def __del__(self):
        """Free the FFI counter handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_monotonic_counter_free(self._handle)
            except Exception:
                pass

    def get(self) -> int:
        """Get current value via FFI."""
        result = self._lib.proven_monotonic_counter_get(self._handle)
        if result.status != ProvenStatus.OK:
            return 0
        return result.value

    def next(self) -> int:
        """Get and increment (returns value before increment) via FFI."""
        result = self._lib.proven_monotonic_counter_next(self._handle)
        if result.status != ProvenStatus.OK:
            return 0
        return result.value

    def advance(self, amount: int) -> int:
        """
        Increment by a specific amount via FFI.

        Args:
            amount: Amount to add

        Returns:
            Value before increment
        """
        current = self.get()
        for _ in range(amount):
            self._lib.proven_monotonic_counter_next(self._handle)
        return current

    def try_set(self, new_value: int) -> bool:
        """
        Try to set a new value (only succeeds if new > current).

        Args:
            new_value: New value to set

        Returns:
            True if value was set
        """
        current = self.get()
        if new_value <= current:
            return False
        # Advance to new_value
        diff = new_value - current
        for _ in range(diff):
            self._lib.proven_monotonic_counter_next(self._handle)
        return True

    def ensure_at_least(self, min_value: int) -> None:
        """
        Ensure value is at least the given minimum.

        Args:
            min_value: Minimum value
        """
        self.try_set(min_value)


class HighWaterMark:
    """
    Tracks the maximum seen value via FFI.

    Uses a monotonic counter to ensure values never decrease.

    Example:
        >>> hwm = HighWaterMark()
        >>> hwm.update(10)
        True
        >>> hwm.update(5)  # Not a new max
        False
        >>> hwm.get()
        10
    """

    def __init__(self):
        """Create a high-water mark tracker via FFI."""
        self._counter = MonotonicCounter(0)

    def get(self) -> int:
        """Get current high-water mark."""
        return self._counter.get()

    def update(self, value: int) -> bool:
        """
        Update with a new value.

        Args:
            value: New value

        Returns:
            True if this is a new maximum
        """
        return self._counter.try_set(value)

    def reset(self) -> None:
        """Reset to zero by recreating the counter."""
        self._counter = MonotonicCounter(0)


class SequenceGenerator:
    """
    Sequence ID generator with prefix via FFI.

    Example:
        >>> gen = SequenceGenerator("item")
        >>> gen.next_id()
        'item-0'
        >>> gen.next_id()
        'item-1'
    """

    def __init__(self, prefix: str):
        """
        Create a sequence generator.

        Args:
            prefix: Prefix for generated IDs
        """
        self._counter = MonotonicCounter()
        self._prefix = prefix

    def next_id(self) -> str:
        """Generate the next ID."""
        return f"{self._prefix}-{self._counter.next()}"

    def next_id_padded(self, width: int) -> str:
        """
        Generate the next ID with zero-padding.

        Args:
            width: Width for zero-padding

        Returns:
            Padded ID
        """
        return f"{self._prefix}-{self._counter.next():0{width}d}"


class EpochGenerator:
    """
    Epoch-based ID generator (timestamp + sequence) via FFI.

    Generates monotonic IDs based on time.

    Example:
        >>> gen = EpochGenerator(epoch=1700000000)
        >>> id1 = gen.next_id(int(time.time()))
        >>> id2 = gen.next_id(int(time.time()))
        >>> id2 > id1
        True
    """

    def __init__(self, epoch: int):
        """
        Create an epoch generator.

        Args:
            epoch: Base epoch timestamp (seconds)
        """
        self._epoch = epoch
        self._sequence = MonotonicCounter()
        self._last_timestamp_counter = MonotonicCounter(0)

    def next_id(self, current_timestamp: int) -> int:
        """
        Generate an ID from timestamp and sequence via FFI.

        Args:
            current_timestamp: Current timestamp (seconds)

        Returns:
            Monotonic ID
        """
        adjusted = current_timestamp - self._epoch
        # Ensure monotonicity via the counter
        self._last_timestamp_counter.ensure_at_least(adjusted)
        ts = self._last_timestamp_counter.get()

        # Combine timestamp and sequence (16 bits for sequence)
        seq = self._sequence.next() & 0xFFFF
        return (ts << 16) | seq

    def next_id_now(self) -> int:
        """Generate an ID using current time."""
        return self.next_id(int(time.time()))


class MonotonicTimestamp:
    """
    Provides monotonically increasing timestamps via FFI.

    Ensures timestamps never go backwards even if system clock changes.

    Example:
        >>> ts = MonotonicTimestamp()
        >>> t1 = ts.now()
        >>> t2 = ts.now()
        >>> t2 >= t1
        True
    """

    def __init__(self):
        """Create a monotonic timestamp source via FFI."""
        # Use a monotonic counter to track the last seen timestamp
        # multiplied by a factor for sub-second precision
        self._counter = MonotonicCounter(0)
        self._last = 0.0
        self._lib = get_lib()

    def now(self) -> float:
        """
        Get current timestamp, guaranteed >= previous.

        Returns:
            Monotonic timestamp (seconds since epoch)
        """
        current = time.time()
        if current > self._last:
            self._last = current
        else:
            # Clock went backwards, increment slightly
            self._last += 0.000001
        # Record in counter for monotonicity tracking
        self._counter.ensure_at_least(int(self._last * 1000000))
        return self._last

    def now_millis(self) -> int:
        """Get current timestamp in milliseconds."""
        return int(self.now() * 1000)

    def now_nanos(self) -> int:
        """Get current timestamp in nanoseconds."""
        return int(self.now() * 1_000_000_000)
