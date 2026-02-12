# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeMonotonic - Monotonically increasing sequences.

Provides counters and sequence generators that cannot go backwards.
"""

from typing import Optional
import threading
import time


class MonotonicCounter:
    """
    Thread-safe monotonically increasing counter.

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
        Create a monotonic counter.

        Args:
            initial: Starting value
        """
        self._value = initial
        self._lock = threading.Lock()

    def get(self) -> int:
        """Get current value."""
        with self._lock:
            return self._value

    def next(self) -> int:
        """Get and increment (returns value before increment)."""
        with self._lock:
            current = self._value
            self._value += 1
            return current

    def advance(self, amount: int) -> int:
        """
        Increment by a specific amount.

        Args:
            amount: Amount to add

        Returns:
            Value before increment
        """
        with self._lock:
            current = self._value
            self._value += amount
            return current

    def try_set(self, new_value: int) -> bool:
        """
        Try to set a new value (only succeeds if new > current).

        Args:
            new_value: New value to set

        Returns:
            True if value was set
        """
        with self._lock:
            if new_value > self._value:
                self._value = new_value
                return True
            return False

    def ensure_at_least(self, min_value: int) -> None:
        """
        Ensure value is at least the given minimum.

        Args:
            min_value: Minimum value
        """
        with self._lock:
            if self._value < min_value:
                self._value = min_value


class HighWaterMark:
    """
    Tracks the maximum seen value.

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
        """Create a high-water mark tracker."""
        self._value = 0
        self._lock = threading.Lock()

    def get(self) -> int:
        """Get current high-water mark."""
        with self._lock:
            return self._value

    def update(self, value: int) -> bool:
        """
        Update with a new value.

        Args:
            value: New value

        Returns:
            True if this is a new maximum
        """
        with self._lock:
            if value > self._value:
                self._value = value
                return True
            return False

    def reset(self) -> None:
        """Reset to zero."""
        with self._lock:
            self._value = 0


class SequenceGenerator:
    """
    Sequence ID generator with prefix.

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
    Epoch-based ID generator (timestamp + sequence).

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
        self._last_timestamp = 0
        self._lock = threading.Lock()

    def next_id(self, current_timestamp: int) -> int:
        """
        Generate an ID from timestamp and sequence.

        Args:
            current_timestamp: Current timestamp (seconds)

        Returns:
            Monotonic ID
        """
        with self._lock:
            adjusted = current_timestamp - self._epoch
            # Ensure monotonicity
            ts = max(adjusted, self._last_timestamp)
            self._last_timestamp = ts

            # Combine timestamp and sequence (16 bits for sequence)
            seq = self._sequence.next() & 0xFFFF
            return (ts << 16) | seq

    def next_id_now(self) -> int:
        """Generate an ID using current time."""
        return self.next_id(int(time.time()))


class MonotonicTimestamp:
    """
    Provides monotonically increasing timestamps.

    Ensures timestamps never go backwards even if system clock changes.

    Example:
        >>> ts = MonotonicTimestamp()
        >>> t1 = ts.now()
        >>> t2 = ts.now()
        >>> t2 >= t1
        True
    """

    def __init__(self):
        """Create a monotonic timestamp source."""
        self._last = 0.0
        self._lock = threading.Lock()

    def now(self) -> float:
        """
        Get current timestamp, guaranteed >= previous.

        Returns:
            Monotonic timestamp (seconds since epoch)
        """
        with self._lock:
            current = time.time()
            if current > self._last:
                self._last = current
            else:
                # Clock went backwards, increment slightly
                self._last += 0.000001
            return self._last

    def now_millis(self) -> int:
        """Get current timestamp in milliseconds."""
        return int(self.now() * 1000)

    def now_nanos(self) -> int:
        """Get current timestamp in nanoseconds."""
        return int(self.now() * 1_000_000_000)
