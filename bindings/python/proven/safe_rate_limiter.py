# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeRateLimiter - Rate limiting with token bucket and sliding window.

Provides various rate limiting algorithms for controlling request rates.
"""

from typing import Optional, NamedTuple
from enum import Enum
import time
import threading


class RateLimitResult(NamedTuple):
    """Result of a rate limit check."""
    allowed: bool
    remaining: int
    retry_after: Optional[float]  # Seconds until next allowed request


class TokenBucket:
    """
    Token bucket rate limiter.

    Allows burst traffic up to bucket capacity, then limits to rate.

    Example:
        >>> limiter = TokenBucket(rate=10, capacity=100)  # 10/sec, burst of 100
        >>> limiter.acquire()
        RateLimitResult(allowed=True, remaining=99, retry_after=None)
    """

    def __init__(self, rate: float, capacity: int):
        """
        Create a token bucket.

        Args:
            rate: Tokens per second to add
            capacity: Maximum tokens in bucket

        Raises:
            ValueError: If parameters are invalid
        """
        if rate <= 0:
            raise ValueError("Rate must be positive")
        if capacity <= 0:
            raise ValueError("Capacity must be positive")

        self._rate = rate
        self._capacity = capacity
        self._tokens = float(capacity)
        self._last_update = time.monotonic()
        self._lock = threading.Lock()

    @property
    def rate(self) -> float:
        """Get token fill rate (per second)."""
        return self._rate

    @property
    def capacity(self) -> int:
        """Get bucket capacity."""
        return self._capacity

    def _refill(self) -> None:
        """Refill tokens based on elapsed time."""
        now = time.monotonic()
        elapsed = now - self._last_update
        self._tokens = min(self._capacity, self._tokens + elapsed * self._rate)
        self._last_update = now

    def acquire(self, tokens: int = 1) -> RateLimitResult:
        """
        Try to acquire tokens.

        Args:
            tokens: Number of tokens to acquire

        Returns:
            RateLimitResult with allowed status
        """
        with self._lock:
            self._refill()

            if self._tokens >= tokens:
                self._tokens -= tokens
                return RateLimitResult(
                    allowed=True,
                    remaining=int(self._tokens),
                    retry_after=None,
                )
            else:
                deficit = tokens - self._tokens
                retry_after = deficit / self._rate
                return RateLimitResult(
                    allowed=False,
                    remaining=0,
                    retry_after=retry_after,
                )

    def available(self) -> int:
        """Get current available tokens."""
        with self._lock:
            self._refill()
            return int(self._tokens)


class SlidingWindow:
    """
    Sliding window rate limiter.

    Tracks requests in a sliding time window.

    Example:
        >>> limiter = SlidingWindow(limit=100, window_seconds=60)  # 100/minute
        >>> limiter.acquire()
        RateLimitResult(allowed=True, remaining=99, retry_after=None)
    """

    def __init__(self, limit: int, window_seconds: float):
        """
        Create a sliding window limiter.

        Args:
            limit: Maximum requests in window
            window_seconds: Window duration in seconds

        Raises:
            ValueError: If parameters are invalid
        """
        if limit <= 0:
            raise ValueError("Limit must be positive")
        if window_seconds <= 0:
            raise ValueError("Window must be positive")

        self._limit = limit
        self._window = window_seconds
        self._timestamps: list = []
        self._lock = threading.Lock()

    @property
    def limit(self) -> int:
        """Get request limit."""
        return self._limit

    @property
    def window_seconds(self) -> float:
        """Get window duration."""
        return self._window

    def _cleanup(self, now: float) -> None:
        """Remove expired timestamps."""
        cutoff = now - self._window
        self._timestamps = [t for t in self._timestamps if t > cutoff]

    def acquire(self) -> RateLimitResult:
        """
        Try to acquire a request slot.

        Returns:
            RateLimitResult with allowed status
        """
        with self._lock:
            now = time.monotonic()
            self._cleanup(now)

            if len(self._timestamps) < self._limit:
                self._timestamps.append(now)
                return RateLimitResult(
                    allowed=True,
                    remaining=self._limit - len(self._timestamps),
                    retry_after=None,
                )
            else:
                oldest = self._timestamps[0]
                retry_after = oldest + self._window - now
                return RateLimitResult(
                    allowed=False,
                    remaining=0,
                    retry_after=max(0, retry_after),
                )

    def remaining(self) -> int:
        """Get remaining requests in window."""
        with self._lock:
            self._cleanup(time.monotonic())
            return self._limit - len(self._timestamps)


class FixedWindow:
    """
    Fixed window rate limiter.

    Resets count at fixed intervals. Simpler but can allow 2x burst at window boundaries.

    Example:
        >>> limiter = FixedWindow(limit=100, window_seconds=60)  # 100/minute
        >>> limiter.acquire()
        RateLimitResult(allowed=True, remaining=99, retry_after=None)
    """

    def __init__(self, limit: int, window_seconds: float):
        """
        Create a fixed window limiter.

        Args:
            limit: Maximum requests in window
            window_seconds: Window duration in seconds

        Raises:
            ValueError: If parameters are invalid
        """
        if limit <= 0:
            raise ValueError("Limit must be positive")
        if window_seconds <= 0:
            raise ValueError("Window must be positive")

        self._limit = limit
        self._window = window_seconds
        self._count = 0
        self._window_start = time.monotonic()
        self._lock = threading.Lock()

    @property
    def limit(self) -> int:
        """Get request limit."""
        return self._limit

    @property
    def window_seconds(self) -> float:
        """Get window duration."""
        return self._window

    def _maybe_reset(self, now: float) -> None:
        """Reset if window has passed."""
        if now - self._window_start >= self._window:
            self._window_start = now
            self._count = 0

    def acquire(self) -> RateLimitResult:
        """
        Try to acquire a request slot.

        Returns:
            RateLimitResult with allowed status
        """
        with self._lock:
            now = time.monotonic()
            self._maybe_reset(now)

            if self._count < self._limit:
                self._count += 1
                return RateLimitResult(
                    allowed=True,
                    remaining=self._limit - self._count,
                    retry_after=None,
                )
            else:
                retry_after = self._window_start + self._window - now
                return RateLimitResult(
                    allowed=False,
                    remaining=0,
                    retry_after=max(0, retry_after),
                )

    def remaining(self) -> int:
        """Get remaining requests in window."""
        with self._lock:
            self._maybe_reset(time.monotonic())
            return self._limit - self._count
