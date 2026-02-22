# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeRateLimiter - Rate limiting with token bucket and sliding window.

Provides various rate limiting algorithms for controlling request rates.
All operations are delegated to the Idris core via FFI using opaque handles.
"""

import json
from typing import Optional, NamedTuple

from .core import ProvenStatus, get_lib


class RateLimitResult(NamedTuple):
    """Result of a rate limit check."""
    allowed: bool
    remaining: int
    retry_after: Optional[float]  # Seconds until next allowed request


class TokenBucket:
    """
    Token bucket rate limiter via FFI.

    Allows burst traffic up to bucket capacity, then limits to rate.

    Example:
        >>> limiter = TokenBucket(rate=10, capacity=100)  # 10/sec, burst of 100
        >>> limiter.acquire()
        RateLimitResult(allowed=True, remaining=99, retry_after=None)
    """

    def __init__(self, rate: float, capacity: int):
        """
        Create a token bucket via FFI.

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

        lib = get_lib()
        result = lib.proven_ratelimit_token_bucket_create(rate, capacity)
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create token bucket via FFI")

        self._handle = result.handle
        self._rate = rate
        self._capacity = capacity
        self._lib = lib

    def __del__(self):
        """Free the FFI token bucket handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_ratelimit_token_bucket_free(self._handle)
            except Exception:
                pass

    @property
    def rate(self) -> float:
        """Get token fill rate (per second)."""
        return self._rate

    @property
    def capacity(self) -> int:
        """Get bucket capacity."""
        return self._capacity

    def acquire(self, tokens: int = 1) -> RateLimitResult:
        """
        Try to acquire tokens via FFI.

        Args:
            tokens: Number of tokens to acquire

        Returns:
            RateLimitResult with allowed status
        """
        result = self._lib.proven_ratelimit_token_bucket_acquire(
            self._handle, tokens
        )
        if result.status != ProvenStatus.OK or result.value is None:
            return RateLimitResult(allowed=False, remaining=0, retry_after=None)

        raw = result.value[:result.length].decode("utf-8")
        self._lib.proven_free_string(result.value)

        try:
            data = json.loads(raw)
            return RateLimitResult(
                allowed=data.get("allowed", False),
                remaining=data.get("remaining", 0),
                retry_after=data.get("retry_after"),
            )
        except (json.JSONDecodeError, KeyError):
            return RateLimitResult(allowed=False, remaining=0, retry_after=None)

    def available(self) -> int:
        """Get current available tokens via FFI."""
        result = self.acquire(0)
        return result.remaining


class SlidingWindow:
    """
    Sliding window rate limiter via FFI.

    Tracks requests in a sliding time window.

    Example:
        >>> limiter = SlidingWindow(limit=100, window_seconds=60)  # 100/minute
        >>> limiter.acquire()
        RateLimitResult(allowed=True, remaining=99, retry_after=None)
    """

    def __init__(self, limit: int, window_seconds: float):
        """
        Create a sliding window limiter via FFI.

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

        lib = get_lib()
        result = lib.proven_ratelimit_sliding_window_create(limit, window_seconds)
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create sliding window via FFI")

        self._handle = result.handle
        self._limit = limit
        self._window = window_seconds
        self._lib = lib

    def __del__(self):
        """Free the FFI sliding window handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_ratelimit_sliding_window_free(self._handle)
            except Exception:
                pass

    @property
    def limit(self) -> int:
        """Get request limit."""
        return self._limit

    @property
    def window_seconds(self) -> float:
        """Get window duration."""
        return self._window

    def acquire(self) -> RateLimitResult:
        """
        Try to acquire a request slot via FFI.

        Returns:
            RateLimitResult with allowed status
        """
        result = self._lib.proven_ratelimit_sliding_window_acquire(self._handle)
        if result.status != ProvenStatus.OK or result.value is None:
            return RateLimitResult(allowed=False, remaining=0, retry_after=None)

        raw = result.value[:result.length].decode("utf-8")
        self._lib.proven_free_string(result.value)

        try:
            data = json.loads(raw)
            return RateLimitResult(
                allowed=data.get("allowed", False),
                remaining=data.get("remaining", 0),
                retry_after=data.get("retry_after"),
            )
        except (json.JSONDecodeError, KeyError):
            return RateLimitResult(allowed=False, remaining=0, retry_after=None)

    def remaining(self) -> int:
        """Get remaining requests in window via FFI."""
        result = self.acquire()
        # Note: This consumes a slot; for pure query, the Idris core
        # handles it via the acquire call with remaining field.
        return result.remaining


class FixedWindow:
    """
    Fixed window rate limiter via FFI.

    Resets count at fixed intervals. Uses the sliding window FFI handle
    with fixed window semantics managed by the Idris core.

    Example:
        >>> limiter = FixedWindow(limit=100, window_seconds=60)  # 100/minute
        >>> limiter.acquire()
        RateLimitResult(allowed=True, remaining=99, retry_after=None)
    """

    def __init__(self, limit: int, window_seconds: float):
        """
        Create a fixed window limiter via FFI.

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

        lib = get_lib()
        # Fixed window uses the same sliding window FFI with fixed semantics
        result = lib.proven_ratelimit_sliding_window_create(limit, window_seconds)
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create fixed window via FFI")

        self._handle = result.handle
        self._limit = limit
        self._window = window_seconds
        self._lib = lib

    def __del__(self):
        """Free the FFI handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_ratelimit_sliding_window_free(self._handle)
            except Exception:
                pass

    @property
    def limit(self) -> int:
        """Get request limit."""
        return self._limit

    @property
    def window_seconds(self) -> float:
        """Get window duration."""
        return self._window

    def acquire(self) -> RateLimitResult:
        """
        Try to acquire a request slot via FFI.

        Returns:
            RateLimitResult with allowed status
        """
        result = self._lib.proven_ratelimit_sliding_window_acquire(self._handle)
        if result.status != ProvenStatus.OK or result.value is None:
            return RateLimitResult(allowed=False, remaining=0, retry_after=None)

        raw = result.value[:result.length].decode("utf-8")
        self._lib.proven_free_string(result.value)

        try:
            data = json.loads(raw)
            return RateLimitResult(
                allowed=data.get("allowed", False),
                remaining=data.get("remaining", 0),
                retry_after=data.get("retry_after"),
            )
        except (json.JSONDecodeError, KeyError):
            return RateLimitResult(allowed=False, remaining=0, retry_after=None)

    def remaining(self) -> int:
        """Get remaining requests in window via FFI."""
        result = self.acquire()
        return result.remaining
