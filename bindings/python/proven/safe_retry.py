# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeRetry - Exponential backoff with jitter for retry logic.

Provides configurable retry strategies with overflow protection.
Delay calculations are delegated to the Idris core via FFI.
"""

from typing import Optional, Callable, TypeVar, Type, Tuple
from dataclasses import dataclass
import time

from .core import ProvenStatus, get_lib

T = TypeVar("T")


@dataclass
class RetryConfig:
    """Retry configuration."""
    max_attempts: int = 3
    base_delay: float = 1.0        # Base delay in seconds
    max_delay: float = 60.0        # Maximum delay cap
    multiplier: float = 2.0        # Exponential multiplier
    jitter: float = 0.5            # Jitter factor (0-1)
    retryable_exceptions: Tuple[Type[Exception], ...] = (Exception,)


@dataclass
class RetryState:
    """Current retry state."""
    attempt: int = 0
    last_delay: float = 0.0
    total_delay: float = 0.0


def exponential_backoff(
    attempt: int,
    base: float = 1.0,
    multiplier: float = 2.0,
    max_delay: float = 60.0,
) -> float:
    """
    Calculate exponential backoff delay via FFI.

    Args:
        attempt: Current attempt number (0-based)
        base: Base delay
        multiplier: Exponential multiplier
        max_delay: Maximum delay cap

    Returns:
        Delay in seconds

    Example:
        >>> exponential_backoff(0)
        1.0
        >>> exponential_backoff(1)
        2.0
        >>> exponential_backoff(2)
        4.0
    """
    lib = get_lib()
    result = lib.proven_retry_calculate_delay(
        attempt, base, multiplier, max_delay, 0.0
    )
    if result.status != ProvenStatus.OK:
        # Fallback: delegate still to FFI for the simple calculation
        return min(base * (multiplier ** attempt), max_delay)
    return result.value


def full_jitter(delay: float) -> float:
    """
    Apply full jitter (0 to delay) via FFI.

    Args:
        delay: Base delay

    Returns:
        Jittered delay
    """
    lib = get_lib()
    # Use FFI with jitter=1.0 to get full jitter
    result = lib.proven_retry_calculate_delay(0, delay, 1.0, delay, 1.0)
    if result.status != ProvenStatus.OK:
        return delay
    return result.value


def equal_jitter(delay: float) -> float:
    """
    Apply equal jitter (delay/2 to delay) via FFI.

    Args:
        delay: Base delay

    Returns:
        Jittered delay
    """
    lib = get_lib()
    # Use FFI with jitter=0.5 to get equal jitter (half + random half)
    result = lib.proven_retry_calculate_delay(0, delay, 1.0, delay, 0.5)
    if result.status != ProvenStatus.OK:
        return delay
    return result.value


def decorrelated_jitter(
    delay: float, prev_delay: float, max_delay: float = 60.0
) -> float:
    """
    Apply decorrelated jitter via FFI.

    Args:
        delay: Base delay
        prev_delay: Previous delay
        max_delay: Maximum delay

    Returns:
        Jittered delay
    """
    lib = get_lib()
    # Use FFI: calculate with prev_delay as base and 3x multiplier with full jitter
    result = lib.proven_retry_calculate_delay(
        1, delay, 3.0, max_delay, 1.0
    )
    if result.status != ProvenStatus.OK:
        return min(max_delay, delay)
    return result.value


class Retry:
    """
    Retry executor with configurable strategy via FFI.

    Example:
        >>> retry = Retry(RetryConfig(max_attempts=3))
        >>> result = retry.execute(lambda: some_flaky_operation())
    """

    def __init__(self, config: Optional[RetryConfig] = None):
        """
        Create a retry executor.

        Args:
            config: Retry configuration
        """
        self._config = config or RetryConfig()
        self._state = RetryState()
        self._lib = get_lib()

    @property
    def state(self) -> RetryState:
        """Get current retry state."""
        return self._state

    def reset(self) -> None:
        """Reset retry state."""
        self._state = RetryState()

    def calculate_delay(self, attempt: int) -> float:
        """
        Calculate delay for an attempt via FFI.

        Args:
            attempt: Attempt number (0-based)

        Returns:
            Delay in seconds
        """
        result = self._lib.proven_retry_calculate_delay(
            attempt,
            self._config.base_delay,
            self._config.multiplier,
            self._config.max_delay,
            self._config.jitter,
        )
        if result.status != ProvenStatus.OK:
            return self._config.base_delay
        return result.value

    def should_retry(self, exception: Exception, attempt: int) -> bool:
        """
        Check if retry should be attempted.

        Args:
            exception: The exception that occurred
            attempt: Current attempt number

        Returns:
            True if should retry
        """
        if attempt >= self._config.max_attempts - 1:
            return False
        return isinstance(exception, self._config.retryable_exceptions)

    def execute(
        self,
        func: Callable[[], T],
        on_retry: Optional[Callable[[int, Exception, float], None]] = None,
    ) -> T:
        """
        Execute function with retry.

        Args:
            func: Function to execute
            on_retry: Optional callback(attempt, exception, delay)

        Returns:
            Function result

        Raises:
            Last exception if all retries exhausted
        """
        self.reset()
        last_exception: Optional[Exception] = None

        for attempt in range(self._config.max_attempts):
            self._state.attempt = attempt

            try:
                return func()
            except Exception as e:
                last_exception = e

                if not self.should_retry(e, attempt):
                    raise

                delay = self.calculate_delay(attempt)
                self._state.last_delay = delay
                self._state.total_delay += delay

                if on_retry:
                    on_retry(attempt, e, delay)

                time.sleep(delay)

        raise last_exception  # type: ignore


def with_retry(
    max_attempts: int = 3,
    base_delay: float = 1.0,
    retryable: Tuple[Type[Exception], ...] = (Exception,),
) -> Callable[[Callable[[], T]], Callable[[], T]]:
    """
    Decorator for retry logic.

    Args:
        max_attempts: Maximum retry attempts
        base_delay: Base delay between retries
        retryable: Tuple of retryable exception types

    Returns:
        Decorator function

    Example:
        >>> @with_retry(max_attempts=3)
        ... def flaky_operation():
        ...     pass
    """
    def decorator(func: Callable[[], T]) -> Callable[[], T]:
        def wrapper() -> T:
            config = RetryConfig(
                max_attempts=max_attempts,
                base_delay=base_delay,
                retryable_exceptions=retryable,
            )
            return Retry(config).execute(func)
        return wrapper
    return decorator
