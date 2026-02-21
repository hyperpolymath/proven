# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeRetry - Exponential backoff with jitter for retry logic.

Provides configurable retry strategies with overflow protection.
"""

from typing import Optional, Callable, TypeVar, Type, Tuple, List
from dataclasses import dataclass
import time
import random

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
    Calculate exponential backoff delay.

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
    delay = base * (multiplier ** attempt)
    return min(delay, max_delay)


def full_jitter(delay: float) -> float:
    """
    Apply full jitter (0 to delay).

    Args:
        delay: Base delay

    Returns:
        Jittered delay
    """
    return random.uniform(0, delay)


def equal_jitter(delay: float) -> float:
    """
    Apply equal jitter (delay/2 to delay).

    Args:
        delay: Base delay

    Returns:
        Jittered delay
    """
    half = delay / 2
    return half + random.uniform(0, half)


def decorrelated_jitter(delay: float, prev_delay: float, max_delay: float = 60.0) -> float:
    """
    Apply decorrelated jitter.

    Args:
        delay: Base delay
        prev_delay: Previous delay
        max_delay: Maximum delay

    Returns:
        Jittered delay
    """
    return min(max_delay, random.uniform(delay, prev_delay * 3))


class Retry:
    """
    Retry executor with configurable strategy.

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

    @property
    def state(self) -> RetryState:
        """Get current retry state."""
        return self._state

    def reset(self) -> None:
        """Reset retry state."""
        self._state = RetryState()

    def calculate_delay(self, attempt: int) -> float:
        """
        Calculate delay for an attempt.

        Args:
            attempt: Attempt number (0-based)

        Returns:
            Delay in seconds
        """
        base_delay = exponential_backoff(
            attempt,
            self._config.base_delay,
            self._config.multiplier,
            self._config.max_delay,
        )

        if self._config.jitter > 0:
            jitter_amount = base_delay * self._config.jitter
            base_delay = base_delay - jitter_amount + random.uniform(0, jitter_amount * 2)

        return min(base_delay, self._config.max_delay)

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
