# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeCircuitBreaker - Fault tolerance pattern for preventing cascading failures.

Provides circuit breaker implementation with configurable thresholds.
"""

from typing import Optional, Callable, TypeVar, Any
from dataclasses import dataclass
from enum import Enum
import time
import threading

T = TypeVar("T")


class CircuitState(Enum):
    """Circuit breaker states."""
    CLOSED = "closed"       # Normal operation, requests pass through
    OPEN = "open"           # Failure threshold reached, requests blocked
    HALF_OPEN = "half_open"  # Testing if service recovered


@dataclass
class CircuitConfig:
    """Circuit breaker configuration."""
    failure_threshold: int = 5     # Failures before opening
    success_threshold: int = 2     # Successes in half-open before closing
    timeout_seconds: float = 30.0  # Time in open before trying half-open
    half_open_max_calls: int = 3   # Max concurrent calls in half-open


class CircuitBreaker:
    """
    Circuit breaker for fault tolerance.

    States:
    - CLOSED: Normal operation, tracking failures
    - OPEN: Blocking requests after threshold failures
    - HALF_OPEN: Testing with limited requests after timeout

    Example:
        >>> breaker = CircuitBreaker()
        >>> def call_service():
        ...     # Your service call
        ...     pass
        >>> result = breaker.call(call_service)
    """

    def __init__(self, config: Optional[CircuitConfig] = None):
        """
        Create a circuit breaker.

        Args:
            config: Configuration (defaults to standard settings)
        """
        self._config = config or CircuitConfig()
        self._state = CircuitState.CLOSED
        self._failures = 0
        self._successes = 0
        self._last_failure_time: Optional[float] = None
        self._half_open_calls = 0
        self._lock = threading.Lock()

    @property
    def state(self) -> CircuitState:
        """Get current state."""
        with self._lock:
            self._check_state_transition()
            return self._state

    @property
    def failures(self) -> int:
        """Get current failure count."""
        return self._failures

    def _check_state_transition(self) -> None:
        """Check if state should transition."""
        if self._state == CircuitState.OPEN:
            if self._last_failure_time is not None:
                elapsed = time.monotonic() - self._last_failure_time
                if elapsed >= self._config.timeout_seconds:
                    self._state = CircuitState.HALF_OPEN
                    self._half_open_calls = 0
                    self._successes = 0

    def allow_request(self) -> bool:
        """
        Check if a request should be allowed.

        Returns:
            True if request can proceed
        """
        with self._lock:
            self._check_state_transition()

            if self._state == CircuitState.CLOSED:
                return True
            elif self._state == CircuitState.OPEN:
                return False
            else:  # HALF_OPEN
                if self._half_open_calls < self._config.half_open_max_calls:
                    self._half_open_calls += 1
                    return True
                return False

    def record_success(self) -> None:
        """Record a successful request."""
        with self._lock:
            if self._state == CircuitState.HALF_OPEN:
                self._successes += 1
                if self._successes >= self._config.success_threshold:
                    self._state = CircuitState.CLOSED
                    self._failures = 0
                    self._successes = 0
            elif self._state == CircuitState.CLOSED:
                # Reset failure count on success
                self._failures = 0

    def record_failure(self) -> None:
        """Record a failed request."""
        with self._lock:
            self._failures += 1
            self._last_failure_time = time.monotonic()

            if self._state == CircuitState.CLOSED:
                if self._failures >= self._config.failure_threshold:
                    self._state = CircuitState.OPEN
            elif self._state == CircuitState.HALF_OPEN:
                # Any failure in half-open goes back to open
                self._state = CircuitState.OPEN
                self._successes = 0

    def call(
        self,
        func: Callable[[], T],
        fallback: Optional[Callable[[], T]] = None,
    ) -> Optional[T]:
        """
        Execute function through circuit breaker.

        Args:
            func: Function to call
            fallback: Optional fallback if circuit is open

        Returns:
            Function result, fallback result, or None

        Raises:
            CircuitOpenError: If circuit is open and no fallback provided
        """
        if not self.allow_request():
            if fallback:
                return fallback()
            raise CircuitOpenError(f"Circuit is {self._state.value}")

        try:
            result = func()
            self.record_success()
            return result
        except Exception as e:
            self.record_failure()
            if fallback:
                return fallback()
            raise

    def reset(self) -> None:
        """Reset circuit to closed state."""
        with self._lock:
            self._state = CircuitState.CLOSED
            self._failures = 0
            self._successes = 0
            self._last_failure_time = None
            self._half_open_calls = 0

    def force_open(self) -> None:
        """Force circuit to open state."""
        with self._lock:
            self._state = CircuitState.OPEN
            self._last_failure_time = time.monotonic()


class CircuitOpenError(Exception):
    """Raised when circuit is open and request is blocked."""
    pass
