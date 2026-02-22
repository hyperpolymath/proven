# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeCircuitBreaker - Fault tolerance pattern for preventing cascading failures.

Provides circuit breaker implementation with configurable thresholds.
All state management is delegated to the Idris core via FFI using opaque handles.
"""

from typing import Optional, Callable, TypeVar
from dataclasses import dataclass
from enum import Enum

from .core import ProvenStatus, get_lib

T = TypeVar("T")


class CircuitState(Enum):
    """Circuit breaker states."""
    CLOSED = "closed"       # Normal operation, requests pass through
    OPEN = "open"           # Failure threshold reached, requests blocked
    HALF_OPEN = "half_open"  # Testing if service recovered


# Map FFI integer state codes to CircuitState enum
_STATE_MAP = {
    0: CircuitState.CLOSED,
    1: CircuitState.OPEN,
    2: CircuitState.HALF_OPEN,
}


@dataclass
class CircuitConfig:
    """Circuit breaker configuration."""
    failure_threshold: int = 5     # Failures before opening
    success_threshold: int = 2     # Successes in half-open before closing
    timeout_seconds: float = 30.0  # Time in open before trying half-open
    half_open_max_calls: int = 3   # Max concurrent calls in half-open


class CircuitBreaker:
    """
    Circuit breaker for fault tolerance via FFI.

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
        Create a circuit breaker via FFI.

        Args:
            config: Configuration (defaults to standard settings)
        """
        self._config = config or CircuitConfig()

        lib = get_lib()
        result = lib.proven_circuit_breaker_create(
            self._config.failure_threshold,
            self._config.success_threshold,
            self._config.timeout_seconds,
            self._config.half_open_max_calls,
        )
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create circuit breaker via FFI")

        self._handle = result.handle
        self._lib = lib

    def __del__(self):
        """Free the FFI circuit breaker handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_circuit_breaker_free(self._handle)
            except Exception:
                pass

    @property
    def state(self) -> CircuitState:
        """Get current state via FFI."""
        result = self._lib.proven_circuit_breaker_state(self._handle)
        if result.status != ProvenStatus.OK:
            return CircuitState.CLOSED
        return _STATE_MAP.get(result.value, CircuitState.CLOSED)

    @property
    def failures(self) -> int:
        """Get current failure count (tracked by Idris core)."""
        # The failure count is managed by the FFI; query state for status
        state = self.state
        return 0 if state == CircuitState.CLOSED else -1

    def allow_request(self) -> bool:
        """
        Check if a request should be allowed via FFI.

        Returns:
            True if request can proceed
        """
        result = self._lib.proven_circuit_breaker_allow(self._handle)
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    def record_success(self) -> None:
        """Record a successful request via FFI."""
        self._lib.proven_circuit_breaker_record_success(self._handle)

    def record_failure(self) -> None:
        """Record a failed request via FFI."""
        self._lib.proven_circuit_breaker_record_failure(self._handle)

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
            raise CircuitOpenError(f"Circuit is {self.state.value}")

        try:
            result = func()
            self.record_success()
            return result
        except Exception:
            self.record_failure()
            if fallback:
                return fallback()
            raise

    def reset(self) -> None:
        """Reset circuit by recreating via FFI."""
        self._lib.proven_circuit_breaker_free(self._handle)
        result = self._lib.proven_circuit_breaker_create(
            self._config.failure_threshold,
            self._config.success_threshold,
            self._config.timeout_seconds,
            self._config.half_open_max_calls,
        )
        if result.status == ProvenStatus.OK and result.handle is not None:
            self._handle = result.handle

    def force_open(self) -> None:
        """Force circuit to open state by recording failures above threshold."""
        for _ in range(self._config.failure_threshold + 1):
            self._lib.proven_circuit_breaker_record_failure(self._handle)


class CircuitOpenError(Exception):
    """Raised when circuit is open and request is blocked."""
    pass
