# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeStateMachine - Type-safe state transitions.

Provides state machines with validated transitions and guards.
"""

from typing import TypeVar, Generic, Optional, Set, Dict, List, Callable
from dataclasses import dataclass, field

S = TypeVar("S")


class InvalidTransitionError(Exception):
    """Raised when an invalid state transition is attempted."""
    pass


class StateMachine(Generic[S]):
    """
    State machine with validated transitions.

    Example:
        >>> class State:
        ...     PENDING = "pending"
        ...     CONFIRMED = "confirmed"
        ...     SHIPPED = "shipped"
        ...
        >>> sm = StateMachine(State.PENDING)
        >>> sm.add_transition(State.PENDING, State.CONFIRMED)
        >>> sm.add_transition(State.CONFIRMED, State.SHIPPED)
        >>> sm.transition(State.CONFIRMED)
        'confirmed'
        >>> sm.transition(State.SHIPPED)
        'shipped'
    """

    def __init__(self, initial: S, max_history: int = 100):
        """
        Create a state machine.

        Args:
            initial: Initial state
            max_history: Maximum history length
        """
        self._current = initial
        self._transitions: Dict[S, Set[S]] = {}
        self._history: List[S] = []
        self._max_history = max_history
        self._guards: Dict[tuple, Callable[[], bool]] = {}

    @property
    def current(self) -> S:
        """Get current state."""
        return self._current

    @property
    def history(self) -> List[S]:
        """Get transition history."""
        return list(self._history)

    def add_transition(self, from_state: S, to_state: S) -> None:
        """
        Add a valid transition.

        Args:
            from_state: Source state
            to_state: Target state
        """
        if from_state not in self._transitions:
            self._transitions[from_state] = set()
        self._transitions[from_state].add(to_state)

    def add_transitions(self, from_state: S, to_states: List[S]) -> None:
        """
        Add multiple transitions from a state.

        Args:
            from_state: Source state
            to_states: List of target states
        """
        for to_state in to_states:
            self.add_transition(from_state, to_state)

    def add_guard(self, from_state: S, to_state: S, guard: Callable[[], bool]) -> None:
        """
        Add a guard condition for a transition.

        Args:
            from_state: Source state
            to_state: Target state
            guard: Function that returns True if transition is allowed
        """
        self._guards[(from_state, to_state)] = guard

    def can_transition(self, to_state: S) -> bool:
        """
        Check if transition is valid.

        Args:
            to_state: Target state

        Returns:
            True if transition is valid
        """
        if self._current not in self._transitions:
            return False
        if to_state not in self._transitions[self._current]:
            return False

        # Check guard if exists
        guard = self._guards.get((self._current, to_state))
        if guard and not guard():
            return False

        return True

    def valid_transitions(self) -> List[S]:
        """
        Get valid transitions from current state.

        Returns:
            List of states we can transition to
        """
        if self._current not in self._transitions:
            return []

        valid = []
        for to_state in self._transitions[self._current]:
            guard = self._guards.get((self._current, to_state))
            if guard is None or guard():
                valid.append(to_state)
        return valid

    def transition(self, to_state: S) -> S:
        """
        Transition to a new state.

        Args:
            to_state: Target state

        Returns:
            New current state

        Raises:
            InvalidTransitionError: If transition is not valid
        """
        if not self.can_transition(to_state):
            raise InvalidTransitionError(
                f"Invalid transition from {self._current} to {to_state}"
            )

        # Record history
        self._history.append(self._current)
        while len(self._history) > self._max_history:
            self._history.pop(0)

        self._current = to_state
        return self._current

    def force_transition(self, to_state: S) -> S:
        """
        Force transition without validation (use with caution).

        Args:
            to_state: Target state

        Returns:
            New current state
        """
        self._history.append(self._current)
        while len(self._history) > self._max_history:
            self._history.pop(0)
        self._current = to_state
        return self._current

    def reset(self) -> None:
        """Reset to initial state (first in history or current if no history)."""
        if self._history:
            self._current = self._history[0]
        self._history.clear()

    def clear_history(self) -> None:
        """Clear transition history."""
        self._history.clear()


class StateMachineBuilder(Generic[S]):
    """
    Builder for state machines.

    Example:
        >>> sm = (StateMachineBuilder()
        ...     .initial("pending")
        ...     .transition("pending", "confirmed")
        ...     .transition("confirmed", "shipped")
        ...     .build())
    """

    def __init__(self):
        """Create a builder."""
        self._initial: Optional[S] = None
        self._transitions: Dict[S, Set[S]] = {}
        self._guards: Dict[tuple, Callable[[], bool]] = {}

    def initial(self, state: S) -> "StateMachineBuilder[S]":
        """
        Set the initial state.

        Args:
            state: Initial state

        Returns:
            Self for chaining
        """
        self._initial = state
        return self

    def transition(self, from_state: S, to_state: S) -> "StateMachineBuilder[S]":
        """
        Add a transition.

        Args:
            from_state: Source state
            to_state: Target state

        Returns:
            Self for chaining
        """
        if from_state not in self._transitions:
            self._transitions[from_state] = set()
        self._transitions[from_state].add(to_state)
        return self

    def transitions(self, from_state: S, to_states: List[S]) -> "StateMachineBuilder[S]":
        """
        Add multiple transitions.

        Args:
            from_state: Source state
            to_states: List of target states

        Returns:
            Self for chaining
        """
        for to_state in to_states:
            self.transition(from_state, to_state)
        return self

    def guard(
        self, from_state: S, to_state: S, condition: Callable[[], bool]
    ) -> "StateMachineBuilder[S]":
        """
        Add a guard condition.

        Args:
            from_state: Source state
            to_state: Target state
            condition: Guard function

        Returns:
            Self for chaining
        """
        self._guards[(from_state, to_state)] = condition
        return self

    def build(self) -> StateMachine[S]:
        """
        Build the state machine.

        Returns:
            Configured state machine

        Raises:
            ValueError: If no initial state set
        """
        if self._initial is None:
            raise ValueError("Initial state required")

        sm = StateMachine(self._initial)
        sm._transitions = {k: set(v) for k, v in self._transitions.items()}
        sm._guards = dict(self._guards)
        return sm
