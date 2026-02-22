# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeProbability - Probability values clamped to [0,1].

Provides type-safe probability operations with automatic clamping.
All computation is delegated to the Idris core via FFI.
"""

from typing import Optional
from dataclasses import dataclass
from functools import total_ordering

from .core import ProvenStatus, get_lib


@total_ordering
@dataclass(frozen=True)
class Probability:
    """
    Probability value guaranteed to be in [0, 1].

    Example:
        >>> p = Probability(0.7)
        >>> p.complement()
        Probability(value=0.3)
        >>> Probability.and_(Probability(0.5), Probability(0.5))
        Probability(value=0.25)
    """
    value: float

    def __new__(cls, value: float) -> "Probability":
        """Create with clamping to [0, 1] via FFI."""
        instance = object.__new__(cls)
        lib = get_lib()
        result = lib.proven_probability_clamp(value)
        if result.status == ProvenStatus.OK:
            clamped = result.value
        else:
            clamped = max(0.0, min(1.0, value))
        object.__setattr__(instance, "value", clamped)
        return instance

    def complement(self) -> "Probability":
        """Get complement (1 - p) via FFI."""
        lib = get_lib()
        result = lib.proven_probability_not(self.value)
        if result.status == ProvenStatus.OK:
            return Probability(result.value)
        return Probability(1.0 - self.value)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Probability):
            return NotImplemented
        return abs(self.value - other.value) < 1e-10

    def __lt__(self, other: "Probability") -> bool:
        if not isinstance(other, Probability):
            return NotImplemented
        return self.value < other.value

    def __hash__(self) -> int:
        return hash(self.value)

    def __float__(self) -> float:
        return self.value

    def __mul__(self, other: "Probability") -> "Probability":
        """Multiply probabilities (AND for independent events) via FFI."""
        lib = get_lib()
        result = lib.proven_probability_and(self.value, other.value)
        if result.status == ProvenStatus.OK:
            return Probability(result.value)
        return Probability(self.value * other.value)

    def is_certain(self) -> bool:
        """Check if probability is 1.0."""
        return abs(self.value - 1.0) < 1e-10

    def is_impossible(self) -> bool:
        """Check if probability is 0.0."""
        return abs(self.value) < 1e-10

    def to_odds(self) -> Optional[float]:
        """
        Convert to odds (p / (1-p)).

        Returns:
            Odds ratio, or None if p == 1
        """
        if abs(self.value - 1.0) < 1e-10:
            return None
        lib = get_lib()
        result = lib.proven_float_div(self.value, 1.0 - self.value)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    def to_percentage(self) -> float:
        """Convert to percentage (0-100)."""
        lib = get_lib()
        result = lib.proven_float_mul(self.value, 100.0)
        if result.status != ProvenStatus.OK:
            return self.value * 100.0
        return result.value

    @staticmethod
    def from_odds(odds: float) -> "Probability":
        """
        Create from odds ratio.

        Args:
            odds: Odds ratio (p / (1-p))

        Returns:
            Probability
        """
        if odds < 0:
            return Probability(0.0)
        lib = get_lib()
        result = lib.proven_float_div(odds, 1.0 + odds)
        if result.status != ProvenStatus.OK:
            return Probability(0.0)
        return Probability(result.value)

    @staticmethod
    def from_percentage(percent: float) -> "Probability":
        """
        Create from percentage (0-100).

        Args:
            percent: Percentage value

        Returns:
            Probability
        """
        lib = get_lib()
        result = lib.proven_float_div(percent, 100.0)
        if result.status != ProvenStatus.OK:
            return Probability(percent / 100.0)
        return Probability(result.value)

    @staticmethod
    def and_(a: "Probability", b: "Probability") -> "Probability":
        """
        P(A and B) for independent events via FFI.

        Args:
            a: P(A)
            b: P(B)

        Returns:
            P(A and B) = P(A) * P(B)
        """
        lib = get_lib()
        result = lib.proven_probability_and(a.value, b.value)
        if result.status == ProvenStatus.OK:
            return Probability(result.value)
        return Probability(a.value * b.value)

    @staticmethod
    def or_(a: "Probability", b: "Probability") -> "Probability":
        """
        P(A or B) for independent events via FFI.

        Args:
            a: P(A)
            b: P(B)

        Returns:
            P(A or B) = P(A) + P(B) - P(A)*P(B)
        """
        lib = get_lib()
        result = lib.proven_probability_or(a.value, b.value)
        if result.status == ProvenStatus.OK:
            return Probability(result.value)
        return Probability(a.value + b.value - a.value * b.value)

    @staticmethod
    def not_(p: "Probability") -> "Probability":
        """
        P(not A) via FFI.

        Args:
            p: P(A)

        Returns:
            P(not A) = 1 - P(A)
        """
        return p.complement()

    @staticmethod
    def conditional(a_given_b: "Probability", b: "Probability") -> "Probability":
        """
        P(A and B) given P(A|B) and P(B) via FFI.

        Args:
            a_given_b: P(A|B)
            b: P(B)

        Returns:
            P(A and B) = P(A|B) * P(B)
        """
        return Probability.and_(a_given_b, b)

    @staticmethod
    def bayes(
        likelihood: "Probability",  # P(B|A)
        prior: "Probability",       # P(A)
        evidence: "Probability",    # P(B)
    ) -> Optional["Probability"]:
        """
        Bayes' theorem: P(A|B) = P(B|A) * P(A) / P(B) via FFI.

        Args:
            likelihood: P(B|A)
            prior: P(A)
            evidence: P(B)

        Returns:
            P(A|B), or None if P(B) == 0
        """
        if evidence.is_impossible():
            return None
        lib = get_lib()
        result = lib.proven_probability_bayes(likelihood.value, prior.value, evidence.value)
        if result.status != ProvenStatus.OK:
            return None
        return Probability(result.value)

    @staticmethod
    def binomial(n: int, k: int, p: "Probability") -> "Probability":
        """
        Binomial probability: P(X = k) for n trials with probability p via FFI.

        Args:
            n: Number of trials
            k: Number of successes
            p: Probability of success per trial

        Returns:
            P(X = k)
        """
        if k < 0 or k > n or n < 0:
            return Probability(0.0)

        lib = get_lib()
        result = lib.proven_probability_binomial(n, k, p.value)
        if result.status != ProvenStatus.OK:
            return Probability(0.0)
        return Probability(result.value)


class SafeProbability:
    """Safe probability utilities via FFI."""

    CERTAIN = Probability(1.0)
    IMPOSSIBLE = Probability(0.0)
    EVEN = Probability(0.5)

    @staticmethod
    def create(value: float) -> Probability:
        """Create a probability with clamping."""
        return Probability(value)

    @staticmethod
    def is_valid(value: float) -> bool:
        """Check if value is in valid probability range."""
        return 0.0 <= value <= 1.0

    @staticmethod
    def weighted_average(probs: list, weights: list) -> Optional[Probability]:
        """
        Calculate weighted average of probabilities via FFI.

        Args:
            probs: List of Probability values
            weights: List of weights (must be non-negative)

        Returns:
            Weighted average, or None if invalid
        """
        if len(probs) != len(weights) or not probs:
            return None
        if any(w < 0 for w in weights):
            return None

        lib = get_lib()
        total_weight = sum(weights)
        if total_weight == 0:
            return None

        weighted_sum = 0.0
        for p, w in zip(probs, weights):
            product = lib.proven_float_mul(p.value, w)
            if product.status == ProvenStatus.OK:
                weighted_sum += product.value
            else:
                weighted_sum += p.value * w

        result = lib.proven_float_div(weighted_sum, total_weight)
        if result.status != ProvenStatus.OK:
            return None
        return Probability(result.value)
