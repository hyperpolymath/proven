# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeCalculator - Expression evaluator with overflow and division protection.

Evaluates mathematical expressions safely without crashes or security issues.
All expression evaluation is delegated to the Idris core via FFI.
"""

from typing import Optional, Dict

from .core import ProvenStatus, get_lib


class SafeCalculator:
    """Safe mathematical expression evaluator via FFI."""

    @staticmethod
    def evaluate(expression: str, variables: Optional[Dict[str, float]] = None) -> Optional[float]:
        """
        Evaluate a mathematical expression safely via FFI.

        Args:
            expression: Mathematical expression string
            variables: Optional variable bindings

        Returns:
            Result, or None if expression is invalid or would overflow

        Example:
            >>> SafeCalculator.evaluate("2 + 3 * 4")
            14.0
            >>> SafeCalculator.evaluate("sqrt(16) + x", {"x": 5})
            9.0
            >>> SafeCalculator.evaluate("1 / 0")
            None
        """
        if not expression:
            return None

        # Substitute variables into the expression string
        expr = expression
        if variables:
            for name, value in variables.items():
                expr = expr.replace(name, str(value))

        lib = get_lib()
        encoded = expr.encode("utf-8")
        result = lib.proven_calculator_evaluate(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return None
        return result.value
