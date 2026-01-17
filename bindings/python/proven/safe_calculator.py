# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeCalculator - Expression evaluator with overflow and division protection.

Evaluates mathematical expressions safely without crashes or security issues.
"""

from typing import Optional, Dict, List, Union
from dataclasses import dataclass
from enum import Enum
import re
import math


class TokenType(Enum):
    """Token types for expression parsing."""
    NUMBER = "number"
    OPERATOR = "operator"
    LPAREN = "lparen"
    RPAREN = "rparen"
    FUNCTION = "function"
    VARIABLE = "variable"
    COMMA = "comma"


@dataclass
class Token:
    """Expression token."""
    type: TokenType
    value: str
    number: Optional[float] = None


class SafeCalculator:
    """Safe mathematical expression evaluator."""

    # Supported operators with precedence
    _OPERATORS = {
        "+": (1, "left"),
        "-": (1, "left"),
        "*": (2, "left"),
        "/": (2, "left"),
        "%": (2, "left"),
        "^": (3, "right"),
    }

    # Supported functions
    _FUNCTIONS = {
        "abs": (abs, 1),
        "sqrt": (math.sqrt, 1),
        "sin": (math.sin, 1),
        "cos": (math.cos, 1),
        "tan": (math.tan, 1),
        "log": (math.log, 1),
        "log10": (math.log10, 1),
        "exp": (math.exp, 1),
        "floor": (math.floor, 1),
        "ceil": (math.ceil, 1),
        "round": (round, 1),
        "min": (min, 2),
        "max": (max, 2),
        "pow": (math.pow, 2),
    }

    # Built-in constants
    _CONSTANTS = {
        "pi": math.pi,
        "e": math.e,
        "tau": math.tau,
    }

    @staticmethod
    def evaluate(expression: str, variables: Optional[Dict[str, float]] = None) -> Optional[float]:
        """
        Evaluate a mathematical expression safely.

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
        try:
            tokens = SafeCalculator._tokenize(expression)
            if tokens is None:
                return None

            # Substitute variables
            vars_dict = {**SafeCalculator._CONSTANTS}
            if variables:
                vars_dict.update(variables)

            # Convert to postfix (RPN)
            postfix = SafeCalculator._to_postfix(tokens)
            if postfix is None:
                return None

            # Evaluate
            result = SafeCalculator._evaluate_postfix(postfix, vars_dict)
            if result is None or not math.isfinite(result):
                return None

            return result
        except Exception:
            return None

    @staticmethod
    def _tokenize(expression: str) -> Optional[List[Token]]:
        """Tokenize an expression."""
        tokens = []
        i = 0
        expression = expression.strip()

        while i < len(expression):
            c = expression[i]

            # Skip whitespace
            if c.isspace():
                i += 1
                continue

            # Number (including decimals)
            if c.isdigit() or (c == "." and i + 1 < len(expression) and expression[i + 1].isdigit()):
                j = i
                has_dot = False
                while j < len(expression) and (expression[j].isdigit() or (expression[j] == "." and not has_dot)):
                    if expression[j] == ".":
                        has_dot = True
                    j += 1
                num_str = expression[i:j]
                try:
                    num = float(num_str)
                    tokens.append(Token(TokenType.NUMBER, num_str, num))
                except ValueError:
                    return None
                i = j
                continue

            # Operator
            if c in SafeCalculator._OPERATORS:
                # Handle unary minus
                if c == "-" and (not tokens or tokens[-1].type in {TokenType.OPERATOR, TokenType.LPAREN, TokenType.COMMA}):
                    # Look for number after minus
                    j = i + 1
                    while j < len(expression) and expression[j].isspace():
                        j += 1
                    if j < len(expression) and (expression[j].isdigit() or expression[j] == "."):
                        k = j
                        has_dot = False
                        while k < len(expression) and (expression[k].isdigit() or (expression[k] == "." and not has_dot)):
                            if expression[k] == ".":
                                has_dot = True
                            k += 1
                        num_str = expression[j:k]
                        try:
                            num = -float(num_str)
                            tokens.append(Token(TokenType.NUMBER, f"-{num_str}", num))
                        except ValueError:
                            return None
                        i = k
                        continue
                tokens.append(Token(TokenType.OPERATOR, c))
                i += 1
                continue

            # Parentheses
            if c == "(":
                tokens.append(Token(TokenType.LPAREN, c))
                i += 1
                continue
            if c == ")":
                tokens.append(Token(TokenType.RPAREN, c))
                i += 1
                continue

            # Comma
            if c == ",":
                tokens.append(Token(TokenType.COMMA, c))
                i += 1
                continue

            # Identifier (function or variable)
            if c.isalpha() or c == "_":
                j = i
                while j < len(expression) and (expression[j].isalnum() or expression[j] == "_"):
                    j += 1
                name = expression[i:j].lower()

                # Check if function
                if name in SafeCalculator._FUNCTIONS:
                    tokens.append(Token(TokenType.FUNCTION, name))
                else:
                    tokens.append(Token(TokenType.VARIABLE, name))
                i = j
                continue

            # Unknown character
            return None

        return tokens

    @staticmethod
    def _to_postfix(tokens: List[Token]) -> Optional[List[Token]]:
        """Convert infix tokens to postfix (RPN) using shunting-yard."""
        output = []
        operator_stack: List[Token] = []

        for token in tokens:
            if token.type == TokenType.NUMBER or token.type == TokenType.VARIABLE:
                output.append(token)

            elif token.type == TokenType.FUNCTION:
                operator_stack.append(token)

            elif token.type == TokenType.COMMA:
                while operator_stack and operator_stack[-1].type != TokenType.LPAREN:
                    output.append(operator_stack.pop())
                if not operator_stack:
                    return None

            elif token.type == TokenType.OPERATOR:
                prec, assoc = SafeCalculator._OPERATORS[token.value]
                while (
                    operator_stack
                    and operator_stack[-1].type == TokenType.OPERATOR
                    and (
                        (assoc == "left" and SafeCalculator._OPERATORS[operator_stack[-1].value][0] >= prec)
                        or (assoc == "right" and SafeCalculator._OPERATORS[operator_stack[-1].value][0] > prec)
                    )
                ):
                    output.append(operator_stack.pop())
                operator_stack.append(token)

            elif token.type == TokenType.LPAREN:
                operator_stack.append(token)

            elif token.type == TokenType.RPAREN:
                while operator_stack and operator_stack[-1].type != TokenType.LPAREN:
                    output.append(operator_stack.pop())
                if not operator_stack:
                    return None
                operator_stack.pop()  # Pop LPAREN
                if operator_stack and operator_stack[-1].type == TokenType.FUNCTION:
                    output.append(operator_stack.pop())

        while operator_stack:
            if operator_stack[-1].type == TokenType.LPAREN:
                return None
            output.append(operator_stack.pop())

        return output

    @staticmethod
    def _evaluate_postfix(tokens: List[Token], variables: Dict[str, float]) -> Optional[float]:
        """Evaluate postfix expression."""
        stack: List[float] = []

        for token in tokens:
            if token.type == TokenType.NUMBER:
                stack.append(token.number)

            elif token.type == TokenType.VARIABLE:
                if token.value not in variables:
                    return None
                stack.append(variables[token.value])

            elif token.type == TokenType.OPERATOR:
                if len(stack) < 2:
                    return None
                b = stack.pop()
                a = stack.pop()
                result = SafeCalculator._apply_operator(token.value, a, b)
                if result is None:
                    return None
                stack.append(result)

            elif token.type == TokenType.FUNCTION:
                func, arity = SafeCalculator._FUNCTIONS[token.value]
                if len(stack) < arity:
                    return None
                args = [stack.pop() for _ in range(arity)][::-1]
                try:
                    result = func(*args)
                    if not math.isfinite(result):
                        return None
                    stack.append(result)
                except (ValueError, ZeroDivisionError, OverflowError):
                    return None

        if len(stack) != 1:
            return None
        return stack[0]

    @staticmethod
    def _apply_operator(op: str, a: float, b: float) -> Optional[float]:
        """Apply operator safely."""
        try:
            if op == "+":
                result = a + b
            elif op == "-":
                result = a - b
            elif op == "*":
                result = a * b
            elif op == "/":
                if b == 0:
                    return None
                result = a / b
            elif op == "%":
                if b == 0:
                    return None
                result = a % b
            elif op == "^":
                result = math.pow(a, b)
            else:
                return None

            if not math.isfinite(result):
                return None
            return result
        except (ValueError, OverflowError):
            return None
