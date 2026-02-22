# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafePassword - Password validation and strength analysis.

Provides secure password policy validation without security holes.
All validation logic is delegated to the Idris core via FFI.
"""

import json
from typing import Optional, List, NamedTuple
from dataclasses import dataclass
from enum import Enum

from .core import ProvenStatus, ProvenError, get_lib, check_status


class PasswordStrength(Enum):
    """Password strength levels."""
    VERY_WEAK = 0
    WEAK = 1
    FAIR = 2
    STRONG = 3
    VERY_STRONG = 4


@dataclass
class PasswordPolicy:
    """Password policy configuration."""
    min_length: int = 8
    max_length: int = 128
    require_uppercase: bool = True
    require_lowercase: bool = True
    require_digit: bool = True
    require_special: bool = False
    special_chars: str = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
    min_unique_chars: int = 4
    disallow_common: bool = True


class PasswordValidation(NamedTuple):
    """Result of password validation."""
    valid: bool
    errors: List[str]
    strength: PasswordStrength
    entropy_bits: float


class SafePassword:
    """Secure password validation and analysis via FFI."""

    @staticmethod
    def validate(password: str,
                 policy: Optional[PasswordPolicy] = None) -> PasswordValidation:
        """
        Validate a password against a policy via FFI.

        Args:
            password: The password to validate
            policy: The policy to use (defaults to standard policy)

        Returns:
            PasswordValidation with validity, errors, strength, and entropy
        """
        if policy is None:
            policy = PasswordPolicy()

        lib = get_lib()
        encoded = password.encode("utf-8")
        result = lib.proven_password_validate(
            encoded, len(encoded),
            policy.min_length,
            policy.max_length,
            policy.require_uppercase,
            policy.require_lowercase,
            policy.require_digit,
            policy.require_special,
        )

        if result.status != ProvenStatus.OK or result.value is None:
            # Treat FFI failure as validation failure
            return PasswordValidation(
                valid=False,
                errors=["FFI validation error"],
                strength=PasswordStrength.VERY_WEAK,
                entropy_bits=0.0,
            )

        # Parse the JSON result from FFI
        result_json = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        parsed = json.loads(result_json)

        strength_map = {
            0: PasswordStrength.VERY_WEAK,
            1: PasswordStrength.WEAK,
            2: PasswordStrength.FAIR,
            3: PasswordStrength.STRONG,
            4: PasswordStrength.VERY_STRONG,
        }

        return PasswordValidation(
            valid=parsed.get("valid", False),
            errors=parsed.get("errors", []),
            strength=strength_map.get(parsed.get("strength", 0),
                                      PasswordStrength.VERY_WEAK),
            entropy_bits=parsed.get("entropy_bits", 0.0),
        )

    @staticmethod
    def calculate_entropy(password: str) -> float:
        """
        Calculate password entropy in bits via FFI.

        Args:
            password: The password to analyze

        Returns:
            Entropy in bits
        """
        if not password:
            return 0.0

        lib = get_lib()
        encoded = password.encode("utf-8")
        result = lib.proven_password_entropy(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return 0.0
        return result.value

    @staticmethod
    def check_common(password: str) -> bool:
        """
        Check if password is in the common passwords list via FFI.

        Args:
            password: The password to check

        Returns:
            True if password is common (bad), False if not common (good)
        """
        lib = get_lib()
        encoded = password.encode("utf-8")
        result = lib.proven_password_is_common(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def has_sequential(password: str, length: int = 3) -> bool:
        """
        Check if password contains sequential characters via FFI.

        Args:
            password: The password to check
            length: Minimum sequence length to detect

        Returns:
            True if sequential pattern found
        """
        lib = get_lib()
        encoded = password.encode("utf-8")
        result = lib.proven_password_has_sequential(encoded, len(encoded), length)
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def has_repeated(password: str, length: int = 3) -> bool:
        """
        Check if password contains repeated characters via FFI.

        Args:
            password: The password to check
            length: Minimum repetition length to detect

        Returns:
            True if repetition found
        """
        lib = get_lib()
        encoded = password.encode("utf-8")
        result = lib.proven_password_has_repeated(encoded, len(encoded), length)
        if result.status != ProvenStatus.OK:
            return False
        return result.value
