# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafePassword - Password validation and strength analysis.

Provides secure password policy validation without security holes.
"""

from typing import Optional, List, NamedTuple
from dataclasses import dataclass
from enum import Enum
import re
import math


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
    """Secure password validation and analysis."""

    # Common passwords to reject
    COMMON_PASSWORDS = frozenset([
        "password", "123456", "12345678", "qwerty", "abc123",
        "monkey", "1234567", "letmein", "trustno1", "dragon",
        "baseball", "iloveyou", "master", "sunshine", "ashley",
        "bailey", "shadow", "123123", "654321", "superman",
        "qazwsx", "michael", "football", "password1", "password123",
    ])

    @staticmethod
    def validate(password: str, policy: Optional[PasswordPolicy] = None) -> PasswordValidation:
        """
        Validate a password against a policy.

        Args:
            password: The password to validate
            policy: The policy to use (defaults to standard policy)

        Returns:
            PasswordValidation with validity, errors, strength, and entropy

        Example:
            >>> result = SafePassword.validate("MyP@ssw0rd!")
            >>> result.valid
            True
            >>> result.strength
            PasswordStrength.STRONG
        """
        if policy is None:
            policy = PasswordPolicy()

        errors: List[str] = []

        # Length checks
        if len(password) < policy.min_length:
            errors.append(f"Password must be at least {policy.min_length} characters")
        if len(password) > policy.max_length:
            errors.append(f"Password must be at most {policy.max_length} characters")

        # Character class checks
        has_upper = any(c.isupper() for c in password)
        has_lower = any(c.islower() for c in password)
        has_digit = any(c.isdigit() for c in password)
        has_special = any(c in policy.special_chars for c in password)

        if policy.require_uppercase and not has_upper:
            errors.append("Password must contain at least one uppercase letter")
        if policy.require_lowercase and not has_lower:
            errors.append("Password must contain at least one lowercase letter")
        if policy.require_digit and not has_digit:
            errors.append("Password must contain at least one digit")
        if policy.require_special and not has_special:
            errors.append("Password must contain at least one special character")

        # Unique characters
        unique_chars = len(set(password))
        if unique_chars < policy.min_unique_chars:
            errors.append(f"Password must contain at least {policy.min_unique_chars} unique characters")

        # Common password check
        if policy.disallow_common and password.lower() in SafePassword.COMMON_PASSWORDS:
            errors.append("Password is too common")

        # Calculate entropy
        entropy = SafePassword.calculate_entropy(password)

        # Determine strength
        strength = SafePassword._calculate_strength(password, entropy)

        return PasswordValidation(
            valid=len(errors) == 0,
            errors=errors,
            strength=strength,
            entropy_bits=entropy,
        )

    @staticmethod
    def calculate_entropy(password: str) -> float:
        """
        Calculate password entropy in bits.

        Args:
            password: The password to analyze

        Returns:
            Entropy in bits
        """
        if not password:
            return 0.0

        # Count character classes
        charset_size = 0
        if any(c.islower() for c in password):
            charset_size += 26
        if any(c.isupper() for c in password):
            charset_size += 26
        if any(c.isdigit() for c in password):
            charset_size += 10
        if any(not c.isalnum() for c in password):
            charset_size += 32  # Approximate special chars

        if charset_size == 0:
            return 0.0

        return len(password) * math.log2(charset_size)

    @staticmethod
    def _calculate_strength(password: str, entropy: float) -> PasswordStrength:
        """Determine password strength based on entropy and patterns."""
        if entropy < 28:
            return PasswordStrength.VERY_WEAK
        elif entropy < 36:
            return PasswordStrength.WEAK
        elif entropy < 60:
            return PasswordStrength.FAIR
        elif entropy < 80:
            return PasswordStrength.STRONG
        else:
            return PasswordStrength.VERY_STRONG

    @staticmethod
    def check_common(password: str) -> bool:
        """
        Check if password is in the common passwords list.

        Args:
            password: The password to check

        Returns:
            True if password is common (bad), False if not common (good)
        """
        return password.lower() in SafePassword.COMMON_PASSWORDS

    @staticmethod
    def has_sequential(password: str, length: int = 3) -> bool:
        """
        Check if password contains sequential characters.

        Args:
            password: The password to check
            length: Minimum sequence length to detect

        Returns:
            True if sequential pattern found
        """
        if len(password) < length:
            return False

        for i in range(len(password) - length + 1):
            substr = password[i:i + length]
            # Check ascending sequence
            if all(ord(substr[j + 1]) == ord(substr[j]) + 1 for j in range(length - 1)):
                return True
            # Check descending sequence
            if all(ord(substr[j + 1]) == ord(substr[j]) - 1 for j in range(length - 1)):
                return True

        return False

    @staticmethod
    def has_repeated(password: str, length: int = 3) -> bool:
        """
        Check if password contains repeated characters.

        Args:
            password: The password to check
            length: Minimum repetition length to detect

        Returns:
            True if repetition found
        """
        if len(password) < length:
            return False

        for i in range(len(password) - length + 1):
            if len(set(password[i:i + length])) == 1:
                return True

        return False
