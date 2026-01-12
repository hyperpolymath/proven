# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeEmail - Email validation that cannot crash.

Provides safe email validation without regex catastrophic backtracking.
"""

from typing import Optional, Tuple

from .core import ProvenStatus, ProvenError, get_lib, check_status


class SafeEmail:
    """Safe email operations with proven correctness guarantees."""

    @staticmethod
    def is_valid(email: str) -> bool:
        """
        Check if an email address is valid.

        Uses a proven-correct parser that cannot suffer from
        regex catastrophic backtracking.

        Args:
            email: The email address to validate

        Returns:
            True if valid, False otherwise

        Example:
            >>> SafeEmail.is_valid("user@example.com")
            True
            >>> SafeEmail.is_valid("not-an-email")
            False
            >>> SafeEmail.is_valid("user@.com")
            False
        """
        lib = get_lib()
        encoded = email.encode("utf-8")
        result = lib.proven_email_is_valid(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def split(email: str) -> Optional[Tuple[str, str]]:
        """
        Split an email into local part and domain.

        Args:
            email: The email address

        Returns:
            Tuple of (local_part, domain), or None if invalid

        Example:
            >>> SafeEmail.split("user@example.com")
            ("user", "example.com")
        """
        if not SafeEmail.is_valid(email):
            return None

        # Safe to split since we validated
        at_pos = email.rfind("@")
        if at_pos == -1:
            return None

        return (email[:at_pos], email[at_pos + 1:])

    @staticmethod
    def get_domain(email: str) -> Optional[str]:
        """
        Extract the domain from an email address.

        Args:
            email: The email address

        Returns:
            The domain, or None if invalid

        Example:
            >>> SafeEmail.get_domain("user@example.com")
            "example.com"
        """
        parts = SafeEmail.split(email)
        if parts is None:
            return None
        return parts[1]

    @staticmethod
    def get_local_part(email: str) -> Optional[str]:
        """
        Extract the local part from an email address.

        Args:
            email: The email address

        Returns:
            The local part (before @), or None if invalid

        Example:
            >>> SafeEmail.get_local_part("user@example.com")
            "user"
        """
        parts = SafeEmail.split(email)
        if parts is None:
            return None
        return parts[0]

    @staticmethod
    def normalize(email: str) -> Optional[str]:
        """
        Normalize an email address (lowercase domain).

        Args:
            email: The email address

        Returns:
            Normalized email, or None if invalid

        Example:
            >>> SafeEmail.normalize("User@EXAMPLE.COM")
            "User@example.com"
        """
        parts = SafeEmail.split(email)
        if parts is None:
            return None
        return f"{parts[0]}@{parts[1].lower()}"
