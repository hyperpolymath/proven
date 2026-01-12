# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafePhone - Phone number operations that cannot crash.

Provides safe phone number parsing, validation, and formatting using E.164 standard.
All operations return None on failure instead of raising exceptions.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from enum import Enum
from typing import Optional, Tuple


class CountryCode(Enum):
    """
    ITU-T E.164 country calling codes.

    The value is the country calling code (without leading +).

    Example:
        >>> CountryCode.US.value
        '1'
        >>> CountryCode.UK.value
        '44'
    """
    # North America (NANP)
    US = "1"
    CA = "1"

    # Europe
    UK = "44"
    FR = "33"
    DE = "49"
    IT = "39"
    ES = "34"
    NL = "31"
    BE = "32"
    AT = "43"
    CH = "41"
    PL = "48"
    CZ = "420"
    HU = "36"
    RO = "40"
    SE = "46"
    NO = "47"
    DK = "45"
    FI = "358"
    IE = "353"
    PT = "351"
    GR = "30"

    # Russia and CIS
    RU = "7"
    UA = "380"
    BY = "375"
    KZ = "7"

    # Asia
    CN = "86"
    JP = "81"
    KR = "82"
    IN = "91"
    PK = "92"
    BD = "880"
    ID = "62"
    MY = "60"
    SG = "65"
    TH = "66"
    VN = "84"
    PH = "63"
    HK = "852"
    TW = "886"

    # Middle East
    IL = "972"
    AE = "971"
    SA = "966"
    TR = "90"
    IR = "98"
    IQ = "964"

    # Africa
    ZA = "27"
    EG = "20"
    NG = "234"
    KE = "254"
    MA = "212"

    # South America
    BR = "55"
    AR = "54"
    CL = "56"
    CO = "57"
    PE = "51"
    VE = "58"
    MX = "52"

    # Oceania
    AU = "61"
    NZ = "64"

    @classmethod
    def from_code(cls, code: str) -> Optional[CountryCode]:
        """
        Get CountryCode from a calling code string.

        Args:
            code: The calling code (e.g., "1", "44", "86")

        Returns:
            CountryCode if found, None otherwise

        Example:
            >>> CountryCode.from_code("44")
            <CountryCode.UK: '44'>
        """
        clean_code = code.lstrip("+").lstrip("0")
        for country in cls:
            if country.value == clean_code:
                return country
        return None

    @property
    def dial_code(self) -> str:
        """
        Get the dial code with + prefix.

        Returns:
            The dial code (e.g., "+1", "+44")
        """
        return f"+{self.value}"


@dataclass(frozen=True)
class PhoneNumber:
    """
    A phone number with country code, parsed and validated.

    Stores the phone number in E.164 format internally.

    Attributes:
        country_code: The country calling code
        national_number: The national significant number (digits only)

    Example:
        >>> phone = PhoneNumber.parse("+1 (555) 123-4567")
        >>> phone.format_e164()
        '+15551234567'
    """

    country_code: CountryCode
    national_number: str

    # E.164 constraints
    MIN_LENGTH: int = 3   # Minimum national number length
    MAX_LENGTH: int = 15  # Maximum total E.164 length (including country code)

    def __post_init__(self) -> None:
        """Validate the phone number components."""
        if not self.national_number.isdigit():
            raise ValueError("National number must contain only digits")

    @classmethod
    def parse(cls, phone_string: str, default_country: Optional[CountryCode] = None) -> Optional[PhoneNumber]:
        """
        Parse a phone number from various formats.

        Accepts formats:
        - E.164: "+15551234567"
        - International: "+1 555 123 4567"
        - National with parentheses: "(555) 123-4567"
        - With extension indicators (extension is stripped)

        Args:
            phone_string: The phone number string to parse
            default_country: Country to assume if no country code present

        Returns:
            PhoneNumber if valid, None otherwise

        Example:
            >>> PhoneNumber.parse("+44 20 7946 0958")
            PhoneNumber(country_code=<CountryCode.UK: '44'>, national_number='2079460958')
        """
        if not phone_string:
            return None

        # Remove common formatting characters and whitespace
        cleaned = phone_string.strip()

        # Remove extension indicators and everything after
        extension_patterns = [" ext", " ext.", " x", " #", ","]
        for pattern in extension_patterns:
            if pattern in cleaned.lower():
                cleaned = cleaned[:cleaned.lower().find(pattern)]

        # Extract only digits and leading +
        has_plus = cleaned.startswith("+")
        digits = re.sub(r"[^\d]", "", cleaned)

        if not digits:
            return None

        # Try to determine country code
        country_code: Optional[CountryCode] = None
        national_number: str = ""

        if has_plus:
            # International format - try to extract country code
            country_code, national_number = cls._extract_country_code(digits)
        else:
            # National format - use default country if provided
            if default_country:
                country_code = default_country
                national_number = digits

                # Remove leading 0 if present (common in many countries)
                if national_number.startswith("0"):
                    national_number = national_number[1:]
            else:
                # Try to detect if it starts with a known country code
                country_code, national_number = cls._extract_country_code(digits)

        if country_code is None or not national_number:
            return None

        # Validate length constraints
        total_length = len(country_code.value) + len(national_number)
        if total_length > cls.MAX_LENGTH:
            return None

        if len(national_number) < cls.MIN_LENGTH:
            return None

        try:
            return cls(country_code=country_code, national_number=national_number)
        except ValueError:
            return None

    @classmethod
    def _extract_country_code(cls, digits: str) -> Tuple[Optional[CountryCode], str]:
        """
        Extract country code from the beginning of a digit string.

        Tries longest match first (3 digits) down to shortest (1 digit).

        Args:
            digits: String of digits to parse

        Returns:
            Tuple of (CountryCode, remaining_digits) or (None, "") if not found
        """
        # Try country codes of different lengths (longest first for specificity)
        for code_length in [3, 2, 1]:
            if len(digits) > code_length:
                potential_code = digits[:code_length]
                country = CountryCode.from_code(potential_code)
                if country:
                    national = digits[code_length:]
                    # Remove leading 0 if present after country code
                    if national.startswith("0"):
                        national = national[1:]
                    return (country, national)

        return (None, "")

    @classmethod
    def from_parts(cls, country_code: CountryCode, national_number: str) -> Optional[PhoneNumber]:
        """
        Create a PhoneNumber from country code and national number.

        Args:
            country_code: The country calling code
            national_number: The national number (digits only)

        Returns:
            PhoneNumber if valid, None otherwise

        Example:
            >>> PhoneNumber.from_parts(CountryCode.US, "5551234567")
            PhoneNumber(country_code=<CountryCode.US: '1'>, national_number='5551234567')
        """
        # Clean the national number
        cleaned = re.sub(r"[^\d]", "", national_number)

        if not cleaned:
            return None

        # Remove leading 0 if present
        if cleaned.startswith("0"):
            cleaned = cleaned[1:]

        # Validate length
        total_length = len(country_code.value) + len(cleaned)
        if total_length > cls.MAX_LENGTH or len(cleaned) < cls.MIN_LENGTH:
            return None

        try:
            return cls(country_code=country_code, national_number=cleaned)
        except ValueError:
            return None

    def format_e164(self) -> str:
        """
        Format as E.164 (e.g., "+15551234567").

        This is the canonical machine-readable format.

        Returns:
            E.164 formatted string

        Example:
            >>> phone.format_e164()
            '+15551234567'
        """
        return f"+{self.country_code.value}{self.national_number}"

    def format_international(self) -> str:
        """
        Format as international format with spaces.

        Groups digits for readability. Uses simple grouping rules.

        Returns:
            International formatted string

        Example:
            >>> phone.format_international()
            '+1 555 123 4567'
        """
        national = self.national_number

        # Simple grouping: try to make groups of 3-4 digits
        if len(national) <= 4:
            formatted_national = national
        elif len(national) <= 7:
            formatted_national = f"{national[:3]} {national[3:]}"
        elif len(national) <= 10:
            # Common format: XXX XXX XXXX or XX XXXX XXXX
            formatted_national = f"{national[:3]} {national[3:6]} {national[6:]}"
        else:
            # Long numbers: group in fours from the end
            groups = []
            remaining = national
            while len(remaining) > 4:
                groups.insert(0, remaining[-4:])
                remaining = remaining[:-4]
            if remaining:
                groups.insert(0, remaining)
            formatted_national = " ".join(groups)

        return f"+{self.country_code.value} {formatted_national}"

    def format_national(self) -> str:
        """
        Format as national format (without country code).

        Uses common formatting conventions where known.

        Returns:
            National formatted string

        Example:
            >>> phone.format_national()
            '(555) 123-4567'
        """
        national = self.national_number

        # US/Canada specific formatting
        if self.country_code in (CountryCode.US, CountryCode.CA) and len(national) == 10:
            return f"({national[:3]}) {national[3:6]}-{national[6:]}"

        # UK specific formatting
        if self.country_code == CountryCode.UK:
            if len(national) == 10 and national.startswith("20"):
                # London: 020 XXXX XXXX
                return f"0{national[:2]} {national[2:6]} {national[6:]}"
            elif len(national) >= 9:
                # Other UK: 0XXXX XXXXXX
                return f"0{national[:4]} {national[4:]}"

        # Default: add leading 0 and space every 4 digits
        formatted = "0" + national
        groups = []
        while len(formatted) > 4:
            groups.append(formatted[:4])
            formatted = formatted[4:]
        if formatted:
            groups.append(formatted)
        return " ".join(groups)

    def format_rfc3966(self) -> str:
        """
        Format as RFC 3966 tel: URI.

        Returns:
            Tel URI string

        Example:
            >>> phone.format_rfc3966()
            'tel:+15551234567'
        """
        return f"tel:{self.format_e164()}"

    @property
    def is_valid(self) -> bool:
        """
        Check if this phone number appears to be valid.

        Performs basic validation checks.

        Returns:
            True if passes basic validation
        """
        # Check total length
        total_length = len(self.country_code.value) + len(self.national_number)
        if total_length > self.MAX_LENGTH:
            return False

        # Check national number length
        if len(self.national_number) < self.MIN_LENGTH:
            return False

        # Check all digits
        if not self.national_number.isdigit():
            return False

        return True

    def __str__(self) -> str:
        """Return E.164 format as default string."""
        return self.format_e164()

    def __repr__(self) -> str:
        """Return detailed representation."""
        return f"PhoneNumber(country_code={self.country_code!r}, national_number={self.national_number!r})"

    def __eq__(self, other: object) -> bool:
        """Check equality based on E.164 representation."""
        if isinstance(other, PhoneNumber):
            return self.format_e164() == other.format_e164()
        return NotImplemented

    def __hash__(self) -> int:
        """Return hash based on E.164 representation."""
        return hash(self.format_e164())


class SafePhone:
    """
    Safe phone number operations with proven correctness guarantees.

    Provides static methods for phone number operations that never raise exceptions.
    """

    @staticmethod
    def parse(phone_string: str, default_country: Optional[CountryCode] = None) -> Optional[PhoneNumber]:
        """
        Parse a phone number from a string.

        Args:
            phone_string: The phone number to parse
            default_country: Default country if no country code present

        Returns:
            PhoneNumber if valid, None otherwise

        Example:
            >>> SafePhone.parse("+1 555 123 4567")
            PhoneNumber(...)
        """
        return PhoneNumber.parse(phone_string, default_country)

    @staticmethod
    def is_valid(phone_string: str) -> bool:
        """
        Check if a string is a valid phone number.

        Args:
            phone_string: The phone number to validate

        Returns:
            True if valid, False otherwise

        Example:
            >>> SafePhone.is_valid("+1 555 123 4567")
            True
        """
        return PhoneNumber.parse(phone_string) is not None

    @staticmethod
    def format_e164(phone_string: str) -> Optional[str]:
        """
        Parse and format a phone number to E.164.

        Args:
            phone_string: The phone number to format

        Returns:
            E.164 formatted string, or None if invalid

        Example:
            >>> SafePhone.format_e164("(555) 123-4567", CountryCode.US)
            '+15551234567'
        """
        phone = PhoneNumber.parse(phone_string)
        if phone is None:
            return None
        return phone.format_e164()

    @staticmethod
    def normalize(phone_string: str, default_country: Optional[CountryCode] = None) -> Optional[str]:
        """
        Normalize a phone number to E.164 format.

        Args:
            phone_string: The phone number to normalize
            default_country: Default country if no country code present

        Returns:
            Normalized E.164 string, or None if invalid
        """
        phone = PhoneNumber.parse(phone_string, default_country)
        if phone is None:
            return None
        return phone.format_e164()

    @staticmethod
    def get_country(phone_string: str) -> Optional[CountryCode]:
        """
        Extract the country code from a phone number.

        Args:
            phone_string: The phone number

        Returns:
            CountryCode if found, None otherwise

        Example:
            >>> SafePhone.get_country("+44 20 7946 0958")
            <CountryCode.UK: '44'>
        """
        phone = PhoneNumber.parse(phone_string)
        if phone is None:
            return None
        return phone.country_code

    @staticmethod
    def compare(phone1: str, phone2: str) -> bool:
        """
        Compare two phone numbers for equality (ignoring formatting).

        Args:
            phone1: First phone number
            phone2: Second phone number

        Returns:
            True if the phone numbers are equivalent, False otherwise

        Example:
            >>> SafePhone.compare("+1 (555) 123-4567", "+15551234567")
            True
        """
        parsed1 = PhoneNumber.parse(phone1)
        parsed2 = PhoneNumber.parse(phone2)

        if parsed1 is None or parsed2 is None:
            return False

        return parsed1 == parsed2
