# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafePhone - Phone number operations that cannot crash.

Provides safe phone number parsing, validation, and formatting using E.164 standard.
All parsing and validation is delegated to the Idris core via FFI.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Optional

from .core import ProvenStatus, ProvenError, get_lib, check_status


class CountryCode(Enum):
    """ITU-T E.164 country calling codes."""
    US = "1"
    CA = "1"
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
    RU = "7"
    UA = "380"
    BY = "375"
    KZ = "7"
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
    IL = "972"
    AE = "971"
    SA = "966"
    TR = "90"
    IR = "98"
    IQ = "964"
    ZA = "27"
    EG = "20"
    NG = "234"
    KE = "254"
    MA = "212"
    BR = "55"
    AR = "54"
    CL = "56"
    CO = "57"
    PE = "51"
    VE = "58"
    MX = "52"
    AU = "61"
    NZ = "64"

    @classmethod
    def from_code(cls, code: str) -> Optional[CountryCode]:
        """Get CountryCode from a calling code string."""
        clean_code = code.lstrip("+").lstrip("0")
        for country in cls:
            if country.value == clean_code:
                return country
        return None

    @property
    def dial_code(self) -> str:
        """Get the dial code with + prefix."""
        return f"+{self.value}"


@dataclass(frozen=True)
class PhoneNumber:
    """
    A phone number with country code, parsed and validated via FFI.

    Attributes:
        country_code: The country calling code
        national_number: The national significant number (digits only)
    """

    country_code: CountryCode
    national_number: str

    MIN_LENGTH: int = 3
    MAX_LENGTH: int = 15

    def __post_init__(self) -> None:
        """Validate the phone number components."""
        if not self.national_number.isdigit():
            raise ValueError("National number must contain only digits")

    @classmethod
    def parse(cls, phone_string: str,
              default_country: Optional[CountryCode] = None) -> Optional[PhoneNumber]:
        """
        Parse a phone number from various formats via FFI.

        Args:
            phone_string: The phone number string to parse
            default_country: Country to assume if no country code present

        Returns:
            PhoneNumber if valid, None otherwise
        """
        if not phone_string:
            return None

        lib = get_lib()
        encoded = phone_string.encode("utf-8")
        default_cc = (default_country.value if default_country else "").encode("utf-8")
        result = lib.proven_phone_parse(encoded, len(encoded),
                                        default_cc, len(default_cc))
        if result.status != ProvenStatus.OK:
            return None
        if result.value is None:
            return None

        # FFI returns E.164 string like "+15551234567"
        e164 = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)

        if not e164.startswith("+"):
            return None

        # Extract country code from E.164
        digits = e164[1:]
        for code_length in [3, 2, 1]:
            if len(digits) > code_length:
                potential_code = digits[:code_length]
                country = CountryCode.from_code(potential_code)
                if country:
                    national = digits[code_length:]
                    if national.startswith("0"):
                        national = national[1:]
                    try:
                        return cls(country_code=country, national_number=national)
                    except ValueError:
                        return None

        return None

    @classmethod
    def from_parts(cls, country_code: CountryCode,
                   national_number: str) -> Optional[PhoneNumber]:
        """Create a PhoneNumber from country code and national number."""
        lib = get_lib()
        # Validate by formatting and re-parsing through FFI
        e164_candidate = f"+{country_code.value}{national_number}"
        encoded = e164_candidate.encode("utf-8")
        result = lib.proven_phone_is_valid(encoded, len(encoded))
        if result.status != ProvenStatus.OK or not result.value:
            return None
        try:
            return cls(country_code=country_code, national_number=national_number)
        except ValueError:
            return None

    def format_e164(self) -> str:
        """Format as E.164 (e.g., "+15551234567")."""
        return f"+{self.country_code.value}{self.national_number}"

    def format_international(self) -> str:
        """Format as international format with spaces via FFI."""
        lib = get_lib()
        e164 = self.format_e164().encode("utf-8")
        result = lib.proven_phone_format_international(e164, len(e164))
        if result.status != ProvenStatus.OK or result.value is None:
            return self.format_e164()
        output = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return output

    def format_national(self) -> str:
        """Format as national format (without country code) via FFI."""
        lib = get_lib()
        e164 = self.format_e164().encode("utf-8")
        result = lib.proven_phone_format_national(e164, len(e164))
        if result.status != ProvenStatus.OK or result.value is None:
            return self.national_number
        output = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return output

    def format_rfc3966(self) -> str:
        """Format as RFC 3966 tel: URI."""
        return f"tel:{self.format_e164()}"

    @property
    def is_valid(self) -> bool:
        """Check if this phone number appears to be valid via FFI."""
        lib = get_lib()
        e164 = self.format_e164().encode("utf-8")
        result = lib.proven_phone_is_valid(e164, len(e164))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    def __str__(self) -> str:
        return self.format_e164()

    def __repr__(self) -> str:
        return (f"PhoneNumber(country_code={self.country_code!r}, "
                f"national_number={self.national_number!r})")

    def __eq__(self, other: object) -> bool:
        if isinstance(other, PhoneNumber):
            return self.format_e164() == other.format_e164()
        return NotImplemented

    def __hash__(self) -> int:
        return hash(self.format_e164())


class SafePhone:
    """Safe phone number operations via FFI."""

    @staticmethod
    def parse(phone_string: str,
              default_country: Optional[CountryCode] = None) -> Optional[PhoneNumber]:
        """Parse a phone number from a string."""
        return PhoneNumber.parse(phone_string, default_country)

    @staticmethod
    def is_valid(phone_string: str) -> bool:
        """Check if a string is a valid phone number."""
        lib = get_lib()
        encoded = phone_string.encode("utf-8")
        result = lib.proven_phone_is_valid(encoded, len(encoded))
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def format_e164(phone_string: str) -> Optional[str]:
        """Parse and format a phone number to E.164 via FFI."""
        lib = get_lib()
        encoded = phone_string.encode("utf-8")
        result = lib.proven_phone_format_e164(encoded, len(encoded))
        if result.status != ProvenStatus.OK or result.value is None:
            return None
        output = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return output

    @staticmethod
    def normalize(phone_string: str,
                  default_country: Optional[CountryCode] = None) -> Optional[str]:
        """Normalize a phone number to E.164 format."""
        phone = PhoneNumber.parse(phone_string, default_country)
        if phone is None:
            return None
        return phone.format_e164()

    @staticmethod
    def get_country(phone_string: str) -> Optional[CountryCode]:
        """Extract the country code from a phone number via FFI."""
        lib = get_lib()
        encoded = phone_string.encode("utf-8")
        result = lib.proven_phone_get_country_code(encoded, len(encoded))
        if result.status != ProvenStatus.OK or result.value is None:
            return None
        code = result.value[:result.length].decode("utf-8")
        lib.proven_free_string(result.value)
        return CountryCode.from_code(code)

    @staticmethod
    def compare(phone1: str, phone2: str) -> bool:
        """Compare two phone numbers for equality (ignoring formatting)."""
        parsed1 = PhoneNumber.parse(phone1)
        parsed2 = PhoneNumber.parse(phone2)
        if parsed1 is None or parsed2 is None:
            return False
        return parsed1 == parsed2
