# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeCurrency - Currency operations that cannot crash.

Provides safe money arithmetic using minor units (cents, pence, etc.)
to avoid floating-point precision issues. All operations return None
or Result types on failure instead of raising exceptions.
"""

from __future__ import annotations

from dataclasses import dataclass
from decimal import Decimal, InvalidOperation, ROUND_HALF_UP
from enum import Enum
from typing import Optional, Union


class CurrencyCode(Enum):
    """
    ISO 4217 currency codes with their minor unit scale.

    The value is the number of decimal places (minor units).
    For example, USD has 2 decimal places ($1.00 = 100 cents).

    Example:
        >>> CurrencyCode.USD.value
        2
        >>> CurrencyCode.JPY.value
        0
    """
    # Major currencies
    USD = 2   # US Dollar
    EUR = 2   # Euro
    GBP = 2   # British Pound
    JPY = 0   # Japanese Yen (no minor unit)
    CHF = 2   # Swiss Franc
    CAD = 2   # Canadian Dollar
    AUD = 2   # Australian Dollar
    NZD = 2   # New Zealand Dollar

    # Additional currencies
    CNY = 2   # Chinese Yuan
    HKD = 2   # Hong Kong Dollar
    SGD = 2   # Singapore Dollar
    INR = 2   # Indian Rupee
    KRW = 0   # South Korean Won (no minor unit)
    MXN = 2   # Mexican Peso
    BRL = 2   # Brazilian Real
    ZAR = 2   # South African Rand
    SEK = 2   # Swedish Krona
    NOK = 2   # Norwegian Krone
    DKK = 2   # Danish Krone
    PLN = 2   # Polish Zloty
    CZK = 2   # Czech Koruna
    HUF = 2   # Hungarian Forint
    RUB = 2   # Russian Ruble
    TRY = 2   # Turkish Lira

    # Cryptocurrencies (using conventional decimal places)
    BTC = 8   # Bitcoin (satoshis)
    ETH = 18  # Ethereum (wei)

    # Special currencies
    XAU = 6   # Gold (troy ounces, 6 decimal places conventional)
    XAG = 6   # Silver (troy ounces)

    # Zero-decimal currencies
    VND = 0   # Vietnamese Dong
    IDR = 0   # Indonesian Rupiah (technically 2, but practically 0)

    # Three-decimal currencies
    KWD = 3   # Kuwaiti Dinar
    BHD = 3   # Bahraini Dinar
    OMR = 3   # Omani Rial

    @property
    def minor_unit_scale(self) -> int:
        """
        Get the number of decimal places for this currency.

        Returns:
            Number of decimal places (0, 2, 3, etc.)
        """
        return self.value

    @property
    def minor_unit_factor(self) -> int:
        """
        Get the factor to convert major units to minor units.

        Returns:
            10^decimal_places (e.g., 100 for USD, 1 for JPY)

        Example:
            >>> CurrencyCode.USD.minor_unit_factor
            100
        """
        return 10 ** self.value


@dataclass(frozen=True)
class Money:
    """
    A monetary amount with currency, stored as minor units.

    Uses integer arithmetic internally to avoid floating-point errors.
    All operations are safe and return None on failure (overflow, etc.).

    Attributes:
        minor_units: The amount in smallest currency unit (e.g., cents for USD)
        currency: The currency code

    Example:
        >>> price = Money.from_major(19.99, CurrencyCode.USD)
        >>> price.minor_units
        1999
        >>> str(price)
        'USD 19.99'
    """

    minor_units: int
    currency: CurrencyCode

    @classmethod
    def from_major(
        cls,
        amount: Union[int, float, str, Decimal],
        currency: CurrencyCode
    ) -> Optional[Money]:
        """
        Create Money from a major unit amount (e.g., dollars, not cents).

        Args:
            amount: The amount in major units (e.g., 19.99 for $19.99)
            currency: The currency code

        Returns:
            Money instance, or None if conversion fails

        Example:
            >>> Money.from_major(19.99, CurrencyCode.USD)
            Money(minor_units=1999, currency=<CurrencyCode.USD: 2>)
            >>> Money.from_major("invalid", CurrencyCode.USD)
            None
        """
        try:
            decimal_amount = Decimal(str(amount))
            scale = currency.minor_unit_scale

            # Multiply and round to get minor units
            minor_decimal = decimal_amount * (10 ** scale)

            # Round to nearest integer (banker's rounding would be ROUND_HALF_EVEN)
            rounded = minor_decimal.quantize(Decimal("1"), rounding=ROUND_HALF_UP)

            return cls(minor_units=int(rounded), currency=currency)
        except (InvalidOperation, ValueError, OverflowError):
            return None

    @classmethod
    def from_minor(cls, minor_units: int, currency: CurrencyCode) -> Money:
        """
        Create Money from minor units directly.

        Args:
            minor_units: The amount in smallest currency unit
            currency: The currency code

        Returns:
            Money instance

        Example:
            >>> Money.from_minor(1999, CurrencyCode.USD)
            Money(minor_units=1999, currency=<CurrencyCode.USD: 2>)
        """
        return cls(minor_units=minor_units, currency=currency)

    @classmethod
    def zero(cls, currency: CurrencyCode) -> Money:
        """
        Create a zero-value Money for the given currency.

        Args:
            currency: The currency code

        Returns:
            Money instance with value 0
        """
        return cls(minor_units=0, currency=currency)

    @property
    def major_units(self) -> Decimal:
        """
        Get the amount in major units as a Decimal.

        Returns:
            Decimal representation of the major unit amount

        Example:
            >>> Money.from_minor(1999, CurrencyCode.USD).major_units
            Decimal('19.99')
        """
        scale = self.currency.minor_unit_scale
        return Decimal(self.minor_units) / (10 ** scale)

    @property
    def is_zero(self) -> bool:
        """Check if amount is zero."""
        return self.minor_units == 0

    @property
    def is_positive(self) -> bool:
        """Check if amount is positive (greater than zero)."""
        return self.minor_units > 0

    @property
    def is_negative(self) -> bool:
        """Check if amount is negative (less than zero)."""
        return self.minor_units < 0

    def add(self, other: Money) -> Optional[Money]:
        """
        Add two Money values.

        Args:
            other: The Money to add

        Returns:
            Sum, or None if currencies don't match

        Example:
            >>> a = Money.from_minor(100, CurrencyCode.USD)
            >>> b = Money.from_minor(50, CurrencyCode.USD)
            >>> a.add(b)
            Money(minor_units=150, currency=<CurrencyCode.USD: 2>)
        """
        if self.currency != other.currency:
            return None

        try:
            result = self.minor_units + other.minor_units
            return Money(minor_units=result, currency=self.currency)
        except OverflowError:
            return None

    def sub(self, other: Money) -> Optional[Money]:
        """
        Subtract Money values.

        Args:
            other: The Money to subtract

        Returns:
            Difference, or None if currencies don't match

        Example:
            >>> a = Money.from_minor(100, CurrencyCode.USD)
            >>> b = Money.from_minor(50, CurrencyCode.USD)
            >>> a.sub(b)
            Money(minor_units=50, currency=<CurrencyCode.USD: 2>)
        """
        if self.currency != other.currency:
            return None

        try:
            result = self.minor_units - other.minor_units
            return Money(minor_units=result, currency=self.currency)
        except OverflowError:
            return None

    def mul(self, factor: Union[int, float, Decimal]) -> Optional[Money]:
        """
        Multiply Money by a scalar.

        Args:
            factor: The multiplication factor

        Returns:
            Product, or None on overflow/invalid input

        Example:
            >>> price = Money.from_minor(100, CurrencyCode.USD)
            >>> price.mul(3)
            Money(minor_units=300, currency=<CurrencyCode.USD: 2>)
        """
        try:
            decimal_factor = Decimal(str(factor))
            result_decimal = Decimal(self.minor_units) * decimal_factor
            rounded = result_decimal.quantize(Decimal("1"), rounding=ROUND_HALF_UP)
            return Money(minor_units=int(rounded), currency=self.currency)
        except (InvalidOperation, ValueError, OverflowError):
            return None

    def div(self, divisor: Union[int, float, Decimal]) -> Optional[Money]:
        """
        Divide Money by a scalar.

        Args:
            divisor: The divisor (cannot be zero)

        Returns:
            Quotient, or None on division by zero/invalid input

        Example:
            >>> price = Money.from_minor(100, CurrencyCode.USD)
            >>> price.div(4)
            Money(minor_units=25, currency=<CurrencyCode.USD: 2>)
        """
        try:
            decimal_divisor = Decimal(str(divisor))
            if decimal_divisor == 0:
                return None

            result_decimal = Decimal(self.minor_units) / decimal_divisor
            rounded = result_decimal.quantize(Decimal("1"), rounding=ROUND_HALF_UP)
            return Money(minor_units=int(rounded), currency=self.currency)
        except (InvalidOperation, ValueError, OverflowError, ZeroDivisionError):
            return None

    def negate(self) -> Money:
        """
        Return the negation of this Money value.

        Returns:
            Negated Money

        Example:
            >>> Money.from_minor(100, CurrencyCode.USD).negate()
            Money(minor_units=-100, currency=<CurrencyCode.USD: 2>)
        """
        return Money(minor_units=-self.minor_units, currency=self.currency)

    def abs(self) -> Money:
        """
        Return the absolute value of this Money.

        Returns:
            Money with positive amount
        """
        return Money(minor_units=abs(self.minor_units), currency=self.currency)

    def split(self, parts: int) -> Optional[list[Money]]:
        """
        Split Money into equal parts, distributing remainder.

        The first parts will get any remainder cents to ensure sum equals original.

        Args:
            parts: Number of parts to split into (must be positive)

        Returns:
            List of Money values that sum to original, or None if parts <= 0

        Example:
            >>> Money.from_minor(100, CurrencyCode.USD).split(3)
            [Money(minor_units=34, ...), Money(minor_units=33, ...), Money(minor_units=33, ...)]
        """
        if parts <= 0:
            return None

        base_amount = self.minor_units // parts
        remainder = self.minor_units % parts

        result: list[Money] = []

        # First 'remainder' parts get an extra unit
        for i in range(parts):
            extra = 1 if i < remainder else 0
            result.append(Money(
                minor_units=base_amount + extra,
                currency=self.currency
            ))

        return result

    def allocate(self, ratios: list[int]) -> Optional[list[Money]]:
        """
        Allocate Money according to ratios, distributing remainder.

        Args:
            ratios: List of positive integers representing allocation ratios

        Returns:
            List of Money values, or None if ratios invalid

        Example:
            >>> Money.from_minor(100, CurrencyCode.USD).allocate([1, 1, 1])
            [Money(minor_units=34, ...), Money(minor_units=33, ...), Money(minor_units=33, ...)]
        """
        if not ratios or any(r < 0 for r in ratios):
            return None

        total_ratio = sum(ratios)
        if total_ratio == 0:
            return None

        result: list[Money] = []
        allocated = 0

        for i, ratio in enumerate(ratios):
            # Calculate this part's share
            share = (self.minor_units * ratio) // total_ratio

            # For the last part, assign remaining amount
            if i == len(ratios) - 1:
                share = self.minor_units - allocated

            result.append(Money(minor_units=share, currency=self.currency))
            allocated += share

        return result

    def format(self, symbol: Optional[str] = None, decimal_sep: str = ".", thousands_sep: str = ",") -> str:
        """
        Format Money as a human-readable string.

        Args:
            symbol: Currency symbol (e.g., "$"). If None, uses currency code.
            decimal_sep: Decimal separator character
            thousands_sep: Thousands separator character

        Returns:
            Formatted string

        Example:
            >>> Money.from_minor(123456, CurrencyCode.USD).format("$")
            '$1,234.56'
        """
        scale = self.currency.minor_unit_scale

        # Handle sign
        sign = "-" if self.minor_units < 0 else ""
        abs_minor = abs(self.minor_units)

        # Split into major and minor parts
        if scale > 0:
            major_part = abs_minor // (10 ** scale)
            minor_part = abs_minor % (10 ** scale)
            minor_str = str(minor_part).zfill(scale)
        else:
            major_part = abs_minor
            minor_str = ""

        # Format major part with thousands separator
        major_str = f"{major_part:,}".replace(",", thousands_sep)

        # Build result
        prefix = symbol if symbol else f"{self.currency.name} "

        if scale > 0:
            return f"{sign}{prefix}{major_str}{decimal_sep}{minor_str}"
        else:
            return f"{sign}{prefix}{major_str}"

    def __str__(self) -> str:
        """Return default string representation."""
        scale = self.currency.minor_unit_scale
        if scale > 0:
            format_str = f"{{0}} {{1:.{scale}f}}"
            return format_str.format(self.currency.name, float(self.major_units))
        else:
            return f"{self.currency.name} {self.minor_units}"

    def __repr__(self) -> str:
        """Return detailed representation."""
        return f"Money(minor_units={self.minor_units}, currency={self.currency!r})"

    def __eq__(self, other: object) -> bool:
        """Check equality (same currency and amount)."""
        if isinstance(other, Money):
            return self.currency == other.currency and self.minor_units == other.minor_units
        return NotImplemented

    def __lt__(self, other: Money) -> bool:
        """
        Compare Money values.

        Only compares if currencies match, raises TypeError otherwise.
        """
        if not isinstance(other, Money):
            return NotImplemented
        if self.currency != other.currency:
            raise TypeError(f"Cannot compare {self.currency.name} with {other.currency.name}")
        return self.minor_units < other.minor_units

    def __le__(self, other: Money) -> bool:
        """Compare Money values (less than or equal)."""
        if not isinstance(other, Money):
            return NotImplemented
        if self.currency != other.currency:
            raise TypeError(f"Cannot compare {self.currency.name} with {other.currency.name}")
        return self.minor_units <= other.minor_units

    def __gt__(self, other: Money) -> bool:
        """Compare Money values (greater than)."""
        if not isinstance(other, Money):
            return NotImplemented
        if self.currency != other.currency:
            raise TypeError(f"Cannot compare {self.currency.name} with {other.currency.name}")
        return self.minor_units > other.minor_units

    def __ge__(self, other: Money) -> bool:
        """Compare Money values (greater than or equal)."""
        if not isinstance(other, Money):
            return NotImplemented
        if self.currency != other.currency:
            raise TypeError(f"Cannot compare {self.currency.name} with {other.currency.name}")
        return self.minor_units >= other.minor_units

    def __hash__(self) -> int:
        """Return hash for use in sets and dicts."""
        return hash((self.minor_units, self.currency))

    def __add__(self, other: Money) -> Money:
        """Add using + operator (raises on currency mismatch)."""
        result = self.add(other)
        if result is None:
            raise TypeError(f"Cannot add {self.currency.name} and {other.currency.name}")
        return result

    def __sub__(self, other: Money) -> Money:
        """Subtract using - operator (raises on currency mismatch)."""
        result = self.sub(other)
        if result is None:
            raise TypeError(f"Cannot subtract {other.currency.name} from {self.currency.name}")
        return result

    def __mul__(self, factor: Union[int, float, Decimal]) -> Money:
        """Multiply using * operator."""
        result = self.mul(factor)
        if result is None:
            raise ValueError(f"Invalid multiplication factor: {factor}")
        return result

    def __rmul__(self, factor: Union[int, float, Decimal]) -> Money:
        """Multiply using * operator (reversed)."""
        return self.__mul__(factor)

    def __truediv__(self, divisor: Union[int, float, Decimal]) -> Money:
        """Divide using / operator."""
        result = self.div(divisor)
        if result is None:
            raise ValueError(f"Invalid divisor: {divisor}")
        return result

    def __neg__(self) -> Money:
        """Negate using unary - operator."""
        return self.negate()

    def __abs__(self) -> Money:
        """Absolute value using abs() function."""
        return self.abs()
