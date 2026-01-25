<?php
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * ISO 4217 currency codes with minor unit information.
 *
 * This enum includes the most commonly used currencies. The minorUnits
 * value indicates the number of decimal places for the currency.
 */
enum CurrencyCode: string
{
    // Major world currencies
    case USD = 'USD';  // United States Dollar
    case EUR = 'EUR';  // Euro
    case GBP = 'GBP';  // British Pound Sterling
    case JPY = 'JPY';  // Japanese Yen
    case CHF = 'CHF';  // Swiss Franc
    case CNY = 'CNY';  // Chinese Yuan Renminbi
    case AUD = 'AUD';  // Australian Dollar
    case CAD = 'CAD';  // Canadian Dollar
    case NZD = 'NZD';  // New Zealand Dollar
    case HKD = 'HKD';  // Hong Kong Dollar
    case SGD = 'SGD';  // Singapore Dollar

    // European currencies
    case SEK = 'SEK';  // Swedish Krona
    case NOK = 'NOK';  // Norwegian Krone
    case DKK = 'DKK';  // Danish Krone
    case PLN = 'PLN';  // Polish Zloty
    case CZK = 'CZK';  // Czech Koruna
    case HUF = 'HUF';  // Hungarian Forint
    case RON = 'RON';  // Romanian Leu
    case BGN = 'BGN';  // Bulgarian Lev
    case ISK = 'ISK';  // Icelandic Krona
    case RUB = 'RUB';  // Russian Ruble
    case UAH = 'UAH';  // Ukrainian Hryvnia
    case TRY = 'TRY';  // Turkish Lira

    // Asian currencies
    case KRW = 'KRW';  // South Korean Won
    case INR = 'INR';  // Indian Rupee
    case IDR = 'IDR';  // Indonesian Rupiah
    case MYR = 'MYR';  // Malaysian Ringgit
    case THB = 'THB';  // Thai Baht
    case PHP = 'PHP';  // Philippine Peso
    case VND = 'VND';  // Vietnamese Dong
    case TWD = 'TWD';  // Taiwan Dollar

    // Middle Eastern currencies
    case AED = 'AED';  // UAE Dirham
    case SAR = 'SAR';  // Saudi Riyal
    case ILS = 'ILS';  // Israeli New Shekel
    case QAR = 'QAR';  // Qatari Riyal
    case KWD = 'KWD';  // Kuwaiti Dinar
    case BHD = 'BHD';  // Bahraini Dinar
    case OMR = 'OMR';  // Omani Rial
    case JOD = 'JOD';  // Jordanian Dinar

    // American currencies
    case MXN = 'MXN';  // Mexican Peso
    case BRL = 'BRL';  // Brazilian Real
    case ARS = 'ARS';  // Argentine Peso
    case CLP = 'CLP';  // Chilean Peso
    case COP = 'COP';  // Colombian Peso
    case PEN = 'PEN';  // Peruvian Sol

    // African currencies
    case ZAR = 'ZAR';  // South African Rand
    case NGN = 'NGN';  // Nigerian Naira
    case EGP = 'EGP';  // Egyptian Pound
    case KES = 'KES';  // Kenyan Shilling
    case MAD = 'MAD';  // Moroccan Dirham

    // Oceania currencies
    case FJD = 'FJD';  // Fiji Dollar

    // Precious metals and special
    case XAU = 'XAU';  // Gold (troy ounce)
    case XAG = 'XAG';  // Silver (troy ounce)
    case XPT = 'XPT';  // Platinum (troy ounce)
    case XBT = 'XBT';  // Bitcoin (unofficial)

    /**
     * Get the number of minor units (decimal places) for this currency.
     *
     * @return int Number of decimal places (0, 2, 3, or 8 for crypto)
     */
    public function minorUnits(): int
    {
        return match ($this) {
            // Zero decimal places
            self::JPY, self::KRW, self::VND, self::ISK,
            self::CLP, self::HUF, self::IDR => 0,

            // Three decimal places
            self::KWD, self::BHD, self::OMR, self::JOD => 3,

            // Precious metals (no minor units)
            self::XAU, self::XAG, self::XPT => 0,

            // Crypto (8 decimal places - satoshis)
            self::XBT => 8,

            // Standard two decimal places
            default => 2,
        };
    }

    /**
     * Get the currency symbol.
     *
     * @return string The currency symbol or code if no symbol
     */
    public function symbol(): string
    {
        return match ($this) {
            self::USD, self::AUD, self::CAD, self::NZD,
            self::HKD, self::SGD, self::MXN, self::ARS,
            self::CLP, self::COP => '$',
            self::EUR => "\u{20AC}",
            self::GBP, self::EGP => "\u{00A3}",
            self::JPY, self::CNY => "\u{00A5}",
            self::CHF => 'CHF',
            self::INR => "\u{20B9}",
            self::KRW => "\u{20A9}",
            self::RUB => "\u{20BD}",
            self::TRY => "\u{20BA}",
            self::ILS => "\u{20AA}",
            self::PLN => "z\u{0142}",
            self::BRL => 'R$',
            self::ZAR => 'R',
            self::THB => "\u{0E3F}",
            self::UAH => "\u{20B4}",
            self::VND => "\u{20AB}",
            self::PHP => "\u{20B1}",
            self::XBT => "\u{20BF}",
            default => $this->value,
        };
    }

    /**
     * Get the full currency name.
     *
     * @return string The currency name
     */
    public function name(): string
    {
        return match ($this) {
            self::USD => 'United States Dollar',
            self::EUR => 'Euro',
            self::GBP => 'British Pound Sterling',
            self::JPY => 'Japanese Yen',
            self::CHF => 'Swiss Franc',
            self::CNY => 'Chinese Yuan Renminbi',
            self::AUD => 'Australian Dollar',
            self::CAD => 'Canadian Dollar',
            self::NZD => 'New Zealand Dollar',
            self::HKD => 'Hong Kong Dollar',
            self::SGD => 'Singapore Dollar',
            self::SEK => 'Swedish Krona',
            self::NOK => 'Norwegian Krone',
            self::DKK => 'Danish Krone',
            self::PLN => 'Polish Zloty',
            self::CZK => 'Czech Koruna',
            self::HUF => 'Hungarian Forint',
            self::RON => 'Romanian Leu',
            self::BGN => 'Bulgarian Lev',
            self::ISK => 'Icelandic Krona',
            self::RUB => 'Russian Ruble',
            self::UAH => 'Ukrainian Hryvnia',
            self::TRY => 'Turkish Lira',
            self::KRW => 'South Korean Won',
            self::INR => 'Indian Rupee',
            self::IDR => 'Indonesian Rupiah',
            self::MYR => 'Malaysian Ringgit',
            self::THB => 'Thai Baht',
            self::PHP => 'Philippine Peso',
            self::VND => 'Vietnamese Dong',
            self::TWD => 'Taiwan Dollar',
            self::AED => 'UAE Dirham',
            self::SAR => 'Saudi Riyal',
            self::ILS => 'Israeli New Shekel',
            self::QAR => 'Qatari Riyal',
            self::KWD => 'Kuwaiti Dinar',
            self::BHD => 'Bahraini Dinar',
            self::OMR => 'Omani Rial',
            self::JOD => 'Jordanian Dinar',
            self::MXN => 'Mexican Peso',
            self::BRL => 'Brazilian Real',
            self::ARS => 'Argentine Peso',
            self::CLP => 'Chilean Peso',
            self::COP => 'Colombian Peso',
            self::PEN => 'Peruvian Sol',
            self::ZAR => 'South African Rand',
            self::NGN => 'Nigerian Naira',
            self::EGP => 'Egyptian Pound',
            self::KES => 'Kenyan Shilling',
            self::MAD => 'Moroccan Dirham',
            self::FJD => 'Fiji Dollar',
            self::XAU => 'Gold (troy ounce)',
            self::XAG => 'Silver (troy ounce)',
            self::XPT => 'Platinum (troy ounce)',
            self::XBT => 'Bitcoin',
        };
    }

    /**
     * Get the multiplier to convert major units to minor units.
     *
     * @return int The multiplier (e.g., 100 for USD, 1000 for KWD)
     */
    public function minorUnitMultiplier(): int
    {
        return (int)pow(10, $this->minorUnits());
    }
}

/**
 * Immutable monetary value with safe arithmetic operations.
 *
 * All amounts are stored in minor units (cents, pence, etc.) to avoid
 * floating-point precision issues. Operations return null on overflow.
 */
readonly class Money
{
    /**
     * Create a Money instance.
     *
     * @param int $minorUnits Amount in minor units (e.g., cents)
     * @param CurrencyCode $currency The currency
     */
    public function __construct(
        public int $minorUnits,
        public CurrencyCode $currency,
    ) {}

    /**
     * Create money from a major unit amount.
     *
     * @param string|int|float $amount The amount in major units
     * @param CurrencyCode $currency The currency
     * @return self|null Money instance or null if conversion fails
     */
    public static function fromMajorUnits(string|int|float $amount, CurrencyCode $currency): ?self
    {
        $multiplier = $currency->minorUnitMultiplier();

        if (is_string($amount)) {
            // Parse string amount carefully
            if (!preg_match('/^-?\d+(?:\.\d+)?$/', $amount)) {
                return null;
            }
            $floatAmount = (float)$amount;
        } else {
            $floatAmount = (float)$amount;
        }

        $minorUnits = (int)round($floatAmount * $multiplier);

        // Check for overflow
        if (abs($floatAmount * $multiplier) > PHP_INT_MAX) {
            return null;
        }

        return new self($minorUnits, $currency);
    }

    /**
     * Create money from minor units.
     *
     * @param int $minorUnits Amount in minor units
     * @param CurrencyCode $currency The currency
     * @return self Money instance
     */
    public static function fromMinorUnits(int $minorUnits, CurrencyCode $currency): self
    {
        return new self($minorUnits, $currency);
    }

    /**
     * Create zero amount in the given currency.
     *
     * @param CurrencyCode $currency The currency
     * @return self Zero money
     */
    public static function zero(CurrencyCode $currency): self
    {
        return new self(0, $currency);
    }

    /**
     * Get the amount as a float in major units.
     *
     * Note: Use this only for display; internal calculations should use minorUnits.
     *
     * @return float The amount in major units
     */
    public function toMajorUnits(): float
    {
        return $this->minorUnits / $this->currency->minorUnitMultiplier();
    }

    /**
     * Add two monetary values.
     *
     * @param self $other The amount to add
     * @return self|null Result or null on currency mismatch/overflow
     */
    public function add(self $other): ?self
    {
        if ($this->currency !== $other->currency) {
            return null;
        }

        $result = SafeMath::addChecked($this->minorUnits, $other->minorUnits);
        if ($result === null) {
            return null;
        }

        return new self($result, $this->currency);
    }

    /**
     * Subtract a monetary value.
     *
     * @param self $other The amount to subtract
     * @return self|null Result or null on currency mismatch/overflow
     */
    public function subtract(self $other): ?self
    {
        if ($this->currency !== $other->currency) {
            return null;
        }

        $result = SafeMath::subChecked($this->minorUnits, $other->minorUnits);
        if ($result === null) {
            return null;
        }

        return new self($result, $this->currency);
    }

    /**
     * Multiply by a factor.
     *
     * @param int|float $factor The multiplier
     * @param int $roundingMode PHP rounding mode constant
     * @return self|null Result or null on overflow
     */
    public function multiply(int|float $factor, int $roundingMode = PHP_ROUND_HALF_UP): ?self
    {
        $result = $this->minorUnits * $factor;

        // Check for overflow
        if (abs($result) > PHP_INT_MAX) {
            return null;
        }

        return new self((int)round($result, 0, $roundingMode), $this->currency);
    }

    /**
     * Divide by a divisor.
     *
     * @param int|float $divisor The divisor
     * @param int $roundingMode PHP rounding mode constant
     * @return self|null Result or null on division by zero
     */
    public function divide(int|float $divisor, int $roundingMode = PHP_ROUND_HALF_UP): ?self
    {
        if ($divisor == 0) {
            return null;
        }

        $result = $this->minorUnits / $divisor;
        return new self((int)round($result, 0, $roundingMode), $this->currency);
    }

    /**
     * Get the absolute value.
     *
     * @return self|null Absolute value or null on overflow (MIN_INT case)
     */
    public function abs(): ?self
    {
        $result = SafeMath::absSafe($this->minorUnits);
        if ($result === null) {
            return null;
        }
        return new self($result, $this->currency);
    }

    /**
     * Negate the amount.
     *
     * @return self|null Negated value or null on overflow
     */
    public function negate(): ?self
    {
        $result = SafeMath::mulChecked($this->minorUnits, -1);
        if ($result === null) {
            return null;
        }
        return new self($result, $this->currency);
    }

    /**
     * Check if amount is zero.
     *
     * @return bool True if zero
     */
    public function isZero(): bool
    {
        return $this->minorUnits === 0;
    }

    /**
     * Check if amount is positive.
     *
     * @return bool True if positive
     */
    public function isPositive(): bool
    {
        return $this->minorUnits > 0;
    }

    /**
     * Check if amount is negative.
     *
     * @return bool True if negative
     */
    public function isNegative(): bool
    {
        return $this->minorUnits < 0;
    }

    /**
     * Compare to another monetary value.
     *
     * @param self $other The value to compare with
     * @return int|null -1, 0, 1, or null on currency mismatch
     */
    public function compare(self $other): ?int
    {
        if ($this->currency !== $other->currency) {
            return null;
        }
        return $this->minorUnits <=> $other->minorUnits;
    }

    /**
     * Check equality with another monetary value.
     *
     * @param self $other The value to compare with
     * @return bool True if equal (same currency and amount)
     */
    public function equals(self $other): bool
    {
        return $this->currency === $other->currency
            && $this->minorUnits === $other->minorUnits;
    }

    /**
     * Check if greater than another value.
     *
     * @param self $other The value to compare with
     * @return bool|null True if greater, null on currency mismatch
     */
    public function greaterThan(self $other): ?bool
    {
        $cmp = $this->compare($other);
        return $cmp === null ? null : $cmp > 0;
    }

    /**
     * Check if less than another value.
     *
     * @param self $other The value to compare with
     * @return bool|null True if less, null on currency mismatch
     */
    public function lessThan(self $other): ?bool
    {
        $cmp = $this->compare($other);
        return $cmp === null ? null : $cmp < 0;
    }

    /**
     * Allocate money across ratios (useful for tax calculations).
     *
     * @param array<int> $ratios The ratios to allocate across
     * @return array<self>|null Array of allocated amounts or null if invalid
     */
    public function allocate(array $ratios): ?array
    {
        if (empty($ratios)) {
            return null;
        }

        $total = array_sum($ratios);
        if ($total <= 0) {
            return null;
        }

        $remainder = $this->minorUnits;
        $results = [];

        foreach ($ratios as $ratio) {
            $share = (int)floor($this->minorUnits * $ratio / $total);
            $results[] = new self($share, $this->currency);
            $remainder -= $share;
        }

        // Distribute remainder to first shares
        for ($i = 0; $remainder > 0 && $i < count($results); $i++) {
            $results[$i] = new self($results[$i]->minorUnits + 1, $this->currency);
            $remainder--;
        }

        return $results;
    }

    /**
     * Format as a string with currency symbol.
     *
     * @param bool $symbolFirst Whether symbol comes before amount
     * @return string Formatted string
     */
    public function format(bool $symbolFirst = true): string
    {
        $decimals = $this->currency->minorUnits();
        $amount = number_format($this->toMajorUnits(), $decimals, '.', ',');
        $symbol = $this->currency->symbol();

        if ($symbolFirst) {
            return $symbol . $amount;
        }
        return $amount . ' ' . $symbol;
    }

    /**
     * Format as ISO string (e.g., "USD 123.45").
     *
     * @return string ISO-formatted string
     */
    public function formatIso(): string
    {
        $decimals = $this->currency->minorUnits();
        $amount = number_format($this->toMajorUnits(), $decimals, '.', '');
        return $this->currency->value . ' ' . $amount;
    }

    /**
     * Get string representation.
     *
     * @return string Formatted money string
     */
    public function __toString(): string
    {
        return $this->format();
    }
}

/**
 * Safe currency operations and utilities.
 */
class SafeCurrency
{
    /**
     * Parse a currency code string.
     *
     * @param string $code The 3-letter currency code
     * @return CurrencyCode|null The currency or null if invalid
     */
    public static function parseCode(string $code): ?CurrencyCode
    {
        $normalized = strtoupper(trim($code));
        return CurrencyCode::tryFrom($normalized);
    }

    /**
     * Check if a currency code is valid.
     *
     * @param string $code The code to check
     * @return bool True if valid
     */
    public static function isValidCode(string $code): bool
    {
        return self::parseCode($code) !== null;
    }

    /**
     * Parse a monetary string (e.g., "USD 123.45" or "$123.45").
     *
     * @param string $value The string to parse
     * @param CurrencyCode|null $defaultCurrency Currency to use if not in string
     * @return Money|null The parsed money or null
     */
    public static function parse(string $value, ?CurrencyCode $defaultCurrency = null): ?Money
    {
        $value = trim($value);

        // Try ISO format first: "USD 123.45"
        if (preg_match('/^([A-Z]{3})\s+(-?\d+(?:\.\d+)?)$/', $value, $matches)) {
            $currency = self::parseCode($matches[1]);
            if ($currency !== null) {
                return Money::fromMajorUnits($matches[2], $currency);
            }
        }

        // Try amount only with default currency
        if ($defaultCurrency !== null && preg_match('/^-?\d+(?:\.\d+)?$/', $value)) {
            return Money::fromMajorUnits($value, $defaultCurrency);
        }

        return null;
    }

    /**
     * Get all available currency codes.
     *
     * @return array<CurrencyCode> All currency codes
     */
    public static function allCurrencies(): array
    {
        return CurrencyCode::cases();
    }

    /**
     * Get currencies with zero decimal places.
     *
     * @return array<CurrencyCode> Zero-decimal currencies
     */
    public static function zeroDecimalCurrencies(): array
    {
        return array_filter(
            CurrencyCode::cases(),
            fn(CurrencyCode $c) => $c->minorUnits() === 0
        );
    }

    /**
     * Get currencies with three decimal places.
     *
     * @return array<CurrencyCode> Three-decimal currencies
     */
    public static function threeDecimalCurrencies(): array
    {
        return array_filter(
            CurrencyCode::cases(),
            fn(CurrencyCode $c) => $c->minorUnits() === 3
        );
    }
}
