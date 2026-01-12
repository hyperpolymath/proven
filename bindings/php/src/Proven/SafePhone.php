<?php
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Country calling codes as defined by ITU-T E.164.
 *
 * This enum includes commonly used country codes. The value is the
 * ITU-assigned calling code (without leading +).
 */
enum CountryCode: string
{
    // North America (NANP)
    case US = '1';       // United States
    case CA = '1';       // Canada (same as US - NANP)

    // Europe
    case GB = '44';      // United Kingdom
    case DE = '49';      // Germany
    case FR = '33';      // France
    case IT = '39';      // Italy
    case ES = '34';      // Spain
    case NL = '31';      // Netherlands
    case BE = '32';      // Belgium
    case AT = '43';      // Austria
    case CH = '41';      // Switzerland
    case SE = '46';      // Sweden
    case NO = '47';      // Norway
    case DK = '45';      // Denmark
    case FI = '358';     // Finland
    case PL = '48';      // Poland
    case PT = '351';     // Portugal
    case IE = '353';     // Ireland
    case GR = '30';      // Greece
    case CZ = '420';     // Czech Republic
    case RO = '40';      // Romania
    case HU = '36';      // Hungary
    case UA = '380';     // Ukraine
    case RU = '7';       // Russia

    // Asia
    case CN = '86';      // China
    case JP = '81';      // Japan
    case KR = '82';      // South Korea
    case IN = '91';      // India
    case ID = '62';      // Indonesia
    case MY = '60';      // Malaysia
    case SG = '65';      // Singapore
    case TH = '66';      // Thailand
    case PH = '63';      // Philippines
    case VN = '84';      // Vietnam
    case TW = '886';     // Taiwan
    case HK = '852';     // Hong Kong
    case PK = '92';      // Pakistan
    case BD = '880';     // Bangladesh

    // Middle East
    case AE = '971';     // United Arab Emirates
    case SA = '966';     // Saudi Arabia
    case IL = '972';     // Israel
    case TR = '90';      // Turkey
    case EG = '20';      // Egypt
    case IR = '98';      // Iran
    case IQ = '964';     // Iraq
    case JO = '962';     // Jordan
    case KW = '965';     // Kuwait
    case QA = '974';     // Qatar
    case BH = '973';     // Bahrain
    case OM = '968';     // Oman
    case LB = '961';     // Lebanon

    // Americas
    case MX = '52';      // Mexico
    case BR = '55';      // Brazil
    case AR = '54';      // Argentina
    case CO = '57';      // Colombia
    case CL = '56';      // Chile
    case PE = '51';      // Peru
    case VE = '58';      // Venezuela

    // Oceania
    case AU = '61';      // Australia
    case NZ = '64';      // New Zealand

    // Africa
    case ZA = '27';      // South Africa
    case NG = '234';     // Nigeria
    case KE = '254';     // Kenya
    case MA = '212';     // Morocco
    case GH = '233';     // Ghana
    case TZ = '255';     // Tanzania
    case ET = '251';     // Ethiopia

    /**
     * Get the full country name.
     *
     * @return string Country name
     */
    public function countryName(): string
    {
        return match ($this) {
            self::US => 'United States',
            self::CA => 'Canada',
            self::GB => 'United Kingdom',
            self::DE => 'Germany',
            self::FR => 'France',
            self::IT => 'Italy',
            self::ES => 'Spain',
            self::NL => 'Netherlands',
            self::BE => 'Belgium',
            self::AT => 'Austria',
            self::CH => 'Switzerland',
            self::SE => 'Sweden',
            self::NO => 'Norway',
            self::DK => 'Denmark',
            self::FI => 'Finland',
            self::PL => 'Poland',
            self::PT => 'Portugal',
            self::IE => 'Ireland',
            self::GR => 'Greece',
            self::CZ => 'Czech Republic',
            self::RO => 'Romania',
            self::HU => 'Hungary',
            self::UA => 'Ukraine',
            self::RU => 'Russia',
            self::CN => 'China',
            self::JP => 'Japan',
            self::KR => 'South Korea',
            self::IN => 'India',
            self::ID => 'Indonesia',
            self::MY => 'Malaysia',
            self::SG => 'Singapore',
            self::TH => 'Thailand',
            self::PH => 'Philippines',
            self::VN => 'Vietnam',
            self::TW => 'Taiwan',
            self::HK => 'Hong Kong',
            self::PK => 'Pakistan',
            self::BD => 'Bangladesh',
            self::AE => 'United Arab Emirates',
            self::SA => 'Saudi Arabia',
            self::IL => 'Israel',
            self::TR => 'Turkey',
            self::EG => 'Egypt',
            self::IR => 'Iran',
            self::IQ => 'Iraq',
            self::JO => 'Jordan',
            self::KW => 'Kuwait',
            self::QA => 'Qatar',
            self::BH => 'Bahrain',
            self::OM => 'Oman',
            self::LB => 'Lebanon',
            self::MX => 'Mexico',
            self::BR => 'Brazil',
            self::AR => 'Argentina',
            self::CO => 'Colombia',
            self::CL => 'Chile',
            self::PE => 'Peru',
            self::VE => 'Venezuela',
            self::AU => 'Australia',
            self::NZ => 'New Zealand',
            self::ZA => 'South Africa',
            self::NG => 'Nigeria',
            self::KE => 'Kenya',
            self::MA => 'Morocco',
            self::GH => 'Ghana',
            self::TZ => 'Tanzania',
            self::ET => 'Ethiopia',
        };
    }

    /**
     * Get the calling code with plus prefix.
     *
     * @return string E.g., "+1", "+44"
     */
    public function callingCodeWithPlus(): string
    {
        return '+' . $this->value;
    }

    /**
     * Get the ISO 3166-1 alpha-2 country code.
     *
     * @return string Two-letter country code
     */
    public function isoCode(): string
    {
        return $this->name;
    }
}

/**
 * Immutable phone number representation with parsing and formatting.
 */
readonly class PhoneNumber
{
    /**
     * Create a phone number.
     *
     * @param string $countryCode The country calling code (without +)
     * @param string $nationalNumber The national number (digits only)
     */
    public function __construct(
        public string $countryCode,
        public string $nationalNumber,
    ) {}

    /**
     * Parse a phone number string.
     *
     * Accepts formats:
     * - +1234567890 (E.164)
     * - +1 234 567 890 (with spaces)
     * - +1-234-567-890 (with hyphens)
     * - (123) 456-7890 (US local with country code hint)
     *
     * @param string $phoneString The phone number string
     * @param CountryCode|null $defaultCountry Default country if not in string
     * @return self|null Parsed phone number or null
     */
    public static function parse(string $phoneString, ?CountryCode $defaultCountry = null): ?self
    {
        // Remove all non-digit characters except leading +
        $normalized = preg_replace('/[^\d+]/', '', trim($phoneString));
        if ($normalized === null || $normalized === '') {
            return null;
        }

        // Handle E.164 format (starts with +)
        if (str_starts_with($normalized, '+')) {
            $digits = substr($normalized, 1);
            return self::parseWithCountryCode($digits);
        }

        // Handle numbers starting with 00 (international prefix)
        if (str_starts_with($normalized, '00')) {
            $digits = substr($normalized, 2);
            return self::parseWithCountryCode($digits);
        }

        // Use default country if provided
        if ($defaultCountry !== null) {
            // Remove leading 0 if present (common in national formats)
            $nationalNumber = ltrim($normalized, '0');
            if (strlen($nationalNumber) >= 6 && strlen($nationalNumber) <= 15) {
                return new self($defaultCountry->value, $nationalNumber);
            }
        }

        return null;
    }

    /**
     * Parse digits that include the country code.
     *
     * @param string $digits Digits including country code
     * @return self|null Parsed phone number or null
     */
    private static function parseWithCountryCode(string $digits): ?self
    {
        if (strlen($digits) < 7 || strlen($digits) > 15) {
            return null;
        }

        // Try matching country codes (1-3 digits)
        $countryCodeLengths = [3, 2, 1];

        foreach ($countryCodeLengths as $length) {
            if (strlen($digits) < $length + 4) {
                continue;
            }

            $possibleCountryCode = substr($digits, 0, $length);
            $nationalNumber = substr($digits, $length);

            // Validate this is a known country code
            foreach (CountryCode::cases() as $country) {
                if ($country->value === $possibleCountryCode) {
                    return new self($possibleCountryCode, $nationalNumber);
                }
            }
        }

        // If no known country code found, try to use the first digit(s) as country code
        // This handles less common country codes
        if (strlen($digits) >= 10) {
            // Assume 1-3 digit country code based on total length
            $countryCodeLen = match (true) {
                $digits[0] === '1' => 1,  // NANP
                $digits[0] === '7' => 1,  // Russia/Kazakhstan
                strlen($digits) > 12 => 3,
                strlen($digits) > 11 => 2,
                default => 2,
            };

            return new self(
                substr($digits, 0, $countryCodeLen),
                substr($digits, $countryCodeLen)
            );
        }

        return null;
    }

    /**
     * Create from E.164 format string.
     *
     * @param string $e164 E.164 formatted string (e.g., "+14155551234")
     * @return self|null Parsed phone number or null
     */
    public static function fromE164(string $e164): ?self
    {
        if (!str_starts_with($e164, '+')) {
            return null;
        }
        return self::parse($e164);
    }

    /**
     * Get the country code enum if it matches a known country.
     *
     * @return CountryCode|null The country or null if unknown
     */
    public function getCountry(): ?CountryCode
    {
        foreach (CountryCode::cases() as $country) {
            if ($country->value === $this->countryCode) {
                return $country;
            }
        }
        return null;
    }

    /**
     * Format as E.164 (international standard).
     *
     * @return string E.g., "+14155551234"
     */
    public function formatE164(): string
    {
        return '+' . $this->countryCode . $this->nationalNumber;
    }

    /**
     * Format as international format with spaces.
     *
     * @return string E.g., "+1 415 555 1234"
     */
    public function formatInternational(): string
    {
        $number = $this->nationalNumber;
        $formatted = '+' . $this->countryCode . ' ';

        // Simple grouping (not perfect for all countries)
        if (strlen($number) === 10) {
            // Common format: XXX XXX XXXX
            $formatted .= substr($number, 0, 3) . ' ';
            $formatted .= substr($number, 3, 3) . ' ';
            $formatted .= substr($number, 6);
        } elseif (strlen($number) === 9) {
            // Common format: XXX XXX XXX
            $formatted .= substr($number, 0, 3) . ' ';
            $formatted .= substr($number, 3, 3) . ' ';
            $formatted .= substr($number, 6);
        } elseif (strlen($number) === 8) {
            // Common format: XXXX XXXX
            $formatted .= substr($number, 0, 4) . ' ';
            $formatted .= substr($number, 4);
        } else {
            // Just add spaces every 3-4 digits
            $formatted .= implode(' ', str_split($number, 4));
        }

        return $formatted;
    }

    /**
     * Format as national format (without country code).
     *
     * @param bool $includeAreaCode Whether to format with area code separator
     * @return string E.g., "(415) 555-1234" for US
     */
    public function formatNational(bool $includeAreaCode = true): string
    {
        $number = $this->nationalNumber;

        // US/Canada format
        if ($this->countryCode === '1' && strlen($number) === 10) {
            if ($includeAreaCode) {
                return sprintf(
                    '(%s) %s-%s',
                    substr($number, 0, 3),
                    substr($number, 3, 3),
                    substr($number, 6)
                );
            }
            return sprintf('%s-%s', substr($number, 3, 3), substr($number, 6));
        }

        // UK format
        if ($this->countryCode === '44') {
            if (strlen($number) === 10) {
                return sprintf(
                    '%s %s %s',
                    substr($number, 0, 4),
                    substr($number, 4, 3),
                    substr($number, 7)
                );
            }
        }

        // Generic format with spaces
        return implode(' ', str_split($number, 4));
    }

    /**
     * Format as RFC 3966 tel URI.
     *
     * @return string E.g., "tel:+1-415-555-1234"
     */
    public function formatRfc3966(): string
    {
        return 'tel:' . $this->formatE164();
    }

    /**
     * Get just the digits (country code + national number).
     *
     * @return string All digits without formatting
     */
    public function digits(): string
    {
        return $this->countryCode . $this->nationalNumber;
    }

    /**
     * Check if this is a valid E.164 number (7-15 digits total).
     *
     * @return bool True if valid
     */
    public function isValid(): bool
    {
        $totalDigits = strlen($this->countryCode) + strlen($this->nationalNumber);
        return $totalDigits >= 7 && $totalDigits <= 15
            && ctype_digit($this->countryCode)
            && ctype_digit($this->nationalNumber);
    }

    /**
     * Check if this is a mobile number (heuristic, not definitive).
     *
     * @return bool|null True/false if detectable, null if unknown
     */
    public function isMobile(): ?bool
    {
        // This is a simplified check - real implementation would need a database
        $number = $this->nationalNumber;

        // UK mobile starts with 7
        if ($this->countryCode === '44' && str_starts_with($number, '7')) {
            return true;
        }

        // US/Canada - can't reliably determine from number alone
        if ($this->countryCode === '1') {
            return null;
        }

        // Germany mobile starts with 15, 16, 17
        if ($this->countryCode === '49') {
            $prefix = substr($number, 0, 2);
            if (in_array($prefix, ['15', '16', '17'], true)) {
                return true;
            }
        }

        return null;
    }

    /**
     * Compare two phone numbers for equality.
     *
     * @param self $other The phone number to compare
     * @return bool True if same number
     */
    public function equals(self $other): bool
    {
        return $this->countryCode === $other->countryCode
            && $this->nationalNumber === $other->nationalNumber;
    }

    /**
     * Get string representation (E.164 format).
     *
     * @return string E.164 formatted number
     */
    public function __toString(): string
    {
        return $this->formatE164();
    }
}

/**
 * Safe phone number operations and utilities.
 */
class SafePhone
{
    /**
     * Parse a phone number string.
     *
     * @param string $phoneString The string to parse
     * @param CountryCode|null $defaultCountry Default country if not specified
     * @return PhoneNumber|null Parsed number or null
     */
    public static function parse(string $phoneString, ?CountryCode $defaultCountry = null): ?PhoneNumber
    {
        return PhoneNumber::parse($phoneString, $defaultCountry);
    }

    /**
     * Check if a string is a valid phone number.
     *
     * @param string $phoneString The string to validate
     * @param CountryCode|null $defaultCountry Default country if not specified
     * @return bool True if valid
     */
    public static function isValid(string $phoneString, ?CountryCode $defaultCountry = null): bool
    {
        $phone = PhoneNumber::parse($phoneString, $defaultCountry);
        return $phone !== null && $phone->isValid();
    }

    /**
     * Normalize a phone number to E.164 format.
     *
     * @param string $phoneString The phone string
     * @param CountryCode|null $defaultCountry Default country
     * @return string|null E.164 formatted number or null
     */
    public static function normalize(string $phoneString, ?CountryCode $defaultCountry = null): ?string
    {
        $phone = PhoneNumber::parse($phoneString, $defaultCountry);
        return $phone?->formatE164();
    }

    /**
     * Format a phone number for display.
     *
     * @param string $phoneString The phone string
     * @param CountryCode|null $defaultCountry Default country
     * @param bool $international Whether to use international format
     * @return string|null Formatted number or null
     */
    public static function format(
        string $phoneString,
        ?CountryCode $defaultCountry = null,
        bool $international = true
    ): ?string {
        $phone = PhoneNumber::parse($phoneString, $defaultCountry);
        if ($phone === null) {
            return null;
        }
        return $international ? $phone->formatInternational() : $phone->formatNational();
    }

    /**
     * Compare two phone numbers for equality (ignoring formatting).
     *
     * @param string $phoneA First phone string
     * @param string $phoneB Second phone string
     * @param CountryCode|null $defaultCountry Default country for both
     * @return bool True if same number
     */
    public static function equals(
        string $phoneA,
        string $phoneB,
        ?CountryCode $defaultCountry = null
    ): bool {
        $a = PhoneNumber::parse($phoneA, $defaultCountry);
        $b = PhoneNumber::parse($phoneB, $defaultCountry);

        if ($a === null || $b === null) {
            return false;
        }

        return $a->equals($b);
    }

    /**
     * Extract all phone numbers from a text.
     *
     * @param string $text The text to search
     * @param CountryCode|null $defaultCountry Default country for numbers without code
     * @return array<PhoneNumber> Found phone numbers
     */
    public static function extractFromText(string $text, ?CountryCode $defaultCountry = null): array
    {
        $phones = [];

        // Match various phone patterns
        $patterns = [
            // E.164: +12345678901
            '/\+\d{7,15}/',
            // International with spaces/dashes: +1 234-567-8901
            '/\+\d{1,3}[\s\-]?\d{2,4}[\s\-]?\d{2,4}[\s\-]?\d{2,4}/',
            // US format: (123) 456-7890
            '/\(\d{3}\)\s*\d{3}[\s\-]?\d{4}/',
            // Generic with dashes: 123-456-7890
            '/\d{3}[\s\-]\d{3}[\s\-]\d{4}/',
        ];

        foreach ($patterns as $pattern) {
            if (preg_match_all($pattern, $text, $matches)) {
                foreach ($matches[0] as $match) {
                    $phone = PhoneNumber::parse($match, $defaultCountry);
                    if ($phone !== null && $phone->isValid()) {
                        // Avoid duplicates
                        $isDuplicate = false;
                        foreach ($phones as $existing) {
                            if ($existing->equals($phone)) {
                                $isDuplicate = true;
                                break;
                            }
                        }
                        if (!$isDuplicate) {
                            $phones[] = $phone;
                        }
                    }
                }
            }
        }

        return $phones;
    }

    /**
     * Get a CountryCode by ISO code.
     *
     * @param string $isoCode Two-letter ISO country code
     * @return CountryCode|null The country or null
     */
    public static function getCountryByIso(string $isoCode): ?CountryCode
    {
        $normalized = strtoupper(trim($isoCode));
        return CountryCode::tryFrom($normalized);
    }

    /**
     * Get all countries with a specific calling code.
     *
     * @param string $callingCode The calling code (without +)
     * @return array<CountryCode> Countries with that code
     */
    public static function getCountriesByCallingCode(string $callingCode): array
    {
        return array_filter(
            CountryCode::cases(),
            fn(CountryCode $c) => $c->value === $callingCode
        );
    }
}
