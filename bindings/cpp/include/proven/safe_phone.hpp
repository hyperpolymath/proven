// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file safe_phone.hpp
 * @brief Safe phone number validation following E.164
 *
 * Provides country calling codes and phone number validation/formatting
 * following the E.164 international telephone numbering plan.
 *
 * @example
 * @code
 * #include <proven/safe_phone.hpp>
 * #include <iostream>
 *
 * int main() {
 *     using namespace proven;
 *
 *     // Parse a phone number
 *     auto phone = SafePhone::parse("+1 555 123 4567");
 *     if (phone) {
 *         std::cout << "E.164: " << phone->toE164() << "\n";
 *         std::cout << "International: " << phone->toInternational() << "\n";
 *         std::cout << "Country: " << countryCodeName(phone->countryCode()) << "\n";
 *     }
 *
 *     // Validate without parsing
 *     if (SafePhone::isValid("+44 20 7946 0958")) {
 *         std::cout << "Valid UK number\n";
 *     }
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_SAFE_PHONE_HPP
#define PROVEN_SAFE_PHONE_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <algorithm>

namespace proven {

// ============================================================================
// Country Calling Code
// ============================================================================

/**
 * @brief ITU-T E.164 country calling codes
 *
 * Common country codes for international dialing.
 */
enum class CountryCode : uint16_t {
    Unknown = 0,

    // North America (Zone 1)
    Us = 1,     ///< USA, Canada, Caribbean

    // Russia and Kazakhstan (Zone 7)
    Ru = 7,     ///< Russia
    Kz = 77,    ///< Kazakhstan (7 + 7)

    // Africa (Zone 2)
    Eg = 20,    ///< Egypt
    Za = 27,    ///< South Africa
    Ng = 234,   ///< Nigeria
    Ke = 254,   ///< Kenya
    Ma = 212,   ///< Morocco

    // Europe (Zone 3-4)
    Fr = 33,    ///< France
    Es = 34,    ///< Spain
    It = 39,    ///< Italy
    Uk = 44,    ///< United Kingdom
    De = 49,    ///< Germany
    Nl = 31,    ///< Netherlands
    Be = 32,    ///< Belgium
    Ch = 41,    ///< Switzerland
    At = 43,    ///< Austria
    Pl = 48,    ///< Poland
    Se = 46,    ///< Sweden
    No = 47,    ///< Norway
    Dk = 45,    ///< Denmark
    Fi = 358,   ///< Finland
    Pt = 351,   ///< Portugal
    Ie = 353,   ///< Ireland
    Gr = 30,    ///< Greece
    Cz = 420,   ///< Czech Republic
    Ro = 40,    ///< Romania
    Hu = 36,    ///< Hungary
    Ua = 380,   ///< Ukraine
    Tr = 90,    ///< Turkey

    // Americas (Zone 5)
    Mx = 52,    ///< Mexico
    Br = 55,    ///< Brazil
    Ar = 54,    ///< Argentina
    Cl = 56,    ///< Chile
    Co = 57,    ///< Colombia
    Pe = 51,    ///< Peru
    Ve = 58,    ///< Venezuela

    // Pacific and Southeast Asia (Zone 6)
    Au = 61,    ///< Australia
    Nz = 64,    ///< New Zealand
    My = 60,    ///< Malaysia
    Sg = 65,    ///< Singapore
    Id = 62,    ///< Indonesia
    Ph = 63,    ///< Philippines
    Th = 66,    ///< Thailand
    Vn = 84,    ///< Vietnam

    // East Asia (Zone 8)
    Jp = 81,    ///< Japan
    Kr = 82,    ///< South Korea
    Cn = 86,    ///< China
    Tw = 886,   ///< Taiwan
    Hk = 852,   ///< Hong Kong

    // South and West Asia (Zone 9)
    In = 91,    ///< India
    Pk = 92,    ///< Pakistan
    Bd = 880,   ///< Bangladesh
    Ir = 98,    ///< Iran
    Sa = 966,   ///< Saudi Arabia
    Ae = 971,   ///< UAE
    Il = 972,   ///< Israel
};

/**
 * @brief Get numeric value of country code
 */
[[nodiscard]] constexpr uint16_t countryCodeValue(CountryCode cc) noexcept {
    return static_cast<uint16_t>(cc);
}

/**
 * @brief Get ISO 3166-1 alpha-2 country code string
 */
[[nodiscard]] constexpr const char* countryCodeIso(CountryCode cc) noexcept {
    switch (cc) {
        case CountryCode::Us: return "US";
        case CountryCode::Ru: return "RU";
        case CountryCode::Kz: return "KZ";
        case CountryCode::Eg: return "EG";
        case CountryCode::Za: return "ZA";
        case CountryCode::Ng: return "NG";
        case CountryCode::Ke: return "KE";
        case CountryCode::Ma: return "MA";
        case CountryCode::Fr: return "FR";
        case CountryCode::Es: return "ES";
        case CountryCode::It: return "IT";
        case CountryCode::Uk: return "GB";
        case CountryCode::De: return "DE";
        case CountryCode::Nl: return "NL";
        case CountryCode::Be: return "BE";
        case CountryCode::Ch: return "CH";
        case CountryCode::At: return "AT";
        case CountryCode::Pl: return "PL";
        case CountryCode::Se: return "SE";
        case CountryCode::No: return "NO";
        case CountryCode::Dk: return "DK";
        case CountryCode::Fi: return "FI";
        case CountryCode::Pt: return "PT";
        case CountryCode::Ie: return "IE";
        case CountryCode::Gr: return "GR";
        case CountryCode::Cz: return "CZ";
        case CountryCode::Ro: return "RO";
        case CountryCode::Hu: return "HU";
        case CountryCode::Ua: return "UA";
        case CountryCode::Tr: return "TR";
        case CountryCode::Mx: return "MX";
        case CountryCode::Br: return "BR";
        case CountryCode::Ar: return "AR";
        case CountryCode::Cl: return "CL";
        case CountryCode::Co: return "CO";
        case CountryCode::Pe: return "PE";
        case CountryCode::Ve: return "VE";
        case CountryCode::Au: return "AU";
        case CountryCode::Nz: return "NZ";
        case CountryCode::My: return "MY";
        case CountryCode::Sg: return "SG";
        case CountryCode::Id: return "ID";
        case CountryCode::Ph: return "PH";
        case CountryCode::Th: return "TH";
        case CountryCode::Vn: return "VN";
        case CountryCode::Jp: return "JP";
        case CountryCode::Kr: return "KR";
        case CountryCode::Cn: return "CN";
        case CountryCode::Tw: return "TW";
        case CountryCode::Hk: return "HK";
        case CountryCode::In: return "IN";
        case CountryCode::Pk: return "PK";
        case CountryCode::Bd: return "BD";
        case CountryCode::Ir: return "IR";
        case CountryCode::Sa: return "SA";
        case CountryCode::Ae: return "AE";
        case CountryCode::Il: return "IL";
        default: return "XX";
    }
}

/**
 * @brief Get country name
 */
[[nodiscard]] constexpr const char* countryCodeName(CountryCode cc) noexcept {
    switch (cc) {
        case CountryCode::Us: return "United States";
        case CountryCode::Ru: return "Russia";
        case CountryCode::Kz: return "Kazakhstan";
        case CountryCode::Eg: return "Egypt";
        case CountryCode::Za: return "South Africa";
        case CountryCode::Ng: return "Nigeria";
        case CountryCode::Ke: return "Kenya";
        case CountryCode::Ma: return "Morocco";
        case CountryCode::Fr: return "France";
        case CountryCode::Es: return "Spain";
        case CountryCode::It: return "Italy";
        case CountryCode::Uk: return "United Kingdom";
        case CountryCode::De: return "Germany";
        case CountryCode::Nl: return "Netherlands";
        case CountryCode::Be: return "Belgium";
        case CountryCode::Ch: return "Switzerland";
        case CountryCode::At: return "Austria";
        case CountryCode::Pl: return "Poland";
        case CountryCode::Se: return "Sweden";
        case CountryCode::No: return "Norway";
        case CountryCode::Dk: return "Denmark";
        case CountryCode::Fi: return "Finland";
        case CountryCode::Pt: return "Portugal";
        case CountryCode::Ie: return "Ireland";
        case CountryCode::Gr: return "Greece";
        case CountryCode::Cz: return "Czech Republic";
        case CountryCode::Ro: return "Romania";
        case CountryCode::Hu: return "Hungary";
        case CountryCode::Ua: return "Ukraine";
        case CountryCode::Tr: return "Turkey";
        case CountryCode::Mx: return "Mexico";
        case CountryCode::Br: return "Brazil";
        case CountryCode::Ar: return "Argentina";
        case CountryCode::Cl: return "Chile";
        case CountryCode::Co: return "Colombia";
        case CountryCode::Pe: return "Peru";
        case CountryCode::Ve: return "Venezuela";
        case CountryCode::Au: return "Australia";
        case CountryCode::Nz: return "New Zealand";
        case CountryCode::My: return "Malaysia";
        case CountryCode::Sg: return "Singapore";
        case CountryCode::Id: return "Indonesia";
        case CountryCode::Ph: return "Philippines";
        case CountryCode::Th: return "Thailand";
        case CountryCode::Vn: return "Vietnam";
        case CountryCode::Jp: return "Japan";
        case CountryCode::Kr: return "South Korea";
        case CountryCode::Cn: return "China";
        case CountryCode::Tw: return "Taiwan";
        case CountryCode::Hk: return "Hong Kong";
        case CountryCode::In: return "India";
        case CountryCode::Pk: return "Pakistan";
        case CountryCode::Bd: return "Bangladesh";
        case CountryCode::Ir: return "Iran";
        case CountryCode::Sa: return "Saudi Arabia";
        case CountryCode::Ae: return "United Arab Emirates";
        case CountryCode::Il: return "Israel";
        default: return "Unknown";
    }
}

/**
 * @brief Get number of digits in country code
 */
[[nodiscard]] constexpr uint8_t countryCodeDigits(CountryCode cc) noexcept {
    const uint16_t value = countryCodeValue(cc);
    if (value >= 100) return 3;
    if (value >= 10) return 2;
    return 1;
}

/**
 * @brief Parse country code from numeric value
 */
[[nodiscard]] constexpr CountryCode countryCodeFromValue(uint16_t value) noexcept {
    switch (value) {
        case 1: return CountryCode::Us;
        case 7: return CountryCode::Ru;
        case 77: return CountryCode::Kz;
        case 20: return CountryCode::Eg;
        case 27: return CountryCode::Za;
        case 234: return CountryCode::Ng;
        case 254: return CountryCode::Ke;
        case 212: return CountryCode::Ma;
        case 33: return CountryCode::Fr;
        case 34: return CountryCode::Es;
        case 39: return CountryCode::It;
        case 44: return CountryCode::Uk;
        case 49: return CountryCode::De;
        case 31: return CountryCode::Nl;
        case 32: return CountryCode::Be;
        case 41: return CountryCode::Ch;
        case 43: return CountryCode::At;
        case 48: return CountryCode::Pl;
        case 46: return CountryCode::Se;
        case 47: return CountryCode::No;
        case 45: return CountryCode::Dk;
        case 358: return CountryCode::Fi;
        case 351: return CountryCode::Pt;
        case 353: return CountryCode::Ie;
        case 30: return CountryCode::Gr;
        case 420: return CountryCode::Cz;
        case 40: return CountryCode::Ro;
        case 36: return CountryCode::Hu;
        case 380: return CountryCode::Ua;
        case 90: return CountryCode::Tr;
        case 52: return CountryCode::Mx;
        case 55: return CountryCode::Br;
        case 54: return CountryCode::Ar;
        case 56: return CountryCode::Cl;
        case 57: return CountryCode::Co;
        case 51: return CountryCode::Pe;
        case 58: return CountryCode::Ve;
        case 61: return CountryCode::Au;
        case 64: return CountryCode::Nz;
        case 60: return CountryCode::My;
        case 65: return CountryCode::Sg;
        case 62: return CountryCode::Id;
        case 63: return CountryCode::Ph;
        case 66: return CountryCode::Th;
        case 84: return CountryCode::Vn;
        case 81: return CountryCode::Jp;
        case 82: return CountryCode::Kr;
        case 86: return CountryCode::Cn;
        case 886: return CountryCode::Tw;
        case 852: return CountryCode::Hk;
        case 91: return CountryCode::In;
        case 92: return CountryCode::Pk;
        case 880: return CountryCode::Bd;
        case 98: return CountryCode::Ir;
        case 966: return CountryCode::Sa;
        case 971: return CountryCode::Ae;
        case 972: return CountryCode::Il;
        default: return CountryCode::Unknown;
    }
}

// ============================================================================
// Phone Number Class
// ============================================================================

/**
 * @brief Validated phone number
 *
 * Stores a validated phone number with separated country code
 * and national number for proper formatting.
 */
class PhoneNumber {
public:
    /// Maximum length of E.164 number (including country code)
    static constexpr size_t MaxDigits = 15;

    /// Minimum length of national number
    static constexpr size_t MinNationalDigits = 4;

    /**
     * @brief Get the country calling code
     */
    [[nodiscard]] constexpr CountryCode countryCode() const noexcept {
        return countryCode_;
    }

    /**
     * @brief Get the national number (without country code)
     */
    [[nodiscard]] const std::string& nationalNumber() const noexcept {
        return nationalNumber_;
    }

    /**
     * @brief Format in E.164 format (+CountryCodeNationalNumber)
     */
    [[nodiscard]] std::string toE164() const {
        return "+" + std::to_string(countryCodeValue(countryCode_)) + nationalNumber_;
    }

    /**
     * @brief Format in international format with spaces
     *
     * Attempts to group the national number for readability.
     */
    [[nodiscard]] std::string toInternational() const {
        std::string result = "+" + std::to_string(countryCodeValue(countryCode_)) + " ";

        const size_t len = nationalNumber_.size();
        if (len <= 4) {
            result += nationalNumber_;
        } else if (len <= 7) {
            result += nationalNumber_.substr(0, 3) + " " + nationalNumber_.substr(3);
        } else if (len <= 10) {
            result += nationalNumber_.substr(0, 3) + " " +
                     nationalNumber_.substr(3, 3) + " " +
                     nationalNumber_.substr(6);
        } else {
            // For longer numbers, group in threes from the end
            size_t pos = len % 3;
            if (pos == 0) pos = 3;
            result += nationalNumber_.substr(0, pos);
            while (pos < len) {
                result += " " + nationalNumber_.substr(pos, 3);
                pos += 3;
            }
        }

        return result;
    }

    /**
     * @brief Format in national format (without country code)
     *
     * Uses parentheses for area code style.
     */
    [[nodiscard]] std::string toNational() const {
        const size_t len = nationalNumber_.size();
        if (len <= 4) {
            return nationalNumber_;
        } else if (len <= 7) {
            return nationalNumber_.substr(0, 3) + "-" + nationalNumber_.substr(3);
        } else if (len == 10) {
            // US style: (XXX) XXX-XXXX
            return "(" + nationalNumber_.substr(0, 3) + ") " +
                   nationalNumber_.substr(3, 3) + "-" +
                   nationalNumber_.substr(6);
        } else {
            // Generic grouping
            return nationalNumber_.substr(0, len - 7) + " " +
                   nationalNumber_.substr(len - 7, 3) + " " +
                   nationalNumber_.substr(len - 4);
        }
    }

    /**
     * @brief Get total digit count (country code + national number)
     */
    [[nodiscard]] size_t digitCount() const noexcept {
        return countryCodeDigits(countryCode_) + nationalNumber_.size();
    }

    /**
     * @brief Check if this is a valid E.164 number (length check)
     */
    [[nodiscard]] bool isValidE164() const noexcept {
        return digitCount() <= MaxDigits &&
               nationalNumber_.size() >= MinNationalDigits;
    }

    // Comparison operators
    [[nodiscard]] bool operator==(const PhoneNumber& other) const noexcept {
        return countryCode_ == other.countryCode_ &&
               nationalNumber_ == other.nationalNumber_;
    }

    [[nodiscard]] bool operator!=(const PhoneNumber& other) const noexcept {
        return !(*this == other);
    }

    [[nodiscard]] bool operator<(const PhoneNumber& other) const noexcept {
        if (countryCode_ != other.countryCode_) {
            return countryCodeValue(countryCode_) < countryCodeValue(other.countryCode_);
        }
        return nationalNumber_ < other.nationalNumber_;
    }

private:
    friend class SafePhone;

    CountryCode countryCode_;
    std::string nationalNumber_;

    PhoneNumber(CountryCode cc, std::string national) noexcept
        : countryCode_(cc), nationalNumber_(std::move(national)) {}
};

// ============================================================================
// SafePhone Operations
// ============================================================================

/**
 * @brief Safe phone number operations
 *
 * Provides parsing and validation utilities for phone numbers.
 */
class SafePhone {
public:
    /**
     * @brief Parse phone number from string
     *
     * Accepts various formats:
     * - E.164: +15551234567
     * - With spaces: +1 555 123 4567
     * - With dashes: +1-555-123-4567
     * - With parentheses: +1 (555) 123-4567
     *
     * @param input The phone number string
     * @return The parsed phone number, or std::nullopt if invalid
     */
    [[nodiscard]] static std::optional<PhoneNumber> parse(std::string_view input) noexcept {
        if (input.empty()) {
            return std::nullopt;
        }

        // Extract digits only
        std::string digits;
        digits.reserve(PhoneNumber::MaxDigits);

        for (char c : input) {
            if (c >= '0' && c <= '9') {
                digits += c;
            }
            // Skip +, spaces, dashes, parentheses
        }

        // Validate length
        if (digits.size() < 7 || digits.size() > PhoneNumber::MaxDigits) {
            return std::nullopt;
        }

        // Try to parse country code (longest match first)
        auto [countryCode, nationalStart] = parseCountryCode(digits);
        if (countryCode == CountryCode::Unknown) {
            return std::nullopt;
        }

        // Validate national number length
        if (digits.size() - nationalStart < PhoneNumber::MinNationalDigits) {
            return std::nullopt;
        }

        return PhoneNumber(countryCode, digits.substr(nationalStart));
    }

    /**
     * @brief Parse phone number with explicit country code
     *
     * @param input The national number string (digits only after country code)
     * @param countryCode The country code to use
     * @return The parsed phone number, or std::nullopt if invalid
     */
    [[nodiscard]] static std::optional<PhoneNumber> parseWithCountry(
        std::string_view input,
        CountryCode countryCode
    ) noexcept {
        if (countryCode == CountryCode::Unknown) {
            return std::nullopt;
        }

        // Extract digits only
        std::string digits;
        digits.reserve(15);

        for (char c : input) {
            if (c >= '0' && c <= '9') {
                digits += c;
            }
        }

        // Validate length
        if (digits.size() < PhoneNumber::MinNationalDigits) {
            return std::nullopt;
        }

        const size_t totalDigits = countryCodeDigits(countryCode) + digits.size();
        if (totalDigits > PhoneNumber::MaxDigits) {
            return std::nullopt;
        }

        return PhoneNumber(countryCode, std::move(digits));
    }

    /**
     * @brief Check if string is a valid phone number
     */
    [[nodiscard]] static bool isValid(std::string_view input) noexcept {
        return parse(input).has_value();
    }

    /**
     * @brief Extract digits from a string
     *
     * Useful for normalizing user input before parsing.
     */
    [[nodiscard]] static std::string extractDigits(std::string_view input) {
        std::string result;
        result.reserve(input.size());

        for (char c : input) {
            if (c >= '0' && c <= '9') {
                result += c;
            }
        }

        return result;
    }

    /**
     * @brief Check if a string contains only digits
     */
    [[nodiscard]] static bool isDigitsOnly(std::string_view input) noexcept {
        if (input.empty()) return false;
        for (char c : input) {
            if (c < '0' || c > '9') return false;
        }
        return true;
    }

private:
    /**
     * @brief Try to parse country code from digit string
     *
     * Tries 3-digit codes first, then 2-digit, then 1-digit.
     *
     * @return Pair of (CountryCode, start index of national number)
     */
    [[nodiscard]] static std::pair<CountryCode, size_t> parseCountryCode(
        const std::string& digits
    ) noexcept {
        // Try 3-digit codes first
        if (digits.size() >= 3) {
            uint16_t value = static_cast<uint16_t>(
                (digits[0] - '0') * 100 +
                (digits[1] - '0') * 10 +
                (digits[2] - '0')
            );
            CountryCode cc = countryCodeFromValue(value);
            if (cc != CountryCode::Unknown) {
                return {cc, 3};
            }
        }

        // Try 2-digit codes
        if (digits.size() >= 2) {
            uint16_t value = static_cast<uint16_t>(
                (digits[0] - '0') * 10 +
                (digits[1] - '0')
            );
            CountryCode cc = countryCodeFromValue(value);
            if (cc != CountryCode::Unknown) {
                return {cc, 2};
            }
        }

        // Try 1-digit codes
        if (digits.size() >= 1) {
            uint16_t value = static_cast<uint16_t>(digits[0] - '0');
            CountryCode cc = countryCodeFromValue(value);
            if (cc != CountryCode::Unknown) {
                return {cc, 1};
            }
        }

        return {CountryCode::Unknown, 0};
    }
};

} // namespace proven

// Hash support for std::unordered_map/set
namespace std {
template <>
struct hash<proven::PhoneNumber> {
    size_t operator()(const proven::PhoneNumber& phone) const noexcept {
        // FNV-1a hash
        size_t hash = 14695981039346656037ULL;
        hash ^= static_cast<size_t>(phone.countryCode());
        hash *= 1099511628211ULL;
        for (char c : phone.nationalNumber()) {
            hash ^= static_cast<size_t>(c);
            hash *= 1099511628211ULL;
        }
        return hash;
    }
};
} // namespace std

#endif // PROVEN_SAFE_PHONE_HPP
