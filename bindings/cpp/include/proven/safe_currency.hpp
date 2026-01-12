// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file safe_currency.hpp
 * @brief Safe currency operations with type-safe monetary values
 *
 * Provides ISO 4217 currency codes and a Money class for safe
 * arithmetic operations on monetary values using minor units
 * to avoid floating-point precision issues.
 *
 * @example
 * @code
 * #include <proven/safe_currency.hpp>
 * #include <iostream>
 *
 * int main() {
 *     using namespace proven;
 *
 *     // Create monetary values
 *     Money price = Money::fromMajor(99, CurrencyCode::USD);
 *     Money tax = Money::fromMinor(825, CurrencyCode::USD);  // $8.25
 *
 *     // Safe arithmetic
 *     auto total = price.add(tax);
 *     if (total) {
 *         std::cout << "Total: " << total->toString() << "\n";  // $107.25
 *     }
 *
 *     // Currency mismatch detection
 *     Money euros = Money::fromMajor(50, CurrencyCode::EUR);
 *     auto invalid = price.add(euros);  // Returns nullopt
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_SAFE_CURRENCY_HPP
#define PROVEN_SAFE_CURRENCY_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <cmath>

namespace proven {

// ============================================================================
// Currency Code
// ============================================================================

/**
 * @brief ISO 4217 currency codes
 *
 * Includes major world currencies and common cryptocurrencies.
 */
enum class CurrencyCode : uint16_t {
    // Major currencies
    USD = 840,  ///< US Dollar
    EUR = 978,  ///< Euro
    GBP = 826,  ///< British Pound
    JPY = 392,  ///< Japanese Yen
    CHF = 756,  ///< Swiss Franc
    CAD = 124,  ///< Canadian Dollar
    AUD = 36,   ///< Australian Dollar
    NZD = 554,  ///< New Zealand Dollar
    CNY = 156,  ///< Chinese Yuan
    INR = 356,  ///< Indian Rupee

    // Americas
    BRL = 986,  ///< Brazilian Real
    MXN = 484,  ///< Mexican Peso
    CLP = 152,  ///< Chilean Peso
    COP = 170,  ///< Colombian Peso
    PEN = 604,  ///< Peruvian Sol
    ARS = 32,   ///< Argentine Peso

    // Asia-Pacific
    KRW = 410,  ///< South Korean Won
    SGD = 702,  ///< Singapore Dollar
    HKD = 344,  ///< Hong Kong Dollar
    THB = 764,  ///< Thai Baht
    MYR = 458,  ///< Malaysian Ringgit
    IDR = 360,  ///< Indonesian Rupiah
    PHP = 608,  ///< Philippine Peso
    VND = 704,  ///< Vietnamese Dong

    // Europe
    SEK = 752,  ///< Swedish Krona
    NOK = 578,  ///< Norwegian Krone
    DKK = 208,  ///< Danish Krone
    PLN = 985,  ///< Polish Zloty
    CZK = 203,  ///< Czech Koruna
    HUF = 348,  ///< Hungarian Forint
    RON = 946,  ///< Romanian Leu
    BGN = 975,  ///< Bulgarian Lev
    HRK = 191,  ///< Croatian Kuna (legacy)
    ISK = 352,  ///< Icelandic Krona
    RUB = 643,  ///< Russian Ruble

    // Middle East & Africa
    AED = 784,  ///< UAE Dirham
    SAR = 682,  ///< Saudi Riyal
    ILS = 376,  ///< Israeli Shekel
    ZAR = 710,  ///< South African Rand
    TRY = 949,  ///< Turkish Lira

    // Cryptocurrencies (using unofficial numeric codes)
    BTC = 9001, ///< Bitcoin
    ETH = 9002, ///< Ethereum

    // Unknown/Invalid
    Unknown = 0
};

/**
 * @brief Get the number of decimal places for a currency
 */
[[nodiscard]] constexpr uint8_t currencyDecimals(CurrencyCode currency) noexcept {
    switch (currency) {
        // Zero decimal currencies
        case CurrencyCode::JPY:
        case CurrencyCode::KRW:
        case CurrencyCode::VND:
        case CurrencyCode::CLP:
        case CurrencyCode::ISK:
        case CurrencyCode::HUF:
            return 0;

        // Cryptocurrencies (8 decimals)
        case CurrencyCode::BTC:
        case CurrencyCode::ETH:
            return 8;

        // Most currencies use 2 decimals
        default:
            return 2;
    }
}

/**
 * @brief Get the currency symbol
 */
[[nodiscard]] constexpr const char* currencySymbol(CurrencyCode currency) noexcept {
    switch (currency) {
        case CurrencyCode::USD:
        case CurrencyCode::CAD:
        case CurrencyCode::AUD:
        case CurrencyCode::NZD:
        case CurrencyCode::HKD:
        case CurrencyCode::SGD:
        case CurrencyCode::MXN:
        case CurrencyCode::CLP:
        case CurrencyCode::COP:
        case CurrencyCode::ARS:
            return "$";

        case CurrencyCode::EUR: return "\xe2\x82\xac";  // Euro sign
        case CurrencyCode::GBP: return "\xc2\xa3";      // Pound sign
        case CurrencyCode::JPY:
        case CurrencyCode::CNY: return "\xc2\xa5";      // Yen sign
        case CurrencyCode::CHF: return "Fr";
        case CurrencyCode::INR: return "\xe2\x82\xb9";  // Rupee sign
        case CurrencyCode::KRW: return "\xe2\x82\xa9"; // Won sign
        case CurrencyCode::RUB: return "\xe2\x82\xbd"; // Ruble sign
        case CurrencyCode::BTC: return "\xe2\x82\xbf"; // Bitcoin sign
        case CurrencyCode::ETH: return "\xce\x9e";     // Greek Xi (Eth symbol)
        case CurrencyCode::BRL: return "R$";
        case CurrencyCode::SEK:
        case CurrencyCode::NOK:
        case CurrencyCode::DKK:
        case CurrencyCode::ISK: return "kr";
        case CurrencyCode::PLN: return "z\xc5\x82";    // zloty
        case CurrencyCode::CZK: return "K\xc4\x8d";    // koruna
        case CurrencyCode::TRY: return "\xe2\x82\xba"; // Lira sign
        case CurrencyCode::ILS: return "\xe2\x82\xaa"; // Shekel sign
        case CurrencyCode::ZAR: return "R";
        case CurrencyCode::THB: return "\xe0\xb8\xbf"; // Baht sign
        case CurrencyCode::PHP: return "\xe2\x82\xb1"; // Peso sign
        case CurrencyCode::VND: return "\xe2\x82\xab"; // Dong sign
        case CurrencyCode::AED:
        case CurrencyCode::SAR: return "\xd8\xb1.\xd8\xb3"; // Arabic

        default: return "";
    }
}

/**
 * @brief Get the ISO 4217 alphabetic code
 */
[[nodiscard]] constexpr const char* currencyCode(CurrencyCode currency) noexcept {
    switch (currency) {
        case CurrencyCode::USD: return "USD";
        case CurrencyCode::EUR: return "EUR";
        case CurrencyCode::GBP: return "GBP";
        case CurrencyCode::JPY: return "JPY";
        case CurrencyCode::CHF: return "CHF";
        case CurrencyCode::CAD: return "CAD";
        case CurrencyCode::AUD: return "AUD";
        case CurrencyCode::NZD: return "NZD";
        case CurrencyCode::CNY: return "CNY";
        case CurrencyCode::INR: return "INR";
        case CurrencyCode::BRL: return "BRL";
        case CurrencyCode::MXN: return "MXN";
        case CurrencyCode::KRW: return "KRW";
        case CurrencyCode::SGD: return "SGD";
        case CurrencyCode::HKD: return "HKD";
        case CurrencyCode::SEK: return "SEK";
        case CurrencyCode::NOK: return "NOK";
        case CurrencyCode::DKK: return "DKK";
        case CurrencyCode::PLN: return "PLN";
        case CurrencyCode::RUB: return "RUB";
        case CurrencyCode::ZAR: return "ZAR";
        case CurrencyCode::TRY: return "TRY";
        case CurrencyCode::THB: return "THB";
        case CurrencyCode::MYR: return "MYR";
        case CurrencyCode::IDR: return "IDR";
        case CurrencyCode::PHP: return "PHP";
        case CurrencyCode::VND: return "VND";
        case CurrencyCode::AED: return "AED";
        case CurrencyCode::SAR: return "SAR";
        case CurrencyCode::ILS: return "ILS";
        case CurrencyCode::CZK: return "CZK";
        case CurrencyCode::HUF: return "HUF";
        case CurrencyCode::RON: return "RON";
        case CurrencyCode::BGN: return "BGN";
        case CurrencyCode::HRK: return "HRK";
        case CurrencyCode::ISK: return "ISK";
        case CurrencyCode::CLP: return "CLP";
        case CurrencyCode::COP: return "COP";
        case CurrencyCode::PEN: return "PEN";
        case CurrencyCode::ARS: return "ARS";
        case CurrencyCode::BTC: return "BTC";
        case CurrencyCode::ETH: return "ETH";
        default: return "XXX";
    }
}

/**
 * @brief Get the currency name
 */
[[nodiscard]] constexpr const char* currencyName(CurrencyCode currency) noexcept {
    switch (currency) {
        case CurrencyCode::USD: return "US Dollar";
        case CurrencyCode::EUR: return "Euro";
        case CurrencyCode::GBP: return "British Pound";
        case CurrencyCode::JPY: return "Japanese Yen";
        case CurrencyCode::CHF: return "Swiss Franc";
        case CurrencyCode::CAD: return "Canadian Dollar";
        case CurrencyCode::AUD: return "Australian Dollar";
        case CurrencyCode::NZD: return "New Zealand Dollar";
        case CurrencyCode::CNY: return "Chinese Yuan";
        case CurrencyCode::INR: return "Indian Rupee";
        case CurrencyCode::BRL: return "Brazilian Real";
        case CurrencyCode::MXN: return "Mexican Peso";
        case CurrencyCode::KRW: return "South Korean Won";
        case CurrencyCode::SGD: return "Singapore Dollar";
        case CurrencyCode::HKD: return "Hong Kong Dollar";
        case CurrencyCode::SEK: return "Swedish Krona";
        case CurrencyCode::NOK: return "Norwegian Krone";
        case CurrencyCode::DKK: return "Danish Krone";
        case CurrencyCode::PLN: return "Polish Zloty";
        case CurrencyCode::RUB: return "Russian Ruble";
        case CurrencyCode::ZAR: return "South African Rand";
        case CurrencyCode::TRY: return "Turkish Lira";
        case CurrencyCode::BTC: return "Bitcoin";
        case CurrencyCode::ETH: return "Ethereum";
        default: return "Unknown Currency";
    }
}

// ============================================================================
// Money Class
// ============================================================================

/**
 * @brief Type-safe monetary value
 *
 * Stores monetary values as minor units (cents, satoshis, etc.) to avoid
 * floating-point precision issues. All arithmetic operations check for
 * currency compatibility.
 */
class Money {
public:
    /**
     * @brief Default constructor creates zero USD
     */
    constexpr Money() noexcept
        : minorUnits_(0), currency_(CurrencyCode::USD) {}

    /**
     * @brief Create money from major units (dollars, euros, etc.)
     */
    [[nodiscard]] static constexpr Money fromMajor(int64_t amount, CurrencyCode currency) noexcept {
        const int64_t multiplier = powerOf10(currencyDecimals(currency));
        return Money(amount * multiplier, currency);
    }

    /**
     * @brief Create money from minor units (cents, satoshis, etc.)
     */
    [[nodiscard]] static constexpr Money fromMinor(int64_t amount, CurrencyCode currency) noexcept {
        return Money(amount, currency);
    }

    /**
     * @brief Create zero amount in specified currency
     */
    [[nodiscard]] static constexpr Money zero(CurrencyCode currency) noexcept {
        return Money(0, currency);
    }

    /**
     * @brief Get the currency code
     */
    [[nodiscard]] constexpr CurrencyCode currency() const noexcept {
        return currency_;
    }

    /**
     * @brief Get the amount in minor units
     */
    [[nodiscard]] constexpr int64_t minor() const noexcept {
        return minorUnits_;
    }

    /**
     * @brief Get the amount in major units (truncated)
     */
    [[nodiscard]] constexpr int64_t major() const noexcept {
        const int64_t divisor = powerOf10(currencyDecimals(currency_));
        return minorUnits_ / divisor;
    }

    /**
     * @brief Get the fractional part in minor units
     */
    [[nodiscard]] constexpr int64_t fractional() const noexcept {
        const int64_t divisor = powerOf10(currencyDecimals(currency_));
        return (minorUnits_ >= 0) ? (minorUnits_ % divisor) : ((-minorUnits_) % divisor);
    }

    /**
     * @brief Check if amount is zero
     */
    [[nodiscard]] constexpr bool isZero() const noexcept {
        return minorUnits_ == 0;
    }

    /**
     * @brief Check if amount is positive
     */
    [[nodiscard]] constexpr bool isPositive() const noexcept {
        return minorUnits_ > 0;
    }

    /**
     * @brief Check if amount is negative
     */
    [[nodiscard]] constexpr bool isNegative() const noexcept {
        return minorUnits_ < 0;
    }

    /**
     * @brief Get absolute value
     */
    [[nodiscard]] constexpr Money abs() const noexcept {
        return Money(minorUnits_ >= 0 ? minorUnits_ : -minorUnits_, currency_);
    }

    /**
     * @brief Negate the amount
     */
    [[nodiscard]] constexpr Money negate() const noexcept {
        return Money(-minorUnits_, currency_);
    }

    /**
     * @brief Add two monetary values (same currency only)
     *
     * @return Sum if currencies match, std::nullopt otherwise
     */
    [[nodiscard]] std::optional<Money> add(const Money& other) const noexcept {
        if (currency_ != other.currency_) {
            return std::nullopt;
        }
        return Money(minorUnits_ + other.minorUnits_, currency_);
    }

    /**
     * @brief Subtract two monetary values (same currency only)
     *
     * @return Difference if currencies match, std::nullopt otherwise
     */
    [[nodiscard]] std::optional<Money> sub(const Money& other) const noexcept {
        if (currency_ != other.currency_) {
            return std::nullopt;
        }
        return Money(minorUnits_ - other.minorUnits_, currency_);
    }

    /**
     * @brief Multiply by scalar
     */
    [[nodiscard]] constexpr Money mul(int64_t scalar) const noexcept {
        return Money(minorUnits_ * scalar, currency_);
    }

    /**
     * @brief Divide by scalar
     *
     * @return Result if divisor non-zero, std::nullopt otherwise
     */
    [[nodiscard]] std::optional<Money> div(int64_t divisor) const noexcept {
        if (divisor == 0) {
            return std::nullopt;
        }
        return Money(minorUnits_ / divisor, currency_);
    }

    /**
     * @brief Calculate percentage (e.g., for taxes)
     *
     * @param percentage The percentage value (e.g., 8.25 for 8.25%)
     * @return The calculated amount
     */
    [[nodiscard]] Money percentage(double percentage) const noexcept {
        const int64_t result = static_cast<int64_t>(
            static_cast<double>(minorUnits_) * percentage / 100.0
        );
        return Money(result, currency_);
    }

    /**
     * @brief Allocate amount among N recipients with no money lost
     *
     * Distributes the amount as evenly as possible, with any remainder
     * distributed one unit at a time to the first recipients.
     *
     * @param count Number of recipients
     * @param results Array to store results (must have at least count elements)
     * @return true if allocation succeeded, false if count is zero
     */
    bool allocate(size_t count, Money* results) const noexcept {
        if (count == 0 || results == nullptr) {
            return false;
        }

        const int64_t share = minorUnits_ / static_cast<int64_t>(count);
        int64_t remainder = minorUnits_ - (share * static_cast<int64_t>(count));

        for (size_t i = 0; i < count; ++i) {
            int64_t extra = 0;
            if (remainder > 0) {
                extra = 1;
                --remainder;
            } else if (remainder < 0) {
                extra = -1;
                ++remainder;
            }
            results[i] = Money(share + extra, currency_);
        }

        return true;
    }

    /**
     * @brief Format as string with symbol
     *
     * @return Formatted string like "$123.45" or "-$50.00"
     */
    [[nodiscard]] std::string toString() const {
        const uint8_t decimals = currencyDecimals(currency_);
        const int64_t divisor = powerOf10(decimals);
        const int64_t absUnits = minorUnits_ >= 0 ? minorUnits_ : -minorUnits_;
        const int64_t majorPart = absUnits / divisor;
        const int64_t minorPart = absUnits % divisor;

        std::string result;
        if (minorUnits_ < 0) {
            result += '-';
        }
        result += currencySymbol(currency_);
        result += std::to_string(majorPart);

        if (decimals > 0) {
            result += '.';
            std::string minorStr = std::to_string(minorPart);
            // Pad with leading zeros
            while (minorStr.size() < decimals) {
                minorStr = '0' + minorStr;
            }
            result += minorStr;
        }

        return result;
    }

    /**
     * @brief Format with ISO currency code
     *
     * @return Formatted string like "123.45 USD"
     */
    [[nodiscard]] std::string toStringWithCode() const {
        const uint8_t decimals = currencyDecimals(currency_);
        const int64_t divisor = powerOf10(decimals);
        const int64_t absUnits = minorUnits_ >= 0 ? minorUnits_ : -minorUnits_;
        const int64_t majorPart = absUnits / divisor;
        const int64_t minorPart = absUnits % divisor;

        std::string result;
        if (minorUnits_ < 0) {
            result += '-';
        }
        result += std::to_string(majorPart);

        if (decimals > 0) {
            result += '.';
            std::string minorStr = std::to_string(minorPart);
            while (minorStr.size() < decimals) {
                minorStr = '0' + minorStr;
            }
            result += minorStr;
        }

        result += ' ';
        result += currencyCode(currency_);

        return result;
    }

    // Comparison operators
    [[nodiscard]] constexpr bool operator==(const Money& other) const noexcept {
        return currency_ == other.currency_ && minorUnits_ == other.minorUnits_;
    }

    [[nodiscard]] constexpr bool operator!=(const Money& other) const noexcept {
        return !(*this == other);
    }

    /**
     * @brief Compare amounts (same currency only)
     *
     * @return Comparison result if currencies match, std::nullopt otherwise
     */
    [[nodiscard]] std::optional<int> compare(const Money& other) const noexcept {
        if (currency_ != other.currency_) {
            return std::nullopt;
        }
        if (minorUnits_ < other.minorUnits_) return -1;
        if (minorUnits_ > other.minorUnits_) return 1;
        return 0;
    }

private:
    int64_t minorUnits_;
    CurrencyCode currency_;

    constexpr Money(int64_t minorUnits, CurrencyCode currency) noexcept
        : minorUnits_(minorUnits), currency_(currency) {}

    [[nodiscard]] static constexpr int64_t powerOf10(uint8_t exponent) noexcept {
        int64_t result = 1;
        for (uint8_t i = 0; i < exponent; ++i) {
            result *= 10;
        }
        return result;
    }
};

// ============================================================================
// SafeCurrency Operations
// ============================================================================

/**
 * @brief Safe currency operations
 *
 * Provides parsing and validation utilities for currency codes.
 */
class SafeCurrency {
public:
    /**
     * @brief Parse currency code from string
     *
     * Case-insensitive. Accepts 3-letter ISO 4217 codes.
     *
     * @param input The currency code string
     * @return The currency code, or std::nullopt if invalid
     */
    [[nodiscard]] static std::optional<CurrencyCode> parse(std::string_view input) noexcept {
        if (input.size() != 3) {
            return std::nullopt;
        }

        // Convert to uppercase for comparison
        char upper[4] = {0};
        for (size_t i = 0; i < 3; ++i) {
            char c = input[i];
            if (c >= 'a' && c <= 'z') {
                upper[i] = c - 'a' + 'A';
            } else if (c >= 'A' && c <= 'Z') {
                upper[i] = c;
            } else {
                return std::nullopt;
            }
        }

        // Match against known codes
        if (upper[0] == 'U' && upper[1] == 'S' && upper[2] == 'D') return CurrencyCode::USD;
        if (upper[0] == 'E' && upper[1] == 'U' && upper[2] == 'R') return CurrencyCode::EUR;
        if (upper[0] == 'G' && upper[1] == 'B' && upper[2] == 'P') return CurrencyCode::GBP;
        if (upper[0] == 'J' && upper[1] == 'P' && upper[2] == 'Y') return CurrencyCode::JPY;
        if (upper[0] == 'C' && upper[1] == 'H' && upper[2] == 'F') return CurrencyCode::CHF;
        if (upper[0] == 'C' && upper[1] == 'A' && upper[2] == 'D') return CurrencyCode::CAD;
        if (upper[0] == 'A' && upper[1] == 'U' && upper[2] == 'D') return CurrencyCode::AUD;
        if (upper[0] == 'N' && upper[1] == 'Z' && upper[2] == 'D') return CurrencyCode::NZD;
        if (upper[0] == 'C' && upper[1] == 'N' && upper[2] == 'Y') return CurrencyCode::CNY;
        if (upper[0] == 'I' && upper[1] == 'N' && upper[2] == 'R') return CurrencyCode::INR;
        if (upper[0] == 'B' && upper[1] == 'R' && upper[2] == 'L') return CurrencyCode::BRL;
        if (upper[0] == 'M' && upper[1] == 'X' && upper[2] == 'N') return CurrencyCode::MXN;
        if (upper[0] == 'K' && upper[1] == 'R' && upper[2] == 'W') return CurrencyCode::KRW;
        if (upper[0] == 'S' && upper[1] == 'G' && upper[2] == 'D') return CurrencyCode::SGD;
        if (upper[0] == 'H' && upper[1] == 'K' && upper[2] == 'D') return CurrencyCode::HKD;
        if (upper[0] == 'S' && upper[1] == 'E' && upper[2] == 'K') return CurrencyCode::SEK;
        if (upper[0] == 'N' && upper[1] == 'O' && upper[2] == 'K') return CurrencyCode::NOK;
        if (upper[0] == 'D' && upper[1] == 'K' && upper[2] == 'K') return CurrencyCode::DKK;
        if (upper[0] == 'P' && upper[1] == 'L' && upper[2] == 'N') return CurrencyCode::PLN;
        if (upper[0] == 'R' && upper[1] == 'U' && upper[2] == 'B') return CurrencyCode::RUB;
        if (upper[0] == 'Z' && upper[1] == 'A' && upper[2] == 'R') return CurrencyCode::ZAR;
        if (upper[0] == 'T' && upper[1] == 'R' && upper[2] == 'Y') return CurrencyCode::TRY;
        if (upper[0] == 'T' && upper[1] == 'H' && upper[2] == 'B') return CurrencyCode::THB;
        if (upper[0] == 'M' && upper[1] == 'Y' && upper[2] == 'R') return CurrencyCode::MYR;
        if (upper[0] == 'I' && upper[1] == 'D' && upper[2] == 'R') return CurrencyCode::IDR;
        if (upper[0] == 'P' && upper[1] == 'H' && upper[2] == 'P') return CurrencyCode::PHP;
        if (upper[0] == 'V' && upper[1] == 'N' && upper[2] == 'D') return CurrencyCode::VND;
        if (upper[0] == 'A' && upper[1] == 'E' && upper[2] == 'D') return CurrencyCode::AED;
        if (upper[0] == 'S' && upper[1] == 'A' && upper[2] == 'R') return CurrencyCode::SAR;
        if (upper[0] == 'I' && upper[1] == 'L' && upper[2] == 'S') return CurrencyCode::ILS;
        if (upper[0] == 'C' && upper[1] == 'Z' && upper[2] == 'K') return CurrencyCode::CZK;
        if (upper[0] == 'H' && upper[1] == 'U' && upper[2] == 'F') return CurrencyCode::HUF;
        if (upper[0] == 'R' && upper[1] == 'O' && upper[2] == 'N') return CurrencyCode::RON;
        if (upper[0] == 'B' && upper[1] == 'G' && upper[2] == 'N') return CurrencyCode::BGN;
        if (upper[0] == 'H' && upper[1] == 'R' && upper[2] == 'K') return CurrencyCode::HRK;
        if (upper[0] == 'I' && upper[1] == 'S' && upper[2] == 'K') return CurrencyCode::ISK;
        if (upper[0] == 'C' && upper[1] == 'L' && upper[2] == 'P') return CurrencyCode::CLP;
        if (upper[0] == 'C' && upper[1] == 'O' && upper[2] == 'P') return CurrencyCode::COP;
        if (upper[0] == 'P' && upper[1] == 'E' && upper[2] == 'N') return CurrencyCode::PEN;
        if (upper[0] == 'A' && upper[1] == 'R' && upper[2] == 'S') return CurrencyCode::ARS;
        if (upper[0] == 'B' && upper[1] == 'T' && upper[2] == 'C') return CurrencyCode::BTC;
        if (upper[0] == 'E' && upper[1] == 'T' && upper[2] == 'H') return CurrencyCode::ETH;

        return std::nullopt;
    }

    /**
     * @brief Check if string is a valid currency code
     */
    [[nodiscard]] static bool isValid(std::string_view input) noexcept {
        return parse(input).has_value();
    }
};

} // namespace proven

#endif // PROVEN_SAFE_CURRENCY_HPP
