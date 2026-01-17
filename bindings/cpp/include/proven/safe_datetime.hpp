// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file safe_datetime.hpp
 * @brief Safe datetime parsing and validation
 *
 * Provides validated date/time handling with proper bounds checking
 * and ISO 8601 parsing support.
 *
 * @example
 * @code
 * #include <proven/safe_datetime.hpp>
 *
 * int main() {
 *     using namespace proven;
 *
 *     // Parse ISO date
 *     auto date = SafeDateTime::parseIsoDate("2025-01-17");
 *     if (date) {
 *         std::cout << "Year: " << date->year << "\n";
 *     }
 *
 *     // Validate date components
 *     auto validated = SafeDateTime::validateDate(2025, 2, 29);
 *     if (!validated) {
 *         std::cout << "2025 is not a leap year\n";
 *     }
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_SAFE_DATETIME_HPP
#define PROVEN_SAFE_DATETIME_HPP

#include "common.hpp"
#include <string>
#include <string_view>
#include <optional>
#include <cstdint>

namespace proven {

// ============================================================================
// Date Components
// ============================================================================

/**
 * @brief Validated date components
 */
struct Date {
    int32_t year;   ///< Year (e.g., 2025)
    uint8_t month;  ///< Month (1-12)
    uint8_t day;    ///< Day (1-31)

    /**
     * @brief Format as ISO 8601 date string (YYYY-MM-DD)
     */
    [[nodiscard]] std::string toIsoString() const {
        char buf[16];
        std::snprintf(buf, sizeof(buf), "%04d-%02u-%02u",
                      year, static_cast<unsigned>(month),
                      static_cast<unsigned>(day));
        return std::string(buf);
    }

    /**
     * @brief Comparison operators
     */
    [[nodiscard]] bool operator==(const Date& other) const noexcept {
        return year == other.year && month == other.month && day == other.day;
    }

    [[nodiscard]] bool operator<(const Date& other) const noexcept {
        if (year != other.year) return year < other.year;
        if (month != other.month) return month < other.month;
        return day < other.day;
    }

    [[nodiscard]] bool operator<=(const Date& other) const noexcept {
        return *this == other || *this < other;
    }

    [[nodiscard]] bool operator>(const Date& other) const noexcept {
        return other < *this;
    }

    [[nodiscard]] bool operator>=(const Date& other) const noexcept {
        return *this == other || *this > other;
    }
};

// ============================================================================
// Time Components
// ============================================================================

/**
 * @brief Validated time components
 */
struct Time {
    uint8_t hour;      ///< Hour (0-23)
    uint8_t minute;    ///< Minute (0-59)
    uint8_t second;    ///< Second (0-59)
    uint32_t nanos;    ///< Nanoseconds (0-999999999)

    /**
     * @brief Format as ISO 8601 time string (HH:MM:SS)
     */
    [[nodiscard]] std::string toIsoString() const {
        char buf[16];
        std::snprintf(buf, sizeof(buf), "%02u:%02u:%02u",
                      static_cast<unsigned>(hour),
                      static_cast<unsigned>(minute),
                      static_cast<unsigned>(second));
        return std::string(buf);
    }

    /**
     * @brief Format with fractional seconds
     */
    [[nodiscard]] std::string toIsoStringWithNanos() const {
        char buf[32];
        if (nanos == 0) {
            std::snprintf(buf, sizeof(buf), "%02u:%02u:%02u",
                          static_cast<unsigned>(hour),
                          static_cast<unsigned>(minute),
                          static_cast<unsigned>(second));
        } else {
            std::snprintf(buf, sizeof(buf), "%02u:%02u:%02u.%09u",
                          static_cast<unsigned>(hour),
                          static_cast<unsigned>(minute),
                          static_cast<unsigned>(second),
                          nanos);
        }
        return std::string(buf);
    }

    [[nodiscard]] bool operator==(const Time& other) const noexcept {
        return hour == other.hour && minute == other.minute &&
               second == other.second && nanos == other.nanos;
    }

    [[nodiscard]] bool operator<(const Time& other) const noexcept {
        if (hour != other.hour) return hour < other.hour;
        if (minute != other.minute) return minute < other.minute;
        if (second != other.second) return second < other.second;
        return nanos < other.nanos;
    }
};

// ============================================================================
// DateTime
// ============================================================================

/**
 * @brief Combined date and time
 */
struct DateTime {
    Date date;
    Time time;
    int16_t tzOffsetMinutes;  ///< Timezone offset in minutes (e.g., -300 for EST)

    /**
     * @brief Format as ISO 8601 datetime string
     */
    [[nodiscard]] std::string toIsoString() const {
        std::string result = date.toIsoString() + "T" + time.toIsoString();

        if (tzOffsetMinutes == 0) {
            result += "Z";
        } else {
            char buf[8];
            int hours = tzOffsetMinutes / 60;
            int mins = std::abs(tzOffsetMinutes) % 60;
            std::snprintf(buf, sizeof(buf), "%+03d:%02d", hours, mins);
            result += buf;
        }

        return result;
    }
};

// ============================================================================
// SafeDateTime Operations
// ============================================================================

/**
 * @brief Safe datetime operations with validation
 */
class SafeDateTime {
public:
    /**
     * @brief Check if a year is a leap year
     */
    [[nodiscard]] static constexpr bool isLeapYear(int32_t year) noexcept {
        return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
    }

    /**
     * @brief Get the number of days in a month
     *
     * @param year The year (for leap year calculation)
     * @param month The month (1-12)
     * @return Days in the month, or std::nullopt if invalid month
     */
    [[nodiscard]] static constexpr std::optional<uint8_t> daysInMonth(
        int32_t year, uint8_t month
    ) noexcept {
        switch (month) {
            case 1: case 3: case 5: case 7: case 8: case 10: case 12:
                return 31;
            case 4: case 6: case 9: case 11:
                return 30;
            case 2:
                return isLeapYear(year) ? 29 : 28;
            default:
                return std::nullopt;
        }
    }

    /**
     * @brief Validate and create a date
     */
    [[nodiscard]] static std::optional<Date> validateDate(
        int32_t year, uint8_t month, uint8_t day
    ) noexcept {
        if (month < 1 || month > 12) {
            return std::nullopt;
        }

        auto maxDay = daysInMonth(year, month);
        if (!maxDay || day < 1 || day > *maxDay) {
            return std::nullopt;
        }

        return Date{year, month, day};
    }

    /**
     * @brief Validate and create a time
     */
    [[nodiscard]] static std::optional<Time> validateTime(
        uint8_t hour, uint8_t minute, uint8_t second, uint32_t nanos = 0
    ) noexcept {
        if (hour > 23) return std::nullopt;
        if (minute > 59) return std::nullopt;
        if (second > 59) return std::nullopt;
        if (nanos > 999999999) return std::nullopt;

        return Time{hour, minute, second, nanos};
    }

    /**
     * @brief Parse ISO 8601 date (YYYY-MM-DD)
     */
    [[nodiscard]] static std::optional<Date> parseIsoDate(std::string_view s) noexcept {
        if (s.size() < 10) return std::nullopt;

        // Check format YYYY-MM-DD
        if (s[4] != '-' || s[7] != '-') return std::nullopt;

        auto year = parseInt32(s.substr(0, 4));
        auto month = parseUint8(s.substr(5, 2));
        auto day = parseUint8(s.substr(8, 2));

        if (!year || !month || !day) return std::nullopt;

        return validateDate(*year, *month, *day);
    }

    /**
     * @brief Parse ISO 8601 time (HH:MM:SS or HH:MM:SS.nnn)
     */
    [[nodiscard]] static std::optional<Time> parseIsoTime(std::string_view s) noexcept {
        if (s.size() < 5) return std::nullopt;

        // Handle HH:MM or HH:MM:SS or HH:MM:SS.nnn
        size_t colonPos = s.find(':');
        if (colonPos == std::string_view::npos) return std::nullopt;

        auto hour = parseUint8(s.substr(0, colonPos));
        if (!hour) return std::nullopt;

        s = s.substr(colonPos + 1);

        uint8_t minute = 0;
        uint8_t second = 0;
        uint32_t nanos = 0;

        colonPos = s.find(':');
        if (colonPos == std::string_view::npos) {
            // HH:MM format
            auto m = parseUint8(s.substr(0, 2));
            if (!m) return std::nullopt;
            minute = *m;
        } else {
            // HH:MM:SS format
            auto m = parseUint8(s.substr(0, colonPos));
            if (!m) return std::nullopt;
            minute = *m;

            s = s.substr(colonPos + 1);

            // Check for fractional seconds
            size_t dotPos = s.find('.');
            if (dotPos != std::string_view::npos) {
                auto sec = parseUint8(s.substr(0, dotPos));
                if (!sec) return std::nullopt;
                second = *sec;

                std::string_view frac = s.substr(dotPos + 1);
                // Trim trailing non-digits (timezone info)
                size_t fracLen = 0;
                while (fracLen < frac.size() && frac[fracLen] >= '0' && frac[fracLen] <= '9') {
                    ++fracLen;
                }
                frac = frac.substr(0, fracLen);

                // Parse fractional seconds to nanoseconds
                uint64_t fracValue = 0;
                for (char c : frac) {
                    fracValue = fracValue * 10 + (c - '0');
                }
                // Pad to nanoseconds (9 digits)
                for (size_t i = frac.size(); i < 9; ++i) {
                    fracValue *= 10;
                }
                nanos = static_cast<uint32_t>(fracValue % 1000000000ULL);
            } else {
                // Find end of seconds (might have timezone)
                size_t endPos = 0;
                while (endPos < s.size() && s[endPos] >= '0' && s[endPos] <= '9') {
                    ++endPos;
                }
                auto sec = parseUint8(s.substr(0, endPos));
                if (!sec) return std::nullopt;
                second = *sec;
            }
        }

        return validateTime(*hour, minute, second, nanos);
    }

    /**
     * @brief Parse ISO 8601 datetime
     */
    [[nodiscard]] static std::optional<DateTime> parseIsoDateTime(std::string_view s) noexcept {
        // Find T separator
        size_t tPos = s.find('T');
        if (tPos == std::string_view::npos) {
            tPos = s.find(' ');  // Also accept space separator
        }

        if (tPos == std::string_view::npos) {
            return std::nullopt;
        }

        auto date = parseIsoDate(s.substr(0, tPos));
        if (!date) return std::nullopt;

        std::string_view timePart = s.substr(tPos + 1);

        // Parse timezone
        int16_t tzOffset = 0;
        size_t tzStart = timePart.find('Z');
        if (tzStart != std::string_view::npos) {
            tzOffset = 0;
            timePart = timePart.substr(0, tzStart);
        } else {
            tzStart = timePart.find('+');
            if (tzStart == std::string_view::npos) {
                tzStart = timePart.find_last_of('-');
                // Avoid matching the - in time like 10:30:00
                if (tzStart != std::string_view::npos && tzStart < 6) {
                    tzStart = std::string_view::npos;
                }
            }

            if (tzStart != std::string_view::npos) {
                char sign = timePart[tzStart];
                std::string_view tzPart = timePart.substr(tzStart + 1);
                timePart = timePart.substr(0, tzStart);

                // Parse timezone offset (HH:MM or HHMM or HH)
                if (tzPart.size() >= 2) {
                    auto tzHour = parseUint8(tzPart.substr(0, 2));
                    if (tzHour) {
                        tzOffset = static_cast<int16_t>(*tzHour * 60);

                        if (tzPart.size() >= 4) {
                            size_t minStart = (tzPart[2] == ':') ? 3 : 2;
                            if (minStart + 2 <= tzPart.size()) {
                                auto tzMin = parseUint8(tzPart.substr(minStart, 2));
                                if (tzMin) {
                                    tzOffset += *tzMin;
                                }
                            }
                        }

                        if (sign == '-') {
                            tzOffset = -tzOffset;
                        }
                    }
                }
            }
        }

        auto time = parseIsoTime(timePart);
        if (!time) return std::nullopt;

        return DateTime{*date, *time, tzOffset};
    }

    /**
     * @brief Get day of week (0 = Sunday, 6 = Saturday)
     *
     * Uses Zeller's formula.
     */
    [[nodiscard]] static uint8_t dayOfWeek(const Date& date) noexcept {
        int y = date.year;
        int m = date.month;
        int d = date.day;

        if (m < 3) {
            m += 12;
            y -= 1;
        }

        int k = y % 100;
        int j = y / 100;

        int h = (d + (13 * (m + 1)) / 5 + k + k / 4 + j / 4 - 2 * j) % 7;
        return static_cast<uint8_t>((h + 6) % 7);  // Convert to 0=Sunday
    }

    /**
     * @brief Get day of year (1-366)
     */
    [[nodiscard]] static uint16_t dayOfYear(const Date& date) noexcept {
        static constexpr uint16_t daysBeforeMonth[] = {
            0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
        };

        uint16_t day = daysBeforeMonth[date.month - 1] + date.day;

        if (date.month > 2 && isLeapYear(date.year)) {
            ++day;
        }

        return day;
    }

    /**
     * @brief Add days to a date
     */
    [[nodiscard]] static Date addDays(const Date& date, int32_t days) {
        // Convert to Julian day number, add days, convert back
        int32_t jdn = toJulianDayNumber(date) + days;
        return fromJulianDayNumber(jdn);
    }

    /**
     * @brief Calculate days between two dates
     */
    [[nodiscard]] static int32_t daysBetween(const Date& from, const Date& to) noexcept {
        return toJulianDayNumber(to) - toJulianDayNumber(from);
    }

    /// Day name constants
    static constexpr const char* dayNames[] = {
        "Sunday", "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday"
    };

    /// Month name constants
    static constexpr const char* monthNames[] = {
        "January", "February", "March", "April",
        "May", "June", "July", "August",
        "September", "October", "November", "December"
    };

private:
    [[nodiscard]] static std::optional<int32_t> parseInt32(std::string_view s) noexcept {
        if (s.empty()) return std::nullopt;

        int32_t result = 0;
        bool negative = false;
        size_t i = 0;

        if (s[0] == '-') {
            negative = true;
            ++i;
        } else if (s[0] == '+') {
            ++i;
        }

        for (; i < s.size(); ++i) {
            if (s[i] < '0' || s[i] > '9') return std::nullopt;
            result = result * 10 + (s[i] - '0');
        }

        return negative ? -result : result;
    }

    [[nodiscard]] static std::optional<uint8_t> parseUint8(std::string_view s) noexcept {
        if (s.empty() || s.size() > 3) return std::nullopt;

        uint16_t result = 0;
        for (char c : s) {
            if (c < '0' || c > '9') return std::nullopt;
            result = result * 10 + (c - '0');
        }

        if (result > 255) return std::nullopt;
        return static_cast<uint8_t>(result);
    }

    [[nodiscard]] static int32_t toJulianDayNumber(const Date& date) noexcept {
        int32_t a = (14 - date.month) / 12;
        int32_t y = date.year + 4800 - a;
        int32_t m = date.month + 12 * a - 3;

        return date.day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045;
    }

    [[nodiscard]] static Date fromJulianDayNumber(int32_t jdn) noexcept {
        int32_t a = jdn + 32044;
        int32_t b = (4 * a + 3) / 146097;
        int32_t c = a - (146097 * b) / 4;
        int32_t d = (4 * c + 3) / 1461;
        int32_t e = c - (1461 * d) / 4;
        int32_t m = (5 * e + 2) / 153;

        Date date;
        date.day = static_cast<uint8_t>(e - (153 * m + 2) / 5 + 1);
        date.month = static_cast<uint8_t>(m + 3 - 12 * (m / 10));
        date.year = 100 * b + d - 4800 + m / 10;

        return date;
    }
};

} // namespace proven

#endif // PROVEN_SAFE_DATETIME_HPP
