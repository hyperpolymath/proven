// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file safe_color.hpp
 * @brief Safe color handling with validation and WCAG contrast calculations
 *
 * Provides RGB/RGBA color types with safe conversions and
 * accessibility-focused contrast ratio calculations.
 *
 * @example
 * @code
 * #include <proven/safe_color.hpp>
 *
 * int main() {
 *     using namespace proven;
 *
 *     // Create colors
 *     auto red = RGB::fromHex("#FF0000");
 *     auto white = RGB::WHITE;
 *     auto black = RGB::BLACK;
 *
 *     // Check WCAG compliance
 *     double ratio = contrastRatio(black, white);
 *     if (meetsWcagAA(black, white)) {
 *         std::cout << "Meets AA standard\n";
 *     }
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_SAFE_COLOR_HPP
#define PROVEN_SAFE_COLOR_HPP

#include "common.hpp"
#include <cstdint>
#include <string>
#include <string_view>
#include <optional>
#include <cmath>

namespace proven {

/**
 * @brief RGB color with values 0-255
 */
struct RGB {
    uint8_t r = 0;
    uint8_t g = 0;
    uint8_t b = 0;

    /**
     * @brief Create a new RGB color
     */
    constexpr RGB(uint8_t red, uint8_t green, uint8_t blue) noexcept
        : r(red), g(green), b(blue) {}

    /**
     * @brief Default constructor (black)
     */
    constexpr RGB() noexcept = default;

    /**
     * @brief Create RGB from hex string (e.g., "#FF0000" or "FF0000")
     */
    [[nodiscard]] static std::optional<RGB> fromHex(std::string_view hex) noexcept {
        // Strip # prefix if present
        if (!hex.empty() && hex[0] == '#') {
            hex.remove_prefix(1);
        }

        if (hex.size() != 6) {
            return std::nullopt;
        }

        auto parseHexByte = [](std::string_view s) -> std::optional<uint8_t> {
            if (s.size() != 2) return std::nullopt;

            uint8_t result = 0;
            for (char c : s) {
                result <<= 4;
                if (c >= '0' && c <= '9') {
                    result |= (c - '0');
                } else if (c >= 'a' && c <= 'f') {
                    result |= (c - 'a' + 10);
                } else if (c >= 'A' && c <= 'F') {
                    result |= (c - 'A' + 10);
                } else {
                    return std::nullopt;
                }
            }
            return result;
        };

        auto rOpt = parseHexByte(hex.substr(0, 2));
        auto gOpt = parseHexByte(hex.substr(2, 2));
        auto bOpt = parseHexByte(hex.substr(4, 2));

        if (!rOpt || !gOpt || !bOpt) {
            return std::nullopt;
        }

        return RGB(*rOpt, *gOpt, *bOpt);
    }

    /**
     * @brief Convert to hex string
     */
    [[nodiscard]] std::string toHex() const {
        static const char* hexDigits = "0123456789ABCDEF";
        std::string result = "#";
        result += hexDigits[(r >> 4) & 0xF];
        result += hexDigits[r & 0xF];
        result += hexDigits[(g >> 4) & 0xF];
        result += hexDigits[g & 0xF];
        result += hexDigits[(b >> 4) & 0xF];
        result += hexDigits[b & 0xF];
        return result;
    }

    /**
     * @brief Calculate relative luminance (WCAG formula)
     */
    [[nodiscard]] double luminance() const noexcept {
        auto gammaCorrect = [](double value) -> double {
            if (value <= 0.03928) {
                return value / 12.92;
            }
            return std::pow((value + 0.055) / 1.055, 2.4);
        };

        double rLinear = gammaCorrect(r / 255.0);
        double gLinear = gammaCorrect(g / 255.0);
        double bLinear = gammaCorrect(b / 255.0);

        return 0.2126 * rLinear + 0.7152 * gLinear + 0.0722 * bLinear;
    }

    /**
     * @brief Equality comparison
     */
    [[nodiscard]] constexpr bool operator==(const RGB& other) const noexcept {
        return r == other.r && g == other.g && b == other.b;
    }

    [[nodiscard]] constexpr bool operator!=(const RGB& other) const noexcept {
        return !(*this == other);
    }

    // Common color constants
    static constexpr RGB BLACK{0, 0, 0};
    static constexpr RGB WHITE{255, 255, 255};
    static constexpr RGB RED{255, 0, 0};
    static constexpr RGB GREEN{0, 255, 0};
    static constexpr RGB BLUE{0, 0, 255};
    static constexpr RGB YELLOW{255, 255, 0};
    static constexpr RGB CYAN{0, 255, 255};
    static constexpr RGB MAGENTA{255, 0, 255};
};

/**
 * @brief RGBA color with alpha channel
 */
struct RGBA {
    uint8_t r = 0;
    uint8_t g = 0;
    uint8_t b = 0;
    uint8_t a = 255;

    /**
     * @brief Create a new RGBA color
     */
    constexpr RGBA(uint8_t red, uint8_t green, uint8_t blue, uint8_t alpha = 255) noexcept
        : r(red), g(green), b(blue), a(alpha) {}

    /**
     * @brief Default constructor (opaque black)
     */
    constexpr RGBA() noexcept = default;

    /**
     * @brief Create from RGB with alpha
     */
    [[nodiscard]] static constexpr RGBA fromRGB(const RGB& rgb, uint8_t alpha = 255) noexcept {
        return RGBA(rgb.r, rgb.g, rgb.b, alpha);
    }

    /**
     * @brief Convert to RGB (discarding alpha)
     */
    [[nodiscard]] constexpr RGB toRGB() const noexcept {
        return RGB(r, g, b);
    }

    /**
     * @brief Get alpha as normalized float [0.0, 1.0]
     */
    [[nodiscard]] constexpr double alphaNormalized() const noexcept {
        return a / 255.0;
    }

    /**
     * @brief Equality comparison
     */
    [[nodiscard]] constexpr bool operator==(const RGBA& other) const noexcept {
        return r == other.r && g == other.g && b == other.b && a == other.a;
    }

    [[nodiscard]] constexpr bool operator!=(const RGBA& other) const noexcept {
        return !(*this == other);
    }
};

/**
 * @brief Calculate WCAG contrast ratio between two colors
 *
 * @return Contrast ratio (1:1 to 21:1)
 */
[[nodiscard]] inline double contrastRatio(const RGB& color1, const RGB& color2) noexcept {
    double l1 = color1.luminance();
    double l2 = color2.luminance();
    double lighter = (l1 > l2) ? l1 : l2;
    double darker = (l1 > l2) ? l2 : l1;
    return (lighter + 0.05) / (darker + 0.05);
}

/**
 * @brief Check if contrast meets WCAG AA standard (4.5:1 for normal text)
 */
[[nodiscard]] inline bool meetsWcagAA(const RGB& color1, const RGB& color2) noexcept {
    return contrastRatio(color1, color2) >= 4.5;
}

/**
 * @brief Check if contrast meets WCAG AA standard for large text (3:1)
 */
[[nodiscard]] inline bool meetsWcagAALarge(const RGB& color1, const RGB& color2) noexcept {
    return contrastRatio(color1, color2) >= 3.0;
}

/**
 * @brief Check if contrast meets WCAG AAA standard (7:1 for normal text)
 */
[[nodiscard]] inline bool meetsWcagAAA(const RGB& color1, const RGB& color2) noexcept {
    return contrastRatio(color1, color2) >= 7.0;
}

/**
 * @brief Check if contrast meets WCAG AAA standard for large text (4.5:1)
 */
[[nodiscard]] inline bool meetsWcagAAALarge(const RGB& color1, const RGB& color2) noexcept {
    return contrastRatio(color1, color2) >= 4.5;
}

/**
 * @brief Blend foreground color with background using alpha
 */
[[nodiscard]] inline RGB blend(const RGBA& fg, const RGB& bg) noexcept {
    double alpha = fg.a / 255.0;
    double invAlpha = 1.0 - alpha;

    return RGB(
        static_cast<uint8_t>(fg.r * alpha + bg.r * invAlpha),
        static_cast<uint8_t>(fg.g * alpha + bg.g * invAlpha),
        static_cast<uint8_t>(fg.b * alpha + bg.b * invAlpha)
    );
}

/**
 * @brief Linear interpolation between two colors
 *
 * @param color1 Start color
 * @param color2 End color
 * @param t Interpolation factor (0.0 to 1.0)
 */
[[nodiscard]] inline RGB lerp(const RGB& color1, const RGB& color2, double t) noexcept {
    t = (t < 0.0) ? 0.0 : (t > 1.0) ? 1.0 : t;
    return RGB(
        static_cast<uint8_t>(color1.r + (color2.r - color1.r) * t),
        static_cast<uint8_t>(color1.g + (color2.g - color1.g) * t),
        static_cast<uint8_t>(color1.b + (color2.b - color1.b) * t)
    );
}

/**
 * @brief Invert a color
 */
[[nodiscard]] inline constexpr RGB invert(const RGB& color) noexcept {
    return RGB(255 - color.r, 255 - color.g, 255 - color.b);
}

/**
 * @brief Convert to grayscale using luminance
 */
[[nodiscard]] inline RGB toGrayscale(const RGB& color) noexcept {
    uint8_t gray = static_cast<uint8_t>(color.luminance() * 255);
    return RGB(gray, gray, gray);
}

} // namespace proven

#endif // PROVEN_SAFE_COLOR_HPP
