// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_color.hpp
/// @brief C++ wrapper for Proven SafeColor FFI functions
///
/// Thin wrapper around proven_color_* functions. Color parsing,
/// conversion, and hex formatting are performed by the Idris2 core
/// via the Zig FFI bridge.

#pragma once

#include "proven/ffi.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace proven {

/// @brief HSL color returned by FFI conversion
struct HSL {
    double h;  ///< Hue: 0-360 degrees
    double s;  ///< Saturation: 0-1
    double l;  ///< Lightness: 0-1
};

/// @brief RGB color wrapping the C struct
struct RGB {
    uint8_t r = 0;
    uint8_t g = 0;
    uint8_t b = 0;

    constexpr RGB() noexcept = default;
    constexpr RGB(uint8_t red, uint8_t green, uint8_t blue) noexcept
        : r(red), g(green), b(blue) {}

    explicit RGB(const ProvenRGBColor& c) noexcept : r(c.r), g(c.g), b(c.b) {}

    /// @brief Parse hex color string via FFI (e.g. "#FF0000" or "FF0000")
    [[nodiscard]] static std::optional<RGB> from_hex(std::string_view hex) noexcept {
        auto result = proven_color_parse_hex(
            reinterpret_cast<const uint8_t*>(hex.data()), hex.size());
        if (result.status != PROVEN_OK) return std::nullopt;
        return RGB(result.color);
    }

    /// @brief Convert to hex string via FFI
    [[nodiscard]] std::optional<std::string> to_hex() const {
        auto result = proven_color_to_hex(to_c());
        if (result.status != PROVEN_OK || result.value == nullptr) {
            return std::nullopt;
        }
        std::string str(result.value, result.length);
        proven_free_string(result.value);
        return str;
    }

    /// @brief Convert to HSL via FFI
    [[nodiscard]] HSL to_hsl() const noexcept {
        auto result = proven_color_rgb_to_hsl(to_c());
        return HSL{result.h, result.s, result.l};
    }

    /// @brief Convert to C struct for FFI calls
    [[nodiscard]] ProvenRGBColor to_c() const noexcept {
        return ProvenRGBColor{r, g, b};
    }

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
};

/// @brief Color operations delegating to libproven FFI
class SafeColor {
public:
    /// @brief Parse hex color string via FFI
    [[nodiscard]] static std::optional<RGB> parse_hex(std::string_view hex) noexcept {
        return RGB::from_hex(hex);
    }
};

} // namespace proven
