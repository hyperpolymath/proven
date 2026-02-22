// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_string.hpp
/// @brief C++ wrapper for Proven SafeString FFI functions
///
/// Thin RAII wrapper around proven_string_* functions. All string
/// processing is performed by the Idris2 core via the Zig FFI bridge.
/// Returned FFI strings are freed via RAII (ProvenString in proven.hpp).

#pragma once

#include "proven/ffi.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace proven {

/// @brief Safe string operations delegating to libproven FFI
class SafeString {
public:
    /// @brief Check if data is valid UTF-8
    [[nodiscard]] static bool is_valid_utf8(std::string_view data) noexcept {
        auto result = proven_string_is_valid_utf8(
            reinterpret_cast<const uint8_t*>(data.data()), data.size());
        return result.status == PROVEN_OK && result.value;
    }

    /// @brief Escape string for SQL (prefer parameterized queries)
    [[nodiscard]] static std::optional<std::string> escape_sql(std::string_view input) {
        auto result = proven_string_escape_sql(
            reinterpret_cast<const uint8_t*>(input.data()), input.size());
        if (result.status != PROVEN_OK || result.value == nullptr) {
            return std::nullopt;
        }
        std::string str(result.value, result.length);
        proven_free_string(result.value);
        return str;
    }

    /// @brief Escape string for HTML (prevents XSS)
    [[nodiscard]] static std::optional<std::string> escape_html(std::string_view input) {
        auto result = proven_string_escape_html(
            reinterpret_cast<const uint8_t*>(input.data()), input.size());
        if (result.status != PROVEN_OK || result.value == nullptr) {
            return std::nullopt;
        }
        std::string str(result.value, result.length);
        proven_free_string(result.value);
        return str;
    }

    /// @brief Escape string for JavaScript string literals
    [[nodiscard]] static std::optional<std::string> escape_js(std::string_view input) {
        auto result = proven_string_escape_js(
            reinterpret_cast<const uint8_t*>(input.data()), input.size());
        if (result.status != PROVEN_OK || result.value == nullptr) {
            return std::nullopt;
        }
        std::string str(result.value, result.length);
        proven_free_string(result.value);
        return str;
    }
};

} // namespace proven
