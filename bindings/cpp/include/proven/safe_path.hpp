// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_path.hpp
/// @brief C++ wrapper for Proven SafePath FFI functions
///
/// Thin wrapper around proven_path_* functions. Path traversal detection
/// and filename sanitization are performed by the Idris2 core.

#pragma once

#include "proven/ffi.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace proven {

/// @brief Safe filesystem path operations delegating to libproven FFI
class SafePath {
public:
    /// @brief Check if path contains directory traversal sequences
    [[nodiscard]] static bool has_traversal(std::string_view path) noexcept {
        auto result = proven_path_has_traversal(
            reinterpret_cast<const uint8_t*>(path.data()), path.size());
        return result.status == PROVEN_OK && result.value;
    }

    /// @brief Check if path is safe (no traversal)
    [[nodiscard]] static bool is_safe(std::string_view path) noexcept {
        return !has_traversal(path);
    }

    /// @brief Sanitize a filename by removing dangerous characters
    [[nodiscard]] static std::optional<std::string> sanitize_filename(std::string_view name) {
        auto result = proven_path_sanitize_filename(
            reinterpret_cast<const uint8_t*>(name.data()), name.size());
        if (result.status != PROVEN_OK || result.value == nullptr) {
            return std::nullopt;
        }
        std::string str(result.value, result.length);
        proven_free_string(result.value);
        return str;
    }
};

} // namespace proven
