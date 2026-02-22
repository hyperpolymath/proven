// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_json.hpp
/// @brief C++ wrapper for Proven SafeJson FFI functions
///
/// Thin wrapper around proven_json_* functions. JSON validation
/// and type detection are performed by the Idris2 core via the
/// Zig FFI bridge.

#pragma once

#include "proven/ffi.hpp"
#include <cstdint>
#include <optional>
#include <string_view>

namespace proven {

/// @brief JSON value types (mirrors C enum)
enum class JsonType : int32_t {
    Null    = PROVEN_JSON_NULL,
    Bool    = PROVEN_JSON_BOOL,
    Number  = PROVEN_JSON_NUMBER,
    String  = PROVEN_JSON_STRING,
    Array   = PROVEN_JSON_ARRAY,
    Object  = PROVEN_JSON_OBJECT,
    Invalid = PROVEN_JSON_INVALID
};

/// @brief Safe JSON operations delegating to libproven FFI
class SafeJson {
public:
    /// @brief Validate JSON syntax via FFI
    [[nodiscard]] static bool is_valid(std::string_view json) noexcept {
        auto result = proven_json_is_valid(
            reinterpret_cast<const uint8_t*>(json.data()), json.size());
        return result.status == PROVEN_OK && result.value;
    }

    /// @brief Detect JSON value type at root level via FFI
    [[nodiscard]] static std::optional<JsonType> detect_type(std::string_view json) noexcept {
        auto type = proven_json_get_type(
            reinterpret_cast<const uint8_t*>(json.data()), json.size());
        if (type == PROVEN_JSON_INVALID) return std::nullopt;
        return static_cast<JsonType>(type);
    }
};

} // namespace proven
