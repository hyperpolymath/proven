// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_email.hpp
/// @brief C++ wrapper for Proven SafeEmail FFI functions
///
/// Thin wrapper around proven_email_* functions. Email validation
/// is performed by the Idris2 core via the Zig FFI bridge.

#pragma once

#include "proven/ffi.hpp"
#include <string_view>

namespace proven {

/// @brief Email validation delegating to libproven FFI
class SafeEmail {
public:
    /// @brief Check if email address is valid (RFC 5321 simplified)
    [[nodiscard]] static bool is_valid(std::string_view email) noexcept {
        auto result = proven_email_is_valid(
            reinterpret_cast<const uint8_t*>(email.data()), email.size());
        return result.status == PROVEN_OK && result.value;
    }
};

} // namespace proven
