// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_crypto.hpp
/// @brief C++ wrapper for Proven SafeCrypto FFI functions
///
/// Thin wrapper around proven_crypto_* functions. Constant-time
/// comparison and random byte generation are performed by the
/// Idris2 core via the Zig FFI bridge.

#pragma once

#include "proven/ffi.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace proven {

/// @brief Cryptographic safety operations delegating to libproven FFI
class SafeCrypto {
public:
    /// @brief Constant-time byte comparison (timing-safe)
    [[nodiscard]] static bool constant_time_compare(
        std::string_view a, std::string_view b
    ) noexcept {
        auto result = proven_crypto_constant_time_eq(
            reinterpret_cast<const uint8_t*>(a.data()), a.size(),
            reinterpret_cast<const uint8_t*>(b.data()), b.size());
        return result.status == PROVEN_OK && result.value;
    }

    /// @brief Generate cryptographically secure random bytes
    [[nodiscard]] static std::optional<std::string> random_bytes(size_t count) {
        std::string buf(count, '\0');
        auto status = proven_crypto_random_bytes(
            reinterpret_cast<uint8_t*>(buf.data()), count);
        if (status != PROVEN_OK) return std::nullopt;
        return buf;
    }

    /// @brief Generate random bytes into an existing buffer
    static bool random_bytes_into(uint8_t* buffer, size_t count) noexcept {
        return proven_crypto_random_bytes(buffer, count) == PROVEN_OK;
    }
};

} // namespace proven
