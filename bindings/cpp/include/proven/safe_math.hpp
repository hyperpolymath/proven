// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_math.hpp
/// @brief C++ wrapper for Proven SafeMath FFI functions
///
/// Thin RAII wrapper around proven_math_* functions. All arithmetic
/// is performed by the Idris2 core via the Zig FFI bridge.

#pragma once

#include "proven/ffi.hpp"
#include <cstdint>
#include <optional>

namespace proven {

/// @brief Safe arithmetic operations delegating to libproven FFI
class SafeMath {
public:
    /// @brief Safe integer division
    /// @return quotient, or std::nullopt on division by zero / overflow
    [[nodiscard]] static std::optional<int64_t> div(int64_t num, int64_t denom) noexcept {
        auto result = proven_math_div(num, denom);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /// @brief Safe division with fallback default value
    [[nodiscard]] static int64_t div_or(int64_t def, int64_t num, int64_t denom) noexcept {
        return div(num, denom).value_or(def);
    }

    /// @brief Safe modulo operation
    /// @return remainder, or std::nullopt on division by zero
    [[nodiscard]] static std::optional<int64_t> mod(int64_t num, int64_t denom) noexcept {
        auto result = proven_math_mod(num, denom);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /// @brief Checked addition with overflow detection
    [[nodiscard]] static std::optional<int64_t> add_checked(int64_t a, int64_t b) noexcept {
        auto result = proven_math_add_checked(a, b);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /// @brief Checked subtraction with underflow detection
    [[nodiscard]] static std::optional<int64_t> sub_checked(int64_t a, int64_t b) noexcept {
        auto result = proven_math_sub_checked(a, b);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /// @brief Checked multiplication with overflow detection
    [[nodiscard]] static std::optional<int64_t> mul_checked(int64_t a, int64_t b) noexcept {
        auto result = proven_math_mul_checked(a, b);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /// @brief Safe absolute value (fails for INT64_MIN)
    [[nodiscard]] static std::optional<int64_t> abs_safe(int64_t n) noexcept {
        auto result = proven_math_abs_safe(n);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /// @brief Clamp value to [lo, hi] range
    [[nodiscard]] static int64_t clamp(int64_t lo, int64_t hi, int64_t val) noexcept {
        return proven_math_clamp(lo, hi, val);
    }

    /// @brief Checked exponentiation with overflow detection
    [[nodiscard]] static std::optional<int64_t> pow_checked(int64_t base, uint32_t exp) noexcept {
        auto result = proven_math_pow_checked(base, exp);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }
};

} // namespace proven
