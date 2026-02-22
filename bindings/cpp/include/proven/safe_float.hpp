// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_float.hpp
/// @brief C++ wrapper for Proven SafeFloat FFI functions
///
/// Thin wrapper around proven_float_* functions. All floating-point
/// safety checks (division-by-zero, NaN prevention, domain validation)
/// are performed by the Idris2 core via the Zig FFI bridge.

#pragma once

#include "proven/ffi.hpp"
#include <optional>

namespace proven {

/// @brief Safe floating-point operations delegating to libproven FFI
class SafeFloat {
public:
    /// @brief Safe floating-point division via FFI
    /// @return quotient, or std::nullopt on division by zero / NaN / overflow
    [[nodiscard]] static std::optional<double> div(double a, double b) noexcept {
        auto result = proven_float_div(a, b);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /// @brief Safe square root via FFI
    /// @return sqrt(x), or std::nullopt for negative / NaN input
    [[nodiscard]] static std::optional<double> sqrt(double x) noexcept {
        auto result = proven_float_sqrt(x);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /// @brief Safe natural logarithm via FFI
    /// @return ln(x), or std::nullopt for non-positive / NaN input
    [[nodiscard]] static std::optional<double> ln(double x) noexcept {
        auto result = proven_float_ln(x);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /// @brief Check if float is finite (not NaN or Inf) via FFI
    [[nodiscard]] static bool is_finite(double x) noexcept {
        return proven_float_is_finite(x);
    }

    /// @brief Check if float is NaN via FFI
    [[nodiscard]] static bool is_nan(double x) noexcept {
        return proven_float_is_nan(x);
    }
};

} // namespace proven
