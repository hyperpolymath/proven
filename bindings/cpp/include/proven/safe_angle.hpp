// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_angle.hpp
/// @brief C++ wrapper for Proven SafeAngle FFI functions
///
/// Thin wrapper around proven_angle_* functions. Angle conversion
/// and normalization are performed by the Idris2 core via the Zig
/// FFI bridge.

#pragma once

#include "proven/ffi.hpp"

namespace proven {

// Forward declaration
class Radians;

/// @brief Angle in degrees, operations delegated to libproven FFI
class Degrees {
public:
    /// @brief Create an angle, normalizing to [0, 360) via FFI
    [[nodiscard]] static Degrees create(double value) noexcept {
        return Degrees(proven_angle_normalize_degrees(value));
    }

    /// @brief Create without normalization
    [[nodiscard]] static Degrees raw(double value) noexcept {
        return Degrees(value);
    }

    [[nodiscard]] double value() const noexcept { return value_; }

    /// @brief Convert to radians via FFI
    [[nodiscard]] inline Radians to_radians() const noexcept;

    /// @brief Normalize to [0, 360) via FFI
    [[nodiscard]] Degrees normalize() const noexcept {
        return Degrees(proven_angle_normalize_degrees(value_));
    }

    /// @brief Add angles with normalization via FFI
    [[nodiscard]] Degrees add(Degrees other) const noexcept {
        return create(value_ + other.value_);
    }

    /// @brief Subtract angles with normalization via FFI
    [[nodiscard]] Degrees sub(Degrees other) const noexcept {
        return create(value_ - other.value_);
    }

private:
    explicit Degrees(double v) noexcept : value_(v) {}
    double value_;
};

/// @brief Angle in radians, operations delegated to libproven FFI
class Radians {
public:
    /// @brief Create an angle, normalizing to [0, 2*PI) via FFI
    [[nodiscard]] static Radians create(double value) noexcept {
        return Radians(proven_angle_normalize_radians(value));
    }

    /// @brief Create without normalization
    [[nodiscard]] static Radians raw(double value) noexcept {
        return Radians(value);
    }

    [[nodiscard]] double value() const noexcept { return value_; }

    /// @brief Convert to degrees via FFI
    [[nodiscard]] Degrees to_degrees() const noexcept {
        return Degrees::raw(proven_angle_rad_to_deg(value_));
    }

    /// @brief Normalize to [0, 2*PI) via FFI
    [[nodiscard]] Radians normalize() const noexcept {
        return Radians(proven_angle_normalize_radians(value_));
    }

    /// @brief Add angles with normalization via FFI
    [[nodiscard]] Radians add(Radians other) const noexcept {
        return create(value_ + other.value_);
    }

    /// @brief Subtract angles with normalization via FFI
    [[nodiscard]] Radians sub(Radians other) const noexcept {
        return create(value_ - other.value_);
    }

private:
    explicit Radians(double v) noexcept : value_(v) {}
    double value_;
};

// Deferred implementation requiring Radians to be defined
inline Radians Degrees::to_radians() const noexcept {
    return Radians::raw(proven_angle_deg_to_rad(value_));
}

/// @brief Convert degrees to radians via FFI
[[nodiscard]] inline double deg_to_rad(double degrees) noexcept {
    return proven_angle_deg_to_rad(degrees);
}

/// @brief Convert radians to degrees via FFI
[[nodiscard]] inline double rad_to_deg(double radians) noexcept {
    return proven_angle_rad_to_deg(radians);
}

/// @brief Normalize degrees to [0, 360) via FFI
[[nodiscard]] inline double normalize_degrees(double degrees) noexcept {
    return proven_angle_normalize_degrees(degrees);
}

/// @brief Normalize radians to [0, 2*PI) via FFI
[[nodiscard]] inline double normalize_radians(double radians) noexcept {
    return proven_angle_normalize_radians(radians);
}

} // namespace proven
