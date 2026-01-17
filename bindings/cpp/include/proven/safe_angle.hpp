// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file safe_angle.hpp
 * @brief Safe angle operations with degree/radian conversions
 *
 * Provides angle types with automatic normalization and
 * safe trigonometric operations.
 *
 * @example
 * @code
 * #include <proven/safe_angle.hpp>
 *
 * int main() {
 *     using namespace proven;
 *
 *     // Create angles
 *     auto deg = Degrees::create(90.0);
 *     auto rad = deg.toRadians();
 *
 *     // Trigonometric operations
 *     double s = deg.sin();  // 1.0
 *     double c = deg.cos();  // 0.0
 *
 *     // Angle arithmetic with normalization
 *     auto result = Degrees::create(350.0).add(Degrees::create(30.0));
 *     // result.value() == 20.0
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_SAFE_ANGLE_HPP
#define PROVEN_SAFE_ANGLE_HPP

#include "common.hpp"
#include <cmath>
#include <numbers>

namespace proven {

// Forward declarations
class Radians;

/**
 * @brief Normalize degrees to [0, 360)
 */
[[nodiscard]] inline double normalizeDegrees(double deg) noexcept {
    double result = std::fmod(deg, 360.0);
    if (result < 0.0) {
        result += 360.0;
    }
    return result;
}

/**
 * @brief Normalize radians to [0, 2*PI)
 */
[[nodiscard]] inline double normalizeRadians(double rad) noexcept {
    constexpr double TWO_PI = 2.0 * std::numbers::pi;
    double result = std::fmod(rad, TWO_PI);
    if (result < 0.0) {
        result += TWO_PI;
    }
    return result;
}

/**
 * @brief Convert degrees to radians
 */
[[nodiscard]] inline constexpr double degToRad(double deg) noexcept {
    return deg * std::numbers::pi / 180.0;
}

/**
 * @brief Convert radians to degrees
 */
[[nodiscard]] inline constexpr double radToDeg(double rad) noexcept {
    return rad * 180.0 / std::numbers::pi;
}

/**
 * @brief Angle in degrees with normalization
 */
class Degrees {
public:
    /**
     * @brief Create a new angle, normalizing to [0, 360)
     */
    [[nodiscard]] static Degrees create(double value) noexcept {
        return Degrees(normalizeDegrees(value));
    }

    /**
     * @brief Create without normalization
     */
    [[nodiscard]] static Degrees raw(double value) noexcept {
        return Degrees(value);
    }

    /**
     * @brief Get the value
     */
    [[nodiscard]] double value() const noexcept {
        return value_;
    }

    /**
     * @brief Convert to radians
     */
    [[nodiscard]] Radians toRadians() const noexcept;

    /**
     * @brief Normalize to [0, 360)
     */
    [[nodiscard]] Degrees normalize() const noexcept {
        return create(value_);
    }

    /**
     * @brief Normalize to [-180, 180)
     */
    [[nodiscard]] Degrees normalizeSigned() const noexcept {
        double v = normalizeDegrees(value_);
        if (v >= 180.0) {
            v -= 360.0;
        }
        return raw(v);
    }

    /**
     * @brief Add angles (with normalization)
     */
    [[nodiscard]] Degrees add(Degrees other) const noexcept {
        return create(value_ + other.value_);
    }

    /**
     * @brief Subtract angles (with normalization)
     */
    [[nodiscard]] Degrees sub(Degrees other) const noexcept {
        return create(value_ - other.value_);
    }

    /**
     * @brief Sine of angle
     */
    [[nodiscard]] double sin() const noexcept {
        return std::sin(degToRad(value_));
    }

    /**
     * @brief Cosine of angle
     */
    [[nodiscard]] double cos() const noexcept {
        return std::cos(degToRad(value_));
    }

    /**
     * @brief Tangent of angle
     */
    [[nodiscard]] double tan() const noexcept {
        return std::tan(degToRad(value_));
    }

    /**
     * @brief Equality comparison (within epsilon)
     */
    [[nodiscard]] bool operator==(const Degrees& other) const noexcept {
        return std::abs(value_ - other.value_) < 1e-10;
    }

    [[nodiscard]] bool operator!=(const Degrees& other) const noexcept {
        return !(*this == other);
    }

private:
    explicit Degrees(double v) noexcept : value_(v) {}
    double value_;
};

/**
 * @brief Angle in radians
 */
class Radians {
public:
    /**
     * @brief Create a new angle, normalizing to [0, 2*PI)
     */
    [[nodiscard]] static Radians create(double value) noexcept {
        return Radians(normalizeRadians(value));
    }

    /**
     * @brief Create without normalization
     */
    [[nodiscard]] static Radians raw(double value) noexcept {
        return Radians(value);
    }

    /**
     * @brief Get the value
     */
    [[nodiscard]] double value() const noexcept {
        return value_;
    }

    /**
     * @brief Convert to degrees
     */
    [[nodiscard]] Degrees toDegrees() const noexcept {
        return Degrees::raw(radToDeg(value_));
    }

    /**
     * @brief Normalize to [0, 2*PI)
     */
    [[nodiscard]] Radians normalize() const noexcept {
        return create(value_);
    }

    /**
     * @brief Add angles (with normalization)
     */
    [[nodiscard]] Radians add(Radians other) const noexcept {
        return create(value_ + other.value_);
    }

    /**
     * @brief Subtract angles (with normalization)
     */
    [[nodiscard]] Radians sub(Radians other) const noexcept {
        return create(value_ - other.value_);
    }

    /**
     * @brief Sine of angle
     */
    [[nodiscard]] double sin() const noexcept {
        return std::sin(value_);
    }

    /**
     * @brief Cosine of angle
     */
    [[nodiscard]] double cos() const noexcept {
        return std::cos(value_);
    }

    /**
     * @brief Tangent of angle
     */
    [[nodiscard]] double tan() const noexcept {
        return std::tan(value_);
    }

    /**
     * @brief Equality comparison (within epsilon)
     */
    [[nodiscard]] bool operator==(const Radians& other) const noexcept {
        return std::abs(value_ - other.value_) < 1e-10;
    }

    [[nodiscard]] bool operator!=(const Radians& other) const noexcept {
        return !(*this == other);
    }

    // Common angle constants
    [[nodiscard]] static Radians PI() noexcept { return raw(std::numbers::pi); }
    [[nodiscard]] static Radians TWO_PI() noexcept { return raw(2.0 * std::numbers::pi); }
    [[nodiscard]] static Radians HALF_PI() noexcept { return raw(std::numbers::pi / 2.0); }

private:
    explicit Radians(double v) noexcept : value_(v) {}
    double value_;
};

// Implementation of Degrees::toRadians (needs Radians to be defined)
inline Radians Degrees::toRadians() const noexcept {
    return Radians::raw(degToRad(value_));
}

/**
 * @brief Calculate angle difference (shortest path)
 */
[[nodiscard]] inline double angleDiffDegrees(double a, double b) noexcept {
    double diff = normalizeDegrees(b - a);
    if (diff > 180.0) {
        diff -= 360.0;
    }
    return diff;
}

/**
 * @brief Calculate angle difference in radians (shortest path)
 */
[[nodiscard]] inline double angleDiffRadians(double a, double b) noexcept {
    constexpr double PI = std::numbers::pi;
    double diff = normalizeRadians(b - a);
    if (diff > PI) {
        diff -= 2.0 * PI;
    }
    return diff;
}

/**
 * @brief Linear interpolation between angles (degrees)
 */
[[nodiscard]] inline double lerpAngleDegrees(double a, double b, double t) noexcept {
    double diff = angleDiffDegrees(a, b);
    return normalizeDegrees(a + diff * t);
}

/**
 * @brief Linear interpolation between angles (radians)
 */
[[nodiscard]] inline double lerpAngleRadians(double a, double b, double t) noexcept {
    double diff = angleDiffRadians(a, b);
    return normalizeRadians(a + diff * t);
}

/**
 * @brief Safe arctangent with quadrant correction (atan2)
 *
 * @return Angle in radians in range [-PI, PI]
 */
[[nodiscard]] inline double safeAtan2(double y, double x) noexcept {
    return std::atan2(y, x);
}

/**
 * @brief Safe arcsine with clamped input
 *
 * @return Angle in radians in range [-PI/2, PI/2]
 */
[[nodiscard]] inline double safeAsin(double x) noexcept {
    x = clamp(x, -1.0, 1.0);
    return std::asin(x);
}

/**
 * @brief Safe arccosine with clamped input
 *
 * @return Angle in radians in range [0, PI]
 */
[[nodiscard]] inline double safeAcos(double x) noexcept {
    x = clamp(x, -1.0, 1.0);
    return std::acos(x);
}

} // namespace proven

#endif // PROVEN_SAFE_ANGLE_HPP
