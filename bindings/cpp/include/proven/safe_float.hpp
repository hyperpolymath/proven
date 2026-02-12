// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file safe_float.hpp
 * @brief Safe floating-point operations with NaN/Infinity prevention
 *
 * Provides mathematically verified safe operations for f32/f64,
 * preventing common pitfalls that lead to NaN, Infinity, or crashes
 * in numerical computing and machine learning applications.
 *
 * @example
 * @code
 * #include <proven/safe_float.hpp>
 *
 * int main() {
 *     using namespace proven;
 *
 *     // Division by zero returns Error instead of Infinity
 *     auto result = SafeFloat::div(1.0, 0.0);
 *     if (!result) {
 *         std::cout << "Division by zero caught\n";
 *     }
 *
 *     // Safe normalization prevents divide-by-zero on zero vectors
 *     std::vector<double> v = {0.0, 0.0, 0.0};
 *     auto normalized = SafeFloat::normalize(v);
 *     if (!normalized) {
 *         std::cout << "Cannot normalize zero vector\n";
 *     }
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_SAFE_FLOAT_HPP
#define PROVEN_SAFE_FLOAT_HPP

#include "common.hpp"
#include <cmath>
#include <vector>
#include <optional>
#include <limits>
#include <numeric>

namespace proven {

/**
 * @brief Safe floating-point operations that never produce NaN or Infinity
 */
class SafeFloat {
public:
    /// Minimum positive value to consider non-zero (prevents denormal issues)
    static constexpr double EPSILON = 1e-10;

    /// Minimum for f32
    static constexpr float EPSILON_F32 = 1e-6f;

    // ========================================================================
    // Basic Arithmetic
    // ========================================================================

    /**
     * @brief Safe division with zero check
     *
     * @return Result with quotient, or Error if divisor is zero/too small
     */
    [[nodiscard]] static Result<double> div(double a, double b) {
        if (std::abs(b) < EPSILON) {
            return Result<double>::err(Error::divisionByZero());
        }

        double result = a / b;
        if (!std::isfinite(result)) {
            return Result<double>::err(Error::overflow("Division result not finite"));
        }

        return Result<double>::ok(result);
    }

    /**
     * @brief Safe division for f32
     */
    [[nodiscard]] static Result<float> divF32(float a, float b) {
        if (std::abs(b) < EPSILON_F32) {
            return Result<float>::err(Error::divisionByZero());
        }

        float result = a / b;
        if (!std::isfinite(result)) {
            return Result<float>::err(Error::overflow("Division result not finite"));
        }

        return Result<float>::ok(result);
    }

    /**
     * @brief Safe reciprocal (1/x)
     */
    [[nodiscard]] static Result<double> reciprocal(double x) {
        return div(1.0, x);
    }

    // ========================================================================
    // Mathematical Functions
    // ========================================================================

    /**
     * @brief Safe natural logarithm (ln)
     *
     * @return Result with ln(x), or Error for non-positive values
     */
    [[nodiscard]] static Result<double> ln(double x) {
        if (x <= 0.0) {
            return Result<double>::err(
                Error::validationError("ln undefined for non-positive values"));
        }
        return Result<double>::ok(std::log(x));
    }

    /**
     * @brief Safe log base 10
     */
    [[nodiscard]] static Result<double> log10(double x) {
        if (x <= 0.0) {
            return Result<double>::err(
                Error::validationError("log10 undefined for non-positive values"));
        }
        return Result<double>::ok(std::log10(x));
    }

    /**
     * @brief Safe log base 2
     */
    [[nodiscard]] static Result<double> log2(double x) {
        if (x <= 0.0) {
            return Result<double>::err(
                Error::validationError("log2 undefined for non-positive values"));
        }
        return Result<double>::ok(std::log2(x));
    }

    /**
     * @brief Safe square root
     *
     * @return Result with sqrt(x), or Error for negative values
     */
    [[nodiscard]] static Result<double> sqrt(double x) {
        if (x < 0.0) {
            return Result<double>::err(
                Error::validationError("sqrt undefined for negative values"));
        }
        return Result<double>::ok(std::sqrt(x));
    }

    /**
     * @brief Safe power operation
     *
     * @return Result with base^exp, or Error if result is NaN/Infinity
     */
    [[nodiscard]] static Result<double> pow(double base, double exp) {
        double result = std::pow(base, exp);
        if (!std::isfinite(result)) {
            return Result<double>::err(Error::overflow("Power result not finite"));
        }
        return Result<double>::ok(result);
    }

    /**
     * @brief Safe exponential (e^x)
     *
     * @return Result with e^x, or Error if overflow
     */
    [[nodiscard]] static Result<double> exp(double x) {
        double result = std::exp(x);
        if (std::isinf(result)) {
            return Result<double>::err(Error::overflow("exp overflow"));
        }
        return Result<double>::ok(result);
    }

    /**
     * @brief Safe exponential for f32
     */
    [[nodiscard]] static Result<float> expF32(float x) {
        float result = std::exp(x);
        if (std::isinf(result)) {
            return Result<float>::err(Error::overflow("exp overflow"));
        }
        return Result<float>::ok(result);
    }

    // ========================================================================
    // Vector Operations
    // ========================================================================

    /**
     * @brief Compute vector magnitude (L2 norm)
     */
    [[nodiscard]] static double magnitude(const std::vector<double>& v) {
        double sum = 0.0;
        for (double x : v) {
            sum += x * x;
        }
        return std::sqrt(sum);
    }

    /**
     * @brief Compute f32 vector magnitude
     */
    [[nodiscard]] static float magnitudeF32(const std::vector<float>& v) {
        float sum = 0.0f;
        for (float x : v) {
            sum += x * x;
        }
        return std::sqrt(sum);
    }

    /**
     * @brief Safe vector normalization (unit vector)
     *
     * @return Normalized vector, or Error if zero magnitude
     */
    [[nodiscard]] static Result<std::vector<double>> normalize(
        const std::vector<double>& v
    ) {
        double mag = magnitude(v);
        if (mag < EPSILON) {
            return Result<std::vector<double>>::err(Error::divisionByZero());
        }

        std::vector<double> result;
        result.reserve(v.size());
        for (double x : v) {
            result.push_back(x / mag);
        }

        return Result<std::vector<double>>::ok(std::move(result));
    }

    /**
     * @brief Safe f32 vector normalization
     */
    [[nodiscard]] static Result<std::vector<float>> normalizeF32(
        const std::vector<float>& v
    ) {
        float mag = magnitudeF32(v);
        if (mag < EPSILON_F32) {
            return Result<std::vector<float>>::err(Error::divisionByZero());
        }

        std::vector<float> result;
        result.reserve(v.size());
        for (float x : v) {
            result.push_back(x / mag);
        }

        return Result<std::vector<float>>::ok(std::move(result));
    }

    /**
     * @brief Dot product of two vectors
     */
    [[nodiscard]] static Result<double> dot(
        const std::vector<double>& a,
        const std::vector<double>& b
    ) {
        if (a.size() != b.size()) {
            return Result<double>::err(Error::invalidInput("Vector size mismatch"));
        }

        double sum = 0.0;
        for (size_t i = 0; i < a.size(); ++i) {
            sum += a[i] * b[i];
        }

        return Result<double>::ok(sum);
    }

    // ========================================================================
    // Statistical Functions
    // ========================================================================

    /**
     * @brief Compute mean of a vector safely
     *
     * @return Mean value, or Error for empty vectors
     */
    [[nodiscard]] static Result<double> mean(const std::vector<double>& v) {
        if (v.empty()) {
            return Result<double>::err(Error::emptyInput());
        }

        double sum = std::accumulate(v.begin(), v.end(), 0.0);
        return div(sum, static_cast<double>(v.size()));
    }

    /**
     * @brief Compute f32 mean
     */
    [[nodiscard]] static Result<float> meanF32(const std::vector<float>& v) {
        if (v.empty()) {
            return Result<float>::err(Error::emptyInput());
        }

        float sum = std::accumulate(v.begin(), v.end(), 0.0f);
        return divF32(sum, static_cast<float>(v.size()));
    }

    /**
     * @brief Compute variance safely
     */
    [[nodiscard]] static Result<double> variance(const std::vector<double>& v) {
        auto meanResult = mean(v);
        if (!meanResult) {
            return meanResult;
        }

        double m = meanResult.value();
        double sumSq = 0.0;
        for (double x : v) {
            double diff = x - m;
            sumSq += diff * diff;
        }

        return div(sumSq, static_cast<double>(v.size()));
    }

    /**
     * @brief Compute standard deviation safely
     */
    [[nodiscard]] static Result<double> stdDev(const std::vector<double>& v) {
        auto varResult = variance(v);
        if (!varResult) {
            return varResult;
        }

        return sqrt(varResult.value());
    }

    // ========================================================================
    // Validation and Clamping
    // ========================================================================

    /**
     * @brief Check if a float is finite (not NaN or Infinity)
     */
    [[nodiscard]] static bool isFinite(double x) noexcept {
        return std::isfinite(x);
    }

    /**
     * @brief Check if a float is safe for division (non-zero and finite)
     */
    [[nodiscard]] static bool isSafeDivisor(double x) noexcept {
        return std::isfinite(x) && std::abs(x) >= EPSILON;
    }

    /**
     * @brief Clamp a float to a range, handling NaN by returning min
     *
     * Unlike std::clamp which is undefined for NaN, this safely handles it.
     */
    [[nodiscard]] static double clamp(double value, double min, double max) noexcept {
        if (std::isnan(value)) {
            return min;
        }
        if (value < min) return min;
        if (value > max) return max;
        return value;
    }

    /**
     * @brief Clamp for f32
     */
    [[nodiscard]] static float clampF32(float value, float min, float max) noexcept {
        if (std::isnan(value)) {
            return min;
        }
        if (value < min) return min;
        if (value > max) return max;
        return value;
    }

    /**
     * @brief Sanitize a float by replacing NaN/Inf with default
     */
    [[nodiscard]] static double sanitize(double value, double defaultValue = 0.0) noexcept {
        if (!std::isfinite(value)) {
            return defaultValue;
        }
        return value;
    }

    // ========================================================================
    // Special Values
    // ========================================================================

    /**
     * @brief Linear interpolation between two values
     *
     * @param a Start value
     * @param b End value
     * @param t Interpolation factor (0.0 to 1.0)
     */
    [[nodiscard]] static double lerp(double a, double b, double t) noexcept {
        return a + t * (b - a);
    }

    /**
     * @brief Inverse linear interpolation
     *
     * Returns where value falls between a and b (0.0 to 1.0)
     */
    [[nodiscard]] static Result<double> inverseLerp(double a, double b, double value) {
        if (std::abs(b - a) < EPSILON) {
            return Result<double>::err(Error::divisionByZero());
        }
        return Result<double>::ok((value - a) / (b - a));
    }

    /**
     * @brief Sigmoid function: 1 / (1 + e^(-x))
     */
    [[nodiscard]] static double sigmoid(double x) noexcept {
        if (x >= 0) {
            double expNegX = std::exp(-x);
            return 1.0 / (1.0 + expNegX);
        } else {
            double expX = std::exp(x);
            return expX / (1.0 + expX);
        }
    }

    /**
     * @brief Softmax function for a vector
     */
    [[nodiscard]] static Result<std::vector<double>> softmax(
        const std::vector<double>& v
    ) {
        if (v.empty()) {
            return Result<std::vector<double>>::err(Error::emptyInput());
        }

        // Find max for numerical stability
        double maxVal = *std::max_element(v.begin(), v.end());

        std::vector<double> expVals;
        expVals.reserve(v.size());
        double sum = 0.0;

        for (double x : v) {
            double e = std::exp(x - maxVal);
            expVals.push_back(e);
            sum += e;
        }

        if (sum < EPSILON) {
            return Result<std::vector<double>>::err(Error::divisionByZero());
        }

        for (double& e : expVals) {
            e /= sum;
        }

        return Result<std::vector<double>>::ok(std::move(expVals));
    }
};

} // namespace proven

#endif // PROVEN_SAFE_FLOAT_HPP
