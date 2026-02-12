// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file common.hpp
 * @brief Common types and utilities for Proven C++ bindings
 *
 * Provides Result<T, E> type, Error types, and common utilities
 * used across all Proven modules.
 */

#ifndef PROVEN_COMMON_HPP
#define PROVEN_COMMON_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <functional>
#include <type_traits>

namespace proven {

// ============================================================================
// Error Types
// ============================================================================

/**
 * @brief Error categories for Proven operations
 */
enum class ErrorKind {
    None = 0,
    InvalidInput,
    ParseError,
    ValidationError,
    Overflow,
    Underflow,
    DivisionByZero,
    OutOfRange,
    EmptyInput,
    TooLong,
    InvalidFormat,
    NotFound,
    AlreadyExists,
    Timeout,
    NetworkError,
    IoError,
    Unknown
};

/**
 * @brief Error type with message and kind
 */
class Error {
public:
    Error() noexcept : kind_(ErrorKind::None) {}

    explicit Error(ErrorKind kind) noexcept : kind_(kind) {}

    Error(ErrorKind kind, std::string message) noexcept
        : kind_(kind), message_(std::move(message)) {}

    explicit Error(std::string message) noexcept
        : kind_(ErrorKind::Unknown), message_(std::move(message)) {}

    [[nodiscard]] ErrorKind kind() const noexcept { return kind_; }

    [[nodiscard]] const std::string& message() const noexcept { return message_; }

    [[nodiscard]] bool ok() const noexcept { return kind_ == ErrorKind::None; }

    [[nodiscard]] explicit operator bool() const noexcept { return !ok(); }

    [[nodiscard]] std::string toString() const {
        if (message_.empty()) {
            return kindToString(kind_);
        }
        return kindToString(kind_) + ": " + message_;
    }

    [[nodiscard]] static const char* kindToString(ErrorKind kind) noexcept {
        switch (kind) {
            case ErrorKind::None: return "None";
            case ErrorKind::InvalidInput: return "InvalidInput";
            case ErrorKind::ParseError: return "ParseError";
            case ErrorKind::ValidationError: return "ValidationError";
            case ErrorKind::Overflow: return "Overflow";
            case ErrorKind::Underflow: return "Underflow";
            case ErrorKind::DivisionByZero: return "DivisionByZero";
            case ErrorKind::OutOfRange: return "OutOfRange";
            case ErrorKind::EmptyInput: return "EmptyInput";
            case ErrorKind::TooLong: return "TooLong";
            case ErrorKind::InvalidFormat: return "InvalidFormat";
            case ErrorKind::NotFound: return "NotFound";
            case ErrorKind::AlreadyExists: return "AlreadyExists";
            case ErrorKind::Timeout: return "Timeout";
            case ErrorKind::NetworkError: return "NetworkError";
            case ErrorKind::IoError: return "IoError";
            case ErrorKind::Unknown: return "Unknown";
            default: return "Unknown";
        }
    }

    // Common error constructors
    [[nodiscard]] static Error invalidInput(std::string msg = "") {
        return Error(ErrorKind::InvalidInput, std::move(msg));
    }

    [[nodiscard]] static Error parseError(std::string msg = "") {
        return Error(ErrorKind::ParseError, std::move(msg));
    }

    [[nodiscard]] static Error validationError(std::string msg = "") {
        return Error(ErrorKind::ValidationError, std::move(msg));
    }

    [[nodiscard]] static Error overflow(std::string msg = "") {
        return Error(ErrorKind::Overflow, std::move(msg));
    }

    [[nodiscard]] static Error divisionByZero() {
        return Error(ErrorKind::DivisionByZero, "Division by zero");
    }

    [[nodiscard]] static Error outOfRange(std::string msg = "") {
        return Error(ErrorKind::OutOfRange, std::move(msg));
    }

    [[nodiscard]] static Error emptyInput() {
        return Error(ErrorKind::EmptyInput, "Empty input");
    }

private:
    ErrorKind kind_;
    std::string message_;
};

// ============================================================================
// Result Type (similar to Rust's Result<T, E>)
// ============================================================================

/**
 * @brief Result type that holds either a value or an error
 *
 * @tparam T The success value type
 * @tparam E The error type (defaults to Error)
 */
template <typename T, typename E = Error>
class Result {
public:
    /// Create a success result
    static Result ok(T value) {
        Result result;
        result.data_ = std::move(value);
        return result;
    }

    /// Create an error result
    static Result err(E error) {
        Result result;
        result.data_ = std::move(error);
        return result;
    }

    /// Check if result is successful
    [[nodiscard]] bool isOk() const noexcept {
        return std::holds_alternative<T>(data_);
    }

    /// Check if result is an error
    [[nodiscard]] bool isErr() const noexcept {
        return std::holds_alternative<E>(data_);
    }

    /// Get value (throws if error)
    [[nodiscard]] T& value() & {
        if (isErr()) {
            throw std::runtime_error("Attempted to access value on error result");
        }
        return std::get<T>(data_);
    }

    [[nodiscard]] const T& value() const& {
        if (isErr()) {
            throw std::runtime_error("Attempted to access value on error result");
        }
        return std::get<T>(data_);
    }

    [[nodiscard]] T&& value() && {
        if (isErr()) {
            throw std::runtime_error("Attempted to access value on error result");
        }
        return std::get<T>(std::move(data_));
    }

    /// Get error (throws if success)
    [[nodiscard]] E& error() & {
        if (isOk()) {
            throw std::runtime_error("Attempted to access error on success result");
        }
        return std::get<E>(data_);
    }

    [[nodiscard]] const E& error() const& {
        if (isOk()) {
            throw std::runtime_error("Attempted to access error on success result");
        }
        return std::get<E>(data_);
    }

    /// Get value or default
    [[nodiscard]] T valueOr(T defaultValue) const& {
        if (isOk()) {
            return std::get<T>(data_);
        }
        return defaultValue;
    }

    /// Get value as optional
    [[nodiscard]] std::optional<T> toOptional() const& {
        if (isOk()) {
            return std::get<T>(data_);
        }
        return std::nullopt;
    }

    /// Map the success value
    template <typename F>
    auto map(F&& f) const& -> Result<std::invoke_result_t<F, const T&>, E> {
        using U = std::invoke_result_t<F, const T&>;
        if (isOk()) {
            return Result<U, E>::ok(std::forward<F>(f)(std::get<T>(data_)));
        }
        return Result<U, E>::err(std::get<E>(data_));
    }

    /// Map the error value
    template <typename F>
    auto mapErr(F&& f) const& -> Result<T, std::invoke_result_t<F, const E&>> {
        using U = std::invoke_result_t<F, const E&>;
        if (isErr()) {
            return Result<T, U>::err(std::forward<F>(f)(std::get<E>(data_)));
        }
        return Result<T, U>::ok(std::get<T>(data_));
    }

    /// Bool conversion (true if ok)
    [[nodiscard]] explicit operator bool() const noexcept {
        return isOk();
    }

private:
    Result() = default;
    std::variant<T, E> data_;
};

/// Specialization for void success type
template <typename E>
class Result<void, E> {
public:
    static Result ok() {
        Result result;
        result.hasError_ = false;
        return result;
    }

    static Result err(E error) {
        Result result;
        result.error_ = std::move(error);
        result.hasError_ = true;
        return result;
    }

    [[nodiscard]] bool isOk() const noexcept { return !hasError_; }
    [[nodiscard]] bool isErr() const noexcept { return hasError_; }

    [[nodiscard]] const E& error() const& {
        if (!hasError_) {
            throw std::runtime_error("Attempted to access error on success result");
        }
        return error_;
    }

    [[nodiscard]] explicit operator bool() const noexcept { return isOk(); }

private:
    Result() = default;
    E error_;
    bool hasError_ = false;
};

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * @brief Constant-time comparison of byte arrays
 *
 * Prevents timing attacks when comparing secrets.
 */
[[nodiscard]] inline bool constantTimeEqual(
    const uint8_t* a, size_t aLen,
    const uint8_t* b, size_t bLen
) noexcept {
    if (aLen != bLen) {
        return false;
    }

    uint8_t diff = 0;
    for (size_t i = 0; i < aLen; ++i) {
        diff |= a[i] ^ b[i];
    }
    return diff == 0;
}

/**
 * @brief Constant-time comparison of strings
 */
[[nodiscard]] inline bool constantTimeEqual(
    std::string_view a,
    std::string_view b
) noexcept {
    return constantTimeEqual(
        reinterpret_cast<const uint8_t*>(a.data()), a.size(),
        reinterpret_cast<const uint8_t*>(b.data()), b.size()
    );
}

/**
 * @brief Clamp a value to a range
 */
template <typename T>
[[nodiscard]] constexpr T clamp(T value, T min, T max) noexcept {
    if (value < min) return min;
    if (value > max) return max;
    return value;
}

/**
 * @brief Check if value is in range [min, max]
 */
template <typename T>
[[nodiscard]] constexpr bool inRange(T value, T min, T max) noexcept {
    return value >= min && value <= max;
}

} // namespace proven

#endif // PROVEN_COMMON_HPP
