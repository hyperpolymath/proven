// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file proven.hpp
 * @brief C++ wrapper for Proven FFI (Zig implementation)
 *
 * This header provides modern C++ wrappers around the Proven C API,
 * offering RAII memory management, std::optional returns, and
 * optional exception-based error handling.
 *
 * @example
 * @code
 * #include <proven/proven.hpp>
 * #include <iostream>
 *
 * int main() {
 *     proven::Runtime runtime;  // RAII init/deinit
 *
 *     // Safe division with optional
 *     auto result = proven::SafeMath::div(10, 0);
 *     if (!result) {
 *         std::cout << "Division by zero handled\n";
 *     }
 *
 *     // HTML escaping
 *     auto safe = proven::SafeString::escape_html("<script>alert(1)</script>");
 *     std::cout << "Safe: " << *safe << "\n";
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_HPP
#define PROVEN_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <stdexcept>
#include <array>
#include <memory>

// Include the C header
extern "C" {
#include "../../c/include/proven.h"
}

namespace proven {

// ============================================================================
// Error Handling
// ============================================================================

/**
 * @brief Exception thrown when Proven operations fail (if exceptions enabled)
 */
class Error : public std::runtime_error {
public:
    explicit Error(ProvenStatus status, const std::string& msg = "")
        : std::runtime_error(msg.empty() ? status_message(status) : msg)
        , status_(status) {}

    [[nodiscard]] ProvenStatus status() const noexcept { return status_; }

    [[nodiscard]] static std::string status_message(ProvenStatus status) {
        switch (status) {
            case PROVEN_OK: return "OK";
            case PROVEN_ERR_NULL_POINTER: return "Null pointer";
            case PROVEN_ERR_INVALID_ARGUMENT: return "Invalid argument";
            case PROVEN_ERR_OVERFLOW: return "Integer overflow";
            case PROVEN_ERR_UNDERFLOW: return "Integer underflow";
            case PROVEN_ERR_DIVISION_BY_ZERO: return "Division by zero";
            case PROVEN_ERR_PARSE_FAILURE: return "Parse failure";
            case PROVEN_ERR_VALIDATION_FAILED: return "Validation failed";
            case PROVEN_ERR_OUT_OF_BOUNDS: return "Out of bounds";
            case PROVEN_ERR_ENCODING_ERROR: return "Encoding error";
            case PROVEN_ERR_ALLOCATION_FAILED: return "Allocation failed";
            default: return "Unknown error";
        }
    }

private:
    ProvenStatus status_;
};

// ============================================================================
// Runtime Management (RAII)
// ============================================================================

/**
 * @brief RAII wrapper for Proven runtime initialization
 *
 * Create one instance of this at program start. Runtime is automatically
 * cleaned up when the object is destroyed.
 */
class Runtime {
public:
    Runtime() {
        auto status = static_cast<ProvenStatus>(proven_init());
        if (status != PROVEN_OK) {
            throw Error(status, "Failed to initialize Proven runtime");
        }
    }

    ~Runtime() {
        proven_deinit();
    }

    // Non-copyable, non-movable
    Runtime(const Runtime&) = delete;
    Runtime& operator=(const Runtime&) = delete;
    Runtime(Runtime&&) = delete;
    Runtime& operator=(Runtime&&) = delete;

    [[nodiscard]] static bool is_initialized() noexcept {
        return proven_is_initialized();
    }

    [[nodiscard]] static uint32_t abi_version() noexcept {
        return proven_ffi_abi_version();
    }
};

// ============================================================================
// Smart Pointer for Proven Strings
// ============================================================================

/**
 * @brief RAII wrapper for strings allocated by Proven
 */
class ProvenString {
public:
    ProvenString() noexcept : ptr_(nullptr) {}

    explicit ProvenString(char* ptr) noexcept : ptr_(ptr) {}

    ~ProvenString() {
        if (ptr_) {
            proven_free_string(ptr_);
        }
    }

    // Move-only
    ProvenString(ProvenString&& other) noexcept : ptr_(other.ptr_) {
        other.ptr_ = nullptr;
    }

    ProvenString& operator=(ProvenString&& other) noexcept {
        if (this != &other) {
            if (ptr_) proven_free_string(ptr_);
            ptr_ = other.ptr_;
            other.ptr_ = nullptr;
        }
        return *this;
    }

    ProvenString(const ProvenString&) = delete;
    ProvenString& operator=(const ProvenString&) = delete;

    [[nodiscard]] const char* c_str() const noexcept { return ptr_; }
    [[nodiscard]] std::string_view view() const noexcept {
        return ptr_ ? std::string_view(ptr_) : std::string_view();
    }
    [[nodiscard]] std::string str() const {
        return ptr_ ? std::string(ptr_) : std::string();
    }

    [[nodiscard]] explicit operator bool() const noexcept { return ptr_ != nullptr; }

private:
    char* ptr_;
};

// ============================================================================
// SafeMath
// ============================================================================

/**
 * @brief Safe arithmetic operations
 */
class SafeMath {
public:
    /**
     * @brief Safe division returning optional
     */
    [[nodiscard]] static std::optional<int64_t> div(int64_t num, int64_t denom) noexcept {
        auto result = proven_math_div(num, denom);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /**
     * @brief Safe division with default value
     */
    [[nodiscard]] static int64_t div_or(int64_t def, int64_t num, int64_t denom) noexcept {
        return div(num, denom).value_or(def);
    }

    /**
     * @brief Safe modulo returning optional
     */
    [[nodiscard]] static std::optional<int64_t> mod(int64_t num, int64_t denom) noexcept {
        auto result = proven_math_mod(num, denom);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /**
     * @brief Checked addition
     */
    [[nodiscard]] static std::optional<int64_t> add_checked(int64_t a, int64_t b) noexcept {
        auto result = proven_math_add_checked(a, b);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /**
     * @brief Checked subtraction
     */
    [[nodiscard]] static std::optional<int64_t> sub_checked(int64_t a, int64_t b) noexcept {
        auto result = proven_math_sub_checked(a, b);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /**
     * @brief Checked multiplication
     */
    [[nodiscard]] static std::optional<int64_t> mul_checked(int64_t a, int64_t b) noexcept {
        auto result = proven_math_mul_checked(a, b);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /**
     * @brief Safe absolute value
     */
    [[nodiscard]] static std::optional<int64_t> abs_safe(int64_t n) noexcept {
        auto result = proven_math_abs_safe(n);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }

    /**
     * @brief Clamp value to range
     */
    [[nodiscard]] static int64_t clamp(int64_t lo, int64_t hi, int64_t val) noexcept {
        return proven_math_clamp(lo, hi, val);
    }

    /**
     * @brief Checked exponentiation
     */
    [[nodiscard]] static std::optional<int64_t> pow_checked(int64_t base, uint32_t exp) noexcept {
        auto result = proven_math_pow_checked(base, exp);
        if (result.status != PROVEN_OK) return std::nullopt;
        return result.value;
    }
};

// ============================================================================
// SafeString
// ============================================================================

/**
 * @brief Safe string operations
 */
class SafeString {
public:
    /**
     * @brief Check if data is valid UTF-8
     */
    [[nodiscard]] static bool is_valid_utf8(std::string_view data) noexcept {
        auto result = proven_string_is_valid_utf8(
            reinterpret_cast<const uint8_t*>(data.data()), data.size()
        );
        return result.status == PROVEN_OK && result.value;
    }

    /**
     * @brief Escape string for SQL
     */
    [[nodiscard]] static std::optional<std::string> escape_sql(std::string_view input) {
        auto result = proven_string_escape_sql(
            reinterpret_cast<const uint8_t*>(input.data()), input.size()
        );
        if (result.status != PROVEN_OK || result.value == nullptr) {
            return std::nullopt;
        }
        std::string str(result.value, result.length);
        proven_free_string(result.value);
        return str;
    }

    /**
     * @brief Escape string for HTML
     */
    [[nodiscard]] static std::optional<std::string> escape_html(std::string_view input) {
        auto result = proven_string_escape_html(
            reinterpret_cast<const uint8_t*>(input.data()), input.size()
        );
        if (result.status != PROVEN_OK || result.value == nullptr) {
            return std::nullopt;
        }
        std::string str(result.value, result.length);
        proven_free_string(result.value);
        return str;
    }

    /**
     * @brief Escape string for JavaScript
     */
    [[nodiscard]] static std::optional<std::string> escape_js(std::string_view input) {
        auto result = proven_string_escape_js(
            reinterpret_cast<const uint8_t*>(input.data()), input.size()
        );
        if (result.status != PROVEN_OK || result.value == nullptr) {
            return std::nullopt;
        }
        std::string str(result.value, result.length);
        proven_free_string(result.value);
        return str;
    }
};

// ============================================================================
// SafePath
// ============================================================================

/**
 * @brief Safe filesystem path operations
 */
class SafePath {
public:
    /**
     * @brief Check if path contains traversal sequences
     */
    [[nodiscard]] static bool has_traversal(std::string_view path) noexcept {
        auto result = proven_path_has_traversal(
            reinterpret_cast<const uint8_t*>(path.data()), path.size()
        );
        return result.status == PROVEN_OK && result.value;
    }

    /**
     * @brief Check if path is safe (no traversal)
     */
    [[nodiscard]] static bool is_safe(std::string_view path) noexcept {
        return !has_traversal(path);
    }

    /**
     * @brief Sanitize a filename
     */
    [[nodiscard]] static std::optional<std::string> sanitize_filename(std::string_view name) {
        auto result = proven_path_sanitize_filename(
            reinterpret_cast<const uint8_t*>(name.data()), name.size()
        );
        if (result.status != PROVEN_OK || result.value == nullptr) {
            return std::nullopt;
        }
        std::string str(result.value, result.length);
        proven_free_string(result.value);
        return str;
    }
};

// ============================================================================
// SafeCrypto
// ============================================================================

/**
 * @brief Cryptographic safety operations
 */
class SafeCrypto {
public:
    /**
     * @brief Constant-time comparison (timing-safe)
     */
    [[nodiscard]] static bool constant_time_compare(
        std::string_view a, std::string_view b
    ) noexcept {
        auto result = proven_crypto_constant_time_eq(
            reinterpret_cast<const uint8_t*>(a.data()), a.size(),
            reinterpret_cast<const uint8_t*>(b.data()), b.size()
        );
        return result.status == PROVEN_OK && result.value;
    }

    /**
     * @brief Generate random bytes
     */
    [[nodiscard]] static std::optional<std::string> random_bytes(size_t count) {
        std::string result(count, '\0');
        auto status = proven_crypto_random_bytes(
            reinterpret_cast<uint8_t*>(result.data()), count
        );
        if (status != PROVEN_OK) return std::nullopt;
        return result;
    }

    /**
     * @brief Generate random bytes into existing buffer
     */
    static bool random_bytes_into(uint8_t* buffer, size_t count) noexcept {
        return proven_crypto_random_bytes(buffer, count) == PROVEN_OK;
    }
};

// ============================================================================
// SafeEmail
// ============================================================================

/**
 * @brief Email validation
 */
class SafeEmail {
public:
    /**
     * @brief Check if email is valid
     */
    [[nodiscard]] static bool is_valid(std::string_view email) noexcept {
        auto result = proven_email_is_valid(
            reinterpret_cast<const uint8_t*>(email.data()), email.size()
        );
        return result.status == PROVEN_OK && result.value;
    }
};

// ============================================================================
// SafeNetwork
// ============================================================================

/**
 * @brief IPv4 address wrapper
 */
class IPv4Address {
public:
    IPv4Address() noexcept : octets_{{0, 0, 0, 0}} {}

    explicit IPv4Address(const ProvenIPv4Address& addr) noexcept
        : octets_{{addr.octets[0], addr.octets[1], addr.octets[2], addr.octets[3]}} {}

    IPv4Address(uint8_t a, uint8_t b, uint8_t c, uint8_t d) noexcept
        : octets_{{a, b, c, d}} {}

    [[nodiscard]] uint8_t operator[](size_t i) const { return octets_.at(i); }

    [[nodiscard]] const std::array<uint8_t, 4>& octets() const noexcept { return octets_; }

    [[nodiscard]] ProvenIPv4Address to_c() const noexcept {
        return {{octets_[0], octets_[1], octets_[2], octets_[3]}};
    }

    [[nodiscard]] std::string to_string() const {
        return std::to_string(octets_[0]) + "." +
               std::to_string(octets_[1]) + "." +
               std::to_string(octets_[2]) + "." +
               std::to_string(octets_[3]);
    }

    [[nodiscard]] bool is_private() const noexcept {
        return proven_network_ipv4_is_private(to_c());
    }

    [[nodiscard]] bool is_loopback() const noexcept {
        return proven_network_ipv4_is_loopback(to_c());
    }

private:
    std::array<uint8_t, 4> octets_;
};

/**
 * @brief Network operations
 */
class SafeNetwork {
public:
    /**
     * @brief Parse IPv4 address
     */
    [[nodiscard]] static std::optional<IPv4Address> parse_ipv4(std::string_view addr) {
        auto result = proven_network_parse_ipv4(
            reinterpret_cast<const uint8_t*>(addr.data()), addr.size()
        );
        if (result.status != PROVEN_OK) return std::nullopt;
        return IPv4Address(result.address);
    }

    /**
     * @brief Check if string is valid IPv4
     */
    [[nodiscard]] static bool is_valid_ipv4(std::string_view addr) noexcept {
        return parse_ipv4(addr).has_value();
    }
};

// ============================================================================
// Version Information
// ============================================================================

/**
 * @brief Get library version
 */
struct Version {
    uint32_t major;
    uint32_t minor;
    uint32_t patch;

    [[nodiscard]] static Version get() noexcept {
        return {
            proven_version_major(),
            proven_version_minor(),
            proven_version_patch()
        };
    }

    [[nodiscard]] std::string to_string() const {
        return std::to_string(major) + "." +
               std::to_string(minor) + "." +
               std::to_string(patch);
    }
};

} // namespace proven

#endif // PROVEN_HPP
