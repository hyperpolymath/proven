// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file proven.hpp
/// @brief Umbrella header for Proven C++ bindings
///
/// Include this single header to get all Proven functionality.
/// The C++ binding is a header-only, thin FFI wrapper. All
/// computation is performed by the Idris2 core via the Zig FFI
/// bridge (libproven). This layer provides only:
///   - RAII lifetime management (Runtime, ProvenString)
///   - std::optional error handling (never throws)
///   - C++ type safety (string_view, arrays)
///
/// @example
/// @code
/// #include <proven/proven.hpp>
/// #include <iostream>
///
/// int main() {
///     proven::Runtime runtime;  // RAII init/deinit
///
///     auto result = proven::SafeMath::div(10, 0);
///     if (!result) {
///         std::cout << "Division by zero handled\n";
///     }
///
///     auto safe = proven::SafeString::escape_html("<script>alert(1)</script>");
///     std::cout << "Safe: " << *safe << "\n";
///
///     return 0;
/// }
/// @endcode

#pragma once

// FFI declarations (all extern "C" functions)
#include "proven/ffi.hpp"

// Standard library
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace proven {

// ============================================================================
// Status enum (C++ mirror of ProvenStatus)
// ============================================================================

/// @brief Proven operation status codes
enum class Status : int32_t {
    Ok               =   0,
    NullPointer      =  -1,
    InvalidArgument  =  -2,
    Overflow         =  -3,
    Underflow        =  -4,
    DivisionByZero   =  -5,
    ParseFailure     =  -6,
    ValidationFailed =  -7,
    OutOfBounds      =  -8,
    EncodingError    =  -9,
    AllocationFailed = -10,
    NotImplemented   = -99
};

/// @brief Get human-readable status message
[[nodiscard]] inline const char* status_message(Status status) noexcept {
    switch (status) {
        case Status::Ok:               return "OK";
        case Status::NullPointer:      return "Null pointer";
        case Status::InvalidArgument:  return "Invalid argument";
        case Status::Overflow:         return "Integer overflow";
        case Status::Underflow:        return "Integer underflow";
        case Status::DivisionByZero:   return "Division by zero";
        case Status::ParseFailure:     return "Parse failure";
        case Status::ValidationFailed: return "Validation failed";
        case Status::OutOfBounds:      return "Out of bounds";
        case Status::EncodingError:    return "Encoding error";
        case Status::AllocationFailed: return "Allocation failed";
        case Status::NotImplemented:   return "Not implemented";
        default:                       return "Unknown error";
    }
}

// ============================================================================
// Runtime Management (RAII)
// ============================================================================

/// @brief RAII wrapper for Proven runtime initialization
///
/// Create one instance at program start. The runtime is automatically
/// cleaned up when the object is destroyed. Initialization status
/// can be checked via is_ok().
class Runtime {
public:
    Runtime() noexcept {
        status_ = static_cast<Status>(proven_init());
    }

    ~Runtime() {
        if (status_ == Status::Ok) {
            proven_deinit();
        }
    }

    // Non-copyable, non-movable
    Runtime(const Runtime&) = delete;
    Runtime& operator=(const Runtime&) = delete;
    Runtime(Runtime&&) = delete;
    Runtime& operator=(Runtime&&) = delete;

    /// @brief Check if initialization succeeded
    [[nodiscard]] bool is_ok() const noexcept {
        return status_ == Status::Ok;
    }

    /// @brief Get initialization status
    [[nodiscard]] Status status() const noexcept {
        return status_;
    }

    /// @brief Check if runtime is currently initialized
    [[nodiscard]] static bool is_initialized() noexcept {
        return proven_is_initialized();
    }

    /// @brief Get FFI ABI version for compatibility checking
    [[nodiscard]] static uint32_t abi_version() noexcept {
        return proven_ffi_abi_version();
    }

private:
    Status status_;
};

// ============================================================================
// RAII String Wrapper
// ============================================================================

/// @brief RAII wrapper for strings allocated by libproven
///
/// Automatically calls proven_free_string on destruction. Move-only.
class ProvenString {
public:
    ProvenString() noexcept : ptr_(nullptr), len_(0) {}

    ProvenString(ProvenStringResult r) noexcept : ptr_(r.value), len_(r.length) {}

    explicit ProvenString(char* ptr, size_t len) noexcept : ptr_(ptr), len_(len) {}

    ~ProvenString() {
        if (ptr_) proven_free_string(ptr_);
    }

    // Move-only
    ProvenString(ProvenString&& other) noexcept : ptr_(other.ptr_), len_(other.len_) {
        other.ptr_ = nullptr;
        other.len_ = 0;
    }

    ProvenString& operator=(ProvenString&& other) noexcept {
        if (this != &other) {
            if (ptr_) proven_free_string(ptr_);
            ptr_ = other.ptr_;
            len_ = other.len_;
            other.ptr_ = nullptr;
            other.len_ = 0;
        }
        return *this;
    }

    ProvenString(const ProvenString&) = delete;
    ProvenString& operator=(const ProvenString&) = delete;

    /// @brief Get as std::string (copies data)
    [[nodiscard]] std::string str() const {
        return ptr_ ? std::string(ptr_, len_) : std::string();
    }

    /// @brief Get as string_view (zero-copy, valid while ProvenString alive)
    [[nodiscard]] std::string_view view() const noexcept {
        return ptr_ ? std::string_view(ptr_, len_) : std::string_view();
    }

    /// @brief Get raw C string
    [[nodiscard]] const char* c_str() const noexcept { return ptr_; }

    /// @brief Get string length
    [[nodiscard]] size_t size() const noexcept { return len_; }

    /// @brief Check if non-null
    [[nodiscard]] explicit operator bool() const noexcept { return ptr_ != nullptr; }

private:
    char* ptr_;
    size_t len_;
};

// ============================================================================
// Version Information
// ============================================================================

/// @brief Library version queried from libproven FFI
struct LibVersion {
    uint32_t major;
    uint32_t minor;
    uint32_t patch;

    [[nodiscard]] static LibVersion get() noexcept {
        return {proven_version_major(), proven_version_minor(), proven_version_patch()};
    }

    [[nodiscard]] std::string to_string() const {
        return std::to_string(major) + "." +
               std::to_string(minor) + "." +
               std::to_string(patch);
    }
};

} // namespace proven

// Module headers
#include "proven/safe_math.hpp"
#include "proven/safe_string.hpp"
#include "proven/safe_path.hpp"
#include "proven/safe_crypto.hpp"
#include "proven/safe_email.hpp"
#include "proven/safe_network.hpp"
#include "proven/safe_url.hpp"
#include "proven/safe_uuid.hpp"
#include "proven/safe_json.hpp"
#include "proven/safe_float.hpp"
#include "proven/safe_version.hpp"
#include "proven/safe_color.hpp"
#include "proven/safe_angle.hpp"
