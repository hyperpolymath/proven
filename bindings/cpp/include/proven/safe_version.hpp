// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_version.hpp
/// @brief C++ wrapper for Proven SafeVersion FFI functions
///
/// Thin RAII wrapper around proven_version_* functions. Semantic
/// version parsing and comparison are performed by the Idris2 core
/// via the Zig FFI bridge.

#pragma once

#include "proven/ffi.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace proven {

/// @brief Semantic version parsed by libproven, with RAII cleanup
class SemVer {
public:
    /// @brief Construct from FFI result (takes ownership of prerelease string)
    explicit SemVer(const ProvenSemanticVersion& v) noexcept
        : major_(v.major), minor_(v.minor), patch_(v.patch) {
        if (v.prerelease != nullptr && v.prerelease_len > 0) {
            prerelease_ = std::string(v.prerelease, v.prerelease_len);
        }
        // The caller frees the C struct; we copied what we needed.
    }

    SemVer(uint32_t maj, uint32_t min, uint32_t pat) noexcept
        : major_(maj), minor_(min), patch_(pat) {}

    [[nodiscard]] uint32_t major() const noexcept { return major_; }
    [[nodiscard]] uint32_t minor() const noexcept { return minor_; }
    [[nodiscard]] uint32_t patch() const noexcept { return patch_; }
    [[nodiscard]] const std::optional<std::string>& prerelease() const noexcept { return prerelease_; }

    [[nodiscard]] bool is_prerelease() const noexcept { return prerelease_.has_value(); }

    [[nodiscard]] bool is_stable() const noexcept {
        return major_ >= 1 && !prerelease_.has_value();
    }

    [[nodiscard]] std::string to_string() const {
        std::string s = std::to_string(major_) + "." +
                        std::to_string(minor_) + "." +
                        std::to_string(patch_);
        if (prerelease_) {
            s += "-" + *prerelease_;
        }
        return s;
    }

    /// @brief Compare two versions via FFI
    [[nodiscard]] int compare(const SemVer& other) const noexcept {
        ProvenSemanticVersion a = to_c();
        ProvenSemanticVersion b = other.to_c();
        int32_t result = proven_version_compare(a, b);
        return result;
    }

    [[nodiscard]] bool operator==(const SemVer& other) const noexcept { return compare(other) == 0; }
    [[nodiscard]] bool operator!=(const SemVer& other) const noexcept { return compare(other) != 0; }
    [[nodiscard]] bool operator<(const SemVer& other)  const noexcept { return compare(other) < 0;  }
    [[nodiscard]] bool operator<=(const SemVer& other) const noexcept { return compare(other) <= 0; }
    [[nodiscard]] bool operator>(const SemVer& other)  const noexcept { return compare(other) > 0;  }
    [[nodiscard]] bool operator>=(const SemVer& other) const noexcept { return compare(other) >= 0; }

private:
    /// @brief Convert to C struct for FFI calls (non-owning pointers)
    [[nodiscard]] ProvenSemanticVersion to_c() const noexcept {
        ProvenSemanticVersion v{};
        v.major = major_;
        v.minor = minor_;
        v.patch = patch_;
        if (prerelease_) {
            v.prerelease = const_cast<char*>(prerelease_->data());
            v.prerelease_len = prerelease_->size();
        } else {
            v.prerelease = nullptr;
            v.prerelease_len = 0;
        }
        return v;
    }

    uint32_t major_ = 0;
    uint32_t minor_ = 0;
    uint32_t patch_ = 0;
    std::optional<std::string> prerelease_;
};

/// @brief Version parsing delegating to libproven FFI
class SafeVersion {
public:
    /// @brief Parse a semantic version string via FFI
    [[nodiscard]] static std::optional<SemVer> parse(std::string_view s) {
        auto result = proven_version_parse(
            reinterpret_cast<const uint8_t*>(s.data()), s.size());
        if (result.status != PROVEN_OK) return std::nullopt;

        // Copy data from FFI result, then free FFI resources
        SemVer ver(result.version);
        proven_version_free(&result.version);
        return ver;
    }
};

} // namespace proven
