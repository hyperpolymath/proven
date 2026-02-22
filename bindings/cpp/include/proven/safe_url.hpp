// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_url.hpp
/// @brief C++ wrapper for Proven SafeUrl FFI functions
///
/// Thin RAII wrapper around proven_url_* functions. URL parsing
/// is performed by the Idris2 core via the Zig FFI bridge.
/// Component strings are freed via RAII destructor.

#pragma once

#include "proven/ffi.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace proven {

/// @brief RAII wrapper for parsed URL components from libproven
class ParsedUrl {
public:
    explicit ParsedUrl(ProvenUrlComponents c) noexcept : components_(c) {}

    ~ParsedUrl() {
        proven_url_free(&components_);
    }

    // Non-copyable, movable
    ParsedUrl(const ParsedUrl&) = delete;
    ParsedUrl& operator=(const ParsedUrl&) = delete;

    ParsedUrl(ParsedUrl&& other) noexcept : components_(other.components_) {
        other.components_ = {};
    }

    ParsedUrl& operator=(ParsedUrl&& other) noexcept {
        if (this != &other) {
            proven_url_free(&components_);
            components_ = other.components_;
            other.components_ = {};
        }
        return *this;
    }

    [[nodiscard]] std::string_view scheme() const noexcept {
        return components_.scheme ? std::string_view(components_.scheme, components_.scheme_len)
                                 : std::string_view();
    }

    [[nodiscard]] std::string_view host() const noexcept {
        return components_.host ? std::string_view(components_.host, components_.host_len)
                                : std::string_view();
    }

    [[nodiscard]] std::optional<uint16_t> port() const noexcept {
        if (components_.has_port) return components_.port;
        return std::nullopt;
    }

    [[nodiscard]] std::string_view path() const noexcept {
        return components_.path ? std::string_view(components_.path, components_.path_len)
                                : std::string_view();
    }

    [[nodiscard]] std::string_view query() const noexcept {
        return components_.query ? std::string_view(components_.query, components_.query_len)
                                 : std::string_view();
    }

    [[nodiscard]] std::string_view fragment() const noexcept {
        return components_.fragment ? std::string_view(components_.fragment, components_.fragment_len)
                                    : std::string_view();
    }

private:
    ProvenUrlComponents components_;
};

/// @brief URL parsing delegating to libproven FFI
class SafeUrl {
public:
    /// @brief Parse a URL into its components
    [[nodiscard]] static std::optional<ParsedUrl> parse(std::string_view url) {
        auto result = proven_url_parse(
            reinterpret_cast<const uint8_t*>(url.data()), url.size());
        if (result.status != PROVEN_OK) return std::nullopt;
        return ParsedUrl(result.components);
    }
};

} // namespace proven
