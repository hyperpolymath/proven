// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_network.hpp
/// @brief C++ wrapper for Proven SafeNetwork FFI functions
///
/// Thin wrapper around proven_network_* functions. IPv4 parsing
/// and classification are performed by the Idris2 core via the
/// Zig FFI bridge.

#pragma once

#include "proven/ffi.hpp"
#include <array>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace proven {

/// @brief C++ wrapper for an IPv4 address returned by libproven
class IPv4Address {
public:
    IPv4Address() noexcept : octets_{{0, 0, 0, 0}} {}

    explicit IPv4Address(const ProvenIPv4Address& addr) noexcept
        : octets_{{addr.octets[0], addr.octets[1], addr.octets[2], addr.octets[3]}} {}

    IPv4Address(uint8_t a, uint8_t b, uint8_t c, uint8_t d) noexcept
        : octets_{{a, b, c, d}} {}

    [[nodiscard]] uint8_t operator[](size_t i) const { return octets_.at(i); }

    [[nodiscard]] const std::array<uint8_t, 4>& octets() const noexcept { return octets_; }

    /// @brief Check if address is private (RFC 1918) via FFI
    [[nodiscard]] bool is_private() const noexcept {
        return proven_network_ipv4_is_private(to_c());
    }

    /// @brief Check if address is loopback (127.0.0.0/8) via FFI
    [[nodiscard]] bool is_loopback() const noexcept {
        return proven_network_ipv4_is_loopback(to_c());
    }

    /// @brief Format as dotted-quad string
    [[nodiscard]] std::string to_string() const {
        return std::to_string(octets_[0]) + "." +
               std::to_string(octets_[1]) + "." +
               std::to_string(octets_[2]) + "." +
               std::to_string(octets_[3]);
    }

    /// @brief Convert to C struct for FFI calls
    [[nodiscard]] ProvenIPv4Address to_c() const noexcept {
        return {{octets_[0], octets_[1], octets_[2], octets_[3]}};
    }

private:
    std::array<uint8_t, 4> octets_;
};

/// @brief Network operations delegating to libproven FFI
class SafeNetwork {
public:
    /// @brief Parse an IPv4 address string
    [[nodiscard]] static std::optional<IPv4Address> parse_ipv4(std::string_view addr) {
        auto result = proven_network_parse_ipv4(
            reinterpret_cast<const uint8_t*>(addr.data()), addr.size());
        if (result.status != PROVEN_OK) return std::nullopt;
        return IPv4Address(result.address);
    }

    /// @brief Check if string is a valid IPv4 address
    [[nodiscard]] static bool is_valid_ipv4(std::string_view addr) noexcept {
        return parse_ipv4(addr).has_value();
    }
};

} // namespace proven
