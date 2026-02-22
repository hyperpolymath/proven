// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// @file safe_uuid.hpp
/// @brief C++ wrapper for Proven SafeUuid FFI functions
///
/// Thin wrapper around the C UUID API declared in safe_uuid.h.
/// All UUID generation, parsing, version/variant detection, and
/// formatting are performed by the Idris2 core via the Zig FFI
/// bridge. This header only provides RAII and std::optional
/// convenience.

#pragma once

#include <array>
#include <cstdint>
#include <cstring>
#include <optional>
#include <string>
#include <string_view>

extern "C" {

// UUID C API types and functions from safe_uuid.h

enum UuidStatus : int32_t {
    UUID_OK                  =  0,
    UUID_ERR_NULL_POINTER    = -1,
    UUID_ERR_INVALID_FORMAT  = -2,
    UUID_ERR_INVALID_LENGTH  = -3,
    UUID_ERR_INVALID_HEX    = -4,
    UUID_ERR_BUFFER_TOO_SMALL = -5,
    UUID_ERR_RANDOM_FAILED   = -6
};

enum UuidVersion_C : int32_t {
    UUID_VERSION_NIL = 0,
    UUID_VERSION_1   = 1,
    UUID_VERSION_2   = 2,
    UUID_VERSION_3   = 3,
    UUID_VERSION_4   = 4,
    UUID_VERSION_5   = 5
};

enum UuidVariant_C : int32_t {
    UUID_VARIANT_NCS       = 0,
    UUID_VARIANT_RFC4122   = 1,
    UUID_VARIANT_MICROSOFT = 2,
    UUID_VARIANT_FUTURE    = 3
};

struct Uuid_C {
    uint8_t bytes[16];
};

struct UuidResult_C {
    UuidStatus status;
    Uuid_C uuid;
};

struct UuidVersionResult_C {
    UuidStatus status;
    UuidVersion_C version;
};

struct UuidVariantResult_C {
    UuidStatus status;
    UuidVariant_C variant;
};

UuidStatus uuid_v4_generate(Uuid_C* uuid);
UuidStatus uuid_v4_from_bytes(const uint8_t random_bytes[16], Uuid_C* uuid);
UuidStatus uuid_parse(const char* str, size_t len, Uuid_C* uuid);
UuidStatus uuid_parse_cstr(const char* str, Uuid_C* uuid);
UuidStatus uuid_from_bytes(const uint8_t bytes[16], Uuid_C* uuid);
UuidStatus uuid_to_string(const Uuid_C* uuid, char* buf, size_t buf_size);
UuidStatus uuid_to_urn(const Uuid_C* uuid, char* buf, size_t buf_size);
UuidStatus uuid_to_string_upper(const Uuid_C* uuid, char* buf, size_t buf_size);
UuidVersionResult_C uuid_get_version(const Uuid_C* uuid);
UuidVariantResult_C uuid_get_variant(const Uuid_C* uuid);
bool uuid_is_nil(const Uuid_C* uuid);
int uuid_compare(const Uuid_C* a, const Uuid_C* b);
bool uuid_equals(const Uuid_C* a, const Uuid_C* b);
bool uuid_is_valid(const char* str, size_t len);
bool uuid_is_valid_cstr(const char* str);

} // extern "C"

namespace proven {

/// @brief UUID version types (mirrors C enum)
enum class UuidVersion : uint8_t {
    Nil = 0, V1 = 1, V2 = 2, V3 = 3, V4 = 4, V5 = 5, Unknown = 255
};

/// @brief UUID variant types (mirrors C enum)
enum class UuidVariant : uint8_t {
    Ncs = 0, Rfc4122 = 1, Microsoft = 2, Future = 3
};

/// @brief A validated UUID (128 bits), backed by libproven FFI
class Uuid {
public:
    static constexpr size_t ByteSize = 16;
    static constexpr size_t StringSize = 36;

    /// @brief Default constructor creates nil UUID
    Uuid() noexcept { std::memset(&raw_, 0, sizeof(raw_)); }

    /// @brief Construct from C UUID struct
    explicit Uuid(const Uuid_C& c) noexcept : raw_(c) {}

    /// @brief Construct from byte array
    explicit Uuid(const std::array<uint8_t, ByteSize>& bytes) noexcept {
        std::memcpy(raw_.bytes, bytes.data(), ByteSize);
    }

    /// @brief Get the underlying bytes
    [[nodiscard]] std::array<uint8_t, ByteSize> bytes() const noexcept {
        std::array<uint8_t, ByteSize> arr{};
        std::memcpy(arr.data(), raw_.bytes, ByteSize);
        return arr;
    }

    /// @brief Get UUID version via FFI
    [[nodiscard]] UuidVersion version() const noexcept {
        auto result = uuid_get_version(&raw_);
        if (result.status != UUID_OK) return UuidVersion::Unknown;
        if (result.version >= UUID_VERSION_NIL && result.version <= UUID_VERSION_5) {
            return static_cast<UuidVersion>(result.version);
        }
        return UuidVersion::Unknown;
    }

    /// @brief Get UUID variant via FFI
    [[nodiscard]] UuidVariant variant() const noexcept {
        auto result = uuid_get_variant(&raw_);
        if (result.status != UUID_OK) return UuidVariant::Future;
        return static_cast<UuidVariant>(result.variant);
    }

    /// @brief Check if nil via FFI
    [[nodiscard]] bool is_nil() const noexcept {
        return uuid_is_nil(&raw_);
    }

    /// @brief Check if RFC 4122 compliant
    [[nodiscard]] bool is_rfc4122() const noexcept {
        return variant() == UuidVariant::Rfc4122;
    }

    /// @brief Format as canonical lowercase string via FFI
    [[nodiscard]] std::string to_string() const {
        char buf[37] = {};
        if (uuid_to_string(&raw_, buf, sizeof(buf)) == UUID_OK) {
            return std::string(buf, StringSize);
        }
        return std::string(StringSize, '0');
    }

    /// @brief Format as uppercase string via FFI
    [[nodiscard]] std::string to_string_upper() const {
        char buf[37] = {};
        if (uuid_to_string_upper(&raw_, buf, sizeof(buf)) == UUID_OK) {
            return std::string(buf, StringSize);
        }
        return std::string(StringSize, '0');
    }

    /// @brief Format as URN via FFI
    [[nodiscard]] std::string to_urn() const {
        char buf[46] = {};
        if (uuid_to_urn(&raw_, buf, sizeof(buf)) == UUID_OK) {
            return std::string(buf);
        }
        return "urn:uuid:" + to_string();
    }

    /// @brief Access underlying C struct for FFI interop
    [[nodiscard]] const Uuid_C& c_uuid() const noexcept { return raw_; }

    // Comparison operators via FFI
    [[nodiscard]] bool operator==(const Uuid& other) const noexcept {
        return uuid_equals(&raw_, &other.raw_);
    }

    [[nodiscard]] bool operator!=(const Uuid& other) const noexcept {
        return !uuid_equals(&raw_, &other.raw_);
    }

    [[nodiscard]] bool operator<(const Uuid& other) const noexcept {
        return uuid_compare(&raw_, &other.raw_) < 0;
    }

    [[nodiscard]] bool operator<=(const Uuid& other) const noexcept {
        return uuid_compare(&raw_, &other.raw_) <= 0;
    }

    [[nodiscard]] bool operator>(const Uuid& other) const noexcept {
        return uuid_compare(&raw_, &other.raw_) > 0;
    }

    [[nodiscard]] bool operator>=(const Uuid& other) const noexcept {
        return uuid_compare(&raw_, &other.raw_) >= 0;
    }

private:
    Uuid_C raw_;
};

/// @brief Safe UUID operations delegating to libproven FFI
class SafeUuid {
public:
    /// @brief Parse UUID from canonical string format via FFI
    [[nodiscard]] static std::optional<Uuid> parse(std::string_view input) noexcept {
        Uuid_C raw{};
        if (uuid_parse(input.data(), input.size(), &raw) != UUID_OK) {
            return std::nullopt;
        }
        return Uuid(raw);
    }

    /// @brief Check if string is valid UUID format via FFI
    [[nodiscard]] static bool is_valid(std::string_view input) noexcept {
        return uuid_is_valid(input.data(), input.size());
    }

    /// @brief Generate a random v4 UUID via FFI
    [[nodiscard]] static std::optional<Uuid> generate_v4() noexcept {
        Uuid_C raw{};
        if (uuid_v4_generate(&raw) != UUID_OK) {
            return std::nullopt;
        }
        return Uuid(raw);
    }

    /// @brief Create a v4 UUID from provided random bytes via FFI
    [[nodiscard]] static std::optional<Uuid> v4_from_bytes(
        const std::array<uint8_t, 16>& random_bytes
    ) noexcept {
        Uuid_C raw{};
        if (uuid_v4_from_bytes(random_bytes.data(), &raw) != UUID_OK) {
            return std::nullopt;
        }
        return Uuid(raw);
    }

    /// @brief Create UUID from raw bytes via FFI
    [[nodiscard]] static std::optional<Uuid> from_bytes(
        const std::array<uint8_t, 16>& bytes
    ) noexcept {
        Uuid_C raw{};
        if (uuid_from_bytes(bytes.data(), &raw) != UUID_OK) {
            return std::nullopt;
        }
        return Uuid(raw);
    }

    /// @brief Get the nil UUID
    [[nodiscard]] static Uuid nil() noexcept {
        return Uuid();
    }
};

} // namespace proven

// Hash support for std::unordered_map/set
namespace std {
template <>
struct hash<proven::Uuid> {
    size_t operator()(const proven::Uuid& uuid) const noexcept {
        auto b = uuid.bytes();
        // FNV-1a hash over 16 bytes
        size_t h = 14695981039346656037ULL;
        for (auto byte : b) {
            h ^= static_cast<size_t>(byte);
            h *= 1099511628211ULL;
        }
        return h;
    }
};
} // namespace std
