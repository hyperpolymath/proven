// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file safe_version.hpp
 * @brief Safe semantic versioning parsing and comparison
 *
 * Provides SemVer parsing with validation and safe comparison
 * operations following the Semantic Versioning 2.0.0 specification.
 *
 * @example
 * @code
 * #include <proven/safe_version.hpp>
 *
 * int main() {
 *     using namespace proven;
 *
 *     auto v1 = Version::parse("1.2.3");
 *     auto v2 = Version::parse("1.2.4-alpha");
 *
 *     if (v1 && v2) {
 *         if (*v1 < *v2) {
 *             std::cout << "v1 is older\n";
 *         }
 *     }
 *
 *     // Version constraints
 *     auto v = Version::parse("1.5.0").value();
 *     if (satisfies(v, "^1.0.0")) {
 *         std::cout << "Compatible with 1.x\n";
 *     }
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_SAFE_VERSION_HPP
#define PROVEN_SAFE_VERSION_HPP

#include "common.hpp"
#include <string>
#include <string_view>
#include <optional>
#include <cstdint>
#include <algorithm>

namespace proven {

/**
 * @brief Semantic version representation
 *
 * Follows SemVer 2.0.0 specification with major.minor.patch
 * plus optional prerelease and build metadata.
 */
class Version {
public:
    uint64_t major = 0;
    uint64_t minor = 0;
    uint64_t patch = 0;
    std::optional<std::string> prerelease;
    std::optional<std::string> buildMetadata;

    /**
     * @brief Create a version with major.minor.patch
     */
    Version(uint64_t maj, uint64_t min, uint64_t pat)
        : major(maj), minor(min), patch(pat) {}

    /**
     * @brief Default constructor (0.0.0)
     */
    Version() = default;

    /**
     * @brief Add prerelease identifier
     */
    Version& withPrerelease(std::string pre) {
        prerelease = std::move(pre);
        return *this;
    }

    /**
     * @brief Add build metadata
     */
    Version& withBuild(std::string build) {
        buildMetadata = std::move(build);
        return *this;
    }

    /**
     * @brief Parse a version string
     *
     * Accepts formats:
     * - 1.2.3
     * - v1.2.3 (leading v stripped)
     * - 1.2.3-alpha.1
     * - 1.2.3+build.123
     * - 1.2.3-alpha.1+build.123
     */
    [[nodiscard]] static std::optional<Version> parse(std::string_view s) noexcept {
        // Trim whitespace
        while (!s.empty() && (s.front() == ' ' || s.front() == '\t')) {
            s.remove_prefix(1);
        }
        while (!s.empty() && (s.back() == ' ' || s.back() == '\t')) {
            s.remove_suffix(1);
        }

        // Strip leading 'v' or 'V'
        if (!s.empty() && (s[0] == 'v' || s[0] == 'V')) {
            s.remove_prefix(1);
        }

        if (s.empty()) return std::nullopt;

        // Split off build metadata (+...)
        std::optional<std::string> build;
        size_t plusPos = s.find('+');
        if (plusPos != std::string_view::npos) {
            build = std::string(s.substr(plusPos + 1));
            s = s.substr(0, plusPos);
        }

        // Split off prerelease (-...)
        std::optional<std::string> pre;
        size_t dashPos = s.find('-');
        if (dashPos != std::string_view::npos) {
            pre = std::string(s.substr(dashPos + 1));
            s = s.substr(0, dashPos);
        }

        // Parse major.minor.patch
        size_t firstDot = s.find('.');
        if (firstDot == std::string_view::npos) return std::nullopt;

        size_t secondDot = s.find('.', firstDot + 1);
        if (secondDot == std::string_view::npos) return std::nullopt;

        auto majorOpt = parseUint64(s.substr(0, firstDot));
        auto minorOpt = parseUint64(s.substr(firstDot + 1, secondDot - firstDot - 1));
        auto patchOpt = parseUint64(s.substr(secondDot + 1));

        if (!majorOpt || !minorOpt || !patchOpt) return std::nullopt;

        Version v(*majorOpt, *minorOpt, *patchOpt);
        v.prerelease = std::move(pre);
        v.buildMetadata = std::move(build);

        return v;
    }

    /**
     * @brief Format as string
     */
    [[nodiscard]] std::string toString() const {
        std::string s = std::to_string(major) + "." +
                        std::to_string(minor) + "." +
                        std::to_string(patch);

        if (prerelease) {
            s += "-" + *prerelease;
        }
        if (buildMetadata) {
            s += "+" + *buildMetadata;
        }

        return s;
    }

    /**
     * @brief Check if this is a prerelease version
     */
    [[nodiscard]] bool isPrerelease() const noexcept {
        return prerelease.has_value();
    }

    /**
     * @brief Check if this is a stable release (>= 1.0.0 and no prerelease)
     */
    [[nodiscard]] bool isStable() const noexcept {
        return major >= 1 && !prerelease.has_value();
    }

    /**
     * @brief Increment major version (resets minor and patch)
     */
    [[nodiscard]] Version bumpMajor() const {
        return Version(major + 1, 0, 0);
    }

    /**
     * @brief Increment minor version (resets patch)
     */
    [[nodiscard]] Version bumpMinor() const {
        return Version(major, minor + 1, 0);
    }

    /**
     * @brief Increment patch version
     */
    [[nodiscard]] Version bumpPatch() const {
        return Version(major, minor, patch + 1);
    }

    // Comparison operators (build metadata is ignored per SemVer spec)

    [[nodiscard]] bool operator==(const Version& other) const noexcept {
        return major == other.major &&
               minor == other.minor &&
               patch == other.patch &&
               prerelease == other.prerelease;
    }

    [[nodiscard]] bool operator!=(const Version& other) const noexcept {
        return !(*this == other);
    }

    [[nodiscard]] bool operator<(const Version& other) const noexcept {
        if (major != other.major) return major < other.major;
        if (minor != other.minor) return minor < other.minor;
        if (patch != other.patch) return patch < other.patch;

        // Prerelease comparison
        // No prerelease > prerelease
        if (!prerelease && other.prerelease) return false;
        if (prerelease && !other.prerelease) return true;
        if (prerelease && other.prerelease) {
            return *prerelease < *other.prerelease;
        }

        return false;
    }

    [[nodiscard]] bool operator<=(const Version& other) const noexcept {
        return *this == other || *this < other;
    }

    [[nodiscard]] bool operator>(const Version& other) const noexcept {
        return other < *this;
    }

    [[nodiscard]] bool operator>=(const Version& other) const noexcept {
        return *this == other || *this > other;
    }

private:
    [[nodiscard]] static std::optional<uint64_t> parseUint64(std::string_view s) noexcept {
        if (s.empty()) return std::nullopt;

        // Reject leading zeros (except "0")
        if (s.size() > 1 && s[0] == '0') return std::nullopt;

        uint64_t result = 0;
        for (char c : s) {
            if (c < '0' || c > '9') return std::nullopt;
            uint64_t prev = result;
            result = result * 10 + (c - '0');
            if (result < prev) return std::nullopt;  // Overflow
        }

        return result;
    }
};

/**
 * @brief Check if version satisfies a constraint
 *
 * Supported constraints:
 * - >=1.0.0 (greater than or equal)
 * - <=1.0.0 (less than or equal)
 * - >1.0.0 (greater than)
 * - <1.0.0 (less than)
 * - =1.0.0 (exact match)
 * - ^1.0.0 (caret: compatible, same major if major > 0)
 * - ~1.2.0 (tilde: same major.minor)
 * - 1.0.0 (exact match)
 */
[[nodiscard]] inline Result<bool> satisfies(
    const Version& version,
    std::string_view constraint
) {
    // Trim whitespace
    while (!constraint.empty() && constraint.front() == ' ') {
        constraint.remove_prefix(1);
    }

    if (constraint.starts_with(">=")) {
        auto target = Version::parse(constraint.substr(2));
        if (!target) {
            return Result<bool>::err(Error::parseError("Invalid version constraint"));
        }
        return Result<bool>::ok(version >= *target);
    }

    if (constraint.starts_with("<=")) {
        auto target = Version::parse(constraint.substr(2));
        if (!target) {
            return Result<bool>::err(Error::parseError("Invalid version constraint"));
        }
        return Result<bool>::ok(version <= *target);
    }

    if (constraint.starts_with(">")) {
        auto target = Version::parse(constraint.substr(1));
        if (!target) {
            return Result<bool>::err(Error::parseError("Invalid version constraint"));
        }
        return Result<bool>::ok(version > *target);
    }

    if (constraint.starts_with("<")) {
        auto target = Version::parse(constraint.substr(1));
        if (!target) {
            return Result<bool>::err(Error::parseError("Invalid version constraint"));
        }
        return Result<bool>::ok(version < *target);
    }

    if (constraint.starts_with("=")) {
        auto target = Version::parse(constraint.substr(1));
        if (!target) {
            return Result<bool>::err(Error::parseError("Invalid version constraint"));
        }
        return Result<bool>::ok(version == *target);
    }

    if (constraint.starts_with("^")) {
        // Caret: compatible with version (same major, if major > 0)
        auto target = Version::parse(constraint.substr(1));
        if (!target) {
            return Result<bool>::err(Error::parseError("Invalid version constraint"));
        }

        if (target->major == 0) {
            // For 0.x.y, compatible means same minor
            return Result<bool>::ok(
                version.major == 0 &&
                version.minor == target->minor &&
                version >= *target
            );
        }

        return Result<bool>::ok(
            version.major == target->major &&
            version >= *target
        );
    }

    if (constraint.starts_with("~")) {
        // Tilde: same major.minor
        auto target = Version::parse(constraint.substr(1));
        if (!target) {
            return Result<bool>::err(Error::parseError("Invalid version constraint"));
        }

        return Result<bool>::ok(
            version.major == target->major &&
            version.minor == target->minor &&
            version >= *target
        );
    }

    // Exact match
    auto target = Version::parse(constraint);
    if (!target) {
        return Result<bool>::err(Error::parseError("Invalid version constraint"));
    }
    return Result<bool>::ok(version == *target);
}

/**
 * @brief Parse a version range (e.g., "1.0.0 - 2.0.0")
 *
 * @return Pair of (min, max) versions
 */
[[nodiscard]] inline std::optional<std::pair<Version, Version>> parseRange(
    std::string_view range
) {
    size_t dashPos = range.find(" - ");
    if (dashPos == std::string_view::npos) {
        return std::nullopt;
    }

    auto min = Version::parse(range.substr(0, dashPos));
    auto max = Version::parse(range.substr(dashPos + 3));

    if (!min || !max) {
        return std::nullopt;
    }

    return std::make_pair(*min, *max);
}

/**
 * @brief Check if version is within a range
 */
[[nodiscard]] inline bool inRange(
    const Version& version,
    const Version& min,
    const Version& max
) {
    return version >= min && version <= max;
}

} // namespace proven

#endif // PROVEN_SAFE_VERSION_HPP
