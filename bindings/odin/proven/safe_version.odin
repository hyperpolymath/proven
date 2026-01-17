// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"
import "core:strconv"
import "core:fmt"

// Semantic version components.
SemanticVersion :: struct {
    major:      u32,
    minor:      u32,
    patch:      u32,
    prerelease: Maybe(string),
    build:      Maybe(string),
}

// Parse a semantic version string.
parse_semver :: proc(s: string, allocator := context.allocator) -> (version: SemanticVersion, ok: bool) {
    input := strings.trim_space(s)

    // Remove leading 'v' if present
    if len(input) > 0 && (input[0] == 'v' || input[0] == 'V') {
        input = input[1:]
    }

    if len(input) == 0 {
        return {}, false
    }

    // Extract build metadata
    build: Maybe(string) = nil
    plus_idx := strings.index(input, "+")
    if plus_idx >= 0 {
        build = input[plus_idx + 1:]
        input = input[:plus_idx]
    }

    // Extract prerelease
    prerelease: Maybe(string) = nil
    hyphen_idx := strings.index(input, "-")
    if hyphen_idx >= 0 {
        prerelease = input[hyphen_idx + 1:]
        input = input[:hyphen_idx]
    }

    // Parse major.minor.patch
    parts := strings.split(input, ".")
    defer delete(parts)

    if len(parts) < 1 || len(parts) > 3 {
        return {}, false
    }

    major, major_ok := strconv.parse_uint(parts[0])
    if !major_ok {
        return {}, false
    }

    minor: uint = 0
    if len(parts) >= 2 {
        minor_val, minor_ok := strconv.parse_uint(parts[1])
        if !minor_ok {
            return {}, false
        }
        minor = minor_val
    }

    patch: uint = 0
    if len(parts) >= 3 {
        patch_val, patch_ok := strconv.parse_uint(parts[2])
        if !patch_ok {
            return {}, false
        }
        patch = patch_val
    }

    return SemanticVersion{
        major = u32(major),
        minor = u32(minor),
        patch = u32(patch),
        prerelease = prerelease,
        build = build,
    }, true
}

// Format a semantic version as a string.
format_semver :: proc(v: SemanticVersion, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)

    fmt.sbprintf(&builder, "%d.%d.%d", v.major, v.minor, v.patch)

    if pre, has_pre := v.prerelease.?; has_pre {
        strings.write_string(&builder, "-")
        strings.write_string(&builder, pre)
    }

    if b, has_build := v.build.?; has_build {
        strings.write_string(&builder, "+")
        strings.write_string(&builder, b)
    }

    return strings.to_string(builder)
}

// Compare two semantic versions.
// Returns -1 if a < b, 0 if a == b, 1 if a > b.
compare_semver :: proc(a, b: SemanticVersion) -> int {
    if a.major < b.major { return -1 }
    if a.major > b.major { return 1 }
    if a.minor < b.minor { return -1 }
    if a.minor > b.minor { return 1 }
    if a.patch < b.patch { return -1 }
    if a.patch > b.patch { return 1 }

    // Prerelease versions have lower precedence than normal versions
    a_pre, a_has_pre := a.prerelease.?
    b_pre, b_has_pre := b.prerelease.?

    if !a_has_pre && b_has_pre { return 1 }
    if a_has_pre && !b_has_pre { return -1 }
    if a_has_pre && b_has_pre {
        if a_pre < b_pre { return -1 }
        if a_pre > b_pre { return 1 }
    }

    return 0
}

// Check if version a is greater than version b.
semver_gt :: proc(a, b: SemanticVersion) -> bool {
    return compare_semver(a, b) > 0
}

// Check if version a is less than version b.
semver_lt :: proc(a, b: SemanticVersion) -> bool {
    return compare_semver(a, b) < 0
}

// Check if version a is greater than or equal to version b.
semver_gte :: proc(a, b: SemanticVersion) -> bool {
    return compare_semver(a, b) >= 0
}

// Check if version a is less than or equal to version b.
semver_lte :: proc(a, b: SemanticVersion) -> bool {
    return compare_semver(a, b) <= 0
}

// Check if two versions are equal (ignoring build metadata).
semver_eq :: proc(a, b: SemanticVersion) -> bool {
    return compare_semver(a, b) == 0
}

// Increment major version.
bump_major :: proc(v: SemanticVersion) -> SemanticVersion {
    return SemanticVersion{
        major = v.major + 1,
        minor = 0,
        patch = 0,
        prerelease = nil,
        build = nil,
    }
}

// Increment minor version.
bump_minor :: proc(v: SemanticVersion) -> SemanticVersion {
    return SemanticVersion{
        major = v.major,
        minor = v.minor + 1,
        patch = 0,
        prerelease = nil,
        build = nil,
    }
}

// Increment patch version.
bump_patch :: proc(v: SemanticVersion) -> SemanticVersion {
    return SemanticVersion{
        major = v.major,
        minor = v.minor,
        patch = v.patch + 1,
        prerelease = nil,
        build = nil,
    }
}

// Check if version is a prerelease.
is_prerelease :: proc(v: SemanticVersion) -> bool {
    _, has := v.prerelease.?
    return has
}

// Check if version is stable (major >= 1 and no prerelease).
is_stable :: proc(v: SemanticVersion) -> bool {
    return v.major >= 1 && !is_prerelease(v)
}

// Check if version is compatible with a constraint (major version match).
is_compatible :: proc(v: SemanticVersion, constraint: SemanticVersion) -> bool {
    if v.major == 0 && constraint.major == 0 {
        // For 0.x.x, minor must match
        return v.minor == constraint.minor && v.patch >= constraint.patch
    }
    return v.major == constraint.major && semver_gte(v, constraint)
}

// Check if version satisfies a minimum requirement.
satisfies_min :: proc(v: SemanticVersion, min_version: SemanticVersion) -> bool {
    return semver_gte(v, min_version)
}

// Check if version is in range [min, max).
in_range :: proc(v, min_version, max_version: SemanticVersion) -> bool {
    return semver_gte(v, min_version) && semver_lt(v, max_version)
}

// Validate a version string.
is_valid_semver :: proc(s: string) -> bool {
    _, ok := parse_semver(s)
    return ok
}
