#!/bin/sh
# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_version.sh - Safe semantic version parsing for POSIX shells
# Source this file: . /path/to/safe_version.sh

# Parsed version components
VERSION_MAJOR=""
VERSION_MINOR=""
VERSION_PATCH=""
VERSION_PRERELEASE=""
VERSION_BUILD=""

# Parse a semantic version string
# Usage: version_parse "1.2.3-beta.1+build.456"
# Sets: VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH, VERSION_PRERELEASE, VERSION_BUILD
# Returns: 0 on success, 1 on error
version_parse() {
    input="$1"
    VERSION_MAJOR=""
    VERSION_MINOR=""
    VERSION_PATCH=""
    VERSION_PRERELEASE=""
    VERSION_BUILD=""
    PROVEN_ERROR=""

    # Strip leading 'v' if present
    case "$input" in
        v*|V*)
            input="${input#?}"
            ;;
    esac

    # Check for empty
    if [ -z "$input" ]; then
        PROVEN_ERROR="empty_version"
        return 1
    fi

    # Extract build metadata first (+build.xxx)
    case "$input" in
        *+*)
            VERSION_BUILD="${input#*+}"
            input="${input%%+*}"
            ;;
    esac

    # Extract prerelease (-alpha.1)
    case "$input" in
        *-*)
            VERSION_PRERELEASE="${input#*-}"
            input="${input%%-*}"
            ;;
    esac

    # Parse major.minor.patch
    case "$input" in
        [0-9]*.[0-9]*.[0-9]*)
            VERSION_MAJOR="${input%%.*}"
            rest="${input#*.}"
            VERSION_MINOR="${rest%%.*}"
            VERSION_PATCH="${rest#*.}"
            ;;
        [0-9]*.[0-9]*)
            VERSION_MAJOR="${input%%.*}"
            VERSION_MINOR="${input#*.}"
            VERSION_PATCH="0"
            ;;
        [0-9]*)
            VERSION_MAJOR="$input"
            VERSION_MINOR="0"
            VERSION_PATCH="0"
            ;;
        *)
            PROVEN_ERROR="invalid_version_format"
            return 1
            ;;
    esac

    # Validate components are numeric
    case "$VERSION_MAJOR" in
        *[!0-9]*|'')
            PROVEN_ERROR="invalid_major"
            return 1
            ;;
    esac

    case "$VERSION_MINOR" in
        *[!0-9]*|'')
            PROVEN_ERROR="invalid_minor"
            return 1
            ;;
    esac

    case "$VERSION_PATCH" in
        *[!0-9]*|'')
            PROVEN_ERROR="invalid_patch"
            return 1
            ;;
    esac

    return 0
}

# Check if version string is valid
# Usage: version_is_valid "1.2.3" && echo "valid"
# Returns: 0 if valid, 1 otherwise
version_is_valid() {
    version_parse "$1" 2>/dev/null
}

# Format version to string
# Usage: str=$(version_format 1 2 3 "beta.1" "build.456")
# Returns: Formatted version string
version_format() {
    major="$1"
    minor="$2"
    patch="$3"
    prerelease="${4:-}"
    build="${5:-}"

    result="${major}.${minor}.${patch}"

    if [ -n "$prerelease" ]; then
        result="${result}-${prerelease}"
    fi

    if [ -n "$build" ]; then
        result="${result}+${build}"
    fi

    printf '%s' "$result"
}

# Compare two versions
# Usage: result=$(version_compare "1.2.3" "1.2.4")
# Returns: -1 if v1 < v2, 0 if equal, 1 if v1 > v2
version_compare() {
    v1="$1"
    v2="$2"

    # Parse first version
    if ! version_parse "$v1"; then
        printf '0'
        return 1
    fi
    maj1="$VERSION_MAJOR"
    min1="$VERSION_MINOR"
    pat1="$VERSION_PATCH"
    pre1="$VERSION_PRERELEASE"

    # Parse second version
    if ! version_parse "$v2"; then
        printf '0'
        return 1
    fi
    maj2="$VERSION_MAJOR"
    min2="$VERSION_MINOR"
    pat2="$VERSION_PATCH"
    pre2="$VERSION_PRERELEASE"

    # Compare major
    if [ "$maj1" -lt "$maj2" ]; then
        printf '%d' "-1"
        return 0
    elif [ "$maj1" -gt "$maj2" ]; then
        printf '%d' "1"
        return 0
    fi

    # Compare minor
    if [ "$min1" -lt "$min2" ]; then
        printf '%d' "-1"
        return 0
    elif [ "$min1" -gt "$min2" ]; then
        printf '%d' "1"
        return 0
    fi

    # Compare patch
    if [ "$pat1" -lt "$pat2" ]; then
        printf '%d' "-1"
        return 0
    elif [ "$pat1" -gt "$pat2" ]; then
        printf '%d' "1"
        return 0
    fi

    # Handle prerelease (version without prerelease > version with prerelease)
    if [ -z "$pre1" ] && [ -n "$pre2" ]; then
        printf '%d' "1"
        return 0
    elif [ -n "$pre1" ] && [ -z "$pre2" ]; then
        printf '%d' "-1"
        return 0
    elif [ -n "$pre1" ] && [ -n "$pre2" ]; then
        # Simple string comparison for prerelease
        if [ "$pre1" \< "$pre2" ]; then
            printf '%d' "-1"
        elif [ "$pre1" \> "$pre2" ]; then
            printf '%d' "1"
        else
            printf '%d' "0"
        fi
        return 0
    fi

    printf '%d' "0"
}

# Check if v1 < v2
# Usage: version_lt "1.0.0" "2.0.0" && echo "older"
# Returns: 0 if v1 < v2, 1 otherwise
version_lt() {
    result=$(version_compare "$1" "$2")
    [ "$result" -eq -1 ]
}

# Check if v1 <= v2
# Usage: version_le "1.0.0" "1.0.0" && echo "same or older"
# Returns: 0 if v1 <= v2, 1 otherwise
version_le() {
    result=$(version_compare "$1" "$2")
    [ "$result" -eq -1 ] || [ "$result" -eq 0 ]
}

# Check if v1 > v2
# Usage: version_gt "2.0.0" "1.0.0" && echo "newer"
# Returns: 0 if v1 > v2, 1 otherwise
version_gt() {
    result=$(version_compare "$1" "$2")
    [ "$result" -eq 1 ]
}

# Check if v1 >= v2
# Usage: version_ge "1.0.0" "1.0.0" && echo "same or newer"
# Returns: 0 if v1 >= v2, 1 otherwise
version_ge() {
    result=$(version_compare "$1" "$2")
    [ "$result" -eq 1 ] || [ "$result" -eq 0 ]
}

# Check if v1 == v2
# Usage: version_eq "1.0.0" "1.0.0" && echo "equal"
# Returns: 0 if equal, 1 otherwise
version_eq() {
    result=$(version_compare "$1" "$2")
    [ "$result" -eq 0 ]
}

# Increment major version
# Usage: newver=$(version_inc_major "1.2.3")
# Returns: 2.0.0
version_inc_major() {
    if ! version_parse "$1"; then
        return 1
    fi

    new_major=$((VERSION_MAJOR + 1))
    version_format "$new_major" "0" "0"
}

# Increment minor version
# Usage: newver=$(version_inc_minor "1.2.3")
# Returns: 1.3.0
version_inc_minor() {
    if ! version_parse "$1"; then
        return 1
    fi

    new_minor=$((VERSION_MINOR + 1))
    version_format "$VERSION_MAJOR" "$new_minor" "0"
}

# Increment patch version
# Usage: newver=$(version_inc_patch "1.2.3")
# Returns: 1.2.4
version_inc_patch() {
    if ! version_parse "$1"; then
        return 1
    fi

    new_patch=$((VERSION_PATCH + 1))
    version_format "$VERSION_MAJOR" "$VERSION_MINOR" "$new_patch"
}

# Check if version is prerelease
# Usage: version_is_prerelease "1.0.0-alpha" && echo "prerelease"
# Returns: 0 if prerelease, 1 otherwise
version_is_prerelease() {
    if ! version_parse "$1"; then
        return 1
    fi

    [ -n "$VERSION_PRERELEASE" ]
}

# Check if version is stable (not prerelease and major > 0)
# Usage: version_is_stable "1.0.0" && echo "stable"
# Returns: 0 if stable, 1 otherwise
version_is_stable() {
    if ! version_parse "$1"; then
        return 1
    fi

    [ -z "$VERSION_PRERELEASE" ] && [ "$VERSION_MAJOR" -gt 0 ]
}

# Get major version
# Usage: major=$(version_get_major "1.2.3")
# Returns: Major version number
version_get_major() {
    if ! version_parse "$1"; then
        return 1
    fi
    printf '%s' "$VERSION_MAJOR"
}

# Get minor version
# Usage: minor=$(version_get_minor "1.2.3")
# Returns: Minor version number
version_get_minor() {
    if ! version_parse "$1"; then
        return 1
    fi
    printf '%s' "$VERSION_MINOR"
}

# Get patch version
# Usage: patch=$(version_get_patch "1.2.3")
# Returns: Patch version number
version_get_patch() {
    if ! version_parse "$1"; then
        return 1
    fi
    printf '%s' "$VERSION_PATCH"
}

# Normalize version (strip v prefix, ensure three parts)
# Usage: normalized=$(version_normalize "v1.2")
# Returns: Normalized version string
version_normalize() {
    if ! version_parse "$1"; then
        return 1
    fi
    version_format "$VERSION_MAJOR" "$VERSION_MINOR" "$VERSION_PATCH" "$VERSION_PRERELEASE" "$VERSION_BUILD"
}

# Check if version satisfies a constraint
# Usage: version_satisfies "1.2.3" ">=1.0.0" && echo "satisfies"
# Returns: 0 if satisfies, 1 otherwise
version_satisfies() {
    version="$1"
    constraint="$2"

    # Parse constraint
    case "$constraint" in
        ">="*)
            target="${constraint#>=}"
            version_ge "$version" "$target"
            ;;
        "<="*)
            target="${constraint#<=}"
            version_le "$version" "$target"
            ;;
        ">"*)
            target="${constraint#>}"
            version_gt "$version" "$target"
            ;;
        "<"*)
            target="${constraint#<}"
            version_lt "$version" "$target"
            ;;
        "="*)
            target="${constraint#=}"
            version_eq "$version" "$target"
            ;;
        "^"*)
            # Caret range: compatible changes (same major, or 0.x same minor)
            target="${constraint#^}"
            if ! version_parse "$target"; then
                return 1
            fi
            target_maj="$VERSION_MAJOR"
            target_min="$VERSION_MINOR"

            if ! version_parse "$version"; then
                return 1
            fi

            if [ "$target_maj" -eq 0 ]; then
                # 0.x.y allows changes in patch only
                [ "$VERSION_MAJOR" -eq 0 ] && [ "$VERSION_MINOR" -eq "$target_min" ]
            else
                # x.y.z allows changes in minor/patch
                [ "$VERSION_MAJOR" -eq "$target_maj" ]
            fi
            ;;
        "~"*)
            # Tilde range: patch-level changes only
            target="${constraint#~}"
            if ! version_parse "$target"; then
                return 1
            fi
            target_maj="$VERSION_MAJOR"
            target_min="$VERSION_MINOR"

            if ! version_parse "$version"; then
                return 1
            fi

            [ "$VERSION_MAJOR" -eq "$target_maj" ] && [ "$VERSION_MINOR" -eq "$target_min" ]
            ;;
        *)
            # Exact match
            version_eq "$version" "$constraint"
            ;;
    esac
}

# Find the maximum version from a list
# Usage: max=$(version_max "1.0.0" "2.0.0" "1.5.0")
# Returns: Maximum version
version_max() {
    max=""

    for v in "$@"; do
        if [ -z "$max" ]; then
            max="$v"
        elif version_gt "$v" "$max"; then
            max="$v"
        fi
    done

    printf '%s' "$max"
}

# Find the minimum version from a list
# Usage: min=$(version_min "1.0.0" "2.0.0" "1.5.0")
# Returns: Minimum version
version_min() {
    min=""

    for v in "$@"; do
        if [ -z "$min" ]; then
            min="$v"
        elif version_lt "$v" "$min"; then
            min="$v"
        fi
    done

    printf '%s' "$min"
}
