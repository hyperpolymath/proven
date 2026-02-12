#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_path.sh - Safe filesystem path operations for Bash
# Source this file: source /path/to/safe_path.sh

# Check if path contains directory traversal sequences
# Usage: has_traversal "../etc/passwd" && echo "dangerous!"
has_traversal() {
    local path="$1"

    # Check for .. sequences
    if [[ "$path" == *".."* ]]; then
        return 0  # true - has traversal
    fi

    # Check for ~ expansion attempts
    if [[ "$path" == "~"* ]]; then
        return 0  # true - has traversal
    fi

    return 1  # false - safe
}

# Check if path is safe (no traversal)
# Usage: is_safe_path "$path" && process_file "$path"
is_safe_path() {
    ! has_traversal "$1"
}

# Sanitize a filename by removing dangerous characters
# Usage: safe_name=$(sanitize_filename "$filename")
sanitize_filename() {
    local filename="$1"
    local safe="$filename"

    # Remove .. sequences
    safe="${safe//../_}"

    # Remove or replace dangerous characters
    safe="${safe//\//_}"
    safe="${safe//\\/_}"
    safe="${safe//</_}"
    safe="${safe//>/_}"
    safe="${safe//:/_}"
    safe="${safe//\"/_}"
    safe="${safe//|/_}"
    safe="${safe//\?/_}"
    safe="${safe//\*/_}"

    # Remove null bytes
    safe="${safe//$'\0'/_}"

    # Remove leading/trailing dots and spaces
    safe="${safe#.}"
    safe="${safe%.}"
    safe="${safe# }"
    safe="${safe% }"

    printf '%s' "$safe"
}

# Safely join path components
# Returns 1 if any component has traversal sequences
# Usage: safe_join "/base" "subdir" "file.txt" && echo "$PROVEN_RESULT"
safe_join() {
    local base="$1"
    shift
    PROVEN_RESULT=""
    PROVEN_ERROR=""

    local path="$base"

    for part in "$@"; do
        if has_traversal "$part"; then
            PROVEN_ERROR="traversal_detected"
            return 1
        fi

        local safe_part
        safe_part=$(sanitize_filename "$part")

        # Join with /
        path="${path%/}/${safe_part}"
    done

    PROVEN_RESULT="$path"
    return 0
}

# Resolve path and verify it's within a base directory
# Usage: resolved=$(resolve_within "/var/www" "$user_path")
resolve_within() {
    local base_path="$1"
    local user_path="$2"

    # Get real path of base
    local real_base
    real_base=$(realpath -q "$base_path" 2>/dev/null)
    if [[ -z "$real_base" ]]; then
        return 1
    fi

    # Construct and resolve full path
    local full_path="${real_base}/${user_path}"
    local real_path
    real_path=$(realpath -q "$full_path" 2>/dev/null)
    if [[ -z "$real_path" ]]; then
        return 1
    fi

    # Verify the resolved path is within base
    if [[ "$real_path" != "$real_base"* ]]; then
        return 1
    fi

    printf '%s' "$real_path"
    return 0
}

# Get safe basename (strip directory components)
# Usage: name=$(safe_basename "/path/to/../../../etc/passwd")
safe_basename() {
    local path="$1"

    # Get basename
    local name
    name=$(basename "$path")

    # Sanitize it
    sanitize_filename "$name"
}

# Check if filename has an allowed extension
# Usage: has_allowed_ext "file.jpg" "jpg" "png" "gif" && echo "allowed"
has_allowed_ext() {
    local filename="$1"
    shift
    local allowed_exts=("$@")

    # Extract extension (lowercase)
    local ext="${filename##*.}"
    ext="${ext,,}"

    for allowed in "${allowed_exts[@]}"; do
        if [[ "$ext" == "${allowed,,}" ]]; then
            return 0
        fi
    done

    return 1
}

# Create directory safely (checks for traversal first)
# Usage: safe_mkdir "/base" "user_provided_name"
safe_mkdir() {
    local base="$1"
    local name="$2"

    if has_traversal "$name"; then
        return 1
    fi

    local safe_name
    safe_name=$(sanitize_filename "$name")

    local full_path="${base%/}/${safe_name}"

    mkdir -p "$full_path"
}

# Check if path is absolute
# Usage: is_absolute_path "/etc/passwd" && echo "absolute"
is_absolute_path() {
    [[ "$1" == /* ]]
}

# Check if path exists and is readable
# Usage: is_readable_path "/etc/passwd" && cat "$path"
is_readable_path() {
    [[ -r "$1" ]]
}

# Check if path is a regular file (not symlink, device, etc)
# Usage: is_regular_file "$path" && process "$path"
is_regular_file() {
    [[ -f "$1" && ! -L "$1" ]]
}
