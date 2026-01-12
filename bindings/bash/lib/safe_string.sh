#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_string.sh - Safe string operations for Bash
# Source this file: source /path/to/safe_string.sh

# Escape string for HTML (prevent XSS)
# Usage: safe_html=$(escape_html "$user_input")
escape_html() {
    local input="$1"
    local output="$input"

    output="${output//&/&amp;}"
    output="${output//</&lt;}"
    output="${output//>/&gt;}"
    output="${output//\"/&quot;}"
    output="${output//\'/&#x27;}"

    printf '%s' "$output"
}

# Escape string for SQL single quotes
# Note: PREFER PARAMETERIZED QUERIES! This is last resort.
# Usage: safe_sql=$(escape_sql "$user_input")
escape_sql() {
    local input="$1"
    printf '%s' "${input//\'/\'\'}"
}

# Escape string for shell command (single quote wrapping)
# Usage: safe_arg=$(escape_shell "$user_input")
escape_shell() {
    local input="$1"
    # Use printf %q for proper quoting
    printf '%q' "$input"
}

# Escape string for use in sed replacement
# Usage: safe_sed=$(escape_sed "$user_input")
escape_sed() {
    local input="$1"
    # Escape special sed characters
    printf '%s' "$input" | sed 's/[&/\]/\\&/g'
}

# Escape string for use in grep pattern
# Usage: safe_grep=$(escape_grep "$user_input")
escape_grep() {
    local input="$1"
    # Escape regex special characters
    printf '%s' "$input" | sed 's/[.^$*+?{}\\[\]|()]/\\&/g'
}

# URL encode a string
# Usage: encoded=$(url_encode "$string")
url_encode() {
    local input="$1"
    local length="${#input}"
    local char
    local output=""

    for ((i = 0; i < length; i++)); do
        char="${input:i:1}"
        case "$char" in
            [a-zA-Z0-9.~_-])
                output+="$char"
                ;;
            *)
                output+=$(printf '%%%02X' "'$char")
                ;;
        esac
    done

    printf '%s' "$output"
}

# URL decode a string
# Usage: decoded=$(url_decode "$encoded")
url_decode() {
    local input="$1"
    # Use printf to decode %XX sequences
    printf '%b' "${input//%/\\x}"
}

# Safely truncate a string
# Usage: truncated=$(truncate_safe "$string" 50 "...")
truncate_safe() {
    local input="$1"
    local max_length="$2"
    local suffix="${3:-...}"

    if [[ -z "$max_length" || "$max_length" -lt 0 ]]; then
        printf ''
        return
    fi

    local input_length="${#input}"
    if [[ "$input_length" -le "$max_length" ]]; then
        printf '%s' "$input"
        return
    fi

    local suffix_length="${#suffix}"
    if [[ "$max_length" -le "$suffix_length" ]]; then
        printf '%s' "${input:0:$max_length}"
        return
    fi

    local truncate_at=$((max_length - suffix_length))
    printf '%s%s' "${input:0:$truncate_at}" "$suffix"
}

# Strip HTML tags (basic - not a full parser)
# Usage: plain=$(strip_html "$html_content")
strip_html() {
    local input="$1"
    # Remove HTML tags
    printf '%s' "$input" | sed 's/<[^>]*>//g'
}

# Check if string contains only alphanumeric characters
# Usage: is_alphanumeric "abc123" && echo "yes"
is_alphanumeric() {
    local input="$1"
    [[ "$input" =~ ^[a-zA-Z0-9]+$ ]]
}

# Check if string is valid UTF-8 (using iconv)
# Usage: is_valid_utf8 "$string" && echo "valid"
is_valid_utf8() {
    local input="$1"
    printf '%s' "$input" | iconv -f UTF-8 -t UTF-8 >/dev/null 2>&1
}

# Sanitize string to contain only safe characters
# Usage: safe=$(sanitize_string "$input" "a-zA-Z0-9_-")
sanitize_string() {
    local input="$1"
    local allowed="${2:-a-zA-Z0-9_-}"
    printf '%s' "$input" | tr -cd "$allowed"
}
