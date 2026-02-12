#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_uuid.sh - Safe UUID operations for Bash
# Source this file: source /path/to/safe_uuid.sh

# UUID version constants
readonly UUID_VERSION_1=1
readonly UUID_VERSION_3=3
readonly UUID_VERSION_4=4
readonly UUID_VERSION_5=5

# Nil UUID constant
readonly UUID_NIL="00000000-0000-0000-0000-000000000000"

# UUID regex pattern (case insensitive)
readonly UUID_PATTERN='^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$'

# Compact UUID pattern (no dashes)
readonly UUID_COMPACT_PATTERN='^[0-9a-fA-F]{32}$'

# Check if a string is a valid UUID
# Usage: uuid_is_valid "$uuid" && echo "valid"
# Returns: 0 if valid, 1 if invalid
uuid_is_valid() {
    local input="$1"

    [[ -z "$input" ]] && return 1

    # Check standard format with dashes
    if [[ "$input" =~ $UUID_PATTERN ]]; then
        return 0
    fi

    # Check compact format without dashes
    if [[ "$input" =~ $UUID_COMPACT_PATTERN ]]; then
        return 0
    fi

    return 1
}

# Check if UUID is nil (all zeros)
# Usage: uuid_is_nil "$uuid" && echo "nil"
# Returns: 0 if nil, 1 if not nil or invalid
uuid_is_nil() {
    local input="$1"
    local normalized

    normalized=$(uuid_format "$input" "standard" 2>/dev/null) || return 1

    [[ "$normalized" == "$UUID_NIL" ]]
}

# Parse a UUID string and output its components
# Usage: uuid_parse "$uuid"
# Output: time_low time_mid time_hi_version clock_seq node version variant
# Returns: 0 on success, 1 on invalid input
uuid_parse() {
    local input="$1"
    local normalized

    # Normalize to lowercase without dashes
    normalized=$(uuid_format "$input" "compact") || return 1
    normalized="${normalized,,}"

    local time_low="${normalized:0:8}"
    local time_mid="${normalized:8:4}"
    local time_hi_version="${normalized:12:4}"
    local clock_seq="${normalized:16:4}"
    local node="${normalized:20:12}"

    # Extract version (bits 4-7 of time_hi_version)
    local version_char="${time_hi_version:0:1}"
    local version
    case "$version_char" in
        [0-9]) version="$version_char" ;;
        a|A) version=10 ;;
        b|B) version=11 ;;
        c|C) version=12 ;;
        d|D) version=13 ;;
        e|E) version=14 ;;
        f|F) version=15 ;;
        *) version=0 ;;
    esac

    # Extract variant (first 2 bits of clock_seq high byte)
    local clock_high="${clock_seq:0:2}"
    local clock_high_dec
    clock_high_dec=$((16#$clock_high))
    local variant

    if ((clock_high_dec & 0x80)) && ! ((clock_high_dec & 0x40)); then
        variant="RFC4122"
    elif ! ((clock_high_dec & 0x80)); then
        variant="NCS"
    elif ((clock_high_dec & 0x80)) && ((clock_high_dec & 0x40)) && ! ((clock_high_dec & 0x20)); then
        variant="Microsoft"
    else
        variant="Future"
    fi

    printf '%s %s %s %s %s %s %s\n' \
        "$time_low" "$time_mid" "$time_hi_version" "$clock_seq" "$node" "$version" "$variant"
    return 0
}

# Format a UUID to a specific format
# Usage: uuid_format "$uuid" "standard|compact|urn|upper|lower"
# Returns: 0 on success, 1 on invalid input
uuid_format() {
    local input="$1"
    local format="${2:-standard}"

    [[ -z "$input" ]] && return 1

    # Remove dashes and convert to lowercase for processing
    local compact="${input//-/}"
    compact="${compact,,}"

    # Validate length
    if [[ "${#compact}" -ne 32 ]]; then
        echo "ERROR: Invalid UUID length" >&2
        return 1
    fi

    # Validate hex characters
    if ! [[ "$compact" =~ ^[0-9a-f]{32}$ ]]; then
        echo "ERROR: Invalid UUID characters" >&2
        return 1
    fi

    case "$format" in
        compact)
            printf '%s' "$compact"
            ;;
        standard|lowercase|lower)
            printf '%s-%s-%s-%s-%s' \
                "${compact:0:8}" "${compact:8:4}" "${compact:12:4}" \
                "${compact:16:4}" "${compact:20:12}"
            ;;
        upper|uppercase)
            local upper="${compact^^}"
            printf '%s-%s-%s-%s-%s' \
                "${upper:0:8}" "${upper:8:4}" "${upper:12:4}" \
                "${upper:16:4}" "${upper:20:12}"
            ;;
        urn)
            printf 'urn:uuid:%s-%s-%s-%s-%s' \
                "${compact:0:8}" "${compact:8:4}" "${compact:12:4}" \
                "${compact:16:4}" "${compact:20:12}"
            ;;
        *)
            echo "ERROR: Unknown format: $format" >&2
            return 1
            ;;
    esac

    return 0
}

# Get the version of a UUID
# Usage: version=$(uuid_version "$uuid")
# Returns: 0-15 on success, empty on invalid
uuid_version() {
    local input="$1"
    local parsed

    parsed=$(uuid_parse "$input") || return 1

    local components
    read -r -a components <<< "$parsed"

    printf '%s' "${components[5]}"
}

# Get the variant of a UUID
# Usage: variant=$(uuid_variant "$uuid")
# Returns: NCS|RFC4122|Microsoft|Future on success, empty on invalid
uuid_variant() {
    local input="$1"
    local parsed

    parsed=$(uuid_parse "$input") || return 1

    local components
    read -r -a components <<< "$parsed"

    printf '%s' "${components[6]}"
}

# Generate a version 4 (random) UUID
# Usage: uuid=$(uuid_generate_v4)
# Returns: 0 on success, 1 if no random source available
uuid_generate_v4() {
    local hex_bytes

    # Generate 16 random bytes as hex
    if [[ -r /dev/urandom ]]; then
        hex_bytes=$(head -c 16 /dev/urandom | xxd -p | tr -d '\n')
    elif command -v openssl >/dev/null 2>&1; then
        hex_bytes=$(openssl rand -hex 16)
    else
        echo "ERROR: No secure random source available" >&2
        return 1
    fi

    # Set version to 4 (bits 4-7 of time_hi_version)
    local time_hi_version="${hex_bytes:12:4}"
    local new_time_hi="4${time_hi_version:1:3}"

    # Set variant to RFC4122 (bits 6-7 of clock_seq_hi_and_reserved)
    local clock_seq="${hex_bytes:16:4}"
    local clock_high
    clock_high=$((16#${clock_seq:0:2}))
    clock_high=$((clock_high & 0x3F | 0x80))
    local new_clock_seq
    new_clock_seq=$(printf '%02x%s' "$clock_high" "${clock_seq:2:2}")

    # Reconstruct UUID
    local uuid="${hex_bytes:0:8}-${hex_bytes:8:4}-${new_time_hi}-${new_clock_seq}-${hex_bytes:20:12}"

    printf '%s' "${uuid,,}"
    return 0
}

# Compare two UUIDs for equality (case-insensitive, format-agnostic)
# Usage: uuid_equals "$uuid1" "$uuid2" && echo "equal"
# Returns: 0 if equal, 1 if not equal or invalid
uuid_equals() {
    local uuid1="$1"
    local uuid2="$2"

    local norm1 norm2

    norm1=$(uuid_format "$uuid1" "compact") || return 1
    norm2=$(uuid_format "$uuid2" "compact") || return 1

    [[ "$norm1" == "$norm2" ]]
}

# Extract timestamp from version 1 UUID
# Usage: timestamp=$(uuid_timestamp "$uuid")
# Returns: Unix timestamp on success, empty on error or non-v1 UUID
uuid_timestamp() {
    local input="$1"
    local parsed

    parsed=$(uuid_parse "$input") || return 1

    local components
    read -r -a components <<< "$parsed"

    local version="${components[5]}"

    if [[ "$version" != "1" ]]; then
        echo "ERROR: Not a version 1 UUID" >&2
        return 1
    fi

    local time_low="${components[0]}"
    local time_mid="${components[1]}"
    local time_hi_version="${components[2]}"

    # Remove version nibble from time_hi_version
    local time_hi="${time_hi_version:1:3}"

    # Reconstruct 60-bit timestamp (100-nanosecond intervals since 1582-10-15)
    local timestamp_hex="${time_hi}${time_mid}${time_low}"
    local timestamp_100ns
    timestamp_100ns=$((16#$timestamp_hex))

    # Convert to Unix timestamp (seconds since 1970-01-01)
    # UUID epoch is 1582-10-15, Unix epoch is 1970-01-01
    # Difference is 122192928000000000 100-nanosecond intervals
    local uuid_epoch_offset=122192928000000000
    local unix_100ns=$((timestamp_100ns - uuid_epoch_offset))
    local unix_seconds=$((unix_100ns / 10000000))

    printf '%s' "$unix_seconds"
    return 0
}

# Validate and sanitize a UUID input
# Usage: sanitized=$(uuid_sanitize "$input") || echo "invalid"
# Returns: Lowercase standard format UUID, or error
uuid_sanitize() {
    local input="$1"

    # Remove common wrapper characters
    local cleaned="${input}"
    cleaned="${cleaned#\{}"  # Remove leading {
    cleaned="${cleaned%\}}"  # Remove trailing }
    cleaned="${cleaned#urn:uuid:}"  # Remove URN prefix

    uuid_format "$cleaned" "standard"
}
