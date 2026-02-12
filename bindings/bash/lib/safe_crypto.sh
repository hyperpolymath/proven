#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_crypto.sh - Cryptographic safety operations for Bash
# Source this file: source /path/to/safe_crypto.sh

# Constant-time string comparison (best effort in Bash)
# Usage: constant_time_compare "$secret" "$input" && echo "match"
constant_time_compare() {
    local a="$1"
    local b="$2"

    # Length check (this leaks length, but that's unavoidable)
    [[ "${#a}" -ne "${#b}" ]] && return 1
    [[ "${#a}" -eq 0 ]] && return 0

    local result=0
    local i

    for ((i = 0; i < ${#a}; i++)); do
        local char_a="${a:i:1}"
        local char_b="${b:i:1}"
        # XOR ASCII values
        local ord_a ord_b
        ord_a=$(printf '%d' "'$char_a")
        ord_b=$(printf '%d' "'$char_b")
        ((result |= ord_a ^ ord_b))
    done

    [[ "$result" -eq 0 ]]
}

# Generate random bytes (hex encoded)
# Usage: random=$(random_hex 32)
random_hex() {
    local count="${1:-32}"

    if [[ -r /dev/urandom ]]; then
        head -c "$count" /dev/urandom | xxd -p | tr -d '\n'
    elif command -v openssl >/dev/null 2>&1; then
        openssl rand -hex "$count"
    else
        # Fallback to $RANDOM (NOT cryptographically secure!)
        local result=""
        local i
        for ((i = 0; i < count; i++)); do
            result+=$(printf '%02x' $((RANDOM % 256)))
        done
        printf '%s' "$result"
    fi
}

# Generate random bytes (base64 encoded)
# Usage: random=$(random_base64 32)
random_base64() {
    local count="${1:-32}"

    if [[ -r /dev/urandom ]]; then
        head -c "$count" /dev/urandom | base64 | tr -d '\n'
    elif command -v openssl >/dev/null 2>&1; then
        openssl rand -base64 "$count"
    else
        # Convert hex to base64 as fallback
        random_hex "$count" | xxd -r -p | base64 | tr -d '\n'
    fi
}

# Generate URL-safe random string
# Usage: token=$(random_urlsafe 32)
random_urlsafe() {
    local count="${1:-32}"

    random_base64 "$count" | tr '+/' '-_' | tr -d '='
}

# Generate random integer in range
# Usage: num=$(random_int 1 100)
random_int() {
    local min="${1:-0}"
    local max="${2:-100}"

    local range=$((max - min + 1))

    if [[ -r /dev/urandom ]]; then
        local random_val
        random_val=$(od -An -tu4 -N4 /dev/urandom | tr -d ' ')
        echo $((min + random_val % range))
    else
        echo $((min + RANDOM % range))
    fi
}

# Hash a string with SHA-256
# Usage: hash=$(sha256 "password")
sha256() {
    local input="$1"
    printf '%s' "$input" | sha256sum | cut -d' ' -f1
}

# Hash a string with SHA-512
# Usage: hash=$(sha512 "password")
sha512() {
    local input="$1"
    printf '%s' "$input" | sha512sum | cut -d' ' -f1
}

# HMAC-SHA256
# Usage: mac=$(hmac_sha256 "key" "message")
hmac_sha256() {
    local key="$1"
    local message="$2"

    if command -v openssl >/dev/null 2>&1; then
        printf '%s' "$message" | openssl dgst -sha256 -hmac "$key" | cut -d' ' -f2
    else
        # Fallback using pure bash (simplified, not constant-time)
        echo "ERROR: openssl required for HMAC" >&2
        return 1
    fi
}

# Verify HMAC
# Usage: verify_hmac "$key" "$message" "$expected_mac" && echo "valid"
verify_hmac() {
    local key="$1"
    local message="$2"
    local expected="$3"

    local actual
    actual=$(hmac_sha256 "$key" "$message")

    constant_time_compare "$actual" "$expected"
}

# Generate a secure token (for sessions, CSRF, etc)
# Usage: token=$(generate_token)
generate_token() {
    random_urlsafe 32
}

# Check if a source of randomness is cryptographically secure
# Usage: is_secure_random && echo "safe"
is_secure_random() {
    [[ -r /dev/urandom ]] || command -v openssl >/dev/null 2>&1
}

# Securely clear a variable (best effort)
# Usage: secure_clear VAR_NAME
secure_clear() {
    local varname="$1"

    # Overwrite with zeros, then unset
    local length
    eval "length=\${#$varname}"

    local zeros
    zeros=$(printf '%*s' "$length" '' | tr ' ' '\0')
    eval "$varname=\"\$zeros\""
    unset "$varname"
}
