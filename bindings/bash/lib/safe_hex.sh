#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_hex.sh - Safe hexadecimal operations for Bash
# Source this file: source /path/to/safe_hex.sh

# Hex character lookup table (lowercase)
readonly HEX_CHARS="0123456789abcdef"

# Hex validation pattern
readonly HEX_PATTERN='^[0-9a-fA-F]*$'

# Encode a string to hexadecimal
# Usage: hex=$(hex_encode "Hello")
# Returns: Hexadecimal string (lowercase)
hex_encode() {
    local input="$1"

    if [[ -z "$input" ]]; then
        printf ''
        return 0
    fi

    # Use xxd if available (faster for large inputs)
    if command -v xxd >/dev/null 2>&1; then
        printf '%s' "$input" | xxd -p | tr -d '\n'
        return 0
    fi

    # Pure bash fallback
    local result=""
    local i
    local length="${#input}"

    for ((i = 0; i < length; i++)); do
        local char="${input:i:1}"
        local ord
        ord=$(printf '%d' "'$char")
        result+=$(printf '%02x' "$ord")
    done

    printf '%s' "$result"
}

# Decode a hexadecimal string to bytes/text
# Usage: text=$(hex_decode "48656c6c6f")
# Returns: Decoded string, or error on invalid input
hex_decode() {
    local input="$1"

    # Empty input -> empty output
    if [[ -z "$input" ]]; then
        printf ''
        return 0
    fi

    # Remove common prefixes
    local hex="${input}"
    hex="${hex#0x}"
    hex="${hex#0X}"
    hex="${hex,,}"  # Lowercase

    # Validate hex string
    if ! [[ "$hex" =~ $HEX_PATTERN ]]; then
        echo "ERROR: Invalid hexadecimal characters" >&2
        return 1
    fi

    # Check for even length
    if [[ $((${#hex} % 2)) -ne 0 ]]; then
        echo "ERROR: Hex string must have even length" >&2
        return 1
    fi

    # Use xxd if available (faster for large inputs)
    if command -v xxd >/dev/null 2>&1; then
        printf '%s' "$hex" | xxd -r -p
        return 0
    fi

    # Pure bash fallback
    local result=""
    local i
    local length="${#hex}"

    for ((i = 0; i < length; i += 2)); do
        local byte="${hex:i:2}"
        local decimal=$((16#$byte))
        # Use printf to convert to character
        result+=$(printf "\\x$byte")
    done

    printf '%s' "$result"
}

# Encode bytes from stdin to hex
# Usage: echo -n "Hello" | hex_encode_stdin
# Returns: Hexadecimal string
hex_encode_stdin() {
    if command -v xxd >/dev/null 2>&1; then
        xxd -p | tr -d '\n'
    else
        local input
        input=$(cat)
        hex_encode "$input"
    fi
}

# Decode hex from stdin to bytes
# Usage: echo "48656c6c6f" | hex_decode_stdin
# Returns: Decoded bytes
hex_decode_stdin() {
    local input
    input=$(cat | tr -d '\n\r ')

    hex_decode "$input"
}

# Constant-time comparison of two hex strings
# Usage: hex_constant_time_equal "$hex1" "$hex2" && echo "equal"
# Returns: 0 if equal, 1 if not equal
# Note: This is best-effort constant-time in Bash
hex_constant_time_equal() {
    local a="${1,,}"  # Lowercase
    local b="${2,,}"  # Lowercase

    # Remove 0x prefix if present
    a="${a#0x}"
    b="${b#0x}"

    # Validate both are hex
    if ! [[ "$a" =~ $HEX_PATTERN ]] || ! [[ "$b" =~ $HEX_PATTERN ]]; then
        return 1
    fi

    # Length check (this leaks length info, but unavoidable)
    local len_a="${#a}"
    local len_b="${#b}"

    if [[ "$len_a" -ne "$len_b" ]]; then
        return 1
    fi

    if [[ "$len_a" -eq 0 ]]; then
        return 0
    fi

    # XOR each character and accumulate
    local result=0
    local i

    for ((i = 0; i < len_a; i++)); do
        local char_a="${a:i:1}"
        local char_b="${b:i:1}"

        local val_a val_b
        case "$char_a" in
            [0-9]) val_a="$char_a" ;;
            a) val_a=10 ;; b) val_a=11 ;; c) val_a=12 ;;
            d) val_a=13 ;; e) val_a=14 ;; f) val_a=15 ;;
        esac
        case "$char_b" in
            [0-9]) val_b="$char_b" ;;
            a) val_b=10 ;; b) val_b=11 ;; c) val_b=12 ;;
            d) val_b=13 ;; e) val_b=14 ;; f) val_b=15 ;;
        esac

        ((result |= val_a ^ val_b))
    done

    [[ "$result" -eq 0 ]]
}

# Validate a hex string
# Usage: hex_is_valid "48656c6c6f" && echo "valid"
# Returns: 0 if valid, 1 if invalid
hex_is_valid() {
    local input="$1"

    # Remove 0x prefix if present
    local hex="${input#0x}"
    hex="${hex#0X}"

    # Must be non-empty
    [[ -z "$hex" ]] && return 1

    # Must match hex pattern
    [[ "$hex" =~ $HEX_PATTERN ]]
}

# Validate hex string has even length (complete bytes)
# Usage: hex_is_valid_bytes "48656c" && echo "valid bytes"
# Returns: 0 if valid complete bytes, 1 otherwise
hex_is_valid_bytes() {
    local input="$1"

    hex_is_valid "$input" || return 1

    local hex="${input#0x}"
    hex="${hex#0X}"

    [[ $((${#hex} % 2)) -eq 0 ]]
}

# Convert hex to uppercase
# Usage: upper=$(hex_upper "48656c6c6f")
# Returns: Uppercase hex string
hex_upper() {
    local input="$1"

    printf '%s' "${input^^}"
}

# Convert hex to lowercase
# Usage: lower=$(hex_lower "48656C6C6F")
# Returns: Lowercase hex string
hex_lower() {
    local input="$1"

    printf '%s' "${input,,}"
}

# Add 0x prefix to hex string
# Usage: prefixed=$(hex_prefix "48656c6c6f")
# Returns: "0x48656c6c6f"
hex_prefix() {
    local input="$1"

    # Don't double-prefix
    if [[ "${input:0:2}" == "0x" || "${input:0:2}" == "0X" ]]; then
        printf '%s' "$input"
    else
        printf '0x%s' "$input"
    fi
}

# Remove 0x prefix from hex string
# Usage: unprefixed=$(hex_unprefix "0x48656c6c6f")
# Returns: "48656c6c6f"
hex_unprefix() {
    local input="$1"

    local hex="${input#0x}"
    hex="${hex#0X}"

    printf '%s' "$hex"
}

# Get the byte length of a hex string
# Usage: bytes=$(hex_byte_length "48656c6c6f")
# Returns: Number of bytes represented
hex_byte_length() {
    local input="$1"

    local hex="${input#0x}"
    hex="${hex#0X}"

    printf '%s' "$(( ${#hex} / 2 ))"
}

# Pad a hex string to a minimum length
# Usage: padded=$(hex_pad "ff" 8)  # Returns "000000ff"
# Returns: Zero-padded hex string
hex_pad() {
    local input="$1"
    local min_length="$2"

    local hex="${input#0x}"
    hex="${hex#0X}"
    hex="${hex,,}"

    while [[ "${#hex}" -lt "$min_length" ]]; do
        hex="0${hex}"
    done

    printf '%s' "$hex"
}

# XOR two hex strings
# Usage: result=$(hex_xor "ff00" "00ff")
# Returns: XOR result as hex string
hex_xor() {
    local a="${1,,}"
    local b="${2,,}"

    # Remove prefixes
    a="${a#0x}"
    b="${b#0x}"

    # Validate
    if ! hex_is_valid_bytes "$a" || ! hex_is_valid_bytes "$b"; then
        echo "ERROR: Invalid hex input" >&2
        return 1
    fi

    # Pad shorter string
    local max_len="${#a}"
    [[ "${#b}" -gt "$max_len" ]] && max_len="${#b}"

    a=$(hex_pad "$a" "$max_len")
    b=$(hex_pad "$b" "$max_len")

    # XOR byte by byte
    local result=""
    local i

    for ((i = 0; i < max_len; i += 2)); do
        local byte_a=$((16#${a:i:2}))
        local byte_b=$((16#${b:i:2}))
        local xor_result=$((byte_a ^ byte_b))
        result+=$(printf '%02x' "$xor_result")
    done

    printf '%s' "$result"
}

# Reverse bytes in a hex string (for endianness conversion)
# Usage: reversed=$(hex_reverse "01020304")  # Returns "04030201"
# Returns: Byte-reversed hex string
hex_reverse() {
    local input="$1"

    local hex="${input#0x}"
    hex="${hex#0X}"
    hex="${hex,,}"

    if ! hex_is_valid_bytes "$hex"; then
        echo "ERROR: Invalid hex input (must be complete bytes)" >&2
        return 1
    fi

    local result=""
    local length="${#hex}"
    local i

    for ((i = length - 2; i >= 0; i -= 2)); do
        result+="${hex:i:2}"
    done

    printf '%s' "$result"
}

# Slice a hex string by byte indices
# Usage: slice=$(hex_slice "0102030405" 1 3)  # Returns "020304" (bytes 1-3)
# Returns: Sliced hex string
hex_slice() {
    local input="$1"
    local start_byte="$2"
    local end_byte="$3"

    local hex="${input#0x}"
    hex="${hex#0X}"
    hex="${hex,,}"

    local start_char=$((start_byte * 2))
    local length=$(( (end_byte - start_byte + 1) * 2 ))

    printf '%s' "${hex:start_char:length}"
}

# Generate random hex bytes
# Usage: random=$(hex_random 16)  # 16 random bytes as hex
# Returns: Random hex string
hex_random() {
    local count="${1:-16}"

    if [[ -r /dev/urandom ]]; then
        head -c "$count" /dev/urandom | xxd -p | tr -d '\n'
    elif command -v openssl >/dev/null 2>&1; then
        openssl rand -hex "$count"
    else
        # Fallback (NOT cryptographically secure)
        local result=""
        local i
        for ((i = 0; i < count; i++)); do
            result+=$(printf '%02x' $((RANDOM % 256)))
        done
        printf '%s' "$result"
    fi
}

# Convert integer to hex
# Usage: hex=$(int_to_hex 255)  # Returns "ff"
# Returns: Hex representation of integer
int_to_hex() {
    local value="$1"
    local min_width="${2:-0}"

    local hex
    hex=$(printf '%x' "$value")

    if [[ "$min_width" -gt 0 ]]; then
        hex=$(hex_pad "$hex" "$min_width")
    fi

    printf '%s' "$hex"
}

# Convert hex to integer
# Usage: num=$(hex_to_int "ff")  # Returns 255
# Returns: Integer value
hex_to_int() {
    local input="$1"

    local hex="${input#0x}"
    hex="${hex#0X}"
    hex="${hex,,}"

    if ! hex_is_valid "$hex"; then
        echo "ERROR: Invalid hex string" >&2
        return 1
    fi

    printf '%d' "$((16#$hex))"
}
