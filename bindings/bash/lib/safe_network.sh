#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_network.sh - Safe network operations for Bash
# Source this file: source /path/to/safe_network.sh

# Parse IPv4 address into octets
# Sets IP_A, IP_B, IP_C, IP_D on success
# Usage: parse_ipv4 "192.168.1.1" && echo "$IP_A.$IP_B.$IP_C.$IP_D"
parse_ipv4() {
    local address="$1"
    IP_A=""
    IP_B=""
    IP_C=""
    IP_D=""

    # Check format with regex
    if [[ ! "$address" =~ ^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        return 1
    fi

    # Split into octets
    IFS='.' read -r IP_A IP_B IP_C IP_D <<< "$address"

    # Validate each octet
    for octet in "$IP_A" "$IP_B" "$IP_C" "$IP_D"; do
        # Check for leading zeros (invalid)
        if [[ "${#octet}" -gt 1 && "$octet" == "0"* ]]; then
            IP_A="" IP_B="" IP_C="" IP_D=""
            return 1
        fi
        # Check range
        if [[ "$octet" -lt 0 || "$octet" -gt 255 ]]; then
            IP_A="" IP_B="" IP_C="" IP_D=""
            return 1
        fi
    done

    return 0
}

# Check if string is valid IPv4
# Usage: is_valid_ipv4 "192.168.1.1" && echo "valid"
is_valid_ipv4() {
    parse_ipv4 "$1"
}

# Check if IPv4 is private (RFC 1918)
# Usage: is_private_ip "192.168.1.1" && echo "private"
is_private_ip() {
    local address="$1"

    if ! parse_ipv4 "$address"; then
        return 1
    fi

    # 10.0.0.0/8
    if [[ "$IP_A" -eq 10 ]]; then
        return 0
    fi

    # 172.16.0.0/12
    if [[ "$IP_A" -eq 172 && "$IP_B" -ge 16 && "$IP_B" -le 31 ]]; then
        return 0
    fi

    # 192.168.0.0/16
    if [[ "$IP_A" -eq 192 && "$IP_B" -eq 168 ]]; then
        return 0
    fi

    return 1
}

# Check if IPv4 is loopback (127.0.0.0/8)
# Usage: is_loopback_ip "127.0.0.1" && echo "loopback"
is_loopback_ip() {
    local address="$1"

    if ! parse_ipv4 "$address"; then
        return 1
    fi

    [[ "$IP_A" -eq 127 ]]
}

# Check if IPv4 is in reserved range
# Usage: is_reserved_ip "0.0.0.0" && echo "reserved"
is_reserved_ip() {
    local address="$1"

    if ! parse_ipv4 "$address"; then
        return 1
    fi

    # 0.0.0.0/8
    [[ "$IP_A" -eq 0 ]] && return 0

    # 100.64.0.0/10 - CGNAT
    [[ "$IP_A" -eq 100 && "$IP_B" -ge 64 && "$IP_B" -le 127 ]] && return 0

    # 169.254.0.0/16 - Link-local
    [[ "$IP_A" -eq 169 && "$IP_B" -eq 254 ]] && return 0

    # 192.0.0.0/24 - IETF
    [[ "$IP_A" -eq 192 && "$IP_B" -eq 0 && "$IP_C" -eq 0 ]] && return 0

    # 192.0.2.0/24 - TEST-NET-1
    [[ "$IP_A" -eq 192 && "$IP_B" -eq 0 && "$IP_C" -eq 2 ]] && return 0

    # 198.51.100.0/24 - TEST-NET-2
    [[ "$IP_A" -eq 198 && "$IP_B" -eq 51 && "$IP_C" -eq 100 ]] && return 0

    # 203.0.113.0/24 - TEST-NET-3
    [[ "$IP_A" -eq 203 && "$IP_B" -eq 0 && "$IP_C" -eq 113 ]] && return 0

    # 224.0.0.0/4 - Multicast
    [[ "$IP_A" -ge 224 && "$IP_A" -le 239 ]] && return 0

    # 240.0.0.0/4 - Reserved
    [[ "$IP_A" -ge 240 ]] && return 0

    return 1
}

# Check if IPv4 is public (not private, loopback, or reserved)
# Usage: is_public_ip "8.8.8.8" && echo "public"
is_public_ip() {
    local address="$1"

    is_valid_ipv4 "$address" || return 1
    is_private_ip "$address" && return 1
    is_loopback_ip "$address" && return 1
    is_reserved_ip "$address" && return 1

    return 0
}

# Format IPv4 from octets
# Usage: formatted=$(format_ipv4 192 168 1 1)
format_ipv4() {
    printf '%d.%d.%d.%d' "$1" "$2" "$3" "$4"
}

# Get IP classification
# Usage: classification=$(classify_ip "192.168.1.1")
classify_ip() {
    local address="$1"

    if ! is_valid_ipv4 "$address"; then
        printf 'invalid'
        return 1
    fi

    if is_loopback_ip "$address"; then
        printf 'loopback'
    elif is_private_ip "$address"; then
        printf 'private'
    elif is_reserved_ip "$address"; then
        printf 'reserved'
    else
        printf 'public'
    fi
}

# Check if port is valid (1-65535)
# Usage: is_valid_port 8080 && echo "valid"
is_valid_port() {
    local port="$1"

    [[ "$port" =~ ^[0-9]+$ ]] || return 1
    [[ "$port" -ge 1 && "$port" -le 65535 ]]
}

# Check if port is privileged (< 1024)
# Usage: is_privileged_port 80 && echo "needs root"
is_privileged_port() {
    local port="$1"

    is_valid_port "$port" || return 1
    [[ "$port" -lt 1024 ]]
}
