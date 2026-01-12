#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_email.sh - Safe email validation for Bash
# Source this file: source /path/to/safe_email.sh

# Validate email address (basic check)
# Usage: is_valid_email "user@example.com" && echo "valid"
is_valid_email() {
    local email="$1"

    # Check for empty
    [[ -z "$email" ]] && return 1

    # Check length
    [[ "${#email}" -gt 254 ]] && return 1

    # Check for @ symbol
    [[ "$email" != *"@"* ]] && return 1

    # Split at @
    local local_part="${email%@*}"
    local domain="${email##*@}"

    # Check local part
    [[ -z "$local_part" ]] && return 1
    [[ "${#local_part}" -gt 64 ]] && return 1

    # Check domain
    [[ -z "$domain" ]] && return 1
    [[ "${#domain}" -lt 3 ]] && return 1

    # Domain must have a dot (unless localhost)
    if [[ "$domain" != *"."* && "$domain" != "localhost" ]]; then
        return 1
    fi

    # Domain can't start or end with dot
    [[ "$domain" == "."* || "$domain" == *"." ]] && return 1

    # Check for multiple @
    local count="${email//[^@]/}"
    [[ "${#count}" -ne 1 ]] && return 1

    return 0
}

# Extract domain from email
# Usage: domain=$(get_email_domain "user@example.com")
get_email_domain() {
    local email="$1"

    if ! is_valid_email "$email"; then
        return 1
    fi

    printf '%s' "${email##*@}"
}

# Extract local part from email
# Usage: local_part=$(get_email_local_part "user@example.com")
get_email_local_part() {
    local email="$1"

    if ! is_valid_email "$email"; then
        return 1
    fi

    printf '%s' "${email%@*}"
}

# Normalize email (lowercase domain)
# Usage: normalized=$(normalize_email "User@EXAMPLE.COM")
normalize_email() {
    local email="$1"

    if ! is_valid_email "$email"; then
        return 1
    fi

    local local_part="${email%@*}"
    local domain="${email##*@}"
    domain="${domain,,}"  # lowercase

    printf '%s@%s' "$local_part" "$domain"
}

# Check if email domain has MX records
# Usage: has_mx_record "user@example.com" && echo "deliverable"
has_mx_record() {
    local email="$1"

    local domain
    domain=$(get_email_domain "$email")
    [[ -z "$domain" ]] && return 1

    # Check for MX records using host command
    host -t MX "$domain" >/dev/null 2>&1
}

# List of common disposable email domains
declare -a DISPOSABLE_DOMAINS=(
    "tempmail.com"
    "throwaway.email"
    "guerrillamail.com"
    "mailinator.com"
    "10minutemail.com"
    "temp-mail.org"
    "fakeinbox.com"
    "trashmail.com"
    "yopmail.com"
    "sharklasers.com"
    "getairmail.com"
)

# Check if email is from disposable service
# Usage: is_disposable_email "user@mailinator.com" && echo "disposable"
is_disposable_email() {
    local email="$1"

    local domain
    domain=$(get_email_domain "$email")
    [[ -z "$domain" ]] && return 1
    domain="${domain,,}"

    for disposable in "${DISPOSABLE_DOMAINS[@]}"; do
        if [[ "$domain" == "$disposable" ]]; then
            return 0
        fi
    done

    return 1
}

# Split email into parts
# Usage: split_email "user@example.com"; echo "Local: $EMAIL_LOCAL, Domain: $EMAIL_DOMAIN"
split_email() {
    local email="$1"
    EMAIL_LOCAL=""
    EMAIL_DOMAIN=""

    if ! is_valid_email "$email"; then
        return 1
    fi

    EMAIL_LOCAL="${email%@*}"
    EMAIL_DOMAIN="${email##*@}"
    return 0
}
