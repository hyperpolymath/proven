#!/bin/sh
# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# safe_url.sh - Safe URL parsing and manipulation for POSIX shells
# Source this file: . /path/to/safe_url.sh

# URL component variables (set by url_parse)
URL_SCHEME=""
URL_USERNAME=""
URL_PASSWORD=""
URL_HOST=""
URL_PORT=""
URL_PATH=""
URL_QUERY=""
URL_FRAGMENT=""

# Parse a URL into components
# Usage: url_parse "https://user:pass@example.com:8080/path?query=1#frag"
# Sets: URL_SCHEME, URL_HOST, URL_PORT, URL_PATH, URL_QUERY, URL_FRAGMENT, URL_USERNAME, URL_PASSWORD
# Returns: 0 on success, 1 on error
url_parse() {
    url="$1"
    URL_SCHEME=""
    URL_USERNAME=""
    URL_PASSWORD=""
    URL_HOST=""
    URL_PORT=""
    URL_PATH=""
    URL_QUERY=""
    URL_FRAGMENT=""
    PROVEN_ERROR=""

    # Check for empty input
    if [ -z "$url" ]; then
        PROVEN_ERROR="empty_url"
        return 1
    fi

    # Extract scheme
    case "$url" in
        *"://"*)
            URL_SCHEME="${url%%://*}"
            rest="${url#*://}"
            ;;
        *)
            PROVEN_ERROR="missing_scheme"
            return 1
            ;;
    esac

    # Validate scheme (alphanumeric, +, -, .)
    case "$URL_SCHEME" in
        *[!a-zA-Z0-9+.-]*)
            PROVEN_ERROR="invalid_scheme"
            return 1
            ;;
    esac

    # Extract fragment
    case "$rest" in
        *"#"*)
            URL_FRAGMENT="${rest#*#}"
            rest="${rest%%#*}"
            ;;
    esac

    # Extract query
    case "$rest" in
        *"?"*)
            URL_QUERY="${rest#*\?}"
            rest="${rest%%\?*}"
            ;;
    esac

    # Extract path
    case "$rest" in
        *"/"*)
            authority="${rest%%/*}"
            URL_PATH="/${rest#*/}"
            ;;
        *)
            authority="$rest"
            URL_PATH="/"
            ;;
    esac

    # Extract userinfo
    case "$authority" in
        *"@"*)
            userinfo="${authority%@*}"
            hostport="${authority##*@}"
            case "$userinfo" in
                *":"*)
                    URL_USERNAME="${userinfo%%:*}"
                    URL_PASSWORD="${userinfo#*:}"
                    ;;
                *)
                    URL_USERNAME="$userinfo"
                    ;;
            esac
            ;;
        *)
            hostport="$authority"
            ;;
    esac

    # Extract port (handle IPv6 addresses)
    case "$hostport" in
        "["*"]"*)
            # IPv6 address
            URL_HOST="${hostport%%]*}"
            URL_HOST="${URL_HOST#\[}"
            remainder="${hostport#*]}"
            case "$remainder" in
                ":"*)
                    URL_PORT="${remainder#:}"
                    ;;
            esac
            ;;
        *":"*)
            URL_HOST="${hostport%:*}"
            URL_PORT="${hostport##*:}"
            ;;
        *)
            URL_HOST="$hostport"
            ;;
    esac

    # Validate port if present
    if [ -n "$URL_PORT" ]; then
        case "$URL_PORT" in
            *[!0-9]*)
                PROVEN_ERROR="invalid_port"
                return 1
                ;;
        esac
        if [ "$URL_PORT" -lt 1 ] || [ "$URL_PORT" -gt 65535 ]; then
            PROVEN_ERROR="port_out_of_range"
            return 1
        fi
    fi

    # Lowercase scheme
    URL_SCHEME=$(printf '%s' "$URL_SCHEME" | tr '[:upper:]' '[:lower:]')

    return 0
}

# Check if a URL is valid
# Usage: url_is_valid "https://example.com" && echo "valid"
# Returns: 0 if valid, 1 if invalid
url_is_valid() {
    url_parse "$1" 2>/dev/null
}

# Get the domain/host from a URL
# Usage: domain=$(url_get_domain "https://example.com/path")
# Returns: Domain on success, empty on error
url_get_domain() {
    if url_parse "$1"; then
        printf '%s' "$URL_HOST"
        return 0
    fi
    return 1
}

# Get the scheme from a URL
# Usage: scheme=$(url_get_scheme "https://example.com")
# Returns: Scheme on success, empty on error
url_get_scheme() {
    if url_parse "$1"; then
        printf '%s' "$URL_SCHEME"
        return 0
    fi
    return 1
}

# Get the path from a URL
# Usage: path=$(url_get_path "https://example.com/foo/bar")
# Returns: Path on success, empty on error
url_get_path() {
    if url_parse "$1"; then
        printf '%s' "$URL_PATH"
        return 0
    fi
    return 1
}

# Get the port from a URL (or default port for scheme)
# Usage: port=$(url_get_port "https://example.com")
# Returns: Port number
url_get_port() {
    if url_parse "$1"; then
        if [ -n "$URL_PORT" ]; then
            printf '%s' "$URL_PORT"
        else
            case "$URL_SCHEME" in
                http) printf '80' ;;
                https) printf '443' ;;
                ftp) printf '21' ;;
                ssh) printf '22' ;;
                *) printf '' ;;
            esac
        fi
        return 0
    fi
    return 1
}

# Check if URL uses HTTPS
# Usage: url_is_https "https://example.com" && echo "secure"
# Returns: 0 if https, 1 otherwise
url_is_https() {
    if url_parse "$1"; then
        [ "$URL_SCHEME" = "https" ]
        return $?
    fi
    return 1
}

# Build a URL from components
# Usage: url=$(url_build "https" "example.com" "8080" "/path" "q=1" "frag")
# Returns: Constructed URL
url_build() {
    scheme="$1"
    host="$2"
    port="${3:-}"
    path="${4:-/}"
    query="${5:-}"
    fragment="${6:-}"

    url="${scheme}://${host}"

    if [ -n "$port" ]; then
        url="${url}:${port}"
    fi

    url="${url}${path}"

    if [ -n "$query" ]; then
        url="${url}?${query}"
    fi

    if [ -n "$fragment" ]; then
        url="${url}#${fragment}"
    fi

    printf '%s' "$url"
}

# Normalize a URL (lowercase scheme/host, remove default port)
# Usage: normalized=$(url_normalize "HTTPS://EXAMPLE.COM:443/path")
# Returns: Normalized URL
url_normalize() {
    if ! url_parse "$1"; then
        return 1
    fi

    # Lowercase host
    host=$(printf '%s' "$URL_HOST" | tr '[:upper:]' '[:lower:]')

    # Remove default ports
    port="$URL_PORT"
    case "$URL_SCHEME" in
        http) [ "$port" = "80" ] && port="" ;;
        https) [ "$port" = "443" ] && port="" ;;
        ftp) [ "$port" = "21" ] && port="" ;;
    esac

    url_build "$URL_SCHEME" "$host" "$port" "$URL_PATH" "$URL_QUERY" "$URL_FRAGMENT"
}

# Encode a query parameter value
# Usage: encoded=$(url_encode_param "hello world")
# Returns: URL-encoded string
url_encode_param() {
    input="$1"
    encoded=""
    i=0
    len=${#input}

    while [ $i -lt $len ]; do
        c=$(printf '%s' "$input" | cut -c$((i + 1)))
        case "$c" in
            [a-zA-Z0-9._~-])
                encoded="${encoded}${c}"
                ;;
            *)
                hex=$(printf '%%%02X' "'$c")
                encoded="${encoded}${hex}"
                ;;
        esac
        i=$((i + 1))
    done

    printf '%s' "$encoded"
}

# Decode a URL-encoded string
# Usage: decoded=$(url_decode "hello%20world")
# Returns: Decoded string
url_decode() {
    input="$1"
    # Replace + with space, then decode %XX
    decoded=$(printf '%s' "$input" | sed 's/+/ /g')
    printf '%b' "$(printf '%s' "$decoded" | sed 's/%\([0-9A-Fa-f][0-9A-Fa-f]\)/\\x\1/g')"
}

# Check if URL points to localhost
# Usage: url_is_localhost "http://127.0.0.1" && echo "local"
# Returns: 0 if localhost, 1 otherwise
url_is_localhost() {
    if url_parse "$1"; then
        case "$URL_HOST" in
            localhost|127.0.0.1|::1|\[::1\])
                return 0
                ;;
        esac
    fi
    return 1
}

# Get origin from URL (scheme + host + port)
# Usage: origin=$(url_get_origin "https://example.com:8080/path")
# Returns: Origin string
url_get_origin() {
    if url_parse "$1"; then
        if [ -n "$URL_PORT" ]; then
            printf '%s://%s:%s' "$URL_SCHEME" "$URL_HOST" "$URL_PORT"
        else
            printf '%s://%s' "$URL_SCHEME" "$URL_HOST"
        fi
        return 0
    fi
    return 1
}
