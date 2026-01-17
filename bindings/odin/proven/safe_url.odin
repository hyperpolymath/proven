// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"
import "core:strconv"

// Parsed URL components.
ParsedUrl :: struct {
    scheme:   string,
    username: Maybe(string),
    password: Maybe(string),
    host:     string,
    port:     Maybe(u16),
    path:     string,
    query:    Maybe(string),
    fragment: Maybe(string),
}

// Result type for URL parsing.
UrlResult :: struct {
    url:   ParsedUrl,
    error: string,
    ok:    bool,
}

// Create a successful UrlResult.
url_ok :: proc(url: ParsedUrl) -> UrlResult {
    return UrlResult{url = url, error = "", ok = true}
}

// Create an error UrlResult.
url_error :: proc(message: string) -> UrlResult {
    return UrlResult{url = {}, error = message, ok = false}
}

// Parse a URL string.
parse_url :: proc(url: string, allocator := context.allocator) -> UrlResult {
    s := strings.trim_space(url)
    if len(s) == 0 {
        return url_error("Empty URL")
    }

    // Extract scheme
    scheme_end := strings.index(s, "://")
    if scheme_end < 0 {
        return url_error("Missing scheme")
    }

    scheme := strings.to_lower(s[:scheme_end], allocator)
    rest := s[scheme_end + 3:]

    // Extract fragment
    fragment: Maybe(string) = nil
    fragment_idx := strings.last_index(rest, "#")
    if fragment_idx >= 0 {
        fragment = rest[fragment_idx + 1:]
        rest = rest[:fragment_idx]
    }

    // Extract query
    query: Maybe(string) = nil
    query_idx := strings.index(rest, "?")
    if query_idx >= 0 {
        query = rest[query_idx + 1:]
        rest = rest[:query_idx]
    }

    // Extract path
    path := "/"
    path_idx := strings.index(rest, "/")
    if path_idx >= 0 {
        path = rest[path_idx:]
        rest = rest[:path_idx]
    }

    // Extract userinfo
    username: Maybe(string) = nil
    password: Maybe(string) = nil
    at_idx := strings.last_index(rest, "@")
    if at_idx >= 0 {
        userinfo := rest[:at_idx]
        rest = rest[at_idx + 1:]

        colon_idx := strings.index(userinfo, ":")
        if colon_idx >= 0 {
            username = userinfo[:colon_idx]
            password = userinfo[colon_idx + 1:]
        } else {
            username = userinfo
        }
    }

    // Extract host and port
    host := rest
    port: Maybe(u16) = nil

    // Handle IPv6 addresses
    if len(host) > 0 && host[0] == '[' {
        bracket_end := strings.index(host, "]")
        if bracket_end < 0 {
            return url_error("Invalid IPv6 address")
        }

        if bracket_end + 1 < len(host) && host[bracket_end + 1] == ':' {
            port_str := host[bracket_end + 2:]
            port_val, port_ok := strconv.parse_int(port_str)
            if port_ok && port_val >= 1 && port_val <= 65535 {
                port = u16(port_val)
            }
        }
        host = host[1:bracket_end]
    } else {
        // Regular host:port
        colon_idx := strings.last_index(host, ":")
        if colon_idx >= 0 {
            port_str := host[colon_idx + 1:]
            port_val, port_ok := strconv.parse_int(port_str)
            if port_ok && port_val >= 1 && port_val <= 65535 {
                port = u16(port_val)
                host = host[:colon_idx]
            }
        }
    }

    return url_ok(ParsedUrl{
        scheme = scheme,
        username = username,
        password = password,
        host = host,
        port = port,
        path = path,
        query = query,
        fragment = fragment,
    })
}

// Check if a URL is valid.
is_valid_url :: proc(url: string) -> bool {
    result := parse_url(url)
    return result.ok
}

// Get the domain from a URL.
get_url_domain :: proc(url: string) -> (domain: string, ok: bool) {
    result := parse_url(url)
    if !result.ok {
        return "", false
    }
    return result.url.host, true
}

// Get the scheme from a URL.
get_url_scheme :: proc(url: string) -> (scheme: string, ok: bool) {
    result := parse_url(url)
    if !result.ok {
        return "", false
    }
    return result.url.scheme, true
}

// Get the path from a URL.
get_url_path :: proc(url: string) -> (path: string, ok: bool) {
    result := parse_url(url)
    if !result.ok {
        return "", false
    }
    return result.url.path, true
}

// Check if URL uses HTTPS.
is_https :: proc(url: string) -> bool {
    scheme, ok := get_url_scheme(url)
    return ok && scheme == "https"
}

// Check if URL uses HTTP.
is_http :: proc(url: string) -> bool {
    scheme, ok := get_url_scheme(url)
    return ok && (scheme == "http" || scheme == "https")
}

// Build a URL from components.
build_url :: proc(
    scheme: string,
    host: string,
    port: Maybe(u16) = nil,
    path: string = "/",
    query: Maybe(string) = nil,
    fragment: Maybe(string) = nil,
    allocator := context.allocator,
) -> string {
    builder := strings.builder_make(allocator)

    strings.write_string(&builder, scheme)
    strings.write_string(&builder, "://")
    strings.write_string(&builder, host)

    if p, has_port := port.?; has_port {
        strings.write_string(&builder, ":")
        strings.write_int(&builder, int(p))
    }

    strings.write_string(&builder, path)

    if q, has_query := query.?; has_query {
        strings.write_string(&builder, "?")
        strings.write_string(&builder, q)
    }

    if f, has_fragment := fragment.?; has_fragment {
        strings.write_string(&builder, "#")
        strings.write_string(&builder, f)
    }

    return strings.to_string(builder)
}

// Normalize a URL (lowercase scheme and host).
normalize_url :: proc(url: string, allocator := context.allocator) -> (result: string, ok: bool) {
    parsed := parse_url(url, allocator)
    if !parsed.ok {
        return "", false
    }

    u := parsed.url
    return build_url(u.scheme, strings.to_lower(u.host, allocator), u.port, u.path, u.query, u.fragment, allocator), true
}

// Join a base URL with a relative path.
join_url :: proc(base: string, relative: string, allocator := context.allocator) -> (result: string, ok: bool) {
    parsed := parse_url(base, allocator)
    if !parsed.ok {
        return "", false
    }

    u := parsed.url

    // Handle absolute relative URLs
    if strings.has_prefix(relative, "//") {
        return strings.concatenate({u.scheme, ":", relative}, allocator), true
    }
    if strings.contains(relative, "://") {
        return relative, true
    }

    // Handle relative paths
    new_path := relative
    if !strings.has_prefix(relative, "/") {
        // Get directory of current path
        last_slash := strings.last_index(u.path, "/")
        if last_slash >= 0 {
            new_path = strings.concatenate({u.path[:last_slash + 1], relative}, allocator)
        } else {
            new_path = strings.concatenate({"/", relative}, allocator)
        }
    }

    return build_url(u.scheme, u.host, u.port, new_path, nil, nil, allocator), true
}
