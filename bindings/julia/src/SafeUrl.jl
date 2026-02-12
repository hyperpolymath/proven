# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeUrl

Safe URL parsing and validation operations.
"""
module SafeUrl

export ParsedUrl, parse_url, is_valid_url, get_host, get_path, is_https

"""
    ParsedUrl

Represents the parsed components of a URL.
"""
struct ParsedUrl
    scheme::String
    host::String
    port::Union{Int, Nothing}
    path::String
    query::Union{String, Nothing}
    fragment::Union{String, Nothing}
end

"""
    parse_url(url::AbstractString) -> Union{ParsedUrl, Nothing}

Parse a URL into its components.
"""
function parse_url(url::AbstractString)::Union{ParsedUrl, Nothing}
    # Match scheme
    scheme_match = match(r"^([a-zA-Z][a-zA-Z0-9+.-]*)://", url)
    scheme_match === nothing && return nothing

    scheme = lowercase(scheme_match.captures[1])
    rest = url[length(scheme_match.match)+1:end]

    # Split off fragment
    fragment = nothing
    if contains(rest, "#")
        idx = findfirst('#', rest)
        fragment = rest[idx+1:end]
        rest = rest[1:idx-1]
    end

    # Split off query
    query = nothing
    if contains(rest, "?")
        idx = findfirst('?', rest)
        query = rest[idx+1:end]
        rest = rest[1:idx-1]
    end

    # Split host and path
    path = "/"
    if contains(rest, "/")
        idx = findfirst('/', rest)
        path = rest[idx:end]
        rest = rest[1:idx-1]
    end

    # Parse host and port
    port = nothing
    host = rest
    if contains(rest, ":")
        idx = findfirst(':', rest)
        host = rest[1:idx-1]
        port_str = rest[idx+1:end]
        port = tryparse(Int, port_str)
        port === nothing && return nothing
        (port < 0 || port > 65535) && return nothing
    end

    isempty(host) && return nothing

    ParsedUrl(scheme, host, port, path, query, fragment)
end

"""
    is_valid_url(url::AbstractString) -> Bool

Check if a string is a valid URL.
"""
function is_valid_url(url::AbstractString)::Bool
    parse_url(url) !== nothing
end

"""
    get_host(url::AbstractString) -> Union{String, Nothing}

Extract the host from a URL.
"""
function get_host(url::AbstractString)::Union{String, Nothing}
    parsed = parse_url(url)
    parsed === nothing ? nothing : parsed.host
end

"""
    get_path(url::AbstractString) -> Union{String, Nothing}

Extract the path from a URL.
"""
function get_path(url::AbstractString)::Union{String, Nothing}
    parsed = parse_url(url)
    parsed === nothing ? nothing : parsed.path
end

"""
    is_https(url::AbstractString) -> Bool

Check if a URL uses HTTPS.
"""
function is_https(url::AbstractString)::Bool
    parsed = parse_url(url)
    parsed !== nothing && parsed.scheme == "https"
end

end # module
