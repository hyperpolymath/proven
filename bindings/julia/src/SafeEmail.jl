# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeEmail

Safe email validation and parsing operations.
"""
module SafeEmail

export EmailParts, is_valid, split_email, get_domain, get_local_part, normalize

"""
    EmailParts

Represents the parts of an email address.
"""
struct EmailParts
    local_part::String
    domain::String
end

"""
    is_valid(email::AbstractString) -> Bool

Check if an email address is valid (basic check).
"""
function is_valid(email::AbstractString)::Bool
    parts = split(email, "@")
    length(parts) != 2 && return false

    local_part, domain = parts
    length(local_part) == 0 && return false
    length(domain) < 3 && return false
    !contains(domain, ".") && return false
    startswith(domain, ".") && return false
    endswith(domain, ".") && return false

    true
end

"""
    split_email(email::AbstractString) -> Union{EmailParts, Nothing}

Split an email into local part and domain.
"""
function split_email(email::AbstractString)::Union{EmailParts, Nothing}
    !is_valid(email) && return nothing

    parts = split(email, "@")
    EmailParts(String(parts[1]), String(parts[2]))
end

"""
    get_domain(email::AbstractString) -> Union{String, Nothing}

Extract the domain from an email address.
"""
function get_domain(email::AbstractString)::Union{String, Nothing}
    parts = split_email(email)
    parts === nothing ? nothing : parts.domain
end

"""
    get_local_part(email::AbstractString) -> Union{String, Nothing}

Extract the local part from an email address.
"""
function get_local_part(email::AbstractString)::Union{String, Nothing}
    parts = split_email(email)
    parts === nothing ? nothing : parts.local_part
end

"""
    normalize(email::AbstractString) -> Union{String, Nothing}

Normalize an email address (lowercase domain).
"""
function normalize(email::AbstractString)::Union{String, Nothing}
    parts = split_email(email)
    parts === nothing && return nothing
    parts.local_part * "@" * lowercase(parts.domain)
end

end # module
