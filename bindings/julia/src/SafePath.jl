# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafePath

Safe filesystem path operations with traversal attack prevention.
"""
module SafePath

export has_traversal, is_safe, sanitize_filename, safe_join

"""
    has_traversal(path::AbstractString) -> Bool

Check if a path contains directory traversal sequences.
"""
function has_traversal(path::AbstractString)::Bool
    contains(path, "..") || contains(path, "~")
end

"""
    is_safe(path::AbstractString) -> Bool

Check if a path is safe (no traversal attacks).
"""
function is_safe(path::AbstractString)::Bool
    !has_traversal(path)
end

"""
    sanitize_filename(filename::AbstractString) -> String

Sanitize a filename by removing dangerous characters.
"""
function sanitize_filename(filename::AbstractString)::String
    filename |>
    x -> replace(x, ".." => "_") |>
    x -> replace(x, "/" => "_") |>
    x -> replace(x, "\\" => "_") |>
    x -> replace(x, "<" => "_") |>
    x -> replace(x, ">" => "_") |>
    x -> replace(x, ":" => "_") |>
    x -> replace(x, "\"" => "_") |>
    x -> replace(x, "|" => "_") |>
    x -> replace(x, "?" => "_") |>
    x -> replace(x, "*" => "_") |>
    x -> replace(x, "\0" => "_")
end

"""
    safe_join(base::AbstractString, parts::AbstractVector{<:AbstractString}) -> Union{String, Nothing}

Safely join path components, rejecting traversal attempts.
Returns `nothing` if any part contains traversal sequences.
"""
function safe_join(base::AbstractString, parts::AbstractVector{<:AbstractString})::Union{String, Nothing}
    any(has_traversal, parts) && return nothing

    sanitized = map(sanitize_filename, parts)
    path = base
    for part in sanitized
        path = endswith(path, "/") ? path * part : path * "/" * part
    end
    path
end

"""
    safe_join(base::AbstractString, parts::AbstractString...) -> Union{String, Nothing}

Variadic version of safe_join.
"""
function safe_join(base::AbstractString, parts::AbstractString...)::Union{String, Nothing}
    safe_join(base, collect(parts))
end

end # module
