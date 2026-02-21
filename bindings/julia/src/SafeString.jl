# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeString

Safe string operations for escaping and sanitization.
"""
module SafeString

export escape_html, escape_sql, escape_js, escape_url, truncate_safe

"""
    escape_html(s::AbstractString) -> String

Escape a string for safe HTML insertion.
"""
function escape_html(s::AbstractString)::String
    s |>
    x -> replace(x, "&" => "&amp;") |>
    x -> replace(x, "<" => "&lt;") |>
    x -> replace(x, ">" => "&gt;") |>
    x -> replace(x, "\"" => "&quot;") |>
    x -> replace(x, "'" => "&#x27;")
end

"""
    escape_sql(s::AbstractString) -> String

Escape a string for safe SQL interpolation.
Note: Prefer parameterized queries over string interpolation.
"""
function escape_sql(s::AbstractString)::String
    replace(s, "'" => "''")
end

"""
    escape_js(s::AbstractString) -> String

Escape a string for safe JavaScript string literal insertion.
"""
function escape_js(s::AbstractString)::String
    s |>
    x -> replace(x, "\\" => "\\\\") |>
    x -> replace(x, "\"" => "\\\"") |>
    x -> replace(x, "'" => "\\'") |>
    x -> replace(x, "\n" => "\\n") |>
    x -> replace(x, "\r" => "\\r") |>
    x -> replace(x, "\t" => "\\t")
end

"""
    escape_url(s::AbstractString) -> String

Percent-encode a string for safe URL inclusion.
"""
function escape_url(s::AbstractString)::String
    # Safe characters that don't need encoding
    safe_chars = Set("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~")

    result = IOBuffer()
    for c in s
        if c in safe_chars
            write(result, c)
        else
            for byte in codeunits(string(c))
                write(result, "%", uppercase(string(byte, base=16, pad=2)))
            end
        end
    end
    String(take!(result))
end

"""
    truncate_safe(s::AbstractString, max_length::Integer; suffix::AbstractString="...") -> String

Safely truncate a string to a maximum length, respecting UTF-8 boundaries.
"""
function truncate_safe(s::AbstractString, max_length::Integer; suffix::AbstractString="...")::String
    max_length < 0 && return ""
    length(s) <= max_length && return String(s)

    suffix_len = length(suffix)
    max_length <= suffix_len && return String(first(s, max_length))

    String(first(s, max_length - suffix_len)) * suffix
end

end # module
