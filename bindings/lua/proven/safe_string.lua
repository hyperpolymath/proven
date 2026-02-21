-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe string operations for escaping and sanitization.
--- @module proven.safe_string

local safe_string = {}

--- Escape a string for safe HTML insertion.
--- @param value string
--- @return string
function safe_string.escape_html(value)
    return value
        :gsub("&", "&amp;")
        :gsub("<", "&lt;")
        :gsub(">", "&gt;")
        :gsub('"', "&quot;")
        :gsub("'", "&#x27;")
end

--- Escape a string for safe SQL interpolation.
--- Note: Prefer parameterized queries over string interpolation.
--- @param value string
--- @return string
function safe_string.escape_sql(value)
    return value:gsub("'", "''")
end

--- Escape a string for safe JavaScript string literal insertion.
--- @param value string
--- @return string
function safe_string.escape_js(value)
    return value
        :gsub("\\", "\\\\")
        :gsub('"', '\\"')
        :gsub("'", "\\'")
        :gsub("\n", "\\n")
        :gsub("\r", "\\r")
        :gsub("\t", "\\t")
end

--- Percent-encode a string for safe URL inclusion.
--- @param value string
--- @return string
function safe_string.escape_url(value)
    return value:gsub("([^%w%-%.%_%~])", function(c)
        return string.format("%%%02X", string.byte(c))
    end)
end

--- Safely truncate a string to a maximum length.
--- @param value string
--- @param max_length number
--- @param suffix string|nil (default "...")
--- @return string
function safe_string.truncate_safe(value, max_length, suffix)
    suffix = suffix or "..."

    if max_length < 0 then
        return ""
    end

    if #value <= max_length then
        return value
    end

    local suffix_len = #suffix
    if max_length <= suffix_len then
        return value:sub(1, max_length)
    end

    return value:sub(1, max_length - suffix_len) .. suffix
end

return safe_string
