-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe filesystem path operations with traversal attack prevention.
--- @module proven.safe_path

local safe_path = {}

--- Check if a path contains directory traversal sequences.
--- @param path string
--- @return boolean
function safe_path.has_traversal(path)
    return path:find("%.%.") ~= nil or path:find("~") ~= nil
end

--- Check if a path is safe (no traversal attacks).
--- @param path string
--- @return boolean
function safe_path.is_safe(path)
    return not safe_path.has_traversal(path)
end

--- Sanitize a filename by removing dangerous characters.
--- @param filename string
--- @return string
function safe_path.sanitize_filename(filename)
    return filename
        :gsub("%.%.", "_")
        :gsub("/", "_")
        :gsub("\\", "_")
        :gsub("<", "_")
        :gsub(">", "_")
        :gsub(":", "_")
        :gsub('"', "_")
        :gsub("|", "_")
        :gsub("%?", "_")
        :gsub("%*", "_")
        :gsub("%z", "_")
end

--- Safely join path components, rejecting traversal attempts.
--- @param base string
--- @param parts table Array of path components
--- @return string|nil Returns nil if traversal detected
function safe_path.safe_join(base, parts)
    for _, part in ipairs(parts) do
        if safe_path.has_traversal(part) then
            return nil
        end
    end

    local sanitized = {}
    for _, part in ipairs(parts) do
        table.insert(sanitized, safe_path.sanitize_filename(part))
    end

    local path = base
    for _, part in ipairs(sanitized) do
        if path:sub(-1) == "/" then
            path = path .. part
        else
            path = path .. "/" .. part
        end
    end

    return path
end

return safe_path
