-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe email validation and parsing operations.
--- @module proven.safe_email

local safe_email = {}

--- Check if an email address is valid (basic check).
--- @param email string
--- @return boolean
function safe_email.is_valid(email)
    local at_pos = email:find("@")
    if not at_pos then
        return false
    end

    -- Check for multiple @ symbols
    if email:find("@", at_pos + 1) then
        return false
    end

    local local_part = email:sub(1, at_pos - 1)
    local domain = email:sub(at_pos + 1)

    if #local_part == 0 then
        return false
    end

    if #domain < 3 then
        return false
    end

    if not domain:find("%.") then
        return false
    end

    if domain:sub(1, 1) == "." or domain:sub(-1) == "." then
        return false
    end

    return true
end

--- Split an email into local part and domain.
--- @param email string
--- @return table|nil Returns {local_part=..., domain=...} or nil if invalid
function safe_email.split(email)
    if not safe_email.is_valid(email) then
        return nil
    end

    local at_pos = email:find("@")
    return {
        local_part = email:sub(1, at_pos - 1),
        domain = email:sub(at_pos + 1)
    }
end

--- Extract the domain from an email address.
--- @param email string
--- @return string|nil
function safe_email.get_domain(email)
    local parts = safe_email.split(email)
    if parts then
        return parts.domain
    end
    return nil
end

--- Extract the local part from an email address.
--- @param email string
--- @return string|nil
function safe_email.get_local_part(email)
    local parts = safe_email.split(email)
    if parts then
        return parts.local_part
    end
    return nil
end

--- Normalize an email address (lowercase domain).
--- @param email string
--- @return string|nil
function safe_email.normalize(email)
    local parts = safe_email.split(email)
    if not parts then
        return nil
    end
    return parts.local_part .. "@" .. parts.domain:lower()
end

return safe_email
