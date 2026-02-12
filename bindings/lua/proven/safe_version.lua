-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe semantic version parsing and comparison.
--- @module proven.safe_version

local safe_version = {}

--- Version structure.
--- @class Version
--- @field major number Major version number
--- @field minor number Minor version number
--- @field patch number Patch version number
--- @field prerelease string|nil Prerelease identifier
--- @field build string|nil Build metadata

local Version = {}
Version.__index = Version

--- Create a new Version object.
--- @param major number Major version
--- @param minor number Minor version
--- @param patch number Patch version
--- @param prerelease string|nil Prerelease identifier
--- @param build string|nil Build metadata
--- @return Version|nil version The Version or nil if invalid
--- @return string|nil error Error message if validation failed
function safe_version.new(major, minor, patch, prerelease, build)
    if type(major) ~= "number" or major < 0 or major ~= math.floor(major) then
        return nil, "Invalid major version"
    end
    if type(minor) ~= "number" or minor < 0 or minor ~= math.floor(minor) then
        return nil, "Invalid minor version"
    end
    if type(patch) ~= "number" or patch < 0 or patch ~= math.floor(patch) then
        return nil, "Invalid patch version"
    end

    local self = setmetatable({}, Version)
    self.major = major
    self.minor = minor
    self.patch = patch
    self.prerelease = prerelease
    self.build = build

    return self
end

--- Parse a semantic version string.
--- @param version_string string The version string (e.g., "1.2.3-alpha+build")
--- @return Version|nil version The parsed Version or nil if invalid
--- @return string|nil error Error message if parsing failed
function safe_version.parse(version_string)
    if type(version_string) ~= "string" then
        return nil, "Version must be a string"
    end

    -- Remove leading 'v' if present
    local str = version_string:match("^v?(.+)$") or version_string

    -- Extract build metadata
    local build = nil
    local plus_pos = str:find("%+")
    if plus_pos then
        build = str:sub(plus_pos + 1)
        str = str:sub(1, plus_pos - 1)
    end

    -- Extract prerelease
    local prerelease = nil
    local dash_pos = str:find("%-")
    if dash_pos then
        prerelease = str:sub(dash_pos + 1)
        str = str:sub(1, dash_pos - 1)
    end

    -- Parse major.minor.patch
    local major, minor, patch = str:match("^(%d+)%.(%d+)%.(%d+)$")
    if not major then
        -- Try major.minor
        major, minor = str:match("^(%d+)%.(%d+)$")
        patch = "0"
    end
    if not major then
        -- Try major only
        major = str:match("^(%d+)$")
        minor = "0"
        patch = "0"
    end

    if not major then
        return nil, "Invalid version format"
    end

    return safe_version.new(tonumber(major), tonumber(minor), tonumber(patch), prerelease, build)
end

--- Convert Version to string.
--- @return string The version string
function Version:to_string()
    local result = string.format("%d.%d.%d", self.major, self.minor, self.patch)

    if self.prerelease then
        result = result .. "-" .. self.prerelease
    end
    if self.build then
        result = result .. "+" .. self.build
    end

    return result
end

--- Compare prerelease identifiers.
--- @param a string|nil First prerelease
--- @param b string|nil Second prerelease
--- @return number -1 if a < b, 0 if equal, 1 if a > b
local function compare_prerelease(a, b)
    -- No prerelease has higher precedence
    if a == nil and b == nil then return 0 end
    if a == nil then return 1 end
    if b == nil then return -1 end

    local a_parts = {}
    local b_parts = {}

    for part in a:gmatch("[^%.]+") do
        table.insert(a_parts, part)
    end
    for part in b:gmatch("[^%.]+") do
        table.insert(b_parts, part)
    end

    local max_len = math.max(#a_parts, #b_parts)
    for i = 1, max_len do
        local pa = a_parts[i]
        local pb = b_parts[i]

        if pa == nil then return -1 end
        if pb == nil then return 1 end

        local na = tonumber(pa)
        local nb = tonumber(pb)

        if na and nb then
            if na < nb then return -1 end
            if na > nb then return 1 end
        elseif na then
            return -1 -- Numbers have lower precedence
        elseif nb then
            return 1
        else
            if pa < pb then return -1 end
            if pa > pb then return 1 end
        end
    end

    return 0
end

--- Compare two versions.
--- @param other Version The other version
--- @return number -1 if self < other, 0 if equal, 1 if self > other
function Version:compare(other)
    if self.major ~= other.major then
        return self.major < other.major and -1 or 1
    end
    if self.minor ~= other.minor then
        return self.minor < other.minor and -1 or 1
    end
    if self.patch ~= other.patch then
        return self.patch < other.patch and -1 or 1
    end

    return compare_prerelease(self.prerelease, other.prerelease)
end

--- Check if version is less than another.
--- @param other Version The other version
--- @return boolean is_less Whether self < other
function Version:is_less_than(other)
    return self:compare(other) < 0
end

--- Check if version is greater than another.
--- @param other Version The other version
--- @return boolean is_greater Whether self > other
function Version:is_greater_than(other)
    return self:compare(other) > 0
end

--- Check if version equals another.
--- @param other Version The other version
--- @return boolean is_equal Whether versions are equal
function Version:equals(other)
    return self:compare(other) == 0
end

--- Check if version satisfies a constraint.
--- @param constraint string Version constraint (e.g., ">=1.0.0", "^1.2", "~1.2.3")
--- @return boolean satisfies Whether the version satisfies the constraint
function Version:satisfies(constraint)
    if type(constraint) ~= "string" then
        return false
    end

    -- Parse operator and version from constraint
    local op, ver_str = constraint:match("^([<>=~^]+)(.+)$")
    if not op then
        op = "="
        ver_str = constraint
    end

    local constraint_ver = safe_version.parse(ver_str)
    if not constraint_ver then
        return false
    end

    if op == "=" or op == "==" then
        return self:equals(constraint_ver)
    elseif op == ">" then
        return self:is_greater_than(constraint_ver)
    elseif op == ">=" then
        return not self:is_less_than(constraint_ver)
    elseif op == "<" then
        return self:is_less_than(constraint_ver)
    elseif op == "<=" then
        return not self:is_greater_than(constraint_ver)
    elseif op == "~" or op == "~=" then
        -- Approximately equivalent: same major.minor
        return self.major == constraint_ver.major and self.minor == constraint_ver.minor
    elseif op == "^" then
        -- Compatible: same major (for major >= 1)
        if constraint_ver.major >= 1 then
            return self.major == constraint_ver.major and not self:is_less_than(constraint_ver)
        else
            -- For 0.x, same minor
            return self.major == 0 and self.minor == constraint_ver.minor and not self:is_less_than(constraint_ver)
        end
    end

    return false
end

--- Increment major version.
--- @return Version The new version
function Version:bump_major()
    return safe_version.new(self.major + 1, 0, 0)
end

--- Increment minor version.
--- @return Version The new version
function Version:bump_minor()
    return safe_version.new(self.major, self.minor + 1, 0)
end

--- Increment patch version.
--- @return Version The new version
function Version:bump_patch()
    return safe_version.new(self.major, self.minor, self.patch + 1)
end

--- Check if version is a prerelease.
--- @return boolean is_prerelease Whether version is a prerelease
function Version:is_prerelease()
    return self.prerelease ~= nil
end

--- Check if version is stable (>= 1.0.0, no prerelease).
--- @return boolean is_stable Whether version is stable
function Version:is_stable()
    return self.major >= 1 and self.prerelease == nil
end

safe_version.Version = Version

--- Sort a list of version strings.
--- @param versions table List of version strings
--- @return table sorted Sorted list of version strings
function safe_version.sort(versions)
    local parsed = {}
    for i, v in ipairs(versions) do
        local ver = safe_version.parse(v)
        if ver then
            table.insert(parsed, {original = v, parsed = ver})
        end
    end

    table.sort(parsed, function(a, b)
        return a.parsed:is_less_than(b.parsed)
    end)

    local result = {}
    for _, v in ipairs(parsed) do
        table.insert(result, v.original)
    end
    return result
end

--- Get the maximum version from a list.
--- @param versions table List of version strings
--- @return string|nil max_version The maximum version string or nil
function safe_version.max(versions)
    local sorted = safe_version.sort(versions)
    return sorted[#sorted]
end

--- Get the minimum version from a list.
--- @param versions table List of version strings
--- @return string|nil min_version The minimum version string or nil
function safe_version.min(versions)
    local sorted = safe_version.sort(versions)
    return sorted[1]
end

return safe_version
