-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe UUID generation and validation following RFC 4122.
--- @module proven.safe_uuid

local safe_uuid = {}

--- UUID version types.
safe_uuid.Version = {
    V1 = 1,   -- Time-based
    V2 = 2,   -- DCE Security
    V3 = 3,   -- Name-based (MD5)
    V4 = 4,   -- Random
    V5 = 5,   -- Name-based (SHA-1)
    NIL = 0,  -- Nil UUID
}

--- UUID variant types.
safe_uuid.Variant = {
    NCS = "ncs",
    RFC4122 = "rfc4122",
    MICROSOFT = "microsoft",
    FUTURE = "future",
}

--- Namespace UUIDs (per RFC 4122).
safe_uuid.NAMESPACE_DNS = {
    0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
}

safe_uuid.NAMESPACE_URL = {
    0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
    0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
}

--- NIL UUID (all zeros).
safe_uuid.NIL = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

-- UUID metatable for OOP behavior.
local Uuid = {}
Uuid.__index = Uuid

--- Create a new UUID object from bytes.
--- @param bytes table Array of 16 bytes
--- @return table UUID object
function Uuid.new(bytes)
    if type(bytes) ~= "table" or #bytes ~= 16 then
        return nil, "UUID requires exactly 16 bytes"
    end
    local self = setmetatable({}, Uuid)
    self._bytes = {}
    for i = 1, 16 do
        local byte_value = bytes[i]
        if type(byte_value) ~= "number" or byte_value < 0 or byte_value > 255 then
            return nil, "Invalid byte value at index " .. i
        end
        self._bytes[i] = math.floor(byte_value)
    end
    return self
end

--- Get the UUID bytes.
--- @return table Array of 16 bytes
function Uuid:bytes()
    local copy = {}
    for i = 1, 16 do
        copy[i] = self._bytes[i]
    end
    return copy
end

--- Get the UUID version.
--- @return number Version number (0-5)
function Uuid:version()
    local version_nibble = math.floor(self._bytes[7] / 16) % 16
    if version_nibble >= 1 and version_nibble <= 5 then
        return version_nibble
    end
    return safe_uuid.Version.NIL
end

--- Get the UUID variant.
--- @return string Variant identifier
function Uuid:variant()
    local byte8 = self._bytes[9]
    local high_bit = math.floor(byte8 / 128)
    if high_bit == 0 then
        return safe_uuid.Variant.NCS
    end
    local high_two_bits = math.floor(byte8 / 64)
    if high_two_bits == 2 then
        return safe_uuid.Variant.RFC4122
    end
    local high_three_bits = math.floor(byte8 / 32)
    if high_three_bits == 6 then
        return safe_uuid.Variant.MICROSOFT
    end
    return safe_uuid.Variant.FUTURE
end

--- Check if this is the nil UUID.
--- @return boolean
function Uuid:is_nil()
    for i = 1, 16 do
        if self._bytes[i] ~= 0 then
            return false
        end
    end
    return true
end

--- Format as canonical string (8-4-4-4-12).
--- @return string
function Uuid:format()
    local hex_chars = {}
    for i = 1, 16 do
        hex_chars[i] = string.format("%02x", self._bytes[i])
    end
    return table.concat(hex_chars, "", 1, 4) .. "-" ..
           table.concat(hex_chars, "", 5, 6) .. "-" ..
           table.concat(hex_chars, "", 7, 8) .. "-" ..
           table.concat(hex_chars, "", 9, 10) .. "-" ..
           table.concat(hex_chars, "", 11, 16)
end

--- Format as URN.
--- @return string
function Uuid:to_urn()
    return "urn:uuid:" .. self:format()
end

--- String representation.
function Uuid:__tostring()
    return self:format()
end

--- Equality comparison.
function Uuid:__eq(other)
    if getmetatable(other) ~= Uuid then
        return false
    end
    for i = 1, 16 do
        if self._bytes[i] ~= other._bytes[i] then
            return false
        end
    end
    return true
end

-- Expose the Uuid class
safe_uuid.Uuid = Uuid

--- Parse UUID from canonical string format.
--- @param uuid_string string The UUID string (8-4-4-4-12 format)
--- @return table|nil UUID object or nil on error
--- @return string|nil Error message if parsing failed
function safe_uuid.parse(uuid_string)
    if type(uuid_string) ~= "string" then
        return nil, "UUID must be a string"
    end

    if #uuid_string ~= 36 then
        return nil, "UUID must be 36 characters, got " .. #uuid_string
    end

    -- Validate hyphen positions
    if uuid_string:sub(9, 9) ~= "-" or
       uuid_string:sub(14, 14) ~= "-" or
       uuid_string:sub(19, 19) ~= "-" or
       uuid_string:sub(24, 24) ~= "-" then
        return nil, "Invalid UUID format: hyphens must be at positions 9, 14, 19, 24"
    end

    -- Remove hyphens and validate hex
    local hex_string = uuid_string:gsub("-", "")
    if #hex_string ~= 32 then
        return nil, "Invalid UUID length after removing hyphens"
    end

    -- Validate all characters are hex
    if not hex_string:match("^[0-9a-fA-F]+$") then
        return nil, "Invalid hex character in UUID"
    end

    -- Convert to bytes
    local bytes = {}
    for i = 1, 16 do
        local byte_hex = hex_string:sub(i * 2 - 1, i * 2)
        bytes[i] = tonumber(byte_hex, 16)
    end

    return Uuid.new(bytes)
end

--- Generate a v4 (random) UUID from provided random bytes.
--- @param random_bytes table Array of 16 random bytes
--- @return table|nil UUID object or nil on error
--- @return string|nil Error message if creation failed
function safe_uuid.v4_from_bytes(random_bytes)
    if type(random_bytes) ~= "table" or #random_bytes ~= 16 then
        return nil, "v4_from_bytes requires exactly 16 random bytes"
    end

    local bytes = {}
    for i = 1, 16 do
        bytes[i] = random_bytes[i]
    end

    -- Set version to 4 (bits 4-7 of byte 7 = 0100)
    local byte7_high = math.floor(bytes[7] / 16) % 16
    local byte7_low = bytes[7] % 16
    bytes[7] = 0x40 + byte7_low  -- 0100 in high nibble

    -- Set variant to RFC 4122 (bits 6-7 of byte 9 = 10)
    local byte9_masked = bytes[9] % 64  -- Clear top 2 bits
    bytes[9] = 0x80 + byte9_masked  -- 10 in high 2 bits

    return Uuid.new(bytes)
end

--- Check if string is valid UUID format.
--- @param uuid_string string The string to validate
--- @return boolean
function safe_uuid.is_valid(uuid_string)
    local uuid, _ = safe_uuid.parse(uuid_string)
    return uuid ~= nil
end

--- Create a nil UUID.
--- @return table UUID object
function safe_uuid.nil_uuid()
    return Uuid.new(safe_uuid.NIL)
end

--- Create UUID from bytes.
--- @param bytes table Array of 16 bytes
--- @return table|nil UUID object or nil on error
--- @return string|nil Error message if creation failed
function safe_uuid.from_bytes(bytes)
    return Uuid.new(bytes)
end

return safe_uuid
