-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe hexadecimal encoding and decoding.
--- @module proven.safe_hex

local safe_hex = {}

-- Lookup tables for fast encoding
local hex_chars_lower = "0123456789abcdef"
local hex_chars_upper = "0123456789ABCDEF"

-- Lookup table for decoding: character -> nibble value
local char_to_nibble = {}
for i = 0, 9 do
    char_to_nibble[string.char(48 + i)] = i  -- '0'-'9'
end
for i = 0, 5 do
    char_to_nibble[string.char(97 + i)] = 10 + i  -- 'a'-'f'
    char_to_nibble[string.char(65 + i)] = 10 + i  -- 'A'-'F'
end

--- Check if character is a valid hex character.
--- @param c string Single character
--- @return boolean
function safe_hex.is_hex_char(c)
    if type(c) ~= "string" or #c ~= 1 then
        return false
    end
    return char_to_nibble[c] ~= nil
end

--- Convert hex character to nibble value (0-15).
--- @param c string Single hex character
--- @return number|nil Nibble value or nil if invalid
function safe_hex.hex_char_to_nibble(c)
    if type(c) ~= "string" or #c ~= 1 then
        return nil
    end
    return char_to_nibble[c]
end

--- Convert nibble value to lowercase hex character.
--- @param n number Nibble value (0-15)
--- @return string|nil Hex character or nil if invalid
function safe_hex.nibble_to_hex_char(n)
    if type(n) ~= "number" or n < 0 or n > 15 then
        return nil
    end
    return hex_chars_lower:sub(n + 1, n + 1)
end

--- Convert nibble value to uppercase hex character.
--- @param n number Nibble value (0-15)
--- @return string|nil Hex character or nil if invalid
function safe_hex.nibble_to_hex_char_upper(n)
    if type(n) ~= "number" or n < 0 or n > 15 then
        return nil
    end
    return hex_chars_upper:sub(n + 1, n + 1)
end

--- Encode bytes to lowercase hex string.
--- @param bytes table Array of bytes (0-255)
--- @return string|nil Hex string or nil on error
--- @return string|nil Error message if encoding failed
function safe_hex.encode(bytes)
    if type(bytes) ~= "table" then
        return nil, "bytes must be a table"
    end

    local result = {}
    for i, byte_value in ipairs(bytes) do
        if type(byte_value) ~= "number" or byte_value < 0 or byte_value > 255 then
            return nil, "Invalid byte value at index " .. i
        end
        local high_nibble = math.floor(byte_value / 16)
        local low_nibble = byte_value % 16
        result[#result + 1] = hex_chars_lower:sub(high_nibble + 1, high_nibble + 1)
        result[#result + 1] = hex_chars_lower:sub(low_nibble + 1, low_nibble + 1)
    end

    return table.concat(result)
end

--- Encode bytes to uppercase hex string.
--- @param bytes table Array of bytes (0-255)
--- @return string|nil Hex string or nil on error
--- @return string|nil Error message if encoding failed
function safe_hex.encode_upper(bytes)
    if type(bytes) ~= "table" then
        return nil, "bytes must be a table"
    end

    local result = {}
    for i, byte_value in ipairs(bytes) do
        if type(byte_value) ~= "number" or byte_value < 0 or byte_value > 255 then
            return nil, "Invalid byte value at index " .. i
        end
        local high_nibble = math.floor(byte_value / 16)
        local low_nibble = byte_value % 16
        result[#result + 1] = hex_chars_upper:sub(high_nibble + 1, high_nibble + 1)
        result[#result + 1] = hex_chars_upper:sub(low_nibble + 1, low_nibble + 1)
    end

    return table.concat(result)
end

--- Encode string to hex (each character's byte value).
--- @param str string Input string
--- @return string|nil Hex string or nil on error
--- @return string|nil Error message if encoding failed
function safe_hex.encode_string(str)
    if type(str) ~= "string" then
        return nil, "Input must be a string"
    end

    local bytes = {}
    for i = 1, #str do
        bytes[i] = str:byte(i)
    end

    return safe_hex.encode(bytes)
end

--- Decode hex string to bytes.
--- @param hex_string string Hex string
--- @return table|nil Array of bytes or nil on error
--- @return string|nil Error message if decoding failed
function safe_hex.decode(hex_string)
    if type(hex_string) ~= "string" then
        return nil, "hex_string must be a string"
    end

    if #hex_string % 2 ~= 0 then
        return nil, "Hex string has odd length"
    end

    if #hex_string == 0 then
        return {}
    end

    local result = {}
    for i = 1, #hex_string, 2 do
        local high_char = hex_string:sub(i, i)
        local low_char = hex_string:sub(i + 1, i + 1)

        local high_nibble = char_to_nibble[high_char]
        local low_nibble = char_to_nibble[low_char]

        if not high_nibble then
            return nil, "Invalid hex character at position " .. i
        end
        if not low_nibble then
            return nil, "Invalid hex character at position " .. (i + 1)
        end

        result[#result + 1] = high_nibble * 16 + low_nibble
    end

    return result
end

--- Decode hex string to string.
--- @param hex_string string Hex string
--- @return string|nil Decoded string or nil on error
--- @return string|nil Error message if decoding failed
function safe_hex.decode_string(hex_string)
    local bytes, err = safe_hex.decode(hex_string)
    if not bytes then
        return nil, err
    end

    local chars = {}
    for i, byte_value in ipairs(bytes) do
        chars[i] = string.char(byte_value)
    end

    return table.concat(chars)
end

--- Validate hex string.
--- @param hex_string string Hex string to validate
--- @return boolean
function safe_hex.is_valid(hex_string)
    if type(hex_string) ~= "string" then
        return false
    end
    for i = 1, #hex_string do
        if not char_to_nibble[hex_string:sub(i, i)] then
            return false
        end
    end
    return true
end

--- Validate hex string with even length (valid bytes).
--- @param hex_string string Hex string to validate
--- @return boolean
function safe_hex.is_valid_bytes(hex_string)
    return #hex_string % 2 == 0 and safe_hex.is_valid(hex_string)
end

--- Constant-time comparison of two hex strings.
--- Note: This is a best-effort implementation in pure Lua.
--- For true constant-time guarantees, use a C library binding.
--- @param a string First hex string
--- @param b string Second hex string
--- @return boolean
function safe_hex.constant_time_equal(a, b)
    if type(a) ~= "string" or type(b) ~= "string" then
        return false
    end

    if #a ~= #b then
        return false
    end

    if #a == 0 then
        return true
    end

    -- Normalize to lowercase for comparison
    local a_lower = a:lower()
    local b_lower = b:lower()

    local diff = 0
    for i = 1, #a_lower do
        -- XOR each byte and OR into result
        -- Use bitwise operations if available (Lua 5.3+) or simulate
        local byte_a = a_lower:byte(i)
        local byte_b = b_lower:byte(i)
        local xor_result
        if bit32 then
            xor_result = bit32.bxor(byte_a, byte_b)
            diff = bit32.bor(diff, xor_result)
        else
            -- Lua 5.3+ native bitwise operators
            xor_result = byte_a ~ byte_b
            diff = diff | xor_result
        end
    end

    return diff == 0
end

--- Format hex with spaces between bytes.
--- @param hex_string string Hex string
--- @return string|nil Formatted string or nil on error
--- @return string|nil Error message if formatting failed
function safe_hex.format_spaced(hex_string)
    if type(hex_string) ~= "string" then
        return nil, "hex_string must be a string"
    end

    if #hex_string % 2 ~= 0 then
        return nil, "Hex string has odd length"
    end

    if #hex_string == 0 then
        return ""
    end

    local parts = {}
    for i = 1, #hex_string, 2 do
        parts[#parts + 1] = hex_string:sub(i, i + 1)
    end

    return table.concat(parts, " ")
end

--- Format hex with colons between bytes (MAC address style).
--- @param hex_string string Hex string
--- @return string|nil Formatted string or nil on error
--- @return string|nil Error message if formatting failed
function safe_hex.format_colons(hex_string)
    if type(hex_string) ~= "string" then
        return nil, "hex_string must be a string"
    end

    if #hex_string % 2 ~= 0 then
        return nil, "Hex string has odd length"
    end

    if #hex_string == 0 then
        return ""
    end

    local parts = {}
    for i = 1, #hex_string, 2 do
        parts[#parts + 1] = hex_string:sub(i, i + 1)
    end

    return table.concat(parts, ":")
end

--- Convert integer to hex string with minimum width.
--- @param value number Integer value
--- @param min_width number Minimum width (padded with zeros)
--- @return string|nil Hex string or nil on error
--- @return string|nil Error message if conversion failed
function safe_hex.int_to_hex(value, min_width)
    if type(value) ~= "number" then
        return nil, "value must be a number"
    end
    if value < 0 then
        return nil, "value must be non-negative"
    end

    min_width = min_width or 1
    if type(min_width) ~= "number" or min_width < 1 then
        min_width = 1
    end

    local hex = string.format("%x", math.floor(value))
    if #hex < min_width then
        hex = string.rep("0", min_width - #hex) .. hex
    end

    return hex
end

--- Parse hex string to integer.
--- @param hex_string string Hex string
--- @return number|nil Integer value or nil on error
--- @return string|nil Error message if parsing failed
function safe_hex.hex_to_int(hex_string)
    if type(hex_string) ~= "string" then
        return nil, "hex_string must be a string"
    end

    if not safe_hex.is_valid(hex_string) then
        return nil, "Invalid hex string"
    end

    if #hex_string == 0 then
        return nil, "Empty hex string"
    end

    return tonumber(hex_string, 16)
end

return safe_hex
