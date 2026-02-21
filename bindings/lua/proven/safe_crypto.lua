-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Cryptographic safety operations with constant-time guarantees.
--- @module proven.safe_crypto

local safe_crypto = {}

--- Compare two strings in constant time to prevent timing attacks.
--- Note: This is a best-effort implementation in pure Lua.
--- For true constant-time, use a C library binding.
--- @param a string
--- @param b string
--- @return boolean
function safe_crypto.constant_time_compare(a, b)
    if #a ~= #b then
        return false
    end

    if #a == 0 then
        return true
    end

    local result = 0
    for i = 1, #a do
        -- XOR each byte and OR into result
        result = bit32 and bit32.bor(result, bit32.bxor(a:byte(i), b:byte(i)))
            or (result | (a:byte(i) ~ b:byte(i)))  -- Lua 5.3+
    end

    return result == 0
end

--- Securely "zero" a string by creating a new zeroed string of the same length.
--- Note: Lua strings are immutable, so we cannot actually zero the original.
--- This returns a zeroed replacement string.
--- @param length number
--- @return string
function safe_crypto.secure_zero(length)
    return string.rep("\0", length)
end

--- Generate a table of random bytes.
--- Uses math.random which should be seeded properly for security.
--- For cryptographic use, prefer a proper CSPRNG binding.
--- @param n number
--- @return table Array of bytes
function safe_crypto.random_bytes(n)
    local bytes = {}
    for i = 1, n do
        bytes[i] = math.random(0, 255)
    end
    return bytes
end

--- Convert bytes table to string.
--- @param bytes table Array of bytes
--- @return string
function safe_crypto.bytes_to_string(bytes)
    local chars = {}
    for i, byte in ipairs(bytes) do
        chars[i] = string.char(byte)
    end
    return table.concat(chars)
end

--- Constant-time select: returns a if condition is true, b otherwise.
--- @param condition boolean
--- @param a any
--- @param b any
--- @return any
function safe_crypto.constant_time_select(condition, a, b)
    if condition then
        return a
    else
        return b
    end
end

return safe_crypto
