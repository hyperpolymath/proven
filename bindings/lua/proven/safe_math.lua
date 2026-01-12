-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe arithmetic operations that cannot crash or overflow unexpectedly.
--- @module proven.safe_math

local safe_math = {}

-- Lua 5.3+ integer limits (simulated for bounded operations)
local MAX_INT = 9223372036854775807
local MIN_INT = -9223372036854775808

--- Safely divide two integers, returning nil on division by zero.
--- @param numerator number
--- @param denominator number
--- @return number|nil
function safe_math.div(numerator, denominator)
    if denominator == 0 then
        return nil
    end
    return math.floor(numerator / denominator)
end

--- Safely compute modulo, returning nil on division by zero.
--- @param numerator number
--- @param denominator number
--- @return number|nil
function safe_math.mod(numerator, denominator)
    if denominator == 0 then
        return nil
    end
    return numerator % denominator
end

--- Safely add two integers, returning nil on overflow.
--- @param a number
--- @param b number
--- @return number|nil
function safe_math.add(a, b)
    local result = a + b
    if result > MAX_INT or result < MIN_INT then
        return nil
    end
    return result
end

--- Safely subtract two integers, returning nil on overflow.
--- @param a number
--- @param b number
--- @return number|nil
function safe_math.sub(a, b)
    local result = a - b
    if result > MAX_INT or result < MIN_INT then
        return nil
    end
    return result
end

--- Safely multiply two integers, returning nil on overflow.
--- @param a number
--- @param b number
--- @return number|nil
function safe_math.mul(a, b)
    local result = a * b
    if result > MAX_INT or result < MIN_INT then
        return nil
    end
    return result
end

--- Checked add that raises error on overflow.
--- @param a number
--- @param b number
--- @return number
function safe_math.checked_add(a, b)
    local result = safe_math.add(a, b)
    if result == nil then
        error("integer overflow")
    end
    return result
end

--- Checked subtract that raises error on overflow.
--- @param a number
--- @param b number
--- @return number
function safe_math.checked_sub(a, b)
    local result = safe_math.sub(a, b)
    if result == nil then
        error("integer overflow")
    end
    return result
end

--- Checked multiply that raises error on overflow.
--- @param a number
--- @param b number
--- @return number
function safe_math.checked_mul(a, b)
    local result = safe_math.mul(a, b)
    if result == nil then
        error("integer overflow")
    end
    return result
end

return safe_math
