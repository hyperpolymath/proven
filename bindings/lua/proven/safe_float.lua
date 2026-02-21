-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe floating-point operations with NaN and infinity handling.
--- @module proven.safe_float

local safe_float = {}

-- Constants
safe_float.EPSILON = 2.2204460492503131e-16
safe_float.MAX_VALUE = 1.7976931348623157e+308
safe_float.MIN_VALUE = 2.2250738585072014e-308
safe_float.NAN_REPLACEMENT = 0
safe_float.INF_REPLACEMENT = safe_float.MAX_VALUE

--- Check if a number is NaN.
--- @param value number The value to check
--- @return boolean is_nan Whether the value is NaN
function safe_float.is_nan(value)
    return value ~= value
end

--- Check if a number is infinite.
--- @param value number The value to check
--- @return boolean is_infinite Whether the value is infinite
function safe_float.is_infinite(value)
    return value == math.huge or value == -math.huge
end

--- Check if a number is finite (not NaN or infinite).
--- @param value number The value to check
--- @return boolean is_finite Whether the value is finite
function safe_float.is_finite(value)
    return type(value) == "number" and not safe_float.is_nan(value) and not safe_float.is_infinite(value)
end

--- Sanitize a number, replacing NaN and infinity with safe values.
--- @param value number The value to sanitize
--- @param nan_replacement number|nil Replacement for NaN (default 0)
--- @param inf_replacement number|nil Replacement for infinity (default MAX_VALUE)
--- @return number sanitized The sanitized value
function safe_float.sanitize(value, nan_replacement, inf_replacement)
    nan_replacement = nan_replacement or safe_float.NAN_REPLACEMENT
    inf_replacement = inf_replacement or safe_float.INF_REPLACEMENT

    if safe_float.is_nan(value) then
        return nan_replacement
    elseif value == math.huge then
        return inf_replacement
    elseif value == -math.huge then
        return -inf_replacement
    else
        return value
    end
end

--- Safe division that handles division by zero.
--- @param numerator number The numerator
--- @param denominator number The denominator
--- @param default number|nil Default value on division by zero (default 0)
--- @return number|nil result The result or nil if invalid
function safe_float.div(numerator, denominator, default)
    if denominator == 0 then
        return default or 0
    end
    local result = numerator / denominator
    return safe_float.sanitize(result)
end

--- Compare two floats with epsilon tolerance.
--- @param a number First value
--- @param b number Second value
--- @param epsilon number|nil Tolerance (default EPSILON)
--- @return boolean equal Whether the values are approximately equal
function safe_float.approx_equal(a, b, epsilon)
    epsilon = epsilon or safe_float.EPSILON

    if safe_float.is_nan(a) or safe_float.is_nan(b) then
        return false
    end

    local diff = math.abs(a - b)
    if diff <= epsilon then
        return true
    end

    local max_abs = math.max(math.abs(a), math.abs(b))
    return diff <= epsilon * max_abs
end

--- Clamp a value to a range.
--- @param value number The value to clamp
--- @param min_val number Minimum value
--- @param max_val number Maximum value
--- @return number clamped The clamped value
function safe_float.clamp(value, min_val, max_val)
    if safe_float.is_nan(value) then
        return min_val
    end
    return math.max(min_val, math.min(max_val, value))
end

--- Linear interpolation between two values.
--- @param a number Start value
--- @param b number End value
--- @param t number Interpolation factor (0-1)
--- @return number result The interpolated value
function safe_float.lerp(a, b, t)
    t = safe_float.clamp(t, 0, 1)
    return a + (b - a) * t
end

--- Inverse linear interpolation.
--- @param a number Start value
--- @param b number End value
--- @param value number Value to find t for
--- @return number|nil t The interpolation factor or nil if a == b
function safe_float.inverse_lerp(a, b, value)
    if a == b then
        return nil
    end
    return safe_float.clamp((value - a) / (b - a), 0, 1)
end

--- Remap a value from one range to another.
--- @param value number The value to remap
--- @param from_min number Source range minimum
--- @param from_max number Source range maximum
--- @param to_min number Target range minimum
--- @param to_max number Target range maximum
--- @return number remapped The remapped value
function safe_float.remap(value, from_min, from_max, to_min, to_max)
    local t = safe_float.inverse_lerp(from_min, from_max, value)
    if t == nil then
        return to_min
    end
    return safe_float.lerp(to_min, to_max, t)
end

--- Round to a specific number of decimal places.
--- @param value number The value to round
--- @param decimals number Number of decimal places
--- @return number rounded The rounded value
function safe_float.round(value, decimals)
    if safe_float.is_nan(value) or safe_float.is_infinite(value) then
        return safe_float.sanitize(value)
    end
    local factor = 10 ^ decimals
    return math.floor(value * factor + 0.5) / factor
end

--- Floor to a specific number of decimal places.
--- @param value number The value to floor
--- @param decimals number Number of decimal places
--- @return number floored The floored value
function safe_float.floor(value, decimals)
    if safe_float.is_nan(value) or safe_float.is_infinite(value) then
        return safe_float.sanitize(value)
    end
    local factor = 10 ^ decimals
    return math.floor(value * factor) / factor
end

--- Ceiling to a specific number of decimal places.
--- @param value number The value to ceil
--- @param decimals number Number of decimal places
--- @return number ceiled The ceiled value
function safe_float.ceil(value, decimals)
    if safe_float.is_nan(value) or safe_float.is_infinite(value) then
        return safe_float.sanitize(value)
    end
    local factor = 10 ^ decimals
    return math.ceil(value * factor) / factor
end

--- Safe square root (returns 0 for negative numbers).
--- @param value number The value
--- @return number result The square root or 0
function safe_float.sqrt(value)
    if value < 0 then
        return 0
    end
    return math.sqrt(value)
end

--- Safe logarithm (returns nil for non-positive numbers).
--- @param value number The value
--- @param base number|nil The base (default e)
--- @return number|nil result The logarithm or nil
function safe_float.log(value, base)
    if value <= 0 then
        return nil
    end
    if base then
        return math.log(value) / math.log(base)
    end
    return math.log(value)
end

--- Safe power that handles edge cases.
--- @param base number The base
--- @param exponent number The exponent
--- @return number result The result (sanitized)
function safe_float.pow(base, exponent)
    local result = base ^ exponent
    return safe_float.sanitize(result)
end

--- Get the sign of a number (-1, 0, or 1).
--- @param value number The value
--- @return number sign The sign
function safe_float.sign(value)
    if safe_float.is_nan(value) then
        return 0
    elseif value > 0 then
        return 1
    elseif value < 0 then
        return -1
    else
        return 0
    end
end

--- Wrap a value to a range.
--- @param value number The value to wrap
--- @param min_val number Minimum value
--- @param max_val number Maximum value
--- @return number wrapped The wrapped value
function safe_float.wrap(value, min_val, max_val)
    if safe_float.is_nan(value) then
        return min_val
    end
    local range = max_val - min_val
    if range <= 0 then
        return min_val
    end
    return min_val + ((value - min_val) % range + range) % range
end

--- Calculate the average of a list of numbers.
--- @param values table List of numbers
--- @return number|nil average The average or nil if empty
function safe_float.average(values)
    if type(values) ~= "table" or #values == 0 then
        return nil
    end

    local sum = 0
    local count = 0
    for _, v in ipairs(values) do
        if type(v) == "number" and safe_float.is_finite(v) then
            sum = sum + v
            count = count + 1
        end
    end

    if count == 0 then
        return nil
    end
    return sum / count
end

--- Calculate the standard deviation of a list of numbers.
--- @param values table List of numbers
--- @return number|nil stddev The standard deviation or nil
function safe_float.stddev(values)
    local avg = safe_float.average(values)
    if avg == nil then
        return nil
    end

    local sum_sq = 0
    local count = 0
    for _, v in ipairs(values) do
        if type(v) == "number" and safe_float.is_finite(v) then
            sum_sq = sum_sq + (v - avg) ^ 2
            count = count + 1
        end
    end

    if count == 0 then
        return nil
    end
    return math.sqrt(sum_sq / count)
end

return safe_float
