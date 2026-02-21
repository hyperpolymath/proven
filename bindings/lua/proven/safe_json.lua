-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe JSON parsing and generation with depth limits.
--- @module proven.safe_json

local safe_json = {}

-- Default limits
local DEFAULT_MAX_DEPTH = 100
local DEFAULT_MAX_STRING_LENGTH = 1048576 -- 1MB
local DEFAULT_MAX_KEYS = 10000

--- Encode a Lua value as JSON.
--- @param value any The value to encode
--- @param options table|nil Optional settings (pretty, max_depth)
--- @return string|nil json The JSON string or nil on error
--- @return string|nil error Error message if encoding failed
function safe_json.encode(value, options)
    options = options or {}
    local max_depth = options.max_depth or DEFAULT_MAX_DEPTH
    local pretty = options.pretty or false
    local indent = options.indent or "  "

    local function encode_value(val, depth, current_indent)
        if depth > max_depth then
            return nil, "Maximum depth exceeded"
        end

        local val_type = type(val)

        if val == nil then
            return "null"
        elseif val_type == "boolean" then
            return val and "true" or "false"
        elseif val_type == "number" then
            if val ~= val then -- NaN check
                return "null"
            elseif val == math.huge or val == -math.huge then
                return "null"
            else
                return tostring(val)
            end
        elseif val_type == "string" then
            return safe_json.encode_string(val)
        elseif val_type == "table" then
            return safe_json.encode_table(val, depth, max_depth, pretty, indent, current_indent)
        else
            return nil, "Cannot encode type: " .. val_type
        end
    end

    return encode_value(value, 0, "")
end

--- Encode a string as a JSON string literal.
--- @param str string The string to encode
--- @return string The JSON string literal
function safe_json.encode_string(str)
    local escaped = str
        :gsub("\\", "\\\\")
        :gsub("\"", "\\\"")
        :gsub("\n", "\\n")
        :gsub("\r", "\\r")
        :gsub("\t", "\\t")
        :gsub("[\x00-\x1f]", function(c)
            return string.format("\\u%04x", string.byte(c))
        end)
    return '"' .. escaped .. '"'
end

--- Encode a table as JSON (array or object).
--- @param tbl table The table to encode
--- @param depth number Current depth
--- @param max_depth number Maximum allowed depth
--- @param pretty boolean Whether to format output
--- @param indent string Indentation string
--- @param current_indent string Current indentation level
--- @return string|nil json The JSON string or nil on error
--- @return string|nil error Error message if encoding failed
function safe_json.encode_table(tbl, depth, max_depth, pretty, indent, current_indent)
    if depth > max_depth then
        return nil, "Maximum depth exceeded"
    end

    -- Check if it's an array
    local is_array = true
    local max_index = 0
    local count = 0

    for k, _ in pairs(tbl) do
        count = count + 1
        if count > DEFAULT_MAX_KEYS then
            return nil, "Too many keys"
        end
        if type(k) ~= "number" or k < 1 or k ~= math.floor(k) then
            is_array = false
        else
            max_index = math.max(max_index, k)
        end
    end

    is_array = is_array and max_index == count

    local next_indent = current_indent .. indent
    local separator = pretty and ",\n" or ","
    local open_brace_suffix = pretty and "\n" or ""
    local close_brace_prefix = pretty and "\n" .. current_indent or ""
    local colon = pretty and ": " or ":"

    local parts = {}

    if is_array then
        for i = 1, max_index do
            local val = tbl[i]
            local encoded, err = safe_json.encode(val, {max_depth = max_depth - depth - 1, pretty = pretty, indent = indent})
            if not encoded then
                return nil, err
            end
            if pretty then
                table.insert(parts, next_indent .. encoded)
            else
                table.insert(parts, encoded)
            end
        end
        return "[" .. open_brace_suffix .. table.concat(parts, separator) .. close_brace_prefix .. "]"
    else
        for k, v in pairs(tbl) do
            local key_str = tostring(k)
            local encoded_value, err = safe_json.encode(v, {max_depth = max_depth - depth - 1, pretty = pretty, indent = indent})
            if not encoded_value then
                return nil, err
            end
            local pair = safe_json.encode_string(key_str) .. colon .. encoded_value
            if pretty then
                table.insert(parts, next_indent .. pair)
            else
                table.insert(parts, pair)
            end
        end
        table.sort(parts)
        return "{" .. open_brace_suffix .. table.concat(parts, separator) .. close_brace_prefix .. "}"
    end
end

--- Decode a JSON string to a Lua value.
--- @param json_string string The JSON string to decode
--- @param options table|nil Optional settings (max_depth, max_string_length)
--- @return any|nil value The decoded value or nil on error
--- @return string|nil error Error message if decoding failed
function safe_json.decode(json_string, options)
    if type(json_string) ~= "string" then
        return nil, "Input must be a string"
    end

    options = options or {}
    local max_depth = options.max_depth or DEFAULT_MAX_DEPTH
    local max_string_length = options.max_string_length or DEFAULT_MAX_STRING_LENGTH

    if #json_string > max_string_length then
        return nil, "JSON string too long"
    end

    local pos = 1
    local depth = 0

    local function skip_whitespace()
        while pos <= #json_string do
            local c = json_string:sub(pos, pos)
            if c ~= " " and c ~= "\t" and c ~= "\n" and c ~= "\r" then
                break
            end
            pos = pos + 1
        end
    end

    local function parse_value()
        skip_whitespace()

        if pos > #json_string then
            return nil, "Unexpected end of input"
        end

        local c = json_string:sub(pos, pos)

        if c == "n" then
            if json_string:sub(pos, pos + 3) == "null" then
                pos = pos + 4
                return nil
            end
        elseif c == "t" then
            if json_string:sub(pos, pos + 3) == "true" then
                pos = pos + 4
                return true
            end
        elseif c == "f" then
            if json_string:sub(pos, pos + 4) == "false" then
                pos = pos + 5
                return false
            end
        elseif c == '"' then
            return parse_string()
        elseif c == "[" then
            return parse_array()
        elseif c == "{" then
            return parse_object()
        elseif c == "-" or (c >= "0" and c <= "9") then
            return parse_number()
        end

        return nil, "Unexpected character: " .. c
    end

    local function parse_string()
        pos = pos + 1 -- Skip opening quote
        local start = pos
        local result = {}

        while pos <= #json_string do
            local c = json_string:sub(pos, pos)
            if c == '"' then
                pos = pos + 1
                return table.concat(result)
            elseif c == "\\" then
                pos = pos + 1
                if pos > #json_string then
                    return nil, "Unexpected end of string"
                end
                local escape = json_string:sub(pos, pos)
                if escape == "n" then
                    table.insert(result, "\n")
                elseif escape == "r" then
                    table.insert(result, "\r")
                elseif escape == "t" then
                    table.insert(result, "\t")
                elseif escape == '"' then
                    table.insert(result, '"')
                elseif escape == "\\" then
                    table.insert(result, "\\")
                elseif escape == "u" then
                    local hex = json_string:sub(pos + 1, pos + 4)
                    local code = tonumber(hex, 16)
                    if code then
                        if code < 128 then
                            table.insert(result, string.char(code))
                        else
                            -- Simple UTF-8 encoding
                            if code < 0x800 then
                                table.insert(result, string.char(0xC0 + math.floor(code / 64), 0x80 + (code % 64)))
                            else
                                table.insert(result, string.char(0xE0 + math.floor(code / 4096), 0x80 + math.floor((code % 4096) / 64), 0x80 + (code % 64)))
                            end
                        end
                        pos = pos + 4
                    end
                else
                    table.insert(result, escape)
                end
                pos = pos + 1
            else
                table.insert(result, c)
                pos = pos + 1
            end
        end

        return nil, "Unterminated string"
    end

    local function parse_number()
        local start = pos
        if json_string:sub(pos, pos) == "-" then
            pos = pos + 1
        end

        while pos <= #json_string and json_string:sub(pos, pos):match("[0-9]") do
            pos = pos + 1
        end

        if pos <= #json_string and json_string:sub(pos, pos) == "." then
            pos = pos + 1
            while pos <= #json_string and json_string:sub(pos, pos):match("[0-9]") do
                pos = pos + 1
            end
        end

        if pos <= #json_string and json_string:sub(pos, pos):lower() == "e" then
            pos = pos + 1
            if pos <= #json_string and json_string:sub(pos, pos):match("[+-]") then
                pos = pos + 1
            end
            while pos <= #json_string and json_string:sub(pos, pos):match("[0-9]") do
                pos = pos + 1
            end
        end

        local num_str = json_string:sub(start, pos - 1)
        return tonumber(num_str)
    end

    local function parse_array()
        depth = depth + 1
        if depth > max_depth then
            return nil, "Maximum depth exceeded"
        end

        pos = pos + 1 -- Skip [
        local arr = {}

        skip_whitespace()
        if json_string:sub(pos, pos) == "]" then
            pos = pos + 1
            depth = depth - 1
            return arr
        end

        while true do
            local val, err = parse_value()
            if err then
                return nil, err
            end
            table.insert(arr, val)

            skip_whitespace()
            local c = json_string:sub(pos, pos)
            if c == "]" then
                pos = pos + 1
                depth = depth - 1
                return arr
            elseif c == "," then
                pos = pos + 1
            else
                return nil, "Expected , or ]"
            end
        end
    end

    local function parse_object()
        depth = depth + 1
        if depth > max_depth then
            return nil, "Maximum depth exceeded"
        end

        pos = pos + 1 -- Skip {
        local obj = {}

        skip_whitespace()
        if json_string:sub(pos, pos) == "}" then
            pos = pos + 1
            depth = depth - 1
            return obj
        end

        while true do
            skip_whitespace()
            if json_string:sub(pos, pos) ~= '"' then
                return nil, "Expected string key"
            end

            local key, err = parse_string()
            if err then
                return nil, err
            end

            skip_whitespace()
            if json_string:sub(pos, pos) ~= ":" then
                return nil, "Expected :"
            end
            pos = pos + 1

            local val
            val, err = parse_value()
            if err then
                return nil, err
            end

            obj[key] = val

            skip_whitespace()
            local c = json_string:sub(pos, pos)
            if c == "}" then
                pos = pos + 1
                depth = depth - 1
                return obj
            elseif c == "," then
                pos = pos + 1
            else
                return nil, "Expected , or }"
            end
        end
    end

    local result, err = parse_value()
    if err then
        return nil, err
    end

    skip_whitespace()
    if pos <= #json_string then
        return nil, "Unexpected content after JSON value"
    end

    return result
end

--- Check if a string is valid JSON.
--- @param json_string string The string to validate
--- @return boolean valid Whether the string is valid JSON
function safe_json.is_valid(json_string)
    local _, err = safe_json.decode(json_string)
    return err == nil
end

--- Safely get a nested value from a decoded JSON object.
--- @param obj table The decoded JSON object
--- @param path string Dot-separated path (e.g., "user.name.first")
--- @return any|nil value The value at the path or nil if not found
function safe_json.get_path(obj, path)
    if type(obj) ~= "table" or type(path) ~= "string" then
        return nil
    end

    local current = obj
    for key in path:gmatch("[^%.]+") do
        if type(current) ~= "table" then
            return nil
        end
        local num_key = tonumber(key)
        current = current[num_key or key]
    end

    return current
end

return safe_json
