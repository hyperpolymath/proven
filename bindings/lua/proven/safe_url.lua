-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe URL parsing and manipulation with validation.
--- @module proven.safe_url

local safe_url = {}

--- URL component structure.
--- @class Url
--- @field scheme string|nil Protocol scheme (http, https, etc.)
--- @field host string|nil Host name
--- @field port number|nil Port number
--- @field path string Path component
--- @field query string|nil Query string (without ?)
--- @field fragment string|nil Fragment (without #)
--- @field username string|nil Username for authentication
--- @field password string|nil Password for authentication

local Url = {}
Url.__index = Url

--- Create a new URL object.
--- @param components table URL components
--- @return Url
function Url.new(components)
    local self = setmetatable({}, Url)
    self.scheme = components.scheme
    self.host = components.host
    self.port = components.port
    self.path = components.path or "/"
    self.query = components.query
    self.fragment = components.fragment
    self.username = components.username
    self.password = components.password
    return self
end

--- Convert URL to string.
--- @return string
function Url:to_string()
    local result = ""

    if self.scheme then
        result = self.scheme .. "://"
    end

    if self.username then
        result = result .. safe_url.percent_encode(self.username)
        if self.password then
            result = result .. ":" .. safe_url.percent_encode(self.password)
        end
        result = result .. "@"
    end

    if self.host then
        result = result .. self.host
    end

    if self.port then
        result = result .. ":" .. tostring(self.port)
    end

    result = result .. self.path

    if self.query then
        result = result .. "?" .. self.query
    end

    if self.fragment then
        result = result .. "#" .. self.fragment
    end

    return result
end

safe_url.Url = Url

--- Parse a URL string into components.
--- @param url_string string The URL to parse
--- @return Url|nil url The parsed URL or nil if invalid
--- @return string|nil error Error message if parsing failed
function safe_url.parse(url_string)
    if type(url_string) ~= "string" then
        return nil, "URL must be a string"
    end

    if #url_string == 0 then
        return nil, "URL cannot be empty"
    end

    if #url_string > 2048 then
        return nil, "URL too long"
    end

    local components = {}
    local remaining = url_string

    -- Extract fragment
    local fragment_start = remaining:find("#")
    if fragment_start then
        components.fragment = remaining:sub(fragment_start + 1)
        remaining = remaining:sub(1, fragment_start - 1)
    end

    -- Extract query
    local query_start = remaining:find("?")
    if query_start then
        components.query = remaining:sub(query_start + 1)
        remaining = remaining:sub(1, query_start - 1)
    end

    -- Extract scheme
    local scheme_end = remaining:find("://")
    if scheme_end then
        components.scheme = remaining:sub(1, scheme_end - 1):lower()
        remaining = remaining:sub(scheme_end + 3)
    end

    -- Extract userinfo
    local at_pos = remaining:find("@")
    if at_pos then
        local userinfo = remaining:sub(1, at_pos - 1)
        remaining = remaining:sub(at_pos + 1)

        local colon_pos = userinfo:find(":")
        if colon_pos then
            components.username = safe_url.percent_decode(userinfo:sub(1, colon_pos - 1))
            components.password = safe_url.percent_decode(userinfo:sub(colon_pos + 1))
        else
            components.username = safe_url.percent_decode(userinfo)
        end
    end

    -- Extract host and port
    local path_start = remaining:find("/")
    local authority = path_start and remaining:sub(1, path_start - 1) or remaining

    if #authority > 0 then
        local port_start = authority:find(":([%d]+)$")
        if port_start then
            local port_str = authority:sub(port_start + 1)
            components.port = tonumber(port_str)
            if not components.port or components.port < 0 or components.port > 65535 then
                return nil, "Invalid port number"
            end
            components.host = authority:sub(1, port_start - 1)
        else
            components.host = authority
        end
    end

    -- Extract path
    if path_start then
        components.path = remaining:sub(path_start)
    else
        components.path = "/"
    end

    return Url.new(components), nil
end

--- Percent-encode a string for use in URLs.
--- @param str string The string to encode
--- @return string The encoded string
function safe_url.percent_encode(str)
    if type(str) ~= "string" then
        return ""
    end

    return (str:gsub("[^A-Za-z0-9%-_.~]", function(c)
        return string.format("%%%02X", string.byte(c))
    end))
end

--- Percent-decode a URL-encoded string.
--- @param str string The encoded string
--- @return string The decoded string
function safe_url.percent_decode(str)
    if type(str) ~= "string" then
        return ""
    end

    return (str:gsub("%%(%x%x)", function(hex)
        return string.char(tonumber(hex, 16))
    end))
end

--- Encode a table as a query string.
--- @param params table Key-value pairs to encode
--- @return string The encoded query string
function safe_url.encode_query(params)
    if type(params) ~= "table" then
        return ""
    end

    local parts = {}
    for key, value in pairs(params) do
        local encoded_key = safe_url.percent_encode(tostring(key))
        local encoded_value = safe_url.percent_encode(tostring(value))
        table.insert(parts, encoded_key .. "=" .. encoded_value)
    end

    table.sort(parts)
    return table.concat(parts, "&")
end

--- Decode a query string into a table.
--- @param query_string string The query string to decode
--- @return table The decoded key-value pairs
function safe_url.decode_query(query_string)
    if type(query_string) ~= "string" then
        return {}
    end

    local params = {}
    for pair in query_string:gmatch("[^&]+") do
        local eq_pos = pair:find("=")
        if eq_pos then
            local key = safe_url.percent_decode(pair:sub(1, eq_pos - 1))
            local value = safe_url.percent_decode(pair:sub(eq_pos + 1))
            params[key] = value
        else
            params[safe_url.percent_decode(pair)] = ""
        end
    end

    return params
end

--- Check if a URL is valid.
--- @param url_string string The URL to validate
--- @return boolean valid Whether the URL is valid
function safe_url.is_valid(url_string)
    local url, err = safe_url.parse(url_string)
    return url ~= nil
end

--- Check if a URL uses HTTPS.
--- @param url_string string The URL to check
--- @return boolean is_https Whether the URL uses HTTPS
function safe_url.is_https(url_string)
    local url = safe_url.parse(url_string)
    return url and url.scheme == "https"
end

--- Join a base URL with a relative path.
--- @param base_url string The base URL
--- @param relative_path string The relative path to join
--- @return string|nil The joined URL or nil on error
function safe_url.join(base_url, relative_path)
    local base = safe_url.parse(base_url)
    if not base then
        return nil
    end

    if relative_path:match("^https?://") then
        return relative_path
    end

    if relative_path:sub(1, 1) == "/" then
        base.path = relative_path
    else
        local base_dir = base.path:match("(.*/)")
        base.path = (base_dir or "/") .. relative_path
    end

    base.query = nil
    base.fragment = nil

    return base:to_string()
end

--- Normalize a URL path (remove . and ..).
--- @param path string The path to normalize
--- @return string The normalized path
function safe_url.normalize_path(path)
    if type(path) ~= "string" or #path == 0 then
        return "/"
    end

    local parts = {}
    for segment in path:gmatch("[^/]+") do
        if segment == ".." then
            if #parts > 0 then
                table.remove(parts)
            end
        elseif segment ~= "." then
            table.insert(parts, segment)
        end
    end

    local result = "/" .. table.concat(parts, "/")
    if path:sub(-1) == "/" and #parts > 0 then
        result = result .. "/"
    end

    return result
end

return safe_url
