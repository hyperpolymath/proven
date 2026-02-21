-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Safe network operations for IP address validation and classification.
--- @module proven.safe_network

local safe_network = {}

--- Parse an IPv4 address string.
--- @param address string
--- @return table|nil Returns {a, b, c, d} or nil if invalid
function safe_network.parse_ipv4(address)
    local parts = {}
    for part in address:gmatch("([^%.]+)") do
        local num = tonumber(part)
        if not num or num < 0 or num > 255 or math.floor(num) ~= num then
            return nil
        end
        table.insert(parts, num)
    end

    if #parts ~= 4 then
        return nil
    end

    return {
        a = parts[1],
        b = parts[2],
        c = parts[3],
        d = parts[4]
    }
end

--- Check if a string is a valid IPv4 address.
--- @param address string
--- @return boolean
function safe_network.is_valid_ipv4(address)
    return safe_network.parse_ipv4(address) ~= nil
end

--- Check if an IPv4 address is in a private range.
--- @param address string
--- @return boolean
function safe_network.is_private(address)
    local ip = safe_network.parse_ipv4(address)
    if not ip then
        return false
    end

    -- 10.0.0.0/8
    if ip.a == 10 then
        return true
    end

    -- 172.16.0.0/12
    if ip.a == 172 and ip.b >= 16 and ip.b <= 31 then
        return true
    end

    -- 192.168.0.0/16
    if ip.a == 192 and ip.b == 168 then
        return true
    end

    return false
end

--- Check if an IPv4 address is a loopback address (127.0.0.0/8).
--- @param address string
--- @return boolean
function safe_network.is_loopback(address)
    local ip = safe_network.parse_ipv4(address)
    if not ip then
        return false
    end
    return ip.a == 127
end

--- Check if an IPv4 address is public (not private or loopback).
--- @param address string
--- @return boolean
function safe_network.is_public(address)
    return safe_network.is_valid_ipv4(address)
        and not safe_network.is_private(address)
        and not safe_network.is_loopback(address)
end

--- Format an IPv4 address as a string.
--- @param ip table {a, b, c, d}
--- @return string
function safe_network.format_ipv4(ip)
    return string.format("%d.%d.%d.%d", ip.a, ip.b, ip.c, ip.d)
end

return safe_network
