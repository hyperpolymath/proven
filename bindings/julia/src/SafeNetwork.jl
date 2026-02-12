# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeNetwork

Safe network operations for IP address validation and classification.
"""
module SafeNetwork

export IPv4, parse_ipv4, is_valid_ipv4, is_private, is_loopback, is_public, format_ipv4

"""
    IPv4

Represents an IPv4 address.
"""
struct IPv4
    a::UInt8
    b::UInt8
    c::UInt8
    d::UInt8
end

"""
    parse_ipv4(address::AbstractString) -> Union{IPv4, Nothing}

Parse an IPv4 address string.
"""
function parse_ipv4(address::AbstractString)::Union{IPv4, Nothing}
    parts = split(address, ".")
    length(parts) != 4 && return nothing

    octets = UInt8[]
    for part in parts
        n = tryparse(Int, part)
        n === nothing && return nothing
        (n < 0 || n > 255) && return nothing
        push!(octets, UInt8(n))
    end

    IPv4(octets[1], octets[2], octets[3], octets[4])
end

"""
    is_valid_ipv4(address::AbstractString) -> Bool

Check if a string is a valid IPv4 address.
"""
function is_valid_ipv4(address::AbstractString)::Bool
    parse_ipv4(address) !== nothing
end

"""
    is_private(address::AbstractString) -> Bool

Check if an IPv4 address is in a private range.
"""
function is_private(address::AbstractString)::Bool
    ip = parse_ipv4(address)
    ip === nothing && return false

    # 10.0.0.0/8
    ip.a == 10 && return true
    # 172.16.0.0/12
    ip.a == 172 && ip.b >= 16 && ip.b <= 31 && return true
    # 192.168.0.0/16
    ip.a == 192 && ip.b == 168 && return true

    false
end

"""
    is_loopback(address::AbstractString) -> Bool

Check if an IPv4 address is a loopback address (127.0.0.0/8).
"""
function is_loopback(address::AbstractString)::Bool
    ip = parse_ipv4(address)
    ip === nothing && return false
    ip.a == 127
end

"""
    is_public(address::AbstractString) -> Bool

Check if an IPv4 address is public (not private or loopback).
"""
function is_public(address::AbstractString)::Bool
    is_valid_ipv4(address) && !is_private(address) && !is_loopback(address)
end

"""
    format_ipv4(ip::IPv4) -> String

Format an IPv4 address as a string.
"""
function format_ipv4(ip::IPv4)::String
    "$(ip.a).$(ip.b).$(ip.c).$(ip.d)"
end

end # module
