# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeHex

Safe hexadecimal encoding and decoding operations.
"""
module SafeHex

export encode_hex, encode_hex_upper, decode_hex
export is_hex_char, is_valid_hex, is_valid_hex_bytes
export hex_char_to_nibble, nibble_to_hex_char, nibble_to_hex_char_upper
export format_hex_spaced, format_hex_colons
export constant_time_equal
export int_to_hex, hex_to_int

"""
    is_hex_char(c::Char) -> Bool

Check if character is a valid hexadecimal digit.
"""
function is_hex_char(c::Char)::Bool
    c in '0':'9' || c in 'a':'f' || c in 'A':'F'
end

"""
    hex_char_to_nibble(c::Char) -> Union{UInt8, Nothing}

Convert hex character to nibble value (0-15).
"""
function hex_char_to_nibble(c::Char)::Union{UInt8, Nothing}
    if c in '0':'9'
        UInt8(c - '0')
    elseif c in 'a':'f'
        UInt8(c - 'a' + 10)
    elseif c in 'A':'F'
        UInt8(c - 'A' + 10)
    else
        nothing
    end
end

"""
    nibble_to_hex_char(n::UInt8) -> Char

Convert nibble (0-15) to lowercase hex character.
"""
function nibble_to_hex_char(n::UInt8)::Char
    if n < 10
        Char(UInt8('0') + n)
    else
        Char(UInt8('a') + n - 10)
    end
end

"""
    nibble_to_hex_char_upper(n::UInt8) -> Char

Convert nibble (0-15) to uppercase hex character.
"""
function nibble_to_hex_char_upper(n::UInt8)::Char
    if n < 10
        Char(UInt8('0') + n)
    else
        Char(UInt8('A') + n - 10)
    end
end

"""
    encode_hex(bytes::AbstractVector{UInt8}) -> String

Encode bytes to lowercase hex string.
"""
function encode_hex(bytes::AbstractVector{UInt8})::String
    result = IOBuffer(sizehint = length(bytes) * 2)
    for b in bytes
        write(result, nibble_to_hex_char(b >> 4))
        write(result, nibble_to_hex_char(b & 0x0F))
    end
    String(take!(result))
end

"""
    encode_hex_upper(bytes::AbstractVector{UInt8}) -> String

Encode bytes to uppercase hex string.
"""
function encode_hex_upper(bytes::AbstractVector{UInt8})::String
    result = IOBuffer(sizehint = length(bytes) * 2)
    for b in bytes
        write(result, nibble_to_hex_char_upper(b >> 4))
        write(result, nibble_to_hex_char_upper(b & 0x0F))
    end
    String(take!(result))
end

"""
    decode_hex(hex::AbstractString) -> Union{Vector{UInt8}, Nothing}

Decode hex string to bytes. Returns `nothing` if invalid.
"""
function decode_hex(hex::AbstractString)::Union{Vector{UInt8}, Nothing}
    length(hex) % 2 != 0 && return nothing

    chars = collect(hex)
    result = UInt8[]
    sizehint!(result, length(hex) ÷ 2)

    for i in 1:2:length(chars)
        high = hex_char_to_nibble(chars[i])
        low = hex_char_to_nibble(chars[i + 1])
        (high === nothing || low === nothing) && return nothing
        push!(result, (high << 4) | low)
    end

    result
end

"""
    is_valid_hex(s::AbstractString) -> Bool

Check if string contains only valid hex characters.
"""
function is_valid_hex(s::AbstractString)::Bool
    all(is_hex_char, s)
end

"""
    is_valid_hex_bytes(s::AbstractString) -> Bool

Check if string is valid hex with even length (represents whole bytes).
"""
function is_valid_hex_bytes(s::AbstractString)::Bool
    length(s) % 2 == 0 && is_valid_hex(s)
end

"""
    format_hex_spaced(hex::AbstractString) -> Union{String, Nothing}

Format hex string with spaces between bytes. Returns `nothing` if invalid.
"""
function format_hex_spaced(hex::AbstractString)::Union{String, Nothing}
    length(hex) % 2 != 0 && return nothing
    isempty(hex) && return ""

    chars = collect(hex)
    result = IOBuffer(sizehint = length(hex) + length(hex) ÷ 2)

    for i in 1:2:length(chars)
        i > 1 && write(result, ' ')
        write(result, chars[i])
        write(result, chars[i + 1])
    end

    String(take!(result))
end

"""
    format_hex_colons(hex::AbstractString) -> Union{String, Nothing}

Format hex string with colons between bytes (MAC address style).
Returns `nothing` if invalid.
"""
function format_hex_colons(hex::AbstractString)::Union{String, Nothing}
    length(hex) % 2 != 0 && return nothing
    isempty(hex) && return ""

    chars = collect(hex)
    result = IOBuffer(sizehint = length(hex) + length(hex) ÷ 2)

    for i in 1:2:length(chars)
        i > 1 && write(result, ':')
        write(result, chars[i])
        write(result, chars[i + 1])
    end

    String(take!(result))
end

"""
    constant_time_equal(a::AbstractString, b::AbstractString) -> Bool

Compare two hex strings in constant time to prevent timing attacks.
Case-insensitive comparison.
"""
function constant_time_equal(a::AbstractString, b::AbstractString)::Bool
    length(a) != length(b) && return false
    isempty(a) && return true

    a_lower = lowercase(a)
    b_lower = lowercase(b)

    diff = UInt8(0)
    for (ca, cb) in zip(a_lower, b_lower)
        diff |= UInt8(ca) ⊻ UInt8(cb)
    end

    diff == 0
end

"""
    constant_time_equal(a::AbstractVector{UInt8}, b::AbstractVector{UInt8}) -> Bool

Compare two byte arrays in constant time to prevent timing attacks.
"""
function constant_time_equal(a::AbstractVector{UInt8}, b::AbstractVector{UInt8})::Bool
    length(a) != length(b) && return false
    isempty(a) && return true

    diff = UInt8(0)
    for i in eachindex(a)
        diff |= a[i] ⊻ b[i]
    end

    diff == 0
end

"""
    int_to_hex(value::Integer, min_width::Integer=0) -> String

Convert integer to lowercase hex string with optional minimum width (zero-padded).
"""
function int_to_hex(value::Integer, min_width::Integer=0)::String
    hex = string(UInt64(value); base=16)
    if length(hex) >= min_width
        hex
    else
        lpad(hex, min_width, '0')
    end
end

"""
    hex_to_int(hex::AbstractString) -> Union{UInt64, Nothing}

Parse hex string to integer. Returns `nothing` if invalid.
"""
function hex_to_int(hex::AbstractString)::Union{UInt64, Nothing}
    tryparse(UInt64, hex; base=16)
end

end # module
