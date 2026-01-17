# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeCrypto

Cryptographic safety operations with constant-time guarantees.
"""
module SafeCrypto

export constant_time_compare, secure_zero!

"""
    constant_time_compare(a::AbstractVector{UInt8}, b::AbstractVector{UInt8}) -> Bool

Compare two byte arrays in constant time to prevent timing attacks.
"""
function constant_time_compare(a::AbstractVector{UInt8}, b::AbstractVector{UInt8})::Bool
    length(a) != length(b) && return false
    isempty(a) && return true

    result = UInt8(0)
    for i in eachindex(a)
        result |= a[i] ⊻ b[i]
    end
    result == 0
end

"""
    constant_time_compare(a::AbstractString, b::AbstractString) -> Bool

Compare two strings in constant time to prevent timing attacks.
"""
function constant_time_compare(a::AbstractString, b::AbstractString)::Bool
    constant_time_compare(Vector{UInt8}(a), Vector{UInt8}(b))
end

"""
    secure_zero!(data::AbstractVector{UInt8}) -> Nothing

Securely zero out a byte array to prevent data leakage.
"""
function secure_zero!(data::AbstractVector{UInt8})::Nothing
    for i in eachindex(data)
        data[i] = 0x00
    end
    # Prevent compiler from optimizing away the zeroing
    @assert all(==(0x00), data)
    nothing
end

"""
    secure_zero!(s::Base.SecretBuffer) -> Nothing

Securely zero a SecretBuffer (Julia's built-in secure string type).
"""
function secure_zero!(s::Base.SecretBuffer)::Nothing
    Base.shred!(s)
    nothing
end

end # module
