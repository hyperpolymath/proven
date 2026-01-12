# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeMath

Safe arithmetic operations that cannot crash or overflow unexpectedly.
"""
module SafeMath

export safe_div, safe_mod, safe_add, safe_sub, safe_mul, checked_add, checked_sub, checked_mul

"""
    safe_div(numerator::Integer, denominator::Integer) -> Union{Integer, Nothing}

Safely divide two integers, returning `nothing` on division by zero.
"""
function safe_div(numerator::T, denominator::T)::Union{T, Nothing} where T <: Integer
    denominator == 0 ? nothing : div(numerator, denominator)
end

"""
    safe_mod(numerator::Integer, denominator::Integer) -> Union{Integer, Nothing}

Safely compute modulo, returning `nothing` on division by zero.
"""
function safe_mod(numerator::T, denominator::T)::Union{T, Nothing} where T <: Integer
    denominator == 0 ? nothing : mod(numerator, denominator)
end

"""
    safe_add(a::Integer, b::Integer) -> Union{Integer, Nothing}

Safely add two integers, returning `nothing` on overflow.
"""
function safe_add(a::T, b::T)::Union{T, Nothing} where T <: Integer
    try
        Base.checked_add(a, b)
    catch e
        e isa OverflowError ? nothing : rethrow(e)
    end
end

"""
    safe_sub(a::Integer, b::Integer) -> Union{Integer, Nothing}

Safely subtract two integers, returning `nothing` on overflow.
"""
function safe_sub(a::T, b::T)::Union{T, Nothing} where T <: Integer
    try
        Base.checked_sub(a, b)
    catch e
        e isa OverflowError ? nothing : rethrow(e)
    end
end

"""
    safe_mul(a::Integer, b::Integer) -> Union{Integer, Nothing}

Safely multiply two integers, returning `nothing` on overflow.
"""
function safe_mul(a::T, b::T)::Union{T, Nothing} where T <: Integer
    try
        Base.checked_mul(a, b)
    catch e
        e isa OverflowError ? nothing : rethrow(e)
    end
end

"""
    checked_add(a::Integer, b::Integer) -> Integer

Add with overflow checking. Throws OverflowError on overflow.
"""
checked_add(a::T, b::T) where T <: Integer = Base.checked_add(a, b)

"""
    checked_sub(a::Integer, b::Integer) -> Integer

Subtract with overflow checking. Throws OverflowError on overflow.
"""
checked_sub(a::T, b::T) where T <: Integer = Base.checked_sub(a, b)

"""
    checked_mul(a::Integer, b::Integer) -> Integer

Multiply with overflow checking. Throws OverflowError on overflow.
"""
checked_mul(a::T, b::T) where T <: Integer = Base.checked_mul(a, b)

end # module
