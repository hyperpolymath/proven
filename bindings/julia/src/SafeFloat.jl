# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeFloat

Safe floating-point operations with NaN and infinity handling.
"""
module SafeFloat

export SafeF64
export safe_div, safe_sqrt, safe_log, safe_log10, safe_pow
export safe_sin, safe_cos, safe_tan, safe_asin, safe_acos, safe_atan
export is_safe, is_finite_value, clamp_finite
export approximately_equal, relative_eq
export EPSILON, MIN_POSITIVE, MAX_FINITE

"""Machine epsilon for Float64."""
const EPSILON = eps(Float64)

"""Smallest positive normalized Float64."""
const MIN_POSITIVE = floatmin(Float64)

"""Largest finite Float64."""
const MAX_FINITE = floatmax(Float64)

"""
    SafeF64

A floating-point wrapper that ensures values are always finite.
"""
struct SafeF64
    value::Float64

    function SafeF64(v::Float64)
        isfinite(v) || error("SafeF64 requires finite values")
        new(v)
    end
end

SafeF64(x::Real) = SafeF64(Float64(x))

"""
    is_safe(x::Float64) -> Bool

Check if a float is safe (finite).
"""
is_safe(x::Float64)::Bool = isfinite(x)

"""
    is_finite_value(x::Float64) -> Bool

Check if a float is finite (not NaN or Inf).
"""
is_finite_value(x::Float64)::Bool = isfinite(x)

"""
    clamp_finite(x::Float64) -> Float64

Clamp a value to finite range, converting NaN to 0 and Inf to max/min.
"""
function clamp_finite(x::Float64)::Float64
    if isnan(x)
        0.0
    elseif isinf(x)
        x > 0 ? MAX_FINITE : -MAX_FINITE
    else
        x
    end
end

"""
    safe_div(a::Float64, b::Float64) -> Union{Float64, Nothing}

Safely divide two floats. Returns nothing on division by zero or non-finite result.
"""
function safe_div(a::Float64, b::Float64)::Union{Float64, Nothing}
    b == 0.0 && return nothing
    result = a / b
    isfinite(result) ? result : nothing
end

"""
    safe_sqrt(x::Float64) -> Union{Float64, Nothing}

Safely compute square root. Returns nothing for negative numbers.
"""
function safe_sqrt(x::Float64)::Union{Float64, Nothing}
    x < 0.0 && return nothing
    result = sqrt(x)
    isfinite(result) ? result : nothing
end

"""
    safe_log(x::Float64) -> Union{Float64, Nothing}

Safely compute natural logarithm. Returns nothing for non-positive numbers.
"""
function safe_log(x::Float64)::Union{Float64, Nothing}
    x <= 0.0 && return nothing
    result = log(x)
    isfinite(result) ? result : nothing
end

"""
    safe_log10(x::Float64) -> Union{Float64, Nothing}

Safely compute base-10 logarithm. Returns nothing for non-positive numbers.
"""
function safe_log10(x::Float64)::Union{Float64, Nothing}
    x <= 0.0 && return nothing
    result = log10(x)
    isfinite(result) ? result : nothing
end

"""
    safe_pow(base::Float64, exp::Float64) -> Union{Float64, Nothing}

Safely compute power. Returns nothing on invalid or overflow.
"""
function safe_pow(base::Float64, exp::Float64)::Union{Float64, Nothing}
    # Handle special cases
    if base < 0.0 && !isinteger(exp)
        return nothing  # Complex result
    end
    if base == 0.0 && exp < 0.0
        return nothing  # Division by zero
    end

    result = base^exp
    isfinite(result) ? result : nothing
end

"""
    safe_sin(x::Float64) -> Union{Float64, Nothing}

Safely compute sine. Returns nothing for non-finite input.
"""
function safe_sin(x::Float64)::Union{Float64, Nothing}
    !isfinite(x) && return nothing
    sin(x)
end

"""
    safe_cos(x::Float64) -> Union{Float64, Nothing}

Safely compute cosine. Returns nothing for non-finite input.
"""
function safe_cos(x::Float64)::Union{Float64, Nothing}
    !isfinite(x) && return nothing
    cos(x)
end

"""
    safe_tan(x::Float64) -> Union{Float64, Nothing}

Safely compute tangent. Returns nothing for non-finite input or near singularity.
"""
function safe_tan(x::Float64)::Union{Float64, Nothing}
    !isfinite(x) && return nothing
    result = tan(x)
    isfinite(result) ? result : nothing
end

"""
    safe_asin(x::Float64) -> Union{Float64, Nothing}

Safely compute arcsine. Returns nothing for |x| > 1.
"""
function safe_asin(x::Float64)::Union{Float64, Nothing}
    abs(x) > 1.0 && return nothing
    asin(x)
end

"""
    safe_acos(x::Float64) -> Union{Float64, Nothing}

Safely compute arccosine. Returns nothing for |x| > 1.
"""
function safe_acos(x::Float64)::Union{Float64, Nothing}
    abs(x) > 1.0 && return nothing
    acos(x)
end

"""
    safe_atan(x::Float64) -> Union{Float64, Nothing}

Safely compute arctangent. Returns nothing for non-finite input.
"""
function safe_atan(x::Float64)::Union{Float64, Nothing}
    !isfinite(x) && return nothing
    atan(x)
end

"""
    approximately_equal(a::Float64, b::Float64; epsilon::Float64=EPSILON) -> Bool

Check if two floats are approximately equal within epsilon.
"""
function approximately_equal(a::Float64, b::Float64; epsilon::Float64=EPSILON)::Bool
    abs(a - b) <= epsilon
end

"""
    relative_eq(a::Float64, b::Float64; max_relative::Float64=EPSILON) -> Bool

Check if two floats are relatively equal.
"""
function relative_eq(a::Float64, b::Float64; max_relative::Float64=EPSILON)::Bool
    (!isfinite(a) || !isfinite(b)) && return false

    diff = abs(a - b)
    max_val = max(abs(a), abs(b))

    if max_val == 0.0
        diff == 0.0
    else
        diff / max_val <= max_relative
    end
end

# SafeF64 arithmetic
Base.:+(a::SafeF64, b::SafeF64) = SafeF64(clamp_finite(a.value + b.value))
Base.:-(a::SafeF64, b::SafeF64) = SafeF64(clamp_finite(a.value - b.value))
Base.:*(a::SafeF64, b::SafeF64) = SafeF64(clamp_finite(a.value * b.value))
Base.:/(a::SafeF64, b::SafeF64) = SafeF64(something(safe_div(a.value, b.value), 0.0))
Base.:-(a::SafeF64) = SafeF64(-a.value)

# Comparison
Base.:(==)(a::SafeF64, b::SafeF64) = a.value == b.value
Base.:<(a::SafeF64, b::SafeF64) = a.value < b.value
Base.:>(a::SafeF64, b::SafeF64) = a.value > b.value
Base.:<=(a::SafeF64, b::SafeF64) = a.value <= b.value
Base.:>=(a::SafeF64, b::SafeF64) = a.value >= b.value

# Conversion
Base.Float64(s::SafeF64) = s.value
Base.convert(::Type{Float64}, s::SafeF64) = s.value

# Display
Base.show(io::IO, s::SafeF64) = print(io, "SafeF64(", s.value, ")")

end # module
