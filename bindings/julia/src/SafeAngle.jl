# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeAngle

Safe angle operations with conversions between degrees, radians, and other units.
"""
module SafeAngle

export Angle, AngleUnit
export from_degrees, from_radians, from_gradians, from_turns
export to_degrees, to_radians, to_gradians, to_turns
export normalize, normalize_signed, complement, supplement
export angle_add, angle_sub, angle_mul, angle_div
export sin_safe, cos_safe, tan_safe
export is_acute, is_right, is_obtuse, is_straight, is_reflex
export angle_between

"""
    AngleUnit

Enumeration of angle units.
"""
@enum AngleUnit begin
    Degrees
    Radians
    Gradians
    Turns
end

const TWO_PI = 2.0 * π
const DEG_TO_RAD = π / 180.0
const RAD_TO_DEG = 180.0 / π
const GRAD_TO_RAD = π / 200.0
const RAD_TO_GRAD = 200.0 / π

"""
    Angle

An angle stored internally in radians.
"""
struct Angle
    radians::Float64
end

"""
    from_degrees(deg::Real) -> Angle

Create angle from degrees.
"""
from_degrees(deg::Real)::Angle = Angle(Float64(deg) * DEG_TO_RAD)

"""
    from_radians(rad::Real) -> Angle

Create angle from radians.
"""
from_radians(rad::Real)::Angle = Angle(Float64(rad))

"""
    from_gradians(grad::Real) -> Angle

Create angle from gradians (1 turn = 400 gradians).
"""
from_gradians(grad::Real)::Angle = Angle(Float64(grad) * GRAD_TO_RAD)

"""
    from_turns(turns::Real) -> Angle

Create angle from turns (1 turn = 360 degrees).
"""
from_turns(turns::Real)::Angle = Angle(Float64(turns) * TWO_PI)

"""
    to_degrees(a::Angle) -> Float64

Convert to degrees.
"""
to_degrees(a::Angle)::Float64 = a.radians * RAD_TO_DEG

"""
    to_radians(a::Angle) -> Float64

Convert to radians.
"""
to_radians(a::Angle)::Float64 = a.radians

"""
    to_gradians(a::Angle) -> Float64

Convert to gradians.
"""
to_gradians(a::Angle)::Float64 = a.radians * RAD_TO_GRAD

"""
    to_turns(a::Angle) -> Float64

Convert to turns.
"""
to_turns(a::Angle)::Float64 = a.radians / TWO_PI

"""
    normalize(a::Angle) -> Angle

Normalize to [0, 2π) range.
"""
function normalize(a::Angle)::Angle
    r = mod(a.radians, TWO_PI)
    r < 0 && (r += TWO_PI)
    Angle(r)
end

"""
    normalize_signed(a::Angle) -> Angle

Normalize to [-π, π) range.
"""
function normalize_signed(a::Angle)::Angle
    r = mod(a.radians + π, TWO_PI) - π
    Angle(r)
end

"""
    complement(a::Angle) -> Union{Angle, Nothing}

Get the complement (90° - angle). Returns nothing if angle > 90°.
"""
function complement(a::Angle)::Union{Angle, Nothing}
    normalized = normalize(a)
    normalized.radians > π/2 && return nothing
    Angle(π/2 - normalized.radians)
end

"""
    supplement(a::Angle) -> Union{Angle, Nothing}

Get the supplement (180° - angle). Returns nothing if angle > 180°.
"""
function supplement(a::Angle)::Union{Angle, Nothing}
    normalized = normalize(a)
    normalized.radians > π && return nothing
    Angle(π - normalized.radians)
end

"""
    angle_add(a::Angle, b::Angle) -> Angle

Add two angles.
"""
angle_add(a::Angle, b::Angle)::Angle = Angle(a.radians + b.radians)

"""
    angle_sub(a::Angle, b::Angle) -> Angle

Subtract two angles.
"""
angle_sub(a::Angle, b::Angle)::Angle = Angle(a.radians - b.radians)

"""
    angle_mul(a::Angle, scalar::Real) -> Angle

Multiply angle by scalar.
"""
angle_mul(a::Angle, scalar::Real)::Angle = Angle(a.radians * scalar)

"""
    angle_div(a::Angle, scalar::Real) -> Union{Angle, Nothing}

Divide angle by scalar. Returns nothing on division by zero.
"""
function angle_div(a::Angle, scalar::Real)::Union{Angle, Nothing}
    scalar == 0 && return nothing
    Angle(a.radians / scalar)
end

"""
    sin_safe(a::Angle) -> Float64

Compute sine of angle.
"""
sin_safe(a::Angle)::Float64 = sin(a.radians)

"""
    cos_safe(a::Angle) -> Float64

Compute cosine of angle.
"""
cos_safe(a::Angle)::Float64 = cos(a.radians)

"""
    tan_safe(a::Angle) -> Union{Float64, Nothing}

Compute tangent of angle. Returns nothing near π/2 + nπ.
"""
function tan_safe(a::Angle)::Union{Float64, Nothing}
    normalized = normalize(a)
    # Check if near 90° or 270°
    near_90 = abs(normalized.radians - π/2) < 1e-10
    near_270 = abs(normalized.radians - 3π/2) < 1e-10
    (near_90 || near_270) && return nothing

    result = tan(a.radians)
    isfinite(result) ? result : nothing
end

"""
    is_acute(a::Angle) -> Bool

Check if angle is acute (0° < a < 90°).
"""
function is_acute(a::Angle)::Bool
    r = normalize(a).radians
    r > 0 && r < π/2
end

"""
    is_right(a::Angle; tolerance::Float64=1e-10) -> Bool

Check if angle is a right angle (90°).
"""
function is_right(a::Angle; tolerance::Float64=1e-10)::Bool
    r = normalize(a).radians
    abs(r - π/2) < tolerance
end

"""
    is_obtuse(a::Angle) -> Bool

Check if angle is obtuse (90° < a < 180°).
"""
function is_obtuse(a::Angle)::Bool
    r = normalize(a).radians
    r > π/2 && r < π
end

"""
    is_straight(a::Angle; tolerance::Float64=1e-10) -> Bool

Check if angle is straight (180°).
"""
function is_straight(a::Angle; tolerance::Float64=1e-10)::Bool
    r = normalize(a).radians
    abs(r - π) < tolerance
end

"""
    is_reflex(a::Angle) -> Bool

Check if angle is reflex (180° < a < 360°).
"""
function is_reflex(a::Angle)::Bool
    r = normalize(a).radians
    r > π && r < TWO_PI
end

"""
    angle_between(from::Angle, to::Angle) -> Angle

Get the smallest angle between two angles (always positive, <= 180°).
"""
function angle_between(from::Angle, to::Angle)::Angle
    diff = normalize(Angle(to.radians - from.radians))
    diff.radians > π ? Angle(TWO_PI - diff.radians) : diff
end

# Operator overloads
Base.:+(a::Angle, b::Angle) = angle_add(a, b)
Base.:-(a::Angle, b::Angle) = angle_sub(a, b)
Base.:-(a::Angle) = Angle(-a.radians)
Base.:*(a::Angle, s::Real) = angle_mul(a, s)
Base.:*(s::Real, a::Angle) = angle_mul(a, s)
Base.:/(a::Angle, s::Real) = something(angle_div(a, s), error("Division by zero"))

# Comparison
Base.:(==)(a::Angle, b::Angle) = normalize(a).radians ≈ normalize(b).radians
Base.:<(a::Angle, b::Angle) = a.radians < b.radians

# Display
function Base.show(io::IO, a::Angle)
    deg = to_degrees(a)
    print(io, "Angle(", round(deg, digits=2), "°)")
end

end # module
