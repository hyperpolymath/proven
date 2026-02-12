# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeUnit

Safe unit conversions with type-safe quantity operations.
"""
module SafeUnit

export Quantity, UnitType
export Length, Mass, Temperature, Duration, DataSize
export meters, kilometers, miles, feet, inches
export kilograms, grams, pounds, ounces
export celsius, fahrenheit, kelvin
export seconds, minutes, hours, days
export bytes, kilobytes, megabytes, gigabytes, terabytes
export convert_to, add_quantities, sub_quantities, mul_quantity, div_quantity

"""
    UnitType

Categories of physical units.
"""
@enum UnitType begin
    LengthUnit
    MassUnit
    TemperatureUnit
    DurationUnit
    DataSizeUnit
end

"""
    Length

Length measurement stored in meters.
"""
struct Length
    meters::Float64
end

"""
    Mass

Mass measurement stored in kilograms.
"""
struct Mass
    kilograms::Float64
end

"""
    Temperature

Temperature stored in Kelvin.
"""
struct Temperature
    kelvin::Float64
end

"""
    Duration

Duration stored in seconds.
"""
struct Duration
    seconds::Float64
end

"""
    DataSize

Data size stored in bytes.
"""
struct DataSize
    bytes_value::UInt64
end

# Length constructors
"""Create length from meters."""
meters(v::Real)::Length = Length(Float64(v))

"""Create length from kilometers."""
kilometers(v::Real)::Length = Length(Float64(v) * 1000.0)

"""Create length from miles."""
miles(v::Real)::Length = Length(Float64(v) * 1609.344)

"""Create length from feet."""
feet(v::Real)::Length = Length(Float64(v) * 0.3048)

"""Create length from inches."""
inches(v::Real)::Length = Length(Float64(v) * 0.0254)

# Mass constructors
"""Create mass from kilograms."""
kilograms(v::Real)::Mass = Mass(Float64(v))

"""Create mass from grams."""
grams(v::Real)::Mass = Mass(Float64(v) / 1000.0)

"""Create mass from pounds."""
pounds(v::Real)::Mass = Mass(Float64(v) * 0.453592)

"""Create mass from ounces."""
ounces(v::Real)::Mass = Mass(Float64(v) * 0.0283495)

# Temperature constructors
"""Create temperature from Kelvin."""
kelvin(v::Real)::Temperature = Temperature(max(0.0, Float64(v)))

"""Create temperature from Celsius."""
celsius(v::Real)::Temperature = Temperature(max(0.0, Float64(v) + 273.15))

"""Create temperature from Fahrenheit."""
fahrenheit(v::Real)::Temperature = Temperature(max(0.0, (Float64(v) - 32.0) * 5.0/9.0 + 273.15))

# Duration constructors
"""Create duration from seconds."""
seconds(v::Real)::Duration = Duration(Float64(v))

"""Create duration from minutes."""
minutes(v::Real)::Duration = Duration(Float64(v) * 60.0)

"""Create duration from hours."""
hours(v::Real)::Duration = Duration(Float64(v) * 3600.0)

"""Create duration from days."""
days(v::Real)::Duration = Duration(Float64(v) * 86400.0)

# DataSize constructors
"""Create data size from bytes."""
bytes(v::Integer)::DataSize = DataSize(UInt64(v))

"""Create data size from kilobytes."""
kilobytes(v::Integer)::DataSize = DataSize(UInt64(v) * 1024)

"""Create data size from megabytes."""
megabytes(v::Integer)::DataSize = DataSize(UInt64(v) * 1024 * 1024)

"""Create data size from gigabytes."""
gigabytes(v::Integer)::DataSize = DataSize(UInt64(v) * 1024 * 1024 * 1024)

"""Create data size from terabytes."""
terabytes(v::Integer)::DataSize = DataSize(UInt64(v) * 1024 * 1024 * 1024 * 1024)

# Quantity wrapper for generic operations
"""
    Quantity{T}

Generic wrapper for unit quantities.
"""
struct Quantity{T}
    value::T
end

# Conversion functions
"""
    convert_to(l::Length, unit::Symbol) -> Float64

Convert length to specified unit (:meters, :kilometers, :miles, :feet, :inches).
"""
function convert_to(l::Length, unit::Symbol)::Float64
    if unit == :meters
        l.meters
    elseif unit == :kilometers
        l.meters / 1000.0
    elseif unit == :miles
        l.meters / 1609.344
    elseif unit == :feet
        l.meters / 0.3048
    elseif unit == :inches
        l.meters / 0.0254
    else
        error("Unknown length unit: $unit")
    end
end

"""
    convert_to(m::Mass, unit::Symbol) -> Float64

Convert mass to specified unit (:kilograms, :grams, :pounds, :ounces).
"""
function convert_to(m::Mass, unit::Symbol)::Float64
    if unit == :kilograms
        m.kilograms
    elseif unit == :grams
        m.kilograms * 1000.0
    elseif unit == :pounds
        m.kilograms / 0.453592
    elseif unit == :ounces
        m.kilograms / 0.0283495
    else
        error("Unknown mass unit: $unit")
    end
end

"""
    convert_to(t::Temperature, unit::Symbol) -> Float64

Convert temperature to specified unit (:kelvin, :celsius, :fahrenheit).
"""
function convert_to(t::Temperature, unit::Symbol)::Float64
    if unit == :kelvin
        t.kelvin
    elseif unit == :celsius
        t.kelvin - 273.15
    elseif unit == :fahrenheit
        (t.kelvin - 273.15) * 9.0/5.0 + 32.0
    else
        error("Unknown temperature unit: $unit")
    end
end

"""
    convert_to(d::Duration, unit::Symbol) -> Float64

Convert duration to specified unit (:seconds, :minutes, :hours, :days).
"""
function convert_to(d::Duration, unit::Symbol)::Float64
    if unit == :seconds
        d.seconds
    elseif unit == :minutes
        d.seconds / 60.0
    elseif unit == :hours
        d.seconds / 3600.0
    elseif unit == :days
        d.seconds / 86400.0
    else
        error("Unknown duration unit: $unit")
    end
end

"""
    convert_to(ds::DataSize, unit::Symbol) -> Float64

Convert data size to specified unit (:bytes, :kilobytes, :megabytes, :gigabytes, :terabytes).
"""
function convert_to(ds::DataSize, unit::Symbol)::Float64
    if unit == :bytes
        Float64(ds.bytes_value)
    elseif unit == :kilobytes
        Float64(ds.bytes_value) / 1024.0
    elseif unit == :megabytes
        Float64(ds.bytes_value) / (1024.0^2)
    elseif unit == :gigabytes
        Float64(ds.bytes_value) / (1024.0^3)
    elseif unit == :terabytes
        Float64(ds.bytes_value) / (1024.0^4)
    else
        error("Unknown data size unit: $unit")
    end
end

# Arithmetic operations
"""Add two lengths."""
add_quantities(a::Length, b::Length)::Length = Length(a.meters + b.meters)

"""Subtract two lengths."""
sub_quantities(a::Length, b::Length)::Length = Length(a.meters - b.meters)

"""Multiply length by scalar."""
mul_quantity(a::Length, scalar::Real)::Length = Length(a.meters * scalar)

"""Divide length by scalar."""
function div_quantity(a::Length, scalar::Real)::Union{Length, Nothing}
    scalar == 0 && return nothing
    Length(a.meters / scalar)
end

"""Add two masses."""
add_quantities(a::Mass, b::Mass)::Mass = Mass(a.kilograms + b.kilograms)

"""Subtract two masses."""
sub_quantities(a::Mass, b::Mass)::Mass = Mass(a.kilograms - b.kilograms)

"""Multiply mass by scalar."""
mul_quantity(a::Mass, scalar::Real)::Mass = Mass(a.kilograms * scalar)

"""Divide mass by scalar."""
function div_quantity(a::Mass, scalar::Real)::Union{Mass, Nothing}
    scalar == 0 && return nothing
    Mass(a.kilograms / scalar)
end

"""Add two durations."""
add_quantities(a::Duration, b::Duration)::Duration = Duration(a.seconds + b.seconds)

"""Subtract two durations."""
sub_quantities(a::Duration, b::Duration)::Duration = Duration(a.seconds - b.seconds)

"""Multiply duration by scalar."""
mul_quantity(a::Duration, scalar::Real)::Duration = Duration(a.seconds * scalar)

"""Divide duration by scalar."""
function div_quantity(a::Duration, scalar::Real)::Union{Duration, Nothing}
    scalar == 0 && return nothing
    Duration(a.seconds / scalar)
end

"""Add two data sizes."""
add_quantities(a::DataSize, b::DataSize)::DataSize = DataSize(a.bytes_value + b.bytes_value)

"""Subtract two data sizes."""
function sub_quantities(a::DataSize, b::DataSize)::Union{DataSize, Nothing}
    b.bytes_value > a.bytes_value && return nothing
    DataSize(a.bytes_value - b.bytes_value)
end

"""Multiply data size by scalar."""
mul_quantity(a::DataSize, scalar::Integer)::DataSize = DataSize(a.bytes_value * UInt64(scalar))

"""Divide data size by scalar."""
function div_quantity(a::DataSize, scalar::Integer)::Union{DataSize, Nothing}
    scalar == 0 && return nothing
    DataSize(div(a.bytes_value, UInt64(scalar)))
end

# Operator overloads
Base.:+(a::Length, b::Length) = add_quantities(a, b)
Base.:-(a::Length, b::Length) = sub_quantities(a, b)
Base.:*(a::Length, s::Real) = mul_quantity(a, s)
Base.:*(s::Real, a::Length) = mul_quantity(a, s)

Base.:+(a::Mass, b::Mass) = add_quantities(a, b)
Base.:-(a::Mass, b::Mass) = sub_quantities(a, b)
Base.:*(a::Mass, s::Real) = mul_quantity(a, s)
Base.:*(s::Real, a::Mass) = mul_quantity(a, s)

Base.:+(a::Duration, b::Duration) = add_quantities(a, b)
Base.:-(a::Duration, b::Duration) = sub_quantities(a, b)
Base.:*(a::Duration, s::Real) = mul_quantity(a, s)
Base.:*(s::Real, a::Duration) = mul_quantity(a, s)

Base.:+(a::DataSize, b::DataSize) = add_quantities(a, b)

# Comparison operators
Base.:(==)(a::Length, b::Length) = a.meters == b.meters
Base.:<(a::Length, b::Length) = a.meters < b.meters
Base.:>(a::Length, b::Length) = a.meters > b.meters

Base.:(==)(a::Mass, b::Mass) = a.kilograms == b.kilograms
Base.:<(a::Mass, b::Mass) = a.kilograms < b.kilograms
Base.:>(a::Mass, b::Mass) = a.kilograms > b.kilograms

Base.:(==)(a::Temperature, b::Temperature) = a.kelvin == b.kelvin
Base.:<(a::Temperature, b::Temperature) = a.kelvin < b.kelvin
Base.:>(a::Temperature, b::Temperature) = a.kelvin > b.kelvin

Base.:(==)(a::Duration, b::Duration) = a.seconds == b.seconds
Base.:<(a::Duration, b::Duration) = a.seconds < b.seconds
Base.:>(a::Duration, b::Duration) = a.seconds > b.seconds

Base.:(==)(a::DataSize, b::DataSize) = a.bytes_value == b.bytes_value
Base.:<(a::DataSize, b::DataSize) = a.bytes_value < b.bytes_value
Base.:>(a::DataSize, b::DataSize) = a.bytes_value > b.bytes_value

# Display
Base.show(io::IO, l::Length) = print(io, "Length($(round(l.meters, digits=4)) m)")
Base.show(io::IO, m::Mass) = print(io, "Mass($(round(m.kilograms, digits=4)) kg)")
Base.show(io::IO, t::Temperature) = print(io, "Temperature($(round(convert_to(t, :celsius), digits=2)) °C)")
Base.show(io::IO, d::Duration) = print(io, "Duration($(round(d.seconds, digits=2)) s)")
Base.show(io::IO, ds::DataSize) = print(io, "DataSize($(ds.bytes_value) bytes)")

end # module
