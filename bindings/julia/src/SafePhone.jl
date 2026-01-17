# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafePhone

Safe phone number validation following E.164 standard.
"""
module SafePhone

export CountryCode, PhoneNumber
export parse_phone, is_valid_phone
export country_code, national_number, digit_count
export to_e164, to_international
export country_code_value, country_from_value

"""
    CountryCode

Country calling codes (E.164).
"""
@enum CountryCode begin
    Unknown = 0
    Us = 1       # USA, Canada
    Ru = 7       # Russia
    Eg = 20      # Egypt
    Za = 27      # South Africa
    Fr = 33      # France
    Es = 34      # Spain
    It = 39      # Italy
    Uk = 44      # UK
    De = 49      # Germany
    Mx = 52      # Mexico
    Br = 55      # Brazil
    Au = 61      # Australia
    Jp = 81      # Japan
    Kr = 82      # South Korea
    Cn = 86      # China
    In = 91      # India
end

"""
    country_code_value(cc::CountryCode) -> UInt16

Get numeric value of country code.
"""
function country_code_value(cc::CountryCode)::UInt16
    UInt16(Int(cc))
end

"""
    country_from_value(value::Integer) -> CountryCode

Parse country code from numeric value.
"""
function country_from_value(value::Integer)::CountryCode
    v = Int(value)
    if v == 1
        Us
    elseif v == 7
        Ru
    elseif v == 20
        Eg
    elseif v == 27
        Za
    elseif v == 33
        Fr
    elseif v == 34
        Es
    elseif v == 39
        It
    elseif v == 44
        Uk
    elseif v == 49
        De
    elseif v == 52
        Mx
    elseif v == 55
        Br
    elseif v == 61
        Au
    elseif v == 81
        Jp
    elseif v == 82
        Kr
    elseif v == 86
        Cn
    elseif v == 91
        In
    else
        Unknown
    end
end

"""
    PhoneNumber

Validated phone number with country code and national number.
"""
struct PhoneNumber
    country_code::CountryCode
    national_number::String
end

"""
    country_code(phone::PhoneNumber) -> CountryCode

Get the country code.
"""
function country_code(phone::PhoneNumber)::CountryCode
    phone.country_code
end

"""
    national_number(phone::PhoneNumber) -> String

Get the national number.
"""
function national_number(phone::PhoneNumber)::String
    phone.national_number
end

"""
    to_e164(phone::PhoneNumber) -> String

Format in E.164 format (+15551234567).
"""
function to_e164(phone::PhoneNumber)::String
    "+$(country_code_value(phone.country_code))$(phone.national_number)"
end

"""
    to_international(phone::PhoneNumber) -> String

Format with spaces for readability.
"""
function to_international(phone::PhoneNumber)::String
    cc = country_code_value(phone.country_code)
    nat = phone.national_number
    len = length(nat)

    if len <= 4
        "+$(cc) $(nat)"
    elseif len <= 7
        "+$(cc) $(nat[1:3]) $(nat[4:end])"
    elseif len <= 10
        "+$(cc) $(nat[1:3]) $(nat[4:6]) $(nat[7:end])"
    else
        "+$(cc) $(nat)"
    end
end

"""
    digit_count(phone::PhoneNumber) -> Int

Get total digit count (country code + national number).
"""
function digit_count(phone::PhoneNumber)::Int
    cc_value = country_code_value(phone.country_code)
    cc_digits = cc_value >= 100 ? 3 : (cc_value >= 10 ? 2 : 1)
    cc_digits + length(phone.national_number)
end

"""
    parse_phone(input::AbstractString) -> Union{PhoneNumber, Nothing}

Parse phone number from string. Returns `nothing` if parsing fails.
"""
function parse_phone(input::AbstractString)::Union{PhoneNumber, Nothing}
    trimmed = strip(input)
    isempty(trimmed) && return nothing

    # Extract digits only
    digits = filter(isdigit, trimmed)

    length(digits) < 7 && return nothing
    length(digits) > 15 && return nothing

    # Try to parse country code (3, 2, 1 digit lengths)
    result = parse_country_code_from_digits(digits)
    result === nothing && return nothing

    cc, national_start = result
    national = digits[national_start:end]

    length(national) < 4 && return nothing

    PhoneNumber(cc, national)
end

"""
    is_valid_phone(input::AbstractString) -> Bool

Check if string is a valid phone number.
"""
function is_valid_phone(input::AbstractString)::Bool
    parse_phone(input) !== nothing
end

# Helper function to parse country code from digit string
function parse_country_code_from_digits(digits::AbstractString)::Union{Tuple{CountryCode, Int}, Nothing}
    # Try 3-digit codes first, then 2, then 1
    for len in [3, 2, 1]
        if length(digits) >= len
            value = tryparse(Int, digits[1:len])
            value === nothing && continue

            cc = country_from_value(value)
            if cc != Unknown
                return (cc, len + 1)
            end
        end
    end
    nothing
end

# Display methods
Base.show(io::IO, phone::PhoneNumber) = print(io, to_e164(phone))
Base.string(phone::PhoneNumber) = to_e164(phone)

end # module
