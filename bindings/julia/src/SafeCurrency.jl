# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
    SafeCurrency

Safe currency operations with type-safe monetary values.
Follows ISO 4217 for currency codes.
"""
module SafeCurrency

export CurrencyCode, Money
export parse_currency_code, is_valid_currency_code
export decimals, symbol, currency_name
export from_major, from_minor, zero_money
export major_units, minor_units, currency
export money_add, money_sub, money_mul, money_div
export is_zero, is_positive, is_negative, money_abs
export format_money

"""
    CurrencyCode

ISO 4217 currency codes.
"""
@enum CurrencyCode begin
    USD
    EUR
    GBP
    JPY
    CHF
    CAD
    AUD
    NZD
    CNY
    INR
    BRL
    MXN
    KRW
    SGD
    HKD
    SEK
    NOK
    DKK
    PLN
    RUB
    ZAR
    TRY
    THB
    MYR
    IDR
    PHP
    VND
    AED
    SAR
    ILS
    CZK
    HUF
    RON
    BGN
    HRK
    ISK
    CLP
    COP
    PEN
    ARS
    BTC
    ETH
end

"""
    decimals(code::CurrencyCode) -> UInt8

Get number of decimal places for a currency.
"""
function decimals(code::CurrencyCode)::UInt8
    if code in (JPY, KRW, VND)
        UInt8(0)
    elseif code in (BTC, ETH)
        UInt8(8)
    else
        UInt8(2)
    end
end

"""
    symbol(code::CurrencyCode) -> String

Get currency symbol.
"""
function symbol(code::CurrencyCode)::String
    if code == USD
        "\$"
    elseif code == EUR
        "€"
    elseif code == GBP
        "£"
    elseif code in (JPY, CNY)
        "¥"
    elseif code == CHF
        "Fr"
    elseif code == INR
        "₹"
    elseif code == KRW
        "₩"
    elseif code == RUB
        "₽"
    elseif code == BTC
        "₿"
    elseif code == ETH
        "Ξ"
    else
        ""
    end
end

"""
    currency_name(code::CurrencyCode) -> String

Get currency name.
"""
function currency_name(code::CurrencyCode)::String
    if code == USD
        "US Dollar"
    elseif code == EUR
        "Euro"
    elseif code == GBP
        "British Pound"
    elseif code == JPY
        "Japanese Yen"
    elseif code == CHF
        "Swiss Franc"
    elseif code == CAD
        "Canadian Dollar"
    elseif code == AUD
        "Australian Dollar"
    elseif code == NZD
        "New Zealand Dollar"
    elseif code == CNY
        "Chinese Yuan"
    elseif code == INR
        "Indian Rupee"
    elseif code == BRL
        "Brazilian Real"
    elseif code == MXN
        "Mexican Peso"
    elseif code == KRW
        "South Korean Won"
    elseif code == BTC
        "Bitcoin"
    elseif code == ETH
        "Ethereum"
    else
        "Currency"
    end
end

"""
    parse_currency_code(s::AbstractString) -> Union{CurrencyCode, Nothing}

Parse currency code from string.
"""
function parse_currency_code(s::AbstractString)::Union{CurrencyCode, Nothing}
    code_str = uppercase(strip(s))
    currency_map = Dict(
        "USD" => USD, "EUR" => EUR, "GBP" => GBP, "JPY" => JPY, "CHF" => CHF,
        "CAD" => CAD, "AUD" => AUD, "NZD" => NZD, "CNY" => CNY, "INR" => INR,
        "BRL" => BRL, "MXN" => MXN, "KRW" => KRW, "SGD" => SGD, "HKD" => HKD,
        "SEK" => SEK, "NOK" => NOK, "DKK" => DKK, "PLN" => PLN, "RUB" => RUB,
        "ZAR" => ZAR, "TRY" => TRY, "THB" => THB, "MYR" => MYR, "IDR" => IDR,
        "PHP" => PHP, "VND" => VND, "AED" => AED, "SAR" => SAR, "ILS" => ILS,
        "CZK" => CZK, "HUF" => HUF, "RON" => RON, "BGN" => BGN, "HRK" => HRK,
        "ISK" => ISK, "CLP" => CLP, "COP" => COP, "PEN" => PEN, "ARS" => ARS,
        "BTC" => BTC, "ETH" => ETH
    )
    get(currency_map, code_str, nothing)
end

"""
    is_valid_currency_code(s::AbstractString) -> Bool

Check if string is a valid currency code.
"""
function is_valid_currency_code(s::AbstractString)::Bool
    parse_currency_code(s) !== nothing
end

"""
    Money

Type-safe monetary value stored in minor units.
"""
struct Money
    minor_units::Int64
    currency::CurrencyCode
end

"""
    from_major(amount::Integer, currency::CurrencyCode) -> Money

Create Money from major units (dollars, euros, etc.).
"""
function from_major(amount::Integer, curr::CurrencyCode)::Money
    multiplier = Int64(10)^decimals(curr)
    Money(Int64(amount) * multiplier, curr)
end

"""
    from_minor(amount::Integer, currency::CurrencyCode) -> Money

Create Money from minor units (cents, satoshis, etc.).
"""
function from_minor(amount::Integer, curr::CurrencyCode)::Money
    Money(Int64(amount), curr)
end

"""
    zero_money(currency::CurrencyCode) -> Money

Create a zero amount for a currency.
"""
function zero_money(curr::CurrencyCode)::Money
    Money(Int64(0), curr)
end

"""
    currency(money::Money) -> CurrencyCode

Get the currency code.
"""
function currency(money::Money)::CurrencyCode
    money.currency
end

"""
    major_units(money::Money) -> Int64

Get major units (truncated).
"""
function major_units(money::Money)::Int64
    divisor = Int64(10)^decimals(money.currency)
    div(money.minor_units, divisor)
end

"""
    minor_units(money::Money) -> Int64

Get minor units.
"""
function minor_units(money::Money)::Int64
    money.minor_units
end

"""
    money_add(a::Money, b::Money) -> Union{Money, Nothing}

Add two monetary values. Returns `nothing` on currency mismatch.
"""
function money_add(a::Money, b::Money)::Union{Money, Nothing}
    a.currency != b.currency && return nothing
    Money(a.minor_units + b.minor_units, a.currency)
end

"""
    money_sub(a::Money, b::Money) -> Union{Money, Nothing}

Subtract two monetary values. Returns `nothing` on currency mismatch.
"""
function money_sub(a::Money, b::Money)::Union{Money, Nothing}
    a.currency != b.currency && return nothing
    Money(a.minor_units - b.minor_units, a.currency)
end

"""
    money_mul(money::Money, scalar::Integer) -> Money

Multiply by scalar.
"""
function money_mul(money::Money, scalar::Integer)::Money
    Money(money.minor_units * Int64(scalar), money.currency)
end

"""
    money_div(money::Money, scalar::Integer) -> Union{Money, Nothing}

Divide by scalar. Returns `nothing` on division by zero.
"""
function money_div(money::Money, scalar::Integer)::Union{Money, Nothing}
    scalar == 0 && return nothing
    Money(div(money.minor_units, Int64(scalar)), money.currency)
end

"""
    is_zero(money::Money) -> Bool

Check if amount is zero.
"""
function is_zero(money::Money)::Bool
    money.minor_units == 0
end

"""
    is_positive(money::Money) -> Bool

Check if amount is positive.
"""
function is_positive(money::Money)::Bool
    money.minor_units > 0
end

"""
    is_negative(money::Money) -> Bool

Check if amount is negative.
"""
function is_negative(money::Money)::Bool
    money.minor_units < 0
end

"""
    money_abs(money::Money) -> Money

Get absolute value.
"""
function money_abs(money::Money)::Money
    Money(abs(money.minor_units), money.currency)
end

"""
    format_money(money::Money) -> String

Format money with currency symbol.
"""
function format_money(money::Money)::String
    dec = decimals(money.currency)
    divisor = Int64(10)^dec
    abs_units = abs(money.minor_units)
    major_part = div(abs_units, divisor)
    minor_part = mod(abs_units, divisor)
    sign_str = money.minor_units < 0 ? "-" : ""
    sym = symbol(money.currency)

    if dec == 0
        "$(sign_str)$(sym)$(major_part)"
    else
        minor_str = lpad(string(minor_part), dec, '0')
        "$(sign_str)$(sym)$(major_part).$(minor_str)"
    end
end

# Operator overloads for convenience
Base.:+(a::Money, b::Money) = something(money_add(a, b), error("Currency mismatch"))
Base.:-(a::Money, b::Money) = something(money_sub(a, b), error("Currency mismatch"))
Base.:*(m::Money, s::Integer) = money_mul(m, s)
Base.:*(s::Integer, m::Money) = money_mul(m, s)

# Comparison operators
Base.:(==)(a::Money, b::Money) = a.currency == b.currency && a.minor_units == b.minor_units
Base.:<(a::Money, b::Money) = a.currency == b.currency ? a.minor_units < b.minor_units : error("Currency mismatch")
Base.:>(a::Money, b::Money) = a.currency == b.currency ? a.minor_units > b.minor_units : error("Currency mismatch")
Base.:<=(a::Money, b::Money) = a.currency == b.currency ? a.minor_units <= b.minor_units : error("Currency mismatch")
Base.:>=(a::Money, b::Money) = a.currency == b.currency ? a.minor_units >= b.minor_units : error("Currency mismatch")

# Display methods
Base.show(io::IO, m::Money) = print(io, format_money(m))

end # module
